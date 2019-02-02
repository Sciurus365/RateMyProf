# data3 -------------------------------------------------------------------
library(tidyverse)
library(lubridate)
pat <- "./data3"
all <- read_csv(str_c(pat, "/","new.csv",sep = ""))
all <- all %>%
  filter(is.na(professor_name) | (professor_name != "NULL") & (professor_name != "professor_name")) %>%
  mutate(post_date_standard = mdy(post_date))
  
all_psy <- all %>%
  filter(str_detect(name_onlines, "PSY") |
           str_detect(name_onlines, "psy") |
           str_detect(name_onlines, "PY") |
           str_detect(name_onlines, "py")) %>%
  filter(str_detect(name_onlines, "[0-9]")) %>%
  mutate(course_code_number = 
           map_chr(
             .f = function(x){str_extract(x, "[0-9]+")},
             .x = name_onlines
           )) %>%
  mutate(course_level = 
           map_int(
             .f = function(x){as.integer(substr(x,1,1))},
             .x = course_code_number
           ))
length(unique(all_psy$professor_name))
qplot(all_psy$course_level)

ug_psy <- all_psy %>%
  filter(course_level >=1, course_level <=4)

p1 <- ggplot(data = ug_psy, mapping = aes(y = as.numeric(student_star), x = as.numeric(student_difficult), color = as.factor(course_level))) +
  #geom_jitter() +
  geom_smooth(method = "lm") +
  theme_bw()
  #scale_color_manual(values=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928'))

library(ggExtra)
ggMarginal(p1, type = "density", groupFill = TRUE)
# ↑ this is an important plot. 评分基本上和课程等级无关，但是在4xx的课上人们更多打中间的分？

qplot(data = ug_psy, x = post_date, y = star_rating)

# 标签数据清理
all_prof <- all %>%
  select(professor_name:num_student) %>%
  unique()

all_prof <- all_prof %>%
  mutate(
    tags_text = map(
      .x = tag_professor,
      .f = function(x){
        y <- str_split(x, pattern = "\\ \\([0-9]+\\)\\ \\ ")
        if(!is.na(y[[length(y)]])){
          y[[1]][length(y[[1]])] <- str_remove(y[[1]][length(y[[1]])], "\\ \\([0-9]+\\)")
        }
        return(y)
      }
    ),
    tags_freq = map(
      .x = tag_professor,
      .f = function(x){
        y <- str_extract_all(x, "[1-9]+?")
        return(y)
      }
    )
  )

all_tags_category <- unique(na.omit(c(unlist(all_prof$tags))))
length(unique(all_tags_category))

all_tags <- all_prof %>%
  filter(!is.na(tag_professor))

all_tags$tags_category <- list(unique(all_tags_category))

for(i in all_tags_category){
  all_tags[[i]] <- 0
  all_tags[[i]] <- as.numeric(all_tags[[i]])
}

for(i in 1:nrow(all_tags)){
  temp <- unlist(all_tags[i, "tags_text"][[1]])
  for(j in 1:length(temp)){
    all_tags[i, temp[j]] <- as.numeric(unlist(all_tags[i, "tags_freq"][[1]])[j])/as.numeric(all_tags[i, "num_student"][[1]])
  }
}

# all_tags$tag_sum <- rowSums(select(all_tags, `Caring`:`BEWARE OF POP QUIZZES`))

# 分析tag和评分
library(psych)
library(corrplot)
all_tags$star_rating <- as.numeric(all_tags$star_rating)
all_tags$diff_index <- as.numeric(all_tags$diff_index)
all_tags_m <- as.matrix(select(all_tags, star_rating, diff_index, `Caring`:`BEWARE OF POP QUIZZES`))
all_tags_m <- apply(all_tags_m, 2, as.numeric)
co <- cor(all_tags_m)
corrplot(co, method = "ellipse")

# NLP
library(tm)
data("stop_words")
all_comment <- all_psy
sentnrc <- get_sentiments("nrc")

all_comment <- all_comment %>%
  as.tibble() %>%
  mutate(post_year = year(post_date_standard)) %>%
  unnest_tokens(input = "comments", output = "words") %>%
  anti_join(stop_words, by = c("words" = "word")) %>%
  mutate(words = stemDocument(words)) #%>%
  #left_join(sentnrc, by = c("words" = "word"))

all_comment_summary <- all_comment %>%
  count(words, sort = TRUE) %>%
  ungroup()

all_comment_summary_by_year <- all_comment %>%
  count(post_year, words, sort = TRUE) %>%
  group_by(post_year) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  ungroup()

all_comment_summary_by_year_10 <- all_comment_summary_by_year %>%
  inner_join(all_comment_summary[1:10,]) %>%
  filter(post_year >= 2009)

qplot(data = all_comment_summary_by_year_10, x = post_year, y = proportion, color = as.factor(words)) +
  geom_smooth(se = FALSE)

qplot(data = all_comment, x = post_year)
