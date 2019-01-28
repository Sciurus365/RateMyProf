# data3 -------------------------------------------------------------------
library(tidyverse)
pat <- "./data3"
all <- read_csv(str_c(pat, "/","new.csv",sep = ""))
all <- all %>%
  filter(is.na(professor_name) | (professor_name != "NULL") & (professor_name != "professor_name"))
length(unique(all$professor_name))
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
library(lubridate)
ug_psy <- ug_psy %>%
  mutate(post_date_standard = mdy(post_date))

qplot(data = ug_psy, x = post_date, y = star_rating)
