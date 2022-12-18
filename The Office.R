# Load Packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

# Working directory
# setwd("~/Desktop/GitHub/The-Office")

# Import Data

episodes <- read_excel("the_office_episodes.xls", 
                                  col_types = c("numeric", "numeric", "numeric", 
                                                "text", "text", "text", "date", "numeric", 
                                                "numeric"))
str(episodes)

imdb <- read_excel("the_office_imdb.xls", 
                              col_types = c("numeric", "numeric", "text", 
                                            "date", "numeric", "numeric", "text"))
str(imdb)

office <- episodes %>%
inner_join(imdb, by = c("original_air_date" = "original_air_date"))
str(office)

office <- office %>%
  select(season.x, episode_num_in_season, episode_num_overall, title.x,
         directed_by, written_by, original_air_date, us_viewers, total_votes, desc, imdb_rating)

# Explore
## by rating
ggplot(office, aes(x = original_air_date, y = imdb_rating)) +
  geom_line() +
  labs(x = "Air Date",
       y = "IMDB Rating",
       title = "The Office Series Ratings",
       subtitle = "Season 1 - Season 9") +
  geom_smooth(se = FALSE, color = "red") +
  theme_minimal()

## by viewers
ggplot(office, aes(x = original_air_date, y = us_viewers)) +
  geom_line() +
  labs(x = "Air Date",
       y = "US Viewers",
       title = "The Office Series Viewers",
       subtitle = "Season 1 - Season 9") +
  geom_smooth(se = FALSE, color = "red") +
  theme_minimal()
### obvious outlier in 2009. Confirmed accurate data point. Episodes aired immediately
###     following Super Bowl XLIII which increased viewers for the 2 part episode

office <- office %>% 
  mutate(recommend = if_else(imdb_rating > 8.0, "Yes", "No"))

ggplot(office, aes(x = recommend, fill = recommend)) +
  geom_bar() +
  facet_wrap(vars(season.x)) +
  labs(y = "Number of Episodes",
       title = "Recommended Episodes by Season")
### With the exception of pilot season (1), there was a switch from "Yes" to "No"
###   starting in season 8 and continuing through season 9

# Logistic Prediction Model
library(rsample)
set.seed(27)
split <- initial_split(office, prop = 0.7)
train <- training(split)
test <- testing(split)

logistic <- glm(as.factor(recommend) ~ us_viewers + total_votes + imdb_rating,
                data = train, family = "binomial")
summary(logistic) 

# RandomForest Prediction Model
library(randomForest)
rf <- randomForest(as.factor(recommend) ~ us_viewers + total_votes + imdb_rating,
                   data = train, ntree = 1000, importance = TRUE)
plot(rf)
varImpPlot(rf) # indicates imdb_rating impacts the prediction accuracy the most

library(cutpointr)
test <- test %>% 
  mutate(prediction = predict(rf, newdata = test, type = "prob") [, 2])

### want to add roc for logistic & randomForest models

rf_office <- randomForest(as.factor(recommend) ~ us_viewers + total_votes + imdb_rating,
                   data = office, ntree = 1000, importance = TRUE)
office <- office %>% 
  mutate(prediction = predict(rf_office, newdata = office, type = "prob") [, 2])

seasons <- office %>%
  group_by(season.x) %>%
  summarize(season_recommend_num = mean(prediction),
            avg_viewer = mean(us_viewers)) %>%
  mutate(season_recommend_char = if_else(season_recommend_num > 0.60, "Yes", "No"))

ggplot(seasons, aes(x = season.x, y = season_recommend_num)) +
  geom_line(color = "blue") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  annotate("text", x = 8.3, y = 0.65, 
           label = "Michael's Last Season") +
  annotate("text", x = 1.8, y = 0.36, 
           label = "Pilot Season") +
  labs(title = "Avg Recommendation by Season",
       subtitle = "The Office",
       x = "Season", y = "Avg Recommendation") +
  geom_point(color = "black")

view(seasons)
(.15-.65)/.65 # 0.50 (76%) decrease in recommending season 7 to season 8

ggplot(seasons, aes(x = season.x, y = avg_viewer)) +
  geom_line(color = "blue") +
  geom_point() +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  labs(title = "Avg Viewers by Season", 
       subtitle = "The Office",
       x = "Season", y = "Avg US Viewers") +
  annotate("text", x = 1.8, y = 6366667, label = "Pilot Season") +
  annotate("text", x = 8.3, y = 7300385, label = "Michael's Final Season") +
  annotate("text", x = 6.5, y = 9164000, label = "Episodes after Super Bowl")


