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

# =============================================================================
# Determine which seasons are best rated by viewers
### by rating
office_summary <- office %>%
  group_by(season.x) %>%
  summarize(avg_rating = mean(imdb_rating),
            avg_viewers = mean(us_viewers)
            )
            
view(office_summary)

ggplot(office_summary, aes(x = season.x, y = avg_rating, color = avg_rating)) +
  geom_line() +
  labs(x = "Season",
       y = "IMDB Rating",
       title = "The Office Series Ratings",
       subtitle = "Season 1 - Season 9") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  theme_minimal()

## by viewers
ggplot(office_summary, aes(x = season.x, y = avg_viewers, color = avg_viewers)) +
  geom_line() +
  labs(x = "Season",
       y = "Avg US Viewers",
       title = "The Office Series Viewers",
       subtitle = "Season 1 - Season 9") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  theme_minimal()

### On average, season 3 was rated the highest, though season 5 had the most viewers due to a 
###   2 part episode airing immediately after Super Bowl XLIII, which significantly increased the avg.
# =============================================================================
# Michael Scott's impact to ratings and viewers

## Michael Scott was the Regional Manager starting in season 1 and departed at the end of season 7
## It's obvious from the previous section that ratings and viewers significantly decreased in season 8 & 9 when compared to
##    the previous seasons, but can we confidently say it was due to Michael Scott's contract with the series ending?


### Ratings
ggplot(office_summary, aes(x = season.x, y = avg_rating, color = avg_rating)) +
  geom_line() +
  geom_point() +
  labs(x = "Season",
       y = "IMDB Rating",
       title = "The Office Series Ratings",
       subtitle = "Season 1 - Season 9 Average") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  annotate("text", x = 1.8, y = 7.92, label = "Pilot Season") +
  annotate("text", x = 8.2, y = 8.33, label = "Michael's Final Season") +
  theme_minimal()
  
ggplot(office, aes(x = original_air_date, y = imdb_rating)) +
  geom_line(aes(color = season.x)) +
  scale_color_identity() +
  geom_smooth(se = FALSE, color = "red") +
  labs(title = "The Office Series Ratings by Episode",
       subtitle = "Season 1 - Season 9", 
       x = "Air Date", y = "IMDB Rating") +
  theme_minimal()
### There is a clear dip in the final season of the series where it reaches some of the highest ratings throughout the series

## Separate season 9 to investigate the increased ratings
season_9 <- office %>%
  filter(season.x == 9)

ggplot(season_9, aes(x = episode_num_in_season, y = imdb_rating)) +
  geom_line() +
  geom_point() # episodes 22/23 & 24/25 have the same ratings

season_9[24:27, ] # 22/23 & 24/25 are 2-part episodes, which explain the ratings
### The 2-part finale has the highest ratings of the season. This is also the episode where Michael Scott makes an appearance

season_9_no_finale <- season_9 %>%
  filter(episode_num_in_season <= 23) # remove finale (with Michael) to compare avg rating

mean(season_9$imdb_rating) # 7.98
mean(season_9_no_finale$imdb_rating) # 7.84
(7.84 - 7.98) / 7.84
### Avg rating drops by 0.14 (1%) when removing the episode without Michael Scott

### Based on charts showing the drop in ratings once Michael left and the increase in ratings when 
###  Michael made a cameo in the finale, I feel confident to conclude ratings were impacted by his departure


## Viewers
ggplot(office_summary, aes(x = season.x, y = avg_viewers, color = avg_viewers)) +
  geom_line() +
  geom_point() +
  labs(x = "Season",
       y = "Avg Viewers",
       title = "The Office Series Viewers",
       subtitle = "Season 1 - Season 9 Average") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  annotate("text", x = 1.8, y = 6366667, label = "Pilot Season") +
  annotate("text", x = 8.2, y = 7300385, label = "Michael's Final Season") +
  theme_minimal()
### average viewers experience a steady decrease starting in season 5, though a significant
###   decrease from season 7 though 9

office_summary <- office_summary %>%
  mutate(difference = avg_viewers-lag(avg_viewers, default = first(avg_viewers))) %>% # calculate value change season over season
  mutate(pct_change = (avg_viewers - lag(avg_viewers)) / avg_viewers * 100) %>% # calculate % change season over season
  mutate(pos = pct_change >= 0) # True if positive number

ggplot(office_summary, aes(x = season.x, y = pct_change, fill = pos)) +
  geom_col() +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  labs(title = "The Office",
       subtitle = "Percent Change in Viewers Season over Season",
       x = "Season", y = "% Change") +
  theme_minimal()
### season 8 (36%) and season 9 (28%) experience the largest change in viewership throughout the series

## individual episode view
ggplot(office, aes(x = original_air_date, y = us_viewers)) +
  geom_line(aes(color = season.x)) +
  scale_color_identity() + 
  theme_minimal() 
### Outlier in season 5 due to episodes following the super bowl

office_no_stress <- office %>%
  filter(title.x !="Stress Relief") # removed the outlier to get a better view of the data

ggplot(office_no_stress, aes(x = original_air_date, y = us_viewers)) +
  geom_line(aes(color = season.x)) +
  scale_color_identity() + 
  geom_smooth(se = FALSE, color = "red") +
  labs(title = "The Office Season Viewers",
       subtitle = "Season 1 - 9 (minus Stres Relief)", 
       x = "Air Date", y = "US Viewers") +
  theme_minimal() 
### Similar to what we saw with ratings, there is an obvious jump in the final season after a consistent downward trend

ggplot(season_9, aes(x = episode_num_in_season, y = us_viewers)) + 
  geom_line() +
  geom_point() +
  labs(title = "The Office Season 9 Viewers",
       x = "Episode Number", y = "US Viewers") +
  theme_minimal()
### the final two episodes are the highest of the season, which are the episodes Michael Scott makes a cameo

mean(season_9$us_viewers) # 4217037
mean(season_9_no_finale$us_viewers) # 4099200
(4099200 - 4217037) / 4099200 # -0.029

### Based on the significant change each season after Michael left (36%) and (27%) in the final two seasons of the show,
###   plus the (3%) decrease in avg viewers once the episodes with Michael are removed from the final season, I can
###   confidently conclude Michael Scott's departure negatively impacted viewership

# =============================================================================
# Seasons & Episodes Recommended to watch

# Seasons
office <- office %>% 
  mutate(recommend = if_else(imdb_rating > 8.0, "Yes", "No")) %>%
  mutate(recommend = as.factor(recommend))

library(randomForest)
rf <- randomForest(recommend ~ us_viewers + total_votes + imdb_rating,
                   data = office)

predicted_rf <- rf %>%
  predict(newdata = office, type = "prob")

office <- office %>%
  mutate(glm_predict = predict(glm, newdata = office, type = "response")) %>%
  mutate(predicted_rf = predicted_rf[, "Yes"])


## visualize yes/no 
ggplot(office, aes(x = recommend, fill = recommend))+
  geom_bar() +
  facet_wrap(vars(season.x)) +
  labs(title = "The Office Season Recommendations",
       subtitle = "based on IMDB Rating of 8.0+",
       x = "Recommendation", y = "")

## view count of yes/no
recommend <- office %>%
  group_by(season.x) %>%
  count(recommend) %>%
  spread(recommend, n)

print(recommend)

## episodes have a 100% recommendation
office_episode_watch <- office %>%
  filter(predicted_rf == 1.00)

office_episode_watch[,c(1, 2, 4, 10)] # details of episodes with 100% recommendation

## episodes with a 0% recommendation
office_episode_avoid <- office %>%
  filter(predicted_rf == 0.00)

office_episode_avoid[,c(1, 2, 4, 10)] # details of episodes with 0% recommendation

ggplot(office, aes(x = season.x, fill = recommend)) +
  geom_bar() +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  labs(title = "The Office",
       subtitle = "Recommendation by Season",
       x = "Season", y = "") +
  theme_minimal()
### the latter half of the series has more 0% recommendations

# =============================================================================


