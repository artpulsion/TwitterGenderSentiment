
# Description : study the emotional magnitude of tweets by gender and localization
# School : ENC 
# Date : 29 novembre 2018

# Load packages, or install them if not 
library(tidyverse)
library(ggthemes)
library(reticulate)
library(tidytext)
library(moments)

# Load python environment
use_virtualenv(virtualenv = "enc_venv", required = TRUE)

# Source python module
source_python("python/processing.py")

# Launch processing() function and select the pertinent variables for our analysis
dt <- processing() %>% select(`_unit_id`, gender, description, Description, text, Tweets, tweet_created, tweet_coord, created, fav_number, retweet_count, user_timezone)

# Rename columns with intelligible words
colnames(dt) <- c("id", "gender", "profil_description", "cln_profile_description", "tweet",  "cln_tweet", "tweet_created_at", "tweet_loc", "profile_created_at", "count_favorite", "retwweet_count", "timezone")

# Restructure timezone and gender column, eliminate words with length > 3 letters 
dt$timezone <- str_replace(dt$timezone, " \\(.*\\)", "") # remove bracket in timezone
dt$gender <- dt$gender %>% as.character() # gender as character not a list
dt$cln_tweet <- lapply(dt$cln_tweet, function(x) { str_extract_all(x, '\\w{3,}') %>% unlist() }) # remove words were length < 3

# Count the number of tweet by Timezone (1.) and remove the unknown timezone (2.) -> 7798 rows removed (38%)
n_tweets_by_loc <- dt %>% group_by(timezone) %>% summarise(count = n()) %>% arrange(desc(count)) # 1.
n_tweets_by_loc <-  n_tweets_by_loc[!n_tweets_by_loc$timezone=="NaN",] # 2.

# Plot the top 10 occurences of Tweet by Timezone
p1 <-
  n_tweets_by_loc %>% slice(1:10) %>%
  ggplot(aes(x=timezone, y=count, fill=timezone)) +
  geom_bar(stat="identity", width=0.8) +
  ggtitle("Nombre de tweet par timezone (top 10)") +
  xlab("Timezone") + ylab("Nombre de tweet") +
  theme_solarized() + scale_colour_solarized() +
  theme(plot.title = element_text(size = 13, face = "bold")) +
  scale_fill_discrete(name="Timezone")

# Count tweet by gender
# -> male : 6194 tweets , female : 6700 tweets
dt %>% group_by(gender) %>%
  summarise(count = n()) %>%
  filter(gender %in% c("male","female"))

# Count tweet by gender and timezone 
gender_tz <- dt %>% group_by(gender, timezone) %>%
  summarise(count = n()) %>% filter(gender %in% c("male", "female")) %>%
  .[!.$timezone=="NaN",] %>% arrange(desc(count))

# Select 10 first lines (top 10)
top_tz <- n_tweets_by_loc[1:10,]$timezone

#
# Viz !
#
p2 <- 
  gender_tz %>% filter(timezone %in% top_tz) %>%
  ggplot(mapping = aes(x = timezone, y = count, fill = gender)) + 
  geom_bar(stat="identity", position = "dodge") + 
  ggtitle("Repartition des tweets par Genre et Timezone (top 10)") +
  xlab("Timezone") + ylab("Nombre de tweet") +
  theme_solarized() + scale_colour_solarized() +
  theme(plot.title = element_text(size = 13, face = "bold")) +
  scale_fill_discrete(name="Genre")



########################
## Sentiment analysis ##
########################

# Create two dataframes, on by each gender
male_tweets <- dt %>% filter(gender == "male")
female_tweets <- dt %>% filter(gender == "female")

# Select one specific timezone
# [1] "Eastern Time"  "Pacific Time"  "Central Time"  "London"        "Atlantic Time" "Quito"         "Amsterdam"    
# [8] "Arizona"       "Mountain Time" "Casablanca"
selected_tz <- top_tz[8]

# Filter tweets from male for the current timezone
eastern_tz_words_male <- 
  male_tweets %>% filter(timezone == selected_tz) %>% 
  pull(cln_tweet) %>% unlist() %>% data.frame(word = .) %>% cbind(gender = "male")

# Filter tweets from female for the current timezone
eastern_tz_words_female <- 
  female_tweets %>% filter(timezone == selected_tz) %>%
  pull(cln_tweet) %>% unlist() %>% data.frame(word = .) %>% cbind(gender = "female")

# Aggregate both
eastern_tz_words_both <- 
  rbind(eastern_tz_words_female, eastern_tz_words_male) %>%
  inner_join(get_sentiments("afinn")) %>% distinct()

# Moments (kurtosis and skewness)
stat_tbl <- 
  eastern_tz_words_both %>% group_by(gender) %>%
  summarise(
    mean = mean(score),
    sd = sd(score), 
    kurtosis = kurtosis(score),
    skew = skewness(score)
)


## ----
### Viz !
## ----

## -> Histogram 
ggplot(data = eastern_tz_words_both, aes(x=score, color=gender)) + 
  geom_histogram(fill="white", position="dodge")

## -> Density
ggplot(data = eastern_tz_words_both, aes(x=score, color=gender, fill=gender)) + 
  geom_density(alpha=.2) +
  ggtitle(sprintf("Emotional magnitude of tweets by gender in %s", selected_tz)) 

