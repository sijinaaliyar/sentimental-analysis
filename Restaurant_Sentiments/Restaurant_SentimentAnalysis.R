#Sentimental analysis of 30 restaurants
# install.packages("dplyr")
# install.packages("tm")
# install.packages("tidytext")
# install.packages("textdata")
# install.packages("stringr")
# install.packages("tibble")
# install.packages("ggplot2")
# install.packages("wordcloud")
# install.packages("reshape2")

library(dplyr)
library(tm)
library(tidytext)
library(textdata)
library(stringr)
library(tibble)
library(ggplot2)
library(wordcloud)
library(reshape2)

# Set the working directory 
setwd("D:\\Msc Data Science\\CourseWorks\\ASDM\\Sentimental Analysis")
getwd()

# Read the input data
review_ds <- read.csv("tourist_accommodation_reviews.csv", header = TRUE)

#Explore the data
names(review_ds)
head(review_ds)
tail(review_ds)
summary(review_ds)
str(review_ds)
dim(review_ds)

#Rename the columns
review_ds <- review_ds %>% 
  rename(
    restaurant_name = Hotel.Restaurant.name,
    review_date = Review.Date
  )

#Convert the column names to lower cases
colnames(review_ds) <- tolower(colnames(review_ds))

# Check for valid data
sum(is.na(review_ds))
sum(review_ds == "")
sum(is.null(review_ds))

# convert the restaurant names to lower cases
review_ds$restaurant_name <- tolower(review_ds$restaurant_name)

#Filter the data with 'restaurant' keyword
review_ds <- review_ds %>% filter(grepl('restaurant', restaurant_name))

#Select 30 restaurants
unique_restaurants <- head(unique(review_ds$restaurant_name), 30)

#Filter the reviews for the selected 30 restaurants
rest_review_ds <- review_ds[review_ds$restaurant_name %in% unique_restaurants,]

#Save the cleansed data for processing in SAS
write.csv(rest_review_ds, file="restaurant_opinion.csv")

head(rest_review_ds$review)

#Cleaning the text data
rest_review_ds$review <- tolower(rest_review_ds$review)
rest_review_ds$review <- removePunctuation(rest_review_ds$review)
rest_review_ds$review <- removeNumbers(rest_review_ds$review)
rest_review_ds$review <- stripWhitespace(rest_review_ds$review)

#Plot the frequency of words
total_rest_review_words <- rest_review_ds %>% 
                      mutate(linenumber = row_number()) %>% 
                      unnest_tokens(word, review) %>%
                      filter(!word %in% stop_words$word)


word_plot <- total_rest_review_words %>% count(word, sort = TRUE) %>% filter( n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab('Words in Review') +
  ylab('Count Of Words') +
  coord_flip()
word_plot + ggtitle("Most Popular Words in Review")

output_df <- data.frame()

# Find the sentiments of 30 hotels
for(item in unique_restaurants){
  rest_review <- rest_review_ds %>% filter(restaurant_name == item)
  
  #Tokenise the review into words
  rest_review_word <- rest_review %>% 
        mutate(linenumber = row_number()) %>% 
        unnest_tokens(word, review) %>% filter(!word %in% stop_words$word)
 
  #Create dataframe with count and value of words
  sentiments <- rest_review_word %>% 
    filter(word != 'restaurant', word != 'food') %>% 
    inner_join(get_sentiments("bing")) %>%
    inner_join(get_sentiments("afinn")) %>%
    inner_join(get_sentiments("nrc")) %>%
    count(word, sentiment, sort = TRUE)
  
  #Extract the sentiments
  positiveSentiments <- subset(sentiments, sentiment == "positive")
  negativeSentiments <- subset(sentiments, sentiment == "negative")
  
  positive_score <- aggregate(n ~ sentiment, data= positiveSentiments, sum)
  negative_score <- aggregate(n ~ sentiment, data = negativeSentiments, sum)
  
  #Calculate the sentiment scores
  total_score <- positive_score$n + negative_score$n
  
  final_pos_percent <- (positive_score$n*100)/total_score
  final_neg_percent <- (negative_score$n*100)/total_score
  
  out_pos_percent<-paste(round(final_pos_percent,2),"%")
  
  #Store the sentiments of hotels
  output = c(str_to_title(item, locale = "en"), positive_score$n ,negative_score$n, out_pos_percent)
  output_df = rbind(output_df, output)
}

#Set column names for the output
colnames(output_df)<-c("Restaurant Name", "Positve Score", "Negative Score", "Overall Positive Sentiment")
output_df



