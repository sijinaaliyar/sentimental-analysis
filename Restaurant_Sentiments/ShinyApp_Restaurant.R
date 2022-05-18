#Word cloud of 30 restaurants - RShiny dashboard
# install.packages("dplyr")
# install.packages("tm")
# install.packages("tidytext")
# install.packages("textdata")
# install.packages("stringr")
# install.packages("tibble")
# install.packages("ggplot2")
# install.packages("wordcloud2")
# install.packages("reshape2")
# install.packages("shinydashboard")

library(dplyr)
library(tm)
library(tidytext)
library(textdata)
library(stringr)
library(tibble)
library(ggplot2)
library(wordcloud2)
library(reshape2)
library(shinydashboard)

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

head(rest_review_ds$review)

#Cleaning the text data
rest_review_ds$review <- tolower(rest_review_ds$review)
rest_review_ds$review <- removePunctuation(rest_review_ds$review)
rest_review_ds$review <- removeNumbers(rest_review_ds$review)
rest_review_ds$review <- stripWhitespace(rest_review_ds$review)

# Set the RShiny UI controls
ui <- fluidPage(
  
  titlePanel("Sentimental Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("rest_name", label = "Restaurant Name", 
                  choices = str_to_title(unique_restaurants, locale = "en"))
    ),
    mainPanel(
      wordcloud2Output("restaurant"),
      br(),
      br()
    )
  )
)

#Set the RShiny server code to find word cloud
server <- function(input, output) {  
  
  create_wordcloud_review <- function(data) {
    
    #Filter the review based on the selected restaurant
    rest_review <- data %>% filter(restaurant_name == tolower(input$rest_name))
    
    #Tokenism the review into words
    rest_review_word <- rest_review %>% mutate(linenumber = row_number()) %>% unnest_tokens(word, review) %>% 
      filter(!word %in% stop_words$word)
    
    #Create word cloud
    rest_review_word_cloud <- rest_review_word %>% filter(word != 'restaurant', word != 'food') %>% 
      filter(!word %in% stop_words$word) %>% 
      inner_join(get_sentiments("afinn")) %>%
      inner_join(get_sentiments("bing")) %>%
      inner_join(get_sentiments("nrc")) %>%
      count(word)
    
  wordcloud2(rest_review_word_cloud)  }
  
  #Render the word cloud
  output$restaurant <- renderWordcloud2({
    create_wordcloud_review(rest_review_ds)
  })
}

#Invoke the Shiny dashboard
shinyApp(ui = ui, server = server)
