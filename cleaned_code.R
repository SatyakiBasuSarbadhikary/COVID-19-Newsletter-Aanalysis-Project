
# required packages
library(tidyverse)
library(ggplot2)
library(vars)


# Set working directory to the project folder (assumes script is inside the project)
setwd("path/to/your/project/folder")
#we define dataset as df.
df=read_csv("dataset_newsletter_2026.csv")
 # frequency plot
df %>%
  group_by(date,theme) %>%
  mutate(
    count = n()
  ) %>%
  ggplot(aes(x = date,y = count,group = theme,col = theme)) +
  geom_line() +
  scale_y_continuous(breaks = seq(1,11,by = 2)) +
  theme_minimal() +
  theme(legend.position = 'bottom')

# see the counts
table(df$theme)

# merge the low frequency groups in one bucket
low_freq_themes = c('EMPLOYEES','INSPIRATIONAL','PSYCHOSOCIAL','TRAINING')
df2 = df %>%
  filter(date < as.Date('2023-01-01')) %>%
  mutate(
    theme = ifelse(theme %in% low_freq_themes,'OTHERS',theme)
  )
View(df2)
# do a better frequency plot
df2 %>%
  group_by(date,theme) %>%
  mutate(
    count = n()
  ) %>%
  ggplot(aes(x = date,y = count,group = theme,col = theme)) +
  geom_line() +
  scale_y_continuous(breaks = seq(1,11,by = 2)) +
  theme_minimal() +
  theme(legend.position = 'bottom')


# empirical estimators for the six themes
min_date = as.Date('2020-02-01') #min(df2$date)
max_date = max(df2$date)
emp_estimate = data.frame(
  date = seq.Date(min_date,max_date,by = 7),
  AWARENESS = 0,
  CITIZENS = 0,
  HOSPITALS = 0,
  OFFICIAL = 0,
  TRAVEL_ADVISORY = 0,
  OTHERS = 0,
  CASES = 0
)
nweeks = nrow(emp_estimate)
for (j in 1:nweeks){
  sub_df = df2 %>% filter(date <= emp_estimate$date[j])
  emp_estimate$AWARENESS[j] = mean(sub_df$theme == 'AWARENESS')
  emp_estimate$CITIZENS[j] = mean(sub_df$theme == 'CITIZENS')
  emp_estimate$HOSPITALS[j] = mean(sub_df$theme == 'HOSPITALS')
  emp_estimate$OFFICIAL[j] = mean(sub_df$theme == 'OFFICIAL')
  emp_estimate$TRAVEL_ADVISORY[j] = mean(sub_df$theme == 'TRAVEL_ADVISORY')
  emp_estimate$OTHERS[j] = mean(sub_df$theme == 'OTHERS')
  emp_estimate$CASES[j] = nrow(sub_df)
}

# visualization of the empirical estimates
p3=emp_estimate %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = AWARENESS,col = 'Awareness')) +
  geom_line(aes(y = CITIZENS,col = 'Citizens')) +
  geom_line(aes(y = HOSPITALS,col = 'Hospitals')) +
  geom_line(aes(y = OFFICIAL,col = 'Official')) +
  geom_line(aes(y = TRAVEL_ADVISORY,col = 'Travel Advisory')) +
  geom_line(aes(y = OTHERS,col = 'Others')) +
  theme_minimal() +
  xlab('Date') +
  ylab('Estimated probability') +
  theme(legend.position = 'right',
        legend.title = element_blank())

# extract covid data
covids = COVID19::covid19(country = 'India',start = min_date,end = max_date)

# merge the two datasets
df3 = merge(emp_estimate,covids[,c(2,3,4)],all.x = T,all.y = F)
df3[is.na(df3)] = 0

# run a simple vector autoregression
ymat = asin((2*df3[,c(2:7)] - 1)/(1+0.75/df3$CASES))
xmat = log(1 + df3[,c(9,10)])
model_output = VAR(
  y = ymat,
  p = 1,
  type = "const",
  exogen = as.matrix(xmat)
)
summary(model_output)

coef_output = data.frame(do.call(rbind,coef(model_output)))
colnames(coef_output) = c('Estimate','SE','t.score','p.value')
regnames = c(paste(colnames(ymat),'L1',sep = "."),'constant','confirmed','deaths')
coef_output = coef_output %>%
  mutate(
    Response = rep(colnames(ymat),each = 9),
    Regressor = rep(regnames,6),
    Significant = ifelse(p.value < 0.05,"Yes","No")
  )
rownames(coef_output) = 1:nrow(coef_output)
coef_output = coef_output %>%
  mutate(
    Response = rep(colnames(ymat), each = 9),
    Regressor = rep(regnames, 6),
    Significant = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )
rownames(coef_output) = 1:nrow(coef_output)

coef_output %>%
  filter(Regressor %in% c('constant','confirmed','deaths')) %>%
  ggplot(aes(x = Response,y = t.score)) +
  geom_line(aes(group = Regressor,col = Regressor)) +
  geom_hline(yintercept = 0,lty = 2) +
  geom_point(aes(pch = Significant),size = 2) +
  scale_shape_manual(values = c('Yes' = 19,'No' = 1)) +
  theme(legend.position = 'bottom')
#write.csv(emp_estimate)

p2=coef_output %>%
  filter(Regressor %in% c('constant', 'confirmed', 'deaths')) %>%
  mutate(
    
    Response=str_replace_all(Response,"_", " "),# Convert Response to title case
    Response = str_to_title(Response),
    Regressor = str_replace_all(Regressor, "_", " "),  # Replace underscores with spaces
    Regressor = str_to_title(Regressor)  # Convert Regressor to title case
  ) %>%
  ggplot(aes(x = Response, y = t.score)) +
  geom_line(aes(group = Regressor, col = Regressor)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(aes(pch = Significant), size = 2) +
  scale_shape_manual(values = c('Yes' = 19, 'No' = 1)) +
  labs(x = "Response", y = "t-Score", color = "Regressor", shape = "Significance") +
  theme_minimal() +
  theme(legend.position = 'bottom')




ggsave(filename = "EMPIRICALESTIMATES.pdf",
       
       plot = p3,
       
       width = 300,
       
       height = 200,
       
       units = "mm",
       
       dpi = 800)


ggsave(filename = "t scores.pdf",
       
       plot = p2,
       
       width = 300,
       
       height = 200,
       
       units = "mm",
       
       dpi = 800)






##############graph of frequency plot
p1=df2 %>%
  group_by(date, theme) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(
    theme = str_replace_all(theme, "_", " "),  
    theme = str_to_title(theme) 
  ) %>%
  ggplot(aes(x = date, y = count, group = theme, col = theme)) +
  geom_line() +
  scale_y_continuous(breaks = seq(1, 11, by = 2)) +
  theme_minimal() +
  
  theme(legend.position = 'bottom',
        
        legend.text = element_text(size = 15),
        
        legend.title = element_text(size = 15),
        
        axis.text = element_text(size = 15),
        
        axis.title = element_text(size = 15))+
  labs(x = "Date", y = "Count", color = "Theme") 

getwd()

ggsave(filename = "Sentiment_combined.pdf",
       
       plot = pq,
       
       width = 300,
       
       height = 200,
       
       units = "mm",
       
       dpi = 800)


getwd()





p3 <- emp_estimate %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = AWARENESS, col = 'Awareness')) +
  geom_line(aes(y = CITIZENS, col = 'Citizens')) +
  geom_line(aes(y = HOSPITALS, col = 'Hospitals')) +
  geom_line(aes(y = OFFICIAL, col = 'Official')) +
  geom_line(aes(y = TRAVEL_ADVISORY, col = 'Travel Advisory')) +
  geom_line(aes(y = OTHERS, col = 'Others')) +
  theme_minimal() +
  xlab('Date') +
  ylab('Estimated Probability') +
  theme(
    legend.position = 'bottom',legend.text = element_text(size = 15),
    
    legend.title = element_text(size = 15),
    
    axis.text = element_text(size = 15),
    
    axis.title = element_text(size = 15)
  ) +
  guides(col = guide_legend(title = "Theme"))





# Load necessary libraries
library(dplyr)
library(tidyr)

# Assuming df is your dataframe and 'topic' is the column to manipulate
# Create a new vector by manipulating the 'topic' column
new_vector <- df %>%
  mutate(topic_modified = sapply(strsplit(as.character(topic), "\\."), function(x) paste(x[-length(x)], collapse = "."))) %>%
  pull(topic_modified)  # Extract the modified column as a vector

# View the new vector
print(new_vector)
nrow(df)
############################################################################
df$new_word <- new_vector  # Adding new_vector as a new column in df
library(dplyr)

# Specified words list
words_to_plot <- c("awareness", "psychosocial", "inspirational", "official", "training", "travel advisory")

# Ensure the date column is in Date format if not already
df$date <- as.Date(df$date)

# Filter and prepare data
df_filtered <- df %>%
  mutate(word = tolower(new_word)) %>%
  filter(word %in% tolower(words_to_plot))
# Calculate frequency by date
word_frequencies_by_date <- df_filtered %>%
  group_by(date, word) %>%
  summarise(frequency = n(), .groups = "drop")

# Print the frequencies by date
print(word_frequencies_by_date)



library(ggplot2)

ggplot(word_frequencies_by_date, aes(x = date, y = frequency, color = word)) +
  geom_line() +
  scale_color_manual(values = c("red", "blue", "green", "yellow", "purple", "orange")) +
  labs(title = "Frequency of Specified Words Over Time",
       x = "Date",
       y = "Frequency",
       color = "Word") +
  theme_minimal()

##############################################
# load relevant libraries
library(readr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(readxl)
library(ClickClust)
library(rtweet)
library(tm)
library(qdapRegex)


# setting working directory
setwd("~/OneDrive - Indian Institute of Management/RESEARCH/COVID/Twitter_network_model/Datasets/")

# read the main data
df <- read_xlsx("tweets_sample_Jul.xlsx")

# cleaning urls from the tweets
twt_vec <- rm_url(new_vector)#this will be the vector

# build a corpus and specify the source to be character of vectors
# a corpus is a collection of written texts
myCorpus <- Corpus(VectorSource(twt_vec))
myCorpus <- tm_map(myCorpus, function(x) iconv(enc2utf8(x), sub = "byte"))

# convert myCorpus into lowercase
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# remove punctuation and numbers
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)

# other text cleaning steps
Textprocessing <- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub('\\b+RT', '', x) ## Remove RT
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub('[[:punct:]]', '', x) ## Remove Punctuations
  gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  gsub(' +',' ',x) ## Remove extra whitespaces
}
myCorpus <- tm_map(myCorpus,Textprocessing)

# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
View(myCorpus)

tdm <- DocumentTermMatrix(myCorpus) 
tdm.tfidf <- weightTfIdf(tdm)

tdm.tfidf <- removeSparseTerms(tdm.tfidf, 0.99) 
tfidf.matrix <- as.matrix(tdm.tfidf) # Cosine distance matrix (useful for specific clustering algorithms) 
dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

clustering.kmeans <- kmeans(tfidf.matrix, 10) 
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 10)

df$cluster = clustering.kmeans$cluster
MIB = df %>% filter(screen_name == 'COVIDNewsByMIB')
WHO = df %>% filter(screen_name == 'WHO')
CDC = df %>% filter(screen_name == 'CDCgov')
table(MIB$cluster)


write.csv(x = df,file = "Clusterered_df.csv",row.names = F)




###########################################################


df$date <- as.Date(df$date)

# Calculate the frequency of each theme per date
theme_counts <- df %>%
  group_by(date, theme) %>%
  summarise(count = n(), .groups = "drop")  # Count occurrences


# Define words of interest
words_of_interest <- c("Awareness", "Travel advisory", "Hospital citizen", "Official")

# Create a regex pattern that matches any of the words (case insensitive)
pattern <- paste0("\\b(", paste(words_of_interest, collapse = "|"), ")\\b", ignore.case = TRUE)

# Filter rows where 'new_vector' contains any of the words and calculate the length of the content
lengths_data <- df %>%
  mutate(new_vector = tolower(new_vector)) %>%  # Convert to lower case for case insensitive matching
  filter(str_detect(new_vector, pattern)) %>%  # Filter rows containing the keywords
  mutate(content_length = nchar(new_vector)) %>%  # Calculate the length of content
  select(date, content_length)  # Select only date and content length columns

# View the results
print(lengths_data)



df$date <- as.Date(df$date)
df$new_vector <- as.character(df$new_vector)

# Define the words of interest and create a case-insensitive pattern
words_of_interest <- c("Awareness", "Travel advisory", "Hospital citizen", "Official")
pattern <- paste(words_of_interest, collapse = "|")
pattern <- tolower(pattern)  # Make the pattern case-insensitive

# Initialize the column for content length
df$content_length <- NA_integer_

# Loop through each row to apply the checks and calculations
for (i in 1:nrow(df)) {
  if (grepl(pattern, tolower(df$new_vector[i]))) {
    df$content_length[i] <- nchar(df$new_vector[i])
  }
}

# Filter and view the results
results <- df[!is.na(df$content_length), c("date", "new_vector", "content_length")]
print(results)









# Define the words of interest
words_of_interest <- c("AWARENESS", "TRAVEL_ADVISORY", "PSYCHOSOCIAL", "INSPIRATIONAL", "OFFICIAL", "TRAINING")

# Prepare the data: Extract occurrences of these words/phrases from the theme column
theme_data <- df %>%
  mutate(theme = toupper(theme)) %>%  # Convert theme to upper case for case insensitive matching
  filter(str_detect(theme, str_c(words_of_interest, collapse = "|"))) %>%  # Filter rows that contain any of the words
  mutate_at(vars(theme), ~str_extract(., str_c(words_of_interest, collapse = "|"))) %>%  # Extract matching words
  group_by(date, theme) %>%
  summarise(count = n(), .groups = "drop")  # Count the occurrences by date







library(dplyr)
library(stringr)

# Define words of interest
words_of_interest <- c("Awareness", "Travel advisory", "Hospital citizen", "Official")

# Create a case-insensitive regex pattern that matches any of the words
pattern <- paste0("(?i)\\b(", paste(words_of_interest, collapse = "|"), ")\\b")
# Process the data
lengths_data <- df %>%
  mutate(
    new_vector = tolower(new_vector),  # Convert to lower case
    contains_keyword = str_detect(new_vector, pattern)  # Detect keywords
  )

# Optionally, you can proceed to filter and calculate content length
filtereddata <- lengths_data %>%
  filter(contains_keyword) %>%
  mutate(content_length = nchar(new_vector))%>%dplyr::select(date,content_length)
filtered_data$da
# View the results
print(filtered_data)
#####################################################################################################




#################newsletter length for each theme###################
library(dplyr)
library(stringr)
library(ggplot2)

# Updated list of keywords, already in uppercase
keywords <- c("AWARENESS", "OFFICIAL", "CITIZENS", "HOSPITALS", "TRAVEL_ADVISORY")

# Update the dataframe to correct categorization
df <- df %>%
  mutate(
    # Convert theme to uppercase to match against keywords
    theme_upper = toupper(theme),
    # Check if the transformed theme is in the keywords and categorize accordingly
    category = if_else(theme_upper %in% keywords, theme_upper, "OTHERS"),
    # Calculate the length of the original theme content
    #content_length = nchar(new_word)
    content_length = str_count(new_word, "\\S+")
  )
View(df)





summary_data <- df %>%
  group_by( date,category) %>%
  summarise(average_length = mean(content_length), .groups = "drop")

p4=ggplot(summary_data, aes(x = date, y = average_length, color = category)) +
  geom_line() +  # Line plot for each category with thicker lines
  geom_point(size = 1) +  # Add larger points to each data point
  scale_color_manual(values = c(
    "AWARENESS" = "blue",
    "OFFICIAL" = "red",
    "CITIZENS" = "purple",
    "HOSPITALS" = "orange",
    "TRAVEL_ADVISORY" = "green",
    "OTHERS" = "grey"
  ), labels = c("Awareness", "Official", "Citizens", "Hospitals", "Travel Advisory", "Others")) +  # Custom colors and labels
  labs(title = "Average Newsletter Content Length Over Time",
       x = "Date",
       y = "Average Content Length",
       color = "Themes") +  # Title for the legend
  theme_minimal() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),  # Enhance x-axis text size
        axis.text.y = element_text(size = 15),  # Enhance y-axis text size
        axis.title.x = element_text(size = 15, face = "bold"),  # Enhance x-axis title
        axis.title.y = element_text(size = 15, face = "bold"),  # Enhance y-axis title
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Centered and bold plot title
        legend.title = element_text(size = 15, face = "bold"),  # Enhance legend title
        legend.text = element_text(size = 15)  # Enhance legend text
  )


##########################################################################################################################




#########overall sentiment score############## 
# Perform sentiment analysis
Sentiment_analysis <- df %>%
  filter(theme %in% selected_themes) %>%
  mutate(sentiment_score = get_sentiment(new_word, method = "syuzhet")) %>%
  group_by(date, theme) %>%
  summarise(avg_sentiment = mean(sentiment_score), .groups = 'drop')

# Combine sentiment scores across all themes for each date
overall_sentiment <- Sentiment_analysis %>%
  group_by(date) %>%
  summarise(overall_sentiment = mean(avg_sentiment, na.rm = TRUE))

# Plot the overall sentiment trend over time
pq=ggplot(overall_sentiment, aes(x = date, y = overall_sentiment)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 2) +
  labs(title = "Overall Sentiment Trend Over Time",
       x = "Date",
       y = "Overall Sentiment Score") +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),axis.text = element_text(size = 15)
  )

#################################################################################################################################


##############Sentiment VAR#########################################################################

library(dplyr)

# Reshape data into wide format with dates as rows and themes as columns
wide_sentiment <- sentiment_analysis %>%
  dplyr::select(date, theme, avg_sentiment)%>%tidyr::pivot_wider(names_from = theme, values_from = avg_sentiment) %>%
  arrange(date)
wide_sentiment[is.na(wide_sentiment)] = 0


# Convert to time series format
ts_sentiment <- ts(wide_sentiment[,-1], start = c(2020, 1), frequency = 365)
#ts_sentiment=na.omit(ts_sentiment)
# Step 3: Fit the VAR(1) model
var_model <- VAR(ts_sentiment, p = 1, type = "const")

# Step 4: Summary of the VAR model
summary(var_model)
############################################################################################





##########################################coreections


DF=df

# Define selected themes
selected_themes <- c("TRAVEL_ADVISORY", "HOSPITALS", "CITIZENS", "AWARENESS", "OFFICIAL")

# Perform sentiment analysis and classify unrecognized themes as "OTHERS"
sentiment_analysis <- DF %>%
  mutate(theme = case_when(
    theme %in% selected_themes ~ theme,
    TRUE ~ "OTHERS"  # Classify as "OTHERS" if not in selected themes
  )) %>%
  mutate(sentiment_score = get_sentiment(new_word, method = "syuzhet")) %>%
  group_by(date,theme, new_word) %>%
  summarise(avg_sentiment = mean(sentiment_score), .groups = 'drop')

# Print the sentiment analysis results
print(sentiment_analysis)
# Update the `theme` labels to desired names
sentiment_analysis <- sentiment_analysis%>%
  mutate(theme = case_when(
    theme == "AWARENESS" ~ "Awareness",
    theme == "TRAVEL_ADVISORY" ~ "Travel Advisory",
    theme == "HOSPITALS" ~ "Hospitals",
    theme == "CITIZENS" ~ "Citizens",
    theme == "OFFICIAL" ~ "Official",
    theme=="OTHERS"~"Others",
    TRUE ~ theme  # Keep other labels unchanged if present
  ))

# Create the plot with enhanced styling
ps <- ggplot(sentiment_analysis, aes(x = date, y = avg_sentiment, color = theme)) +
  geom_line(size = 0.6) +  # Line plot
  geom_point(size = 3) +  # Points for each average sentiment
  labs(title = "Average Sentiment Scores by Theme Over Time",
       x = "Date",
       y = "Average Sentiment Score",
       color = "Theme") +  # Set the legend title to 'Theme'
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16, face = "bold"),  # Enhance legend title
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16, face = "bold"),  # Enhance axis labels
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # Centered and bold title
  )

###################################################################################################


library(ggplot2)
library(dplyr)

Summary_data=summary_data
Summary_data$theme=Summary_data$category
Summary_data<- Summary_data %>%
  mutate(theme = case_when(
    theme == "AWARENESS" ~ "Awareness",
    theme == "TRAVEL_ADVISORY" ~ "Travel Advisory",
    theme == "HOSPITALS" ~ "Hospitals",
    theme == "CITIZENS" ~ "Citizens",
    theme == "OFFICIAL" ~ "Official",
    theme=="OTHERS"~"Others",
    TRUE ~ theme  # Keep other labels unchanged if present
  ))
# Merge the two datasets based on `date` and `theme`
combined_data <- sentiment_analysis %>%
  inner_join(Summary_data, by = c("date", "theme"))
# Create separate plots for each theme
plot_list <- lapply(unique(combined_data$theme), function(thm) {
  # Filter data for the specific theme
  theme_data <- combined_data %>% filter(theme == thm)
  
  # Plot for average sentiment score
  p1 <- ggplot(theme_data, aes(x = date, y = avg_sentiment)) +
    geom_line(color = "blue", size = 1) +
    labs(
      title = paste("Average Sentiment Score:", thm),
      x = "Date",
      y = "Sentiment Score"
    ) +
    theme_minimal()
  
  # Plot for average content length
  p2 <- ggplot(theme_data, aes(x = date, y = average_length)) +
    geom_line(color = "red", size = 1) +
    labs(
      title = paste("Average Content Length:", thm),
      x = "Date",
      y = "Content Length"
    ) +
    theme_minimal()
  
  # Combine the two plots vertically
  p1 / p2
})

# Combine all the individual panels into one layout (2 rows, 3 columns)
final_plot <- (plot_list[[1]] | plot_list[[2]] | plot_list[[3]]) /
  (plot_list[[4]] | plot_list[[5]] | plot_list[[6]])

# Display the final combined plot
print(final_plot)
#####################################################################################
#
