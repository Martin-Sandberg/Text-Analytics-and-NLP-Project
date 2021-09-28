################################################################################
###### IMPORTING LIBRARIES
################################################################################

library(readtext)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)   
library(reshape2)  

################################################################################
###### READING THE FILE
################################################################################

#folder_url <- "https://drive.google.com/drive/folders/1kfsvP5EmVE4pIJwl4IsBznYJpoYruBUB?usp=sharing"
#folder <- drive_get(as_id(folder_url))
#file_names <- drive_ls(path = folder)

#Location of the files
surveys_local_path <- "/Users/nimiipatel/Documents/Grad School/Data 5317/Team12_TeslaMars/Surveys"

#Reading the file
setwd(surveys_local_path)
survey_file_names <- list.files(path = surveys_local_path)

readFile <- function(fileName) {
  file_data <- data.frame(answer = read_lines(fileName, skip_empty_rows = TRUE)) %>%
                          mutate(question = row_number(),
                                 file_name = fileName)
  return(file_data)
}

################################################################################
###### TOKENIZING THE DATA
################################################################################

#Binding rows
survey_data <- bind_rows(lapply(survey_file_names, readFile))

#Tokenizing the data
survey_tokens <- survey_data %>%
  unnest_tokens(word, answer) %>%
  anti_join(stop_words) %>%
  group_by(question) %>%
  count(file_name, word, sort = TRUE)

################################################################################
###### FILTERS BY QUESTIONS
################################################################################

#Filtering by questions
question_01 <- survey_tokens %>%
  filter(question == "1")  %>%
  count(word, sort = TRUE)

question_02 <- survey_tokens %>%
  filter(question == "2")  %>%
  count(word, sort = TRUE)

question_03 <- survey_tokens %>%
  filter(question == "3")  %>%
  count(word, sort = TRUE)

question_04 <- survey_tokens %>%
  filter(question == "4")  %>%
  count(word, sort = TRUE)

question_05 <- survey_tokens %>%
  filter(question == "5")  %>%
  count(word, sort = TRUE)

question_06 <- survey_tokens %>%
  filter(question == "6")  %>%
  count(word, sort = TRUE)

################################################################################
###### HISTOGRAM PLOTS
################################################################################

### Ploting top words in questions ###
freq_hist <- question_01 %>%
  top_n(10) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(x=word, y=n, fill=10))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

freq_hist <- question_02 %>% #stop at n7
  top_n(7) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(x=word, y=n, fill=10))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

freq_hist <- question_03 %>%
  top_n(8) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(x=word, y=n, fill=10))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

freq_hist <- question_04 %>%
  top_n(8) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(x=word, y=n, fill=10))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

freq_hist <- question_05 %>%
  top_n(7) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(x=word, y=n, fill=10))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

freq_hist <- question_06 %>%
  top_n(6) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(x=word, y=n, fill=10))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

################################################################################
###### SENTIMENT ANALYSIS BY QUESTION - CLOUD_WORDS
################################################################################

nrc <- get_sentiments("nrc") 

question_01 %>%                                   
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort=TRUE) %>%                     
  acast(word ~sentiment, value.var="n", fill=0) %>%         
  comparison.cloud(colors = c("grey10", "gray60"),          
                   max.words=100, scale = c(1,0.01))   

question_02 %>%                                   
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort=TRUE) %>%                     
  acast(word ~sentiment, value.var="n", fill=0) %>%         
  comparison.cloud(colors = c("grey10", "gray60"),          
                   max.words=100, scale = c(1,0.01)) 

question_03 %>%                                   
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort=TRUE) %>%                     
  acast(word ~sentiment, value.var="n", fill=0) %>%         
  comparison.cloud(colors = c("grey10", "gray60"),          
                   max.words=100, scale = c(1,0.01))   

question_04 %>%                                   
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort=TRUE) %>%                     
  acast(word ~sentiment, value.var="n", fill=0) %>%         
  comparison.cloud(colors = c("grey10", "gray60"),          
                   max.words=100, scale = c(1,0.01)) 

question_05 %>%                                   
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort=TRUE) %>%                     
  acast(word ~sentiment, value.var="n", fill=0) %>%         
  comparison.cloud(colors = c("grey10", "gray60"),          
                   max.words=100, scale = c(1,0.01))   

question_06 %>%                                   
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort=TRUE) %>%                     
  acast(word ~sentiment, value.var="n", fill=0) %>%         
  comparison.cloud(colors = c("grey10", "gray60"),          
                   max.words=100, scale = c(1,0.01))   

################################################################################
#### FREQUENCIES OF WORDS BY RANK
################################################################################

freq_by_rank <- survey_tokens %>%
  group_by(question) %>%
  mutate(rank = row_number(),
         `term frequency` = n/sum(n))
freq_by_rank

################################################################################
#### LOG GRAPH (crazy results even with log)
################################################################################

#let's plot ZIPF's Law   # ZIPF's Law uses on a log scale #Relation almost linear in a log_scale
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=question))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)
  #scale_x_log10()+
  #scale_y_log10()

################################################################################
#### TD _ IDF
################################################################################

survey_tokens <- survey_tokens %>%
  bind_tf_idf(question, word, n)
survey_tokens 

survey_tokens %>%
  arrange(desc(tf_idf))
