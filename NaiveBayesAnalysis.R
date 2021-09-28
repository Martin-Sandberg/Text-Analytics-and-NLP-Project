# libraries to import
library(readtext)
library(tidyverse)
library(tidytext)
library(readxl)
library(splitstackshape)
library(igraph)
library(ggraph)
library(quanteda)
library(quanteda.textmodels)
library(gmodels)

responses <- read_excel("~/Documents/Grad School/Data 5317/Team12_TeslaMars/FinalQuestionResponses.xlsx")
responses$final_question <- as.integer(as.factor(responses$final_question)) - 1

# local path of the survey files
surveys_local_path <- "/Users/nimiipatel/Documents/Grad School/Data 5317/Team12_TeslaMars/Surveys"
setwd(surveys_local_path)
survey_file_names <- list.files(path = surveys_local_path)

# helper function to read file
readFile <- function(fileName) {
  file_data <- data.frame(answer = read_lines(fileName, 
                                              skip_empty_rows = TRUE)) %>%
                          mutate(question = row_number(), file_name = fileName)
  return(file_data)
}

# loop through each file name in list into a data frame
survey_document <- readtext(survey_file_names) %>%
                    rename(., file_name = doc_id) %>%
                    merge(., responses, by = "file_name")

survey_data <- bind_rows(lapply(survey_file_names, readFile)) %>%
                merge(., responses, by = "file_name")

# tokenize the data frame
data(stop_words)
survey_tokens <- survey_data %>%
                  unnest_tokens(word, answer) %>%
                  anti_join(stop_words) %>%
                  group_by(question)

# naive bayes

sample_data <- stratified(responses,
                          group = 2,
                          size = 0.65,
                          bothSets = TRUE)

msg.dfm <- dfm(corpus(survey_document), tolower = TRUE)

training_index <- sample_data$SAMP1
testing_index <- sample_data$SAMP2

training_data <- msg.dfm[msg.dfm$file_name %in% training_index$file_name, ]
testing_data <- msg.dfm[msg.dfm$file_name %in% testing_index$file_name, ]

nb_classifier <- textmodel_nb(training_data, training_data$final_question)
summary(nb_classifier)

pred <- predict(nb_classifier, testing_data)
pred

accuracy_table <- CrossTable(pred, testing_data$final_question,
                             prop.chisq = FALSE, prop.t = FALSE, 
                             dnn = c('predicted', 'actual'))

