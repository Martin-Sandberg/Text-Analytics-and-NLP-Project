library(textreadr)
library(readxl)
library(readxl)
library(dplyr)
library(scales)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)
data("stop_words")

#Updating stop words
yeah <- data.frame(word="yeah", lexicon="Personalized")
stop_words <- stop_words
stop_words <- rbind(stop_words,yeah )
stop_words <- subset(stop_words, word!="don't")
stop_words <- subset(stop_words, word!="not")


#importing data
database <- read_excel("/Users/nimiipatel/Documents/Grad School/Data 5317/Team12_TeslaMars/Copy of Survey Responses.xlsx")

q1 <- data.frame(text=database$q1, success=database$`Will they visit Mars in 2030?`)
q2 <- data.frame(text=database$q2, success=database$`Will they visit Mars in 2030?`)
q3 <- data.frame(text=database$q3, success=database$`Will they visit Mars in 2030?`)
q4 <- data.frame(text=database$q4, success=database$`Will they visit Mars in 2030?`)
q5 <- data.frame(text=database$q5, success=database$`Will they visit Mars in 2030?`)
q6 <- data.frame(text=database$q6, success=database$`Will they visit Mars in 2030?`)



#creating a unique dataset merging the 3 ones above
allin <- bind_rows(mutate(q1, question="q1"),
                   mutate(q2, question="q2"),
                   mutate(q3, question="q3"),
                   mutate(q4, question="q4"),
                   mutate(q5, question="q5"),
                   mutate(q6, question="q6"))


##################################################
#Tokenization

token <- allin %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

frequencies <- allin %>%
  #filter(success="Yes")%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

###################################################
#Splitting success and failure

success <- filter(token, success=="Yes")
failure <- filter(token, success=="No")


#################################################
#Analysis



##### Sentiment Analysis

afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

#####
#####What are the most significant, positive and negative tokens for the people who answered yes?
succ_sentiment <- success %>%
  count(word, sort=TRUE) %>%
  ungroup()%>%
  inner_join(afinn)

succ_sentiment$tokenfreqsentiment = succ_sentiment$n*succ_sentiment$value
succ_sentiment <- succ_sentiment %>% arrange(tokenfreqsentiment,)

#Visualization
tfs_hist <- succ_sentiment %>% #tfs=token-frequency-sentiment
  mutate(word=reorder(word, tokenfreqsentiment)) %>% #reorder words by frequency with mutate
  filter(n>1) %>%
  ggplot(aes(word, tokenfreqsentiment))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(tfs_hist)

#####
#####What are the most significant positive, and negative tokens for the people who answered no?
fail_sentiment <- failure %>%
  count(word, sort=TRUE) %>%
  ungroup()%>%
  inner_join(afinn)

fail_sentiment$tokenfreqsentiment = fail_sentiment$n*fail_sentiment$value
fail_sentiment <- fail_sentiment %>% arrange(tokenfreqsentiment,)

#Visualization
tfs_hist <- fail_sentiment %>% #tfs=token-frequency-sentiment
  mutate(word=reorder(word, tokenfreqsentiment)) %>% #reorder words by frequency with mutate
  filter(n>1) %>%
  ggplot(aes(word, tokenfreqsentiment))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(tfs_hist)

####################Conclusions
#It does not seem to be very insightful, however,
#1 "success-people" use different negative words while fail-people use fewer words but repeat them times
#would it be that success-people have a more extended vocabulary? at least to explain something bad


########################################################################
#Bigrams and Quadrograms
#success bigrams
succ_bigrams <- allin %>%
  filter(success=="Yes")%>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  filter(!is.na(bigram))%>%
  separate(bigram, c("word1", "word2"), sep=" ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


#success trigrams
succ_trigram <- allin %>%
  filter(success=="Yes")%>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep=" ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

#success quadrogram
succ_quadrogram <- allin %>%
  filter(success=="Yes")%>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  filter(!is.na(quadrogram)) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)%>%
  filter(!word4 %in% stop_words$word)

#fail bigram
fail_bigrams <- allin %>%
  filter(success=="No")%>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  filter(!is.na(bigram))%>%
  separate(bigram, c("word1", "word2"), sep=" ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#fail trigrams
fail_trigram <- allin %>%
  filter(success=="No")%>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep=" ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

#fail quadrogram
fail_quadrogram <- allin %>%
  filter(success=="No")%>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  filter(!is.na(quadrogram)) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)%>%
  filter(!word4 %in% stop_words$word)

#######Visualizing ngrams

#success bigrams
count_succ_bigrams <- succ_bigrams %>%
  unite(word, word1:word2, sep=" ")%>%
  count(word,sort=TRUE)

succ_bigrams_hist <- count_succ_bigrams %>%
  mutate(word=reorder(word, n)) %>% #reorder words by frequency with mutate
  filter(n>1) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(succ_bigrams_hist)

#success trigrams
count_succ_trigram <- succ_trigram %>%
  unite(word, word1:word3, sep=" ")%>%
  count(word,sort=TRUE)

succ_trigram_hist <- count_succ_trigram %>%
  mutate(word=reorder(word, n)) %>% #reorder words by frequency with mutate
  #filter(n>1) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(succ_trigram_hist)

#success quadrograms
count_succ_quadrogram <- succ_quadrogram %>%
  unite(word, word1:word4, sep=" ")%>%
  count(word,sort=TRUE)

succ_quadrogram_hist <- count_succ_quadrogram %>%
  mutate(word=reorder(word, n)) %>% #reorder words by frequency with mutate
  #filter(n>1) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(succ_quadrogram_hist)

#fail bigrams
count_fail_bigrams <- fail_bigrams %>%
  unite(word, word1:word2, sep=" ")%>%
  count(word,sort=TRUE)

fail_bigrams_hist <- count_fail_bigrams %>%
  mutate(word=reorder(word, n)) %>% #reorder words by frequency with mutate
  filter(n>1) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(fail_bigrams_hist)

#fail trigrams
count_fail_trigram <- fail_trigram %>%
  unite(word, word1:word3, sep=" ")%>%
  count(word,sort=TRUE)

fail_trigram_hist <- count_fail_trigram %>%
  mutate(word=reorder(word, n)) %>% #reorder words by frequency with mutate
  #filter(n>1) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(fail_trigram_hist)

#fail quadrograms
count_fail_quadrogram <- fail_quadrogram %>%
  unite(word, word1:word4, sep=" ")%>%
  count(word,sort=TRUE)

fail_quadrogram_hist <- count_fail_quadrogram %>%
  mutate(word=reorder(word, n)) %>% #reorder words by frequency with mutate
  #filter(n>1) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(fail_quadrogram_hist)

####################Conclusions
## no insights from histograms


########################################
###Bigram Networks


#success bigrams networks
succ_bicount <- succ_bigrams %>% #bicount=bigrams count
  count(word1, word2, sort = TRUE) #now count frequencies for BOTH TOKENS TOGETHER

succ_big_net <- succ_bicount %>%
  #filter(n>1) %>%
  graph_from_data_frame()

ggraph(succ_big_net, layout = "fr") +
  geom_edge_link()+ #let us plot the arrows
  geom_node_point()+ #set us with these nodes of semantic structure
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#failure bigrams networks
fail_bicount <- fail_bigrams %>% #bicount=bigrams count
  count(word1, word2, sort = TRUE) #now count frequencies for BOTH TOKENS TOGETHER

fail_big_net <- fail_bicount %>%
  #filter(n>1) %>%
  graph_from_data_frame()

ggraph(fail_big_net, layout = "fr") +
  geom_edge_link()+ #let us plot the arrows
  geom_node_point()+ #set us with these nodes of semantic structure
  geom_node_text(aes(label=name), vjust =1, hjust=1)

###########################################Conclusions
## Need to find a story for the biggest branches and compare
# fail ones and success ones



#########################################
#tf idf
#hanifa

#success tfidf
succ_united <- succ_bigrams %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

succ_tfidf <- succ_united %>%
  count(question, bigram) %>%
  bind_tf_idf(bigram, question, n) %>%
  arrange(desc(tf_idf))


#failure tfidf
fail_united <- fail_bigrams %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

fail_tfidf <- fail_united %>%
  count(question, bigram) %>%
  bind_tf_idf(bigram, question, n) %>%
  arrange(desc(tf_idf))

################################### Conclusions
## Elon Musk, actual trade, and hard question, bigrams with highest tfidf for fail tokens
## Electric Vehicles, "yeah yeah", cell phone, and love electric bigrams with highest tfidf for success tokens

