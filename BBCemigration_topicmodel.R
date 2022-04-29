##AIM: Create corpus of BBC emigration articles, and conduct topic modeling
####Loading packages
library(readxl)
library(tidyverse)
library(tm)
library(ggplot2)
library(stringr)
library(tidytext)
library(dplyr)
library(topicmodels)
library(reshape2)

####Creating corpus ====
Corpusexcel <- read_excel('BBCemigration_corpus.xlsx')

Corpusexcel$article_text_new <- str_replace_all(Corpusexcel$article_text, "[\r\n]" , " ")
Corpusexcel$article_text_new <- str_replace_all(Corpusexcel$article_text_new, "Hong" , "Hongkong")
Corpusexcel$article_text_new <- str_replace_all(Corpusexcel$article_text_new, "Kong" , "")

Corpusexcel$article_text_new

BBCcorpus <- Corpus(VectorSource(Corpusexcel$article_text_new))

print(BBCcorpus)
inspect(BBCcorpus[1:3])

BBCcorpusCopy <- BBCcorpus

BBCcorpus_clean = tm_map(BBCcorpus, tolower) # converts all text to lower case
BBCcorpus_clean = tm_map(BBCcorpus_clean, removePunctuation) #removes punctuation
BBCcorpus_clean = tm_map(BBCcorpus_clean, removeWords, stopwords()) #removes common words like “a”, “the” etc
BBCcorpus_clean = tm_map(BBCcorpus_clean, stemDocument, language = "english") # removes the last few letters of similar words such as get, getting, gets
BBCcorpus_clean <- tm_map(BBCcorpus_clean, content_transformer(stemCompletion), dictionary = BBCcorpusCopy, lazy=TRUE)
BBCcorpus_clean = tm_map(BBCcorpus_clean, stripWhitespace) #Strip extra whitespace from a text document. Multiple whitespace characters are collapsed to a single blank.

inspect(BBCcorpus_clean[1])

BBCdtm = DocumentTermMatrix(BBCcorpus_clean) #turns the corpus into a document term matrix

inspect(BBCdtm99)

####Topic modeling ====
BBCdtm_topic_model <- LDA(BBCdtm, k=5, control = list(seed =321))
BBC_topic <- tidy(BBCdtm_topic_model, matrix = "beta") #convert to tidy format

BBC_top_terms <- BBC_topic %>% #identify top terms
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

final_topic_model <- BBC_top_terms %>%
  mutate(term = reorder(term, beta)) %>% #reorder
  mutate(topic = paste("Topic #", topic)) %>% #change title to "topic # x"
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE, colour = "black") + #remove legend
    facet_wrap(~ topic, scales = "free") + #split into five graphs
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size = 24),
          strip.text.x = element_text(size = 15),
          axis.text=element_text(size=12))+
    theme(text = element_text(family = "Times New Roman")) +
  labs (
    title = "Topic model of BBC emigration articles",
    caption = "Top terms by topic (betas)")+
  ylab("")+
  xlab("")+
  scale_fill_brewer(palette = "Spectral")+
  coord_flip()
  )

ggsave(filename = "BBCemigration topic model.png",
       final_topic_model, dpi=300)

####Ranking top words ====
inspect(BBCdtm99)

BBCdtm99 = removeSparseTerms(BBCdtm, 0.99) # extracts frequently occuring words
finalWords=as.data.frame(as.table(BBCdtm99)) %>% # most frequent words remain in a dataframe, with one column per word
  group_by(Terms) %>%
  summarize(n=sum(Freq))

final_rankingchart <- finalWords %>% 
  filter(n > 20) %>% 
  mutate(word = reorder(Terms, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col(colour = "black", fill="#9E0142") +
  coord_flip() +
  theme_gray() +
  theme(text = element_text(family = "Times New Roman")) +
  labs(x = "Word \n", y = "\n Count ", title = "Frequent Words BBC emigration articles \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 24), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text=element_text(size=12))

ggsave(filename = "BBCemigration word ranking.png",
       final_rankingchart, dpi=300)

####Word cloud ====
library(wordcloud)
library(RColorBrewer)
num_colors <- 8 #set number of colors
pal <- brewer.pal(num_colors, "Accent") #palette using RColorBrewer argument
final_wordcloud <- wordcloud(BBCcorpus_clean, min.freq = 2, max.words = 50, colors = pal, 
                             size = 50, width = 400, height = 200, scale = c(5,1))
