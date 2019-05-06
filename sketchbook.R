install.packages("tokenizers")
install.packages("textreuse")
install.packages("stopwords")
install.packages("text2vec")
install.packages("cleanNLP")
install.packages("quanteda")
install.packages("tidytext")
install.packages("readtext")
install.packages("stm")
devtools::install_github("bmschmidt/wordVectors")

library(tidyverse)
library(quanteda)
library(readtext)
library(stm)
library(Matrix)
library(broom)
library(stringr)
library(xml2)
library(LDAvis)
library(dplyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)
library(tm)

raw1 <- read_html("Political Sermons of the American Founding Era. Vol. 1 (1730-1788) - Online Library of Liberty.html")
raw2 <- read_html("Political Sermons of the American Founding Era. Vol. 2 (1789-1805) - Online Library of Liberty.html")

sermons1 <- raw1 %>% xml_find_all("//*[@class='type-sermon']")
meta1 <- map_chr(sermons1, function(x) {x %>% xml_child() %>% xml_text()})
texts1 <- sermons1 %>% xml_text() %>% str_trim()
vol1 <- tibble(meta = meta1, text = texts1)

sermons2 <- raw2 %>% xml_find_all("//*[@class='type-sermon']")
meta2 <- map_chr(sermons2, function(x) {x %>% xml_child() %>% xml_text()})
texts2 <- sermons2 %>% xml_text() %>% str_trim()
vol2 <- tibble(meta = meta2, text = texts2)

all_sermons <- bind_rows(vol1, vol2) 
write_csv(all_sermons, "~/Desktop/sermons.csv")

my_csv <- read.csv("sermons (1).csv")
my_corpus <- corpus(as.character(my_csv))

rm(csv,raw1, raw2, sermons1, sermons2, vol1, vol2, meta1, meta2, texts1, texts2)

#NEED TEXTS 42 AND 44
text42 <- all_sermons %>% 
  filter(meta == "jonathan edwards, jr. the necessity of the belief of christianity fpage='1185' lpage='1216'")
text42 <- corpus(text42)
rm(text42)

####TOPIC MODELING

mystopwords <- tibble(word = c("g", "i'll", "em", "d", "ye", "mr", "i've", "tis", "O", "o"))

my_corpus <- corpus(all_sermons)
my_dfm <- dfm(my_corpus,
              remove_punct = TRUE,
              remove_symbols = TRUE,
              remove = c(stopwords("en"), mystopwords, as.character(1:100)))
topic_model <- stm(my_dfm, K = 10, verbose = TRUE, seed = 832)

summary(topic_model)
labelTopics(topic_model, n = 20)

findThoughts(topic_model, texts = docnames(my_corpus))
plot(topic_model)
plot(topic_model, type = "labels")

cor <- topicCorr(topic_model)
cor$cor
plot(cor)

processed <- textProcessor(my_texts$text)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
toLDAvis(topic_model, docs = docs)

kwic(my_corpus, "romans", window = 8)
kwic(my_corpus, "rome", window = 8)

##### WORD/DOCUMENT FREQUENCY

sermon_words <- all_sermons %>% 
  unnest_tokens(word, text) %>%
  count(meta, word, sort = TRUE)

total_words <- sermon_words %>% 
  group_by(meta) %>% 
  summarize(total = sum(n))

sermon_words <- left_join(sermon_words, total_words)
rm(total_words)

ggplot(sermon_words, aes(n/total)) +
  geom_histogram(show.legend = FALSE, binwidth = 10000) +
  xlim(NA, 0.0009) +
  facet_wrap(~meta, ncol = 2, scales = "free_y")

freq_by_rank <- sermon_words %>% 
  group_by(meta) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

sermon_words <- sermon_words %>%
  bind_tf_idf(word, meta, n)
sermon_words

sermon_words <- sermon_words %>%
  anti_join(mystopwords) %>% 
  select(-total) %>%
  arrange(desc(tf_idf))

sermon_words %>%
  head(n = 20) %>% 
  arrange(desc(tf_idf)) %>%
  group_by(word) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") + 
  coord_flip()

my_dfm %>% 
  filter(str_to_lower(word) %in% str_to_lower(classics))%>% 
  textplot_wordcloud(my_dfm, min_count = 20,
                     color = c('black', 'pink', 'green', 'purple', 'orange', 'blue'))

dfm_select(my_dfm, pattern = classical_characters)%>% 
  textplot_wordcloud(my_dfm,
                     color = c('black', 'green', 'purple', 'yellow', 'blue'))

classics_words <- sermon_words %>% 
  filter(str_to_lower(word) %in% str_to_lower(classics))

bib_words <- sermon_words %>% 
  filter(str_to_lower(word) %in% str_to_lower(Israel))

ggplot(classics_words, aes(x = word))+
  geom_histogram(stat = "count") +
  coord_flip()

ggplot(bib_words, aes(x = word))+
  geom_histogram(stat = "count") +
  coord_flip()

classics_words %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

####QUANTEDA ANALYSIS

dict <- dictionary(list(
  Rome = c("Rome", "Roman", "Romans"),
  Greece = c("Greece", "Greek", "Greeks", "Sparta", "Athens"),
  Authors = classical_characters
))

classical_dictionary <- c("Rome", "Roman", "Romans", "Greece", "Greek", "Greeks", "Sparta", "Athens", "Sylla", "Marius", "Virgil", "Cicero", "Livy", "Plutarch", "Cato", "Socrates", "Caesar", "Cesar", "Thucydides", "Demosthenes", "Sallust", "Homer", "Virgil", "Tacitus", "Dionysius", "Polybius", "Ovid", "Seneca", "Horace", "Alexander", "Plato")

classical_sermons <- all_sermons[str_detect(all_sermons$text, classical_dictionary),]
head(classical_sermons$text)


sermon_dfm_dict <- dfm(my_corpus, dictionary = dict, remove_punct = TRUE, remove_symbols = TRUE)
classical_ref <- convert(sermon_dfm_dict, to = "data.frame")
ggplot(classical_ref, aes(x = Rome))+
  geom_bar()

my_corpus_sub <- corpus_subset(my_corpus)
kwic(my_corpus_sub, pattern = classics)

textplot_xray(kwic(my_corpus_sub, pattern = classics) +
                labs(x=NULL, y='Frequency'))

kwic(my_corpus, pattern = Greece) %>%
  textplot_xray()

kwic(my_corpus, pattern = bib_authors) %>% 
  textplot_xray()

####REGEX

str_view(all_sermons, "Rome")

classics <- c("Rome", "Roman", "Romans", "Greece", "Greek", "Greeks","Sparta", "Athens", "Virgil", "Cicero", "Livy",
              "Plutarch", "Cato", "Socrates", "Caesar", "Cesar", "Thucydides", 
              "Demosthenes", "Sallust", "Homer", "Virgil", "Tacitus", "Dionysius", 
              "Polybius", "Ovid", "Seneca", "Horace", "Plato", "Sylla", "Marius", "Aristotle")


Rome <- c("Rome", "Roman", "Romans")
Greece <- c("Greece", "Greek", "Greeks", "Sparta", "Athens")
classical_characters <- c("Sylla", "Marius", "Virgil", "Cicero", "Livy",
                       "Plutarch", "Cato", "Socrates", "Caesar", "Cesar", "Thucydides", 
                       "Demosthenes", "Sallust", "Homer", "Virgil", "Tacitus", "Dionysius", 
                       "Polybius", "Ovid", "Seneca", "Horace", "Plato", "Aristotle")
Israel <- c("Israel", "Jerusalem", "Hebrew", "Hebrews", "Jew", "Jews")
bib_authors <- c("Paul", "Moses", "Daniel", "Isaiah", "Ezekiel", "Matthew", "Mark", "Luke", 
                 "John")
classics_match <- str_c(classics, collapse = "|")
classics_match

has_classics <- str_subset(all_sermons, classics_match)
matches <- str_extract(has_classics, classics_match)
head(matches)

more <- all_sermons[str_count(all_sermons, classics_match) > 1]
str_view_all(more, classics_match)
