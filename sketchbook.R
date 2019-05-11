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

my_csv <- read_csv("sermons.csv")
my_corpus <- corpus(my_csv)

doc42_44 <- my_csv %>% 
  filter(doc_id %in% c("doc42", "doc44"))

doc42_44_words <- doc42_44 %>% 
  unnest_tokens(word, text) %>%
  count(meta, word, sort = TRUE)

total_words <- doc42_44_words %>% 
  group_by(meta) %>% 
  summarize(total = sum(n))

doc42_44_words <- left_join(doc42_44_words, total_words)
rm(total_words)

doc42_44_words <- doc42_44_words %>%
  bind_tf_idf(word, meta, n)

doc42_44_classics_words <- doc42_44_words %>% 
  filter(str_to_lower(word) %in% str_to_lower(classics)) %>% 
  arrange(desc(n))

doc42 <- my_csv %>% 
  filter(doc_id %in% c("doc42"))

doc42_words <- doc42 %>% 
  unnest_tokens(word, text) %>%
  count(doc_id, word, sort = TRUE)

total_words <- doc42_words %>% 
  group_by(doc_id) %>% 
  summarize(total = sum(n))

doc42_words <- left_join(doc42_words, total_words)
rm(total_words)

doc42_words <- doc42_words %>%
  bind_tf_idf(word, doc_id, n)

doc42_classics_words <- doc42_words %>% 
  filter(str_to_lower(word) %in% str_to_lower(classics)) %>% 
  arrange(desc(n))

doc44 <- my_csv %>% 
  filter(doc_id %in% c("doc44"))
doc44 <- corpus(doc44)

doc44_words <- doc44 %>% 
  unnest_tokens(word, text) %>%
  count(doc_id, word, sort = TRUE)

total_words <- doc44_words %>% 
  group_by(doc_id) %>% 
  summarize(total = sum(n))

doc44_words <- left_join(doc44_words, total_words)
rm(total_words)

doc44_words <- doc44_words %>%
  bind_tf_idf(word, doc_id, n)

doc44_classics_words <- doc44_words %>% 
  filter(str_to_lower(word) %in% str_to_lower(classics)) %>% 
  arrange(desc(n))
#NEED TEXTS 42 AND 44

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

kwic(doc44, "Romans", window = 8)

kwic(doc42_44, "rome", window = 6)

##### WORD/DOCUMENT FREQUENCY

sermon_words <- my_csv %>% 
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
  filter(str_to_lower(word) %in% str_to_lower(classics)) %>% 
  arrange(desc(n))

bib_words <- sermon_words %>% 
  filter(str_to_lower(word) %in% str_to_lower(Israel))

ggplot(bib_words, aes(x = word))+
  geom_histogram(stat = "count", fct_reorder(word, count)) +
  coord_flip()

classics_words %>% count(word) %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  labs(title = "Classical References", x = "Word", y = "Number of Sermons") +
  geom_col() + 
  coord_flip()
bib_words %>% count(word) %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  labs(title = "Classical References", x = "Word", y = "Number of Sermons") +
  geom_col() + 
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

kwic(my_corpus, pattern = classics) %>%
  textplot_xray()

kwic(my_corpus, pattern = bib_authors) %>% 
  textplot_xray()

textplot_xray(
  kwic(my_corpus, "Rome"),
  kwic(cs_corpus, "Rome")) +
  aes(color = keyword) + 
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "none")

####REGEX

str_view(all_sermons, "Rome")

classics <- c("Rome", "Roman", "Romans", "Greece", "Greek", "Greeks","Sparta", "Athens", "Virgil", "Virgil's", "Cicero", "Livy",
              "Plutarch", "Cato", "Socrates", "Caesar", "Cesar", "Thucydides", 
              "Demosthenes", "Sallust", "Homer", "Tacitus", "Dionysius", 
              "Polybius", "Ovid", "Seneca", "Horace", "Plato", "Sylla", "Marius", "Aristotle")


Rome <- c("Rome", "Roman", "Romans")
Greece <- c("Greece", "Greek", "Greeks", "Sparta", "Athens")
classical_characters <- c("Sylla", "Marius", "Virgil", "Virgil's", "Cicero", "Livy",
                       "Plutarch", "Cato", "Socrates", "Caesar", "Cesar", "Thucydides", 
                       "Demosthenes", "Sallust", "Homer", "Tacitus", "Dionysius", 
                       "Polybius", "Ovid", "Seneca", "Horace", "Plato", "Aristotle")
Israel <- c("Israel", "Jerusalem", "Hebrew", "Hebrews", "Jew", "Jews", "Paul", "Moses", "Daniel", "Isaiah", "Ezekiel", "Matthew", "Mark", "Luke", 
            "John")
bib_concepts <- c("Israel", "Jerusalem", "Hebrew", "Hebrews", "Jew", "Jews")
bib_authors <- c("Paul", "Moses", "Daniel", "Isaiah", "Ezekiel", "Matthew", "Mark", "Luke", 
                 "John")
classics_match <- str_c(classics, collapse = "|")
classics_match

has_classics <- str_subset(all_sermons, classics_match)
matches <- str_extract(has_classics, classics_match)
head(matches)

more <- all_sermons[str_count(all_sermons, classics_match) > 1]
str_view_all(more, classics_match)
