comp <- my_csv %>% 
  filter(doc_id %in% c("doc42", "doc44"))

comp <- bind_rows(doc42_44, cs, aor)

comp_words <- comp %>%
  unnest_tokens(word, text) %>%
  count(doc_id, word, sort = TRUE)

comp_total_words <- comp_words %>% 
  group_by(doc_id) %>% 
  summarize(total = sum(n))

comp_words <- left_join(comp_words, comp_total_words)
rm(comp_total_words, book_words)


comp_classics_words <- comp_words %>% 
  filter(str_to_lower(word) %in% str_to_lower("Romans")) %>% 
  arrange(desc(n))

comp_classics_words %>% count(word) %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  labs(title = "The Classicism of Ministers and Thomas Paine", x = "Word", y = "Number of Sermons") +
  geom_col() + 
  coord_flip()

ggplot(comp_classics_words, aes(x = fct_reorder(word, n), y = n, fill=doc_id)) +
  labs(title="The Classicism of Ministers and Thomas Paine", x="Word", y="Number of References")+
  scale_fill_manual(labels = c("Paine's Age of Reason", "Paine's Common Sense", "Edwards Jr.'s Sermon", "Webster's Sermon"), values = c("blue", "black", "purple", "gold"))+
  geom_bar(stat="identity", position=position_dodge())

zoom_dfm <- dfm(zoom,
              remove_punct = TRUE,
              remove_symbols = TRUE,
              remove = c(stopwords("en"), mystopwords, as.character(1:100)))



zoom_sermon_words <- my_csv %>% 
  filter(doc_id %in% c("doc42", "doc44")) %>% 
  unnest_tokens(word, text) %>%
  count(meta, word, sort = TRUE)

total_words <- zoom_sermon_words %>% 
  group_by(meta) %>% 
  summarize(total = sum(n))

zoom_sermon_words <- left_join(zoom_sermon_words, total_words)
rm(total_words)

ggplot(zoom_sermon_words, aes(n/total)) +
  geom_histogram(show.legend = FALSE, binwidth = 10000) +
  xlim(NA, 0.0009) +
  facet_wrap(~meta, ncol = 2, scales = "free_y")

freq_by_rank <- zoom_sermon_words %>% 
  group_by(meta) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

zoom_sermon_words <- zoom_sermon_words %>%
  bind_tf_idf(word, meta, n)
sermon_words

zoom_sermon_words <- zoom_sermon_words %>%
  anti_join(mystopwords) %>% 
  select(-total) %>%
  arrange(desc(tf_idf))

dfm_select(zoom_dfm, pattern = classical_characters)%>% 
  textplot_wordcloud(zoom_dfm,
                     color = c('black', 'green', 'purple', 'yellow', 'blue'))

zoom_classics_words <- zoom_sermon_words %>% 
  filter(str_to_lower(word) %in% str_to_lower(classics)) %>% 
  arrange(desc(n))


raw1 <- read_html("common sense.html")

cs <- readtext("common_sense.txt")
aor <- readtext("age_of_reason.txt")

cs_corpus <- corpus(cs)
aor_corpus <- corpus(aor)

cs_words <- cs %>% 
  unnest_tokens(word, text) %>%
  count(doc_id, word, sort = TRUE)
aor_words <- aor %>% 
  unnest_tokens(word, text) %>%
  count(doc_id, word, sort = TRUE)


total_words <- cs_words %>% 
  group_by(doc_id) %>% 
  summarize(total = sum(n))

cs_words <- left_join(cs_words, total_words)
rm(total_words)

freq_by_rank <- cs_words %>% 
  group_by(doc_id) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

cs_words <- cs_words %>%
  bind_tf_idf(word, doc_id, n)
aor_words <- aor_words %>%
  bind_tf_idf(word, doc_id, n)

cs_words <- cs_words %>% 
  select(-total) %>%
  arrange(desc(tf_idf))
aor_words <- aor_words %>% 
  select(-total) %>%
  arrange(desc(tf_idf))

cs_classics_words <- cs_words %>% 
  filter(str_to_lower(word) %in% str_to_lower(classics)) %>% 
  arrange(desc(n))
aor_classics_words <- aor_words %>% 
  filter(str_to_lower(word) %in% str_to_lower(classics)) %>% 
  arrange(desc(n))


