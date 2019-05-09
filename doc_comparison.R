zoom <- my_csv %>% 
  filter(doc_id %in% c("doc42", "doc44")) %>% 
  corpus()

comp <- my_csv %>% 
  filter(doc_id %in% c("doc42", "doc44"))

comp <- bind_rows(comp, cs)
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
ggplot(comp_classics_words) +
  geom_bar(aes(x = word))

comp_classics_words %>% count(word) %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n), fill = doc_id) +
  geom_col() + 
  coord_flip()



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

freq_by_rank

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
rm(meta1, texts1)

cs_corpus <- corpus(cs)

cs_words <- cs %>% 
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

freq_by_rank

cs_words <- cs_words %>%
  bind_tf_idf(word, doc_id, n)
sermon_words

cs_words <- cs_words %>% 
  select(-total) %>%
  arrange(desc(tf_idf))

cs_classics_words <- cs_words %>% 
  filter(str_to_lower(word) %in% str_to_lower(classics)) %>% 
  arrange(desc(n))

kwic(cs_corpus, "Caesar") %>%
  textplot_xray()
kwic(cs_corpus, "Caesar", window = 5)