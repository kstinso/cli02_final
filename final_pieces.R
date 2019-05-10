
my_dfm %>% 
  dfm_trim(min_termfreq = 10, verbose = FALSE) %>% 
  textplot_wordcloud(my_dfm, min_count = 20, 
                     color = c('black', 'pink', 'green', 'purple', 'orange', 'blue'))

kwic(my_corpus, "romans", window = 8)
kwic(my_corpus, "rome", window = 8)

classics_words %>% count(word) %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  labs(title = "Classical References", x = "Word", y = "Number of Sermons") +
  geom_col() + 
  coord_flip()

kwic(my_corpus, pattern = Rome) %>%
  textplot_xray() +
  aes(color = keyword)+ 
  scale_color_manual(values = c("purple", "gold", "black")) +
  theme(legend.position = "none")

kwic(my_corpus, pattern = Greece) %>%
  textplot_xray() +
  aes(color = keyword)+ 
  scale_color_manual(values = c("blue", "black", "gold", "red", "green")) +
  theme(legend.position = "none")+
  labs(title = "Ancient Greece in the Sermons", y = "Sermon")


kwic(my_corpus, pattern = classics) %>%
  textplot_xray()
