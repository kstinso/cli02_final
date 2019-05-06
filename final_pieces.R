
my_dfm %>% 
  dfm_trim(min_termfreq = 10, verbose = FALSE) %>% 
  textplot_wordcloud(my_dfm, min_count = 20, 
                     color = c('black', 'pink', 'green', 'purple', 'orange', 'blue'))

kwic(my_corpus, "romans", window = 8)
kwic(my_corpus, "rome", window = 8)

ggplot(classics_words, aes(x = word))+
  geom_histogram(stat = "count") +
  coord_flip()

kwic(my_corpus, pattern = classics) %>%
  textplot_xray()
#ADD COLOR ON THE ABOVE FROM THE QUANTEDA PAGE


