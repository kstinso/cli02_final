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