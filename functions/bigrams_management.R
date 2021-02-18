# Bigram functions adapted from tidytext vignettes

prepare_bigrams <- function(dataset, stopwords){
  dataset %>%
    unnest_tokens(ngram, Narrative, token = "ngrams", n = 2) %>%
    mutate(ngram = str_replace(ngram, "\\s", "|")) %>% 
    separate(ngram, into = c("word1", "word2"), sep = "\\|") %>%
    filter(!word1 %in% stopwords$word,
           !word2 %in% stopwords$word) %>%
    filter(!str_detect(word1, "\\d"),
           !str_detect(word2, "\\d")) %>%
    unite(ngram, word1, word2, sep = " ") %>%
    unite(Name, `Operating Unit`, `Indicator Bundle`, `Indicator`, sep = "|") %>%
    count(Name, ngram, sort = T)
}

count_bigrams <- function(dataset, stopwords){
  dataset %>%
    unnest_tokens(ngram, Narrative, token = "ngrams", n = 2) %>%
    mutate(ngram = str_replace(ngram, "\\s", "|")) %>% 
    separate(ngram, into = c("word1", "word2"), sep = "\\|") %>%
    filter(!word1 %in% stopwords$word,
           !word2 %in% stopwords$word) %>%
    filter(!str_detect(word1, "\\d"),
           !str_detect(word2, "\\d")) %>%
    count(word1, word2, sort = T)
}

visualize_bigrams <- function(bigrams){
  set.seed(2021)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>% 
    graph_from_data_frame() %>%
    ggraph(layout =  "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = F, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}