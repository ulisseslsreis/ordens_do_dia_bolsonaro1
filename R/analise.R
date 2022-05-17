
# Carregando os pacotes

library(tidyverse)
library(tidytext)
library(tm)
library(igraph)
library(ggraph)
library(pdftools)
library(writexl)
library(readxl)
library(wordcloud)

# Baixando os PDF das ordens do dia

ordem_2019 <- pdf_text("data/ordem_do_dia_2019.pdf")
ordem_2020 <- pdf_text("data/ordem_do_dia_2020.pdf")
ordem_2021 <- pdf_text("data/ordem_do_dia_2021.pdf")
ordem_2022 <- pdf_text("data/ordem_do_dia_2022.pdf")

ordens <- tibble(ordem_2019, ordem_2020, ordem_2021, ordem_2022)

ordens_xlsx <- write_xlsx(ordens, "data/ordens.xlsx")

ordens_bolsonaro1 <- read_xlsx("data/ordens_bolsonaro1.xlsx")

# Preparando as stopwords

stopwordsbr <- tibble(stopwords("pt-br"))

stopwordsbr <- stopwordsbr |> 
  rename(word = `stopwords("pt-br")`)

stopwords_custom <- tibble(
  word = c("dessa", "dando", "ensejo", "2", "11", "31")
)

# Contando e plotando a frequência de palavras

ordens_bolsonaro_1_frequencia <- ordens_bolsonaro1 |> 
  unnest_tokens(word, word) |> 
  anti_join(stopwordsbr) |> 
  anti_join(stopwords_custom) |> 
  group_by(ano) |> 
  count(word) |> 
  ungroup() |> 
  arrange(desc(n))

ordens_bolsonaro_1_frequencia |> 
  filter(n >= 3) |> 
  group_by(ano) |> 
  mutate(word = fct_reorder(word, n)) |> 
  ungroup() |> 
  ggplot(aes(word, n, fill = ano)) + 
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~ ano, scales = "free_y") +
  labs(
    x = NULL,
    y = "Frequência de palavras",
    title = "Ordens do Dia (2019-2022)",
    subtitle = "Frequência de palavras mais utilizadas anualmente pelo Ministério da Defesa (n >= 3)",
    caption = "Elaborado Ulisses Levy Silvério dos Reis"
  ) +
  theme_minimal()

ordens_bolsonaro1 |> 
  unnest_tokens(word, word) |> 
  select(word) |>
  anti_join(stopwords_custom) |> 
  anti_join(stopwordsbr) |> 
  count(word) |> 
  arrange(desc(n)) |> 
  mutate(word = fct_reorder(word, n)) |> 
  filter(n > 5) |> 
  ggplot(aes(word, n)) +
  geom_col(fill = 1) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Frequência de palavras",
    title = "Ordens do Dia (2019-2022)",
    subtitle = "Frequência de palavras mais utilizadas anualmente pelo Ministério da Defesa (n > 5)",
    caption = "Elaborado Ulisses Levy Silvério dos Reis"
  ) +
  theme_minimal()

# Usando a estatística tf-idf

ordens_bolsonaro1 |> 
  unnest_tokens(word, word) |> 
  group_by(ano) |> 
  count(word) |> 
  ungroup() |> 
  arrange(desc(n)) |> 
  bind_tf_idf(word, ano, n) |> 
  group_by(ano) |> 
  arrange(desc(tf_idf)) |> 
  slice_max(tf_idf, n = 4) |> 
  ungroup() |> 
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = ano)) + 
  geom_col(show.legend = F) + 
  facet_wrap(~ ano, ncol = 2, scales = "free") +
  theme_minimal()

ordens_bolsonaro1$ano[ordens_bolsonaro1$ano == 2019] <- "ordem"
ordens_bolsonaro1$ano[ordens_bolsonaro1$ano == 2020] <- "ordem"
ordens_bolsonaro1$ano[ordens_bolsonaro1$ano == 2021] <- "ordem"
ordens_bolsonaro1$ano[ordens_bolsonaro1$ano == 2022] <- "ordem"

ordens_bolsonaro1 |> 
  unnest_tokens(word, word) |> 
  group_by(ano) |> 
  count(word) |> 
  ungroup() |> 
  bind_tf_idf(word, ano, n) |> 
  arrange(desc(tf_idf)) |> slice_max(tf_idf, n = 1) |> 
  ungroup() |> 
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = ano)) + 
  geom_col(show.legend = F) +
  theme_minimal()

## O método tf_idf não está funcionando. Pode ser por conta do quantitativo pequeno
## de palavras. Melhor ficar com a contagem comum, tirando as stopwords.

# WordCloud

ordens_unificado <- read_xlsx("data/ordens_unificado.xlsx")

ordens_wordcloud <- ordens_unificado |> 
  unnest_tokens(word, word) |> 
  anti_join(stopwordsbr) |> 
  anti_join(stopwords_custom) |> 
  select(word) |> 
  count(word) |> 
  arrange(desc(n))

wordcloud(
  data = ordens_wordcloud,
  words = ordens_wordcloud$word,
  freq = ordens_wordcloud$n,
  max.words = 60
)

# Grafo

ordens_bigram <- ordens_bolsonaro1 |> 
  unnest_tokens(bigram, word, token = "ngrams", n = 2)

ordens_bigram_separated <- ordens_bigram |> 
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_filtered <- ordens_bigram_separated |> 
  filter(!word1 %in% stopwordsbr$word) |> 
  filter(!word2 %in% stopwordsbr$word) |> 
  filter(!word1 %in% stopwords_custom$word) |> 
  filter(!word2 %in% stopwords_custom$word)

ordens_bigram_count <- bigram_filtered |> 
  count(word1, word2, sort = T)

ordens_bigram_united <- bigram_filtered |> 
  unite(bigram, word1, word2, sep = " ")

ordens_bigram_graph <- ordens_bigram_count |> 
  head(n = 50) |> 
  graph_from_data_frame()

ggraph(ordens_bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(ordens_bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

ggraph(ordens_bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()
