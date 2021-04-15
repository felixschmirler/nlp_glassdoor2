#analysing glassdoor reviews

#load packages
library(tidyverse)
library(tidytext)
library(wordcloud)
library(corrplot)
library(lubridate)
library(viridis)
library(qdap)
library(textstem)
library(gridExtra)
library(ggwordcloud)
library(topicmodels)
library(tm)
library(quanteda)
library(lexicon)
library(ldatuning)
library(magrittr)

#load data
gd <- read.csv("reviews_clean.csv") %>% 
  mutate(
    year = factor(year(date), ordered = TRUE),
    id = paste0("id", row_number()),
    date = ymd(date)
  ) 

#explore frequency of reviews over time
gd %>%
  ggplot(aes(date)) +
  geom_histogram() + 
  facet_wrap(~company)

#explore ratings 
gd %>%
  ggplot(aes(rating)) +
  geom_histogram() + 
  facet_wrap(~company)

gd %>% 
  select(year, rating) %>%
  mutate(rating = factor(rating, ordered = TRUE)) %>%
  group_by(year, rating) %>%
  summarise(n = n()) %>% 
  pivot_wider(names_from = c(rating), values_from = n) %>% 
  replace_na(list(`1` = 0, `2` = 0, `3` = 0, `4` = 0, `5` = 0)) %>%
  mutate(
    `0.5` = (600 - `1` - `2`) - `3` / 2,
    `5.5` = (800 - `5` - `4`) - `3` / 2,
  ) %>% 
  pivot_longer(cols = 2:8, names_to = "rating", values_to = "number") %>% 
  mutate(
    rating = factor(rating, levels = c("5.5", "5", "4", "3", "2", "1", "0.5"),  ordered = TRUE)
  ) %>% 
  ggplot(aes(year, number, fill = rating)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(labels = c("", "5", "4", "3", "2", "1", ""), values = c("#00000000", viridis(5), "#00000000")) +
  ggtitle("Glasdoor Ratings over Time") +
  theme(panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(hjust = 1),
        plot.margin = margin(1, 0.5, 0, 0, "cm")
  ) +
  labs(x = "", y = "")

#explore sub ratings
gd %>% 
  pivot_longer(matches("career|compensation|culture|balance|management")) %>% 
  ggplot(aes(value, fill = name)) + 
  geom_bar() + 
  #facet_wrap(~company+name, nrow = 3, )
  facet_grid(company ~ name, scales = "free_y") +
  theme(legend.position = "none")

#Explore relationship between subratings and main rating
cor_gd <- cor(drop_na(gd[c("rating", "work_life_balance", "culture_values", "career_opportunities", "compensation_benefits", "senior_management")]))
corrplot.mixed(cor_gd)

#tidy pros and cons for text analysis
pros_cons <- gd %>%
  select(id, company, pros, cons) %>%
  pivot_longer(c(pros, cons), names_to = "pros_cons", values_to = "text") %>% 
  mutate(
    nwords = str_count(text,  "\\S+")
  )

pros_cons %>%
  ggplot(aes(nwords)) +
  geom_histogram()

sum(pros_cons$nwords)

pros_cons2 <- pros_cons %>% 
  mutate(
    text_clean = str_squish(text) %>% 
      str_replace_all("`|´|’", "'") %>% 
      replace_contraction() %>%
      str_to_lower() %>%
      lemmatize_strings() %>% 
      str_replace_all("free book", "free_book") %>% 
      str_replace_all("work life balance|work / life balance|work - life balance", "work_life_balance") %>%
      str_replace_all("upper managment|senior management|top management", "senior_management") %>%
      str_remove_all("[0-9]")  
  ) 
  

undesirable_words <- c("prh", "penguin", "random", "house", "pearson", "bbc", "netflix", "education", "technology", "publish")

pros_cons3a <- pros_cons2 %>%
  unnest_tokens(word, text_clean) %>%
  anti_join(stop_words) %>% 
  #distinct() %>%
  filter(!word %in% undesirable_words)

low_occurence <- pros_cons3a %>%
  group_by(pros_cons, word) %>%
  summarise(
    n = n()
  ) %>%
  filter(n < 10) %>%
  select(word) 

pros_cons3a %<>% anti_join(low_occurence) 

#pros_cons3b <- pros_cons2 %>%
#  unnest_tokens(word, text_clean, token = "ngrams", n = 2)
#pros_cons3b %>% 
#  group_by(word) %>%
#  summarise(
#    n = n()
#  ) %>%
#  view()


#pros_cons3c <- pros_cons2 %>%
#  unnest_tokens(word, text_clean, token = "ngrams", n = 3)

#pros_cons3 <- rbind(pros_cons3a, pros_cons3b, pros_cons3c) 

#pros_cons4 <- pros_cons3 %>% 
#  mutate(
#    word = recode(word, 
#                  "salary" = "pay",
#                  "wage" = "pay",
#                  "advancement" = "progression")
#  ) %>%
#  group_by(company, word, pros_cons) %>%
#  summarise(occurrences = n()) %>%
#  ungroup() %>% 
 # filter(occurrences > 10) 
  

#LDA ####
lda_data <- pros_cons3a %>%
  transmute(
    document = paste(id, pros_cons, sep = "_"),
    word = word
  ) %>% 
  count(document, word, sort = TRUE) %>%
  ungroup() 

lda_data_pros <- pros_cons3a %>%
  filter(pros_cons == "pros") %>%
  transmute(
    document = paste(id, pros_cons, sep = "_"),
    word = word
  ) %>% 
  count(document, word, sort = TRUE) %>%
  ungroup() 

lda_data_cons <- pros_cons3a %>%
  filter(pros_cons == "cons") %>%
  transmute(
    document = paste(id, pros_cons, sep = "_"),
    word = word
  ) %>% 
  count(document, word, sort = TRUE) %>%
  ungroup() 

#find best amount of topics
dtm <- cast_dtm(lda_data, document, word, n) 
dtm_pros <- cast_dtm(lda_data_pros, document, word, n) 
dtm_cons <- cast_dtm(lda_data_cons, document, word, n) 

#result <- FindTopicsNumber(
#  dtm,
#  topics = seq(from = 2, to = 30, by = 1),
#  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#  method = "Gibbs",
#  control = list(seed = 77),
#  mc.cores = NA,
#  verbose = TRUE
#)

#FindTopicsNumber_plot(result)

#result_pros <- FindTopicsNumber(
#  dtm_pros,
#  topics = seq(from = 2, to = 30, by = 1),
#  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#  method = "Gibbs",
#  control = list(seed = 77),
#  mc.cores = NA,
#  verbose = TRUE
#)

#FindTopicsNumber_plot(result_pros)

#result_cons <- FindTopicsNumber(
# dtm_cons,
#  topics = seq(from = 2, to = 30, by = 1),
#  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#  method = "Gibbs",
#  control = list(seed = 77),
#  mc.cores = NA,
#  verbose = TRUE
#)

#FindTopicsNumber_plot(result_cons)

lda <- LDA(dtm, k = 7, method = "GIBBS", control = list(seed = 1234)) #7 or 11  
terms(lda, 30) %>% view()
perplexity(lda, dtm)
topicmodels::logLik(lda)

lda_pros <- LDA(dtm_pros, k = 4, method = "GIBBS", control = list(seed = 1234)) #4 OR 5 
terms(lda_pros, 30) %>% view()
perplexity(lda_pros, dtm_pros)
topicmodels::logLik(lda_pros)

lda_cons <- LDA(dtm_cons, k = 3, method = "GIBBS", control = list(seed = 1234)) #4 OR 3
terms(lda_cons, 40) %>% view()
perplexity(lda_cons, dtm_cons)
topicmodels::logLik(lda_cons)
#topics(lda) 

#Explore LDA Topics
betas_pros <- tidy(lda_pros, matrix = "beta")
pros_top_terms <- betas_pros %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
pros_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

betas_cons <- tidy(lda_cons, matrix = "beta")
cons_top_terms <- betas_cons %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
cons_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

gammas_pros <- tidy(lda_pros, matrix = "gamma")
gammas_cons <- tidy(lda_cons, matrix = "gamma")
gammas <- add_row(gammas_pros, gammas_cons)

#Evaluating lda performance on data
#augment(lda_pros, dtm_pros) %>% 
  #group_by(term, .topic) %>% 
  #summarise(n = n()) %>% 
#  view()

pros_cons_augmented <- pros_cons %>%
  mutate(
    document = paste0(id, "_", pros_cons)
  ) %>% left_join(gammas) 

pros_cons_augmented %>%
  group_by(company, pros_cons, topic) %>%
  summarise(
    sum = sum(gamma)
  ) %>% view()

pros_cons_augmented_max <- pros_cons_augmented %>%
  group_by(document, pros_cons) %>%
  slice_max(gamma) %>%
  ungroup()

pros_cons_augmented_max %>% 
  group_by(company, pros_cons, topic) %>%
  summarise(
    n = n()
  ) %>% 
  ungroup() %>%
  group_by(company, pros_cons) %>%
  mutate(
    total = sum(n),
    Percentage = round(n/total, 3)
  ) %>%
  ggplot(aes(topic, Percentage, fill = company)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(
    aes(label = paste0(Percentage * 100, "%")),
    colour="white"
    ) +
  facet_grid(company ~ pros_cons)


#lda following quanteda preprocessing
#pros_corpus <- corpus(as.character(gd$pros))
#docnames(pros_corpus) <- paste0("pros", 1:ndoc(pros_corpus))
#docvars(pros_corpus, "type") <- rep("pros", ndoc(pros_corpus))
#summary(pros_corpus)

#cons_corpus <- corpus(as.character(gd$cons))
#docvars(cons_corpus, "type") <- rep("cons", ndoc(cons_corpus))
#docnames(cons_corpus) <- paste0("cons", 1:ndoc(cons_corpus))
#summary(cons_corpus)

#review_corpus <- c(pros_corpus,  cons_corpus)
  
#summary(review_corpus) %>% head()
#kwic(review_corpus, "balance") %>% view()
#kwic(review_corpus, pattern = regex("growth")) %>% view()
#kwic(review_corpus, pattern = phrase("growth")) %>% view()
#corpus_subset(review_corpus, type == "pros")
#texts(review_corpus)[5793]

#corpus_df <- summary(review_corpus, n = ndoc(review_corpus))
#review_tokens <- tokens(lemmatize_strings(str_squish(char_tolower(replace_contraction(gd$pros)))), 
#                        remove_numbers = TRUE, 
#                        remove_punct = TRUE, 
#                        remove_separators = TRUE, 
#                        remove_symbols = TRUE,
#                        split_hyphens = TRUE,
#                        ) %>%
#  tokens_remove(stopwords("english"), padding  = TRUE) #%>%
#  tokens_ngrams(n = 1:3)
#  tokens_compound(pattern = phrase(c("very unlikely test expression", 
#                                     "and another one"))) 
#review_dfm <- tokens_replace(review_tokens, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>%
#review_dfm <- dfm(review_tokens) #%>% dfm_trim(min_termfreq = 0.99, termfreq_type = "quantile")
#review_dfm[, 1:10]
#topfeatures(review_dfm, n = 20)
#textplot_wordcloud(review_dfm[, 100:300], color = viridis(8))

#review_dtm <- convert(review_dfm, to = "topicmodels")

#result2 <- FindTopicsNumber(
#  review_dtm,
#  topics = seq(from = 2, to = 50, by = 1),
#  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#  method = "Gibbs",
#  control = list(seed = 77),
#  mc.cores = NA,
#  verbose = TRUE
#)
  
#FindTopicsNumber_plot(result2)

#review_lda <- LDA(review_dtm, k = 5, method = "GIBBS")
#terms(review_lda, 10)
#???Terms

perplexity(review_lda)

#PRH ---------------------------------------------------------------------------
prh <- gd %>% filter(company == "PRH")

prh %>%
  ggplot(aes(year, rating, fill = year)) +
  geom_violin() +
  geom_jitter(position=position_jitter(0.1))

prh %>% 
  select(year, rating) %>%
  mutate(rating = factor(rating, ordered = TRUE)) %>%
  group_by(year, rating) %>%
  summarise(n = n()) %>% 
  pivot_wider(names_from = c(rating), values_from = n) %>% 
  replace_na(list(`1` = 0, `2` = 0, `3` = 0, `4` = 0, `5` = 0)) %>%
  mutate(
    `0.5` = (30 - `1` - `2`) - `3` / 2,
    `5.5` = (90 - `5` - `4`) - `3` / 2,
  ) %>% 
  pivot_longer(cols = 2:8, names_to = "rating", values_to = "number") %>% 
  mutate(
    rating = factor(rating, levels = c("5.5", "5", "4", "3", "2", "1", "0.5"),  ordered = TRUE)
  ) %>% 
  ggplot(aes(year, number, fill = rating)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(labels = c("", "5", "4", "3", "2", "1", ""), values = c("#00000000", viridis(5), "#00000000")) +
  ggtitle("Glasdoor Ratings over Time") +
  theme(panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(hjust = 1),
        plot.margin = margin(1, 0.5, 0, 0, "cm")
  ) +
  labs(x = "", y = "")

#explore sub ratings
prh %>% 
  pivot_longer(matches("career|compensation|culture|balance|management")) %>% 
  ggplot(aes(value, fill = name)) + 
  geom_bar() + 
  #facet_wrap(~company+name, nrow = 3, )
  #facet_grid(company ~ name, scales = "free_y") +
  facet_wrap(~name)+
  theme(legend.position = "none")

#Explore relationship between subratings and main rating
cor_prh <- cor(drop_na(prh[c("rating", "work_life_balance", "culture_values", "career_opportunities", "compensation_benefits", "senior_management")]))
corrplot.mixed(cor_prh)

#Word frequencies
pros_prh <- pros_cons4 %>%
  filter(company == "PRH", pros_cons == "pros") %>%
  filter(!word %in% c("company", "to work", "be a", "free", "balance",
                      "great benefit", "lot of", "benefit and", 
                      "good benefit", "work life", "life balance", 
                      "life", "excellent", "in the", "nice", 
                      "amaze", "and the", "there be", "of the",
                      "the company", "work with", "to work with", "lot")) %>%
  slice_max(occurrences, n = 15) %>% 
  mutate(word = reorder(word, occurrences))

pros_plot <- pros_prh %>%
  ggplot(aes(word, occurrences, fill = word)) + 
  geom_col() + 
  coord_flip() +
  #facet_wrap(~pros_cons, scales = "free") +
  theme(legend.position = "none") +
  scale_fill_manual(values = viridis(31)) +
  labs(x = "", y = "") +
  ggtitle("Pros")

cons_prh <- pros_cons4 %>%
  filter(company == "PRH", pros_cons == "cons") %>%
  filter(!word %in% c("be not", "it be", "can be", "a lot", "do not",
                      "a lot of", "pay be", "lot of", "be a", "there be",
                      "of the", "in the", "the company", "salary be", "hard to",
                      "room for", "difficult", "lot", "low", "be low", 
                      "company", "low pay", "department", "level", "job")) %>%
  slice_max(occurrences, n = 15) %>% 
  mutate(word = reorder(word, occurrences))

cons_plot <- cons_prh %>%
  mutate(word = reorder(word, occurrences)) %>% 
  ggplot(aes(word, occurrences, fill = word)) + 
  geom_col() + 
  coord_flip() +
  #facet_wrap(~pros_cons, scales = "free") +
  theme(legend.position = "none") +
  scale_fill_manual(values = viridis(31)) +
  labs(x = "", y = "") +
  ggtitle("Cons")

grid.arrange(pros_plot, cons_plot, ncol = 2, nrow = 1)


pros_cons4 %>%
  filter(company == "PRH") %>%
  filter(!word %in% c("be not", "it be", "can be", "a lot", "do not",
                      "a lot of", "pay be", "lot of", "be a", "there be",
                      "of the", "in the", "the company", "salary be", "hard to",
                      "room for", "difficult", "lot", "low", "be low", 
                      "company", "low pay", "department", "level", "job",
                      "company", "to work", "be a", "free", "balance",
                      "great benefit", "lot of", "benefit and", 
                      "good benefit", "work life", "life balance", 
                      "life", "excellent", "in the", "nice", 
                      "amaze", "and the", "there be", "of the",
                      "the company", "work with", "to work with", "lot")) %>%
  slice_max(occurrences, n = 25) %>%
  mutate(word = reorder(word, occurrences)) %>% 
  ggplot(aes(label = word, size = occurrences, colour = occurrences)) +
  geom_text_wordcloud() +
  theme_minimal() +
  facet_wrap(~pros_cons) +
  scale_color_gradient(low = viridis(3)[2], high = viridis(3)[3]) +
  scale_size_area(max_size = 20) 

#LDA ####
lda_data_pros_prh <- pros_cons3a %>%
  filter(pros_cons == "pros", company == "PRH" ) %>%
  transmute(
    document = paste(id, pros_cons, sep = "_"),
    word = word
  ) %>% 
  count(document, word, sort = TRUE) %>%
  ungroup() 

lda_data_cons_prh <- pros_cons3a %>%
  filter(pros_cons == "cons", company == "PRH" ) %>%
  transmute(
    document = paste(id, pros_cons, sep = "_"),
    word = word
  ) %>% 
  count(document, word, sort = TRUE) %>%
  ungroup() 

#find best amount of topics
dtm_pros_prh <- cast_dtm(lda_data_pros_prh, document, word, n) 
dtm_cons_prh <- cast_dtm(lda_data_cons_prh, document, word, n) 

result_pros <- FindTopicsNumber(
  dtm_pros,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = NA,
  verbose = TRUE
)

result_cons <- FindTopicsNumber(
  dtm_cons,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = NA,
  verbose = TRUE
)

FindTopicsNumber_plot(result_pros)
FindTopicsNumber_plot(result_cons)

lda_pros <- LDA(dtm_pros, k = 5, method = "GIBBS", control = list(seed = 1234))
terms(lda_pros, 30) %>% view()

lda_cons <- LDA(dtm_cons, k = 5, method = "GIBBS", control = list(seed = 1234))
terms(lda_cons, 30) %>% view()

lda_beta <- tidy(lda, matrix = "beta")
lda_gamma <- tidy(lda, matrix = "gamma") 

lda_beta %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 10) %>% view()

lda_gamma %>% view()

#lda following quanteda preprocessing
pros_corpus <- corpus(as.character(gd$pros))
docnames(pros_corpus) <- paste0("pros", 1:ndoc(pros_corpus))
docvars(pros_corpus, "type") <- rep("pros", ndoc(pros_corpus))
summary(pros_corpus)

cons_corpus <- corpus(as.character(gd$cons))
docvars(cons_corpus, "type") <- rep("cons", ndoc(cons_corpus))
docnames(cons_corpus) <- paste0("cons", 1:ndoc(cons_corpus))
summary(cons_corpus)

review_corpus <- c(pros_corpus,  cons_corpus)

summary(review_corpus) %>% head()
kwic(review_corpus, "balance") %>% view()
kwic(review_corpus, pattern = regex("growth")) %>% view()
kwic(review_corpus, pattern = phrase("growth")) %>% view()
corpus_subset(review_corpus, type == "pros")
texts(review_corpus)[5793]

corpus_df <- summary(review_corpus, n = ndoc(review_corpus))
review_tokens <- tokens(lemmatize_strings(str_squish(char_tolower(replace_contraction(gd$pros)))), 
                        remove_numbers = TRUE, 
                        remove_punct = TRUE, 
                        remove_separators = TRUE, 
                        remove_symbols = TRUE,
                        split_hyphens = TRUE,
) %>%
  tokens_remove(stopwords("english"), padding  = TRUE) #%>%
tokens_ngrams(n = 1:3)
#  tokens_compound(pattern = phrase(c("very unlikely test expression", 
#                                     "and another one"))) 
#review_dfm <- tokens_replace(review_tokens, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>%
review_dfm <- dfm(review_tokens) #%>% dfm_trim(min_termfreq = 0.99, termfreq_type = "quantile")
review_dfm[, 1:10]
topfeatures(review_dfm, n = 20)
textplot_wordcloud(review_dfm[, 100:300], color = viridis(8))
textplot_wordcloud()

review_dtm <- convert(review_dfm, to = "topicmodels")

result2 <- FindTopicsNumber(
  review_dtm,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = NA,
  verbose = TRUE
)

FindTopicsNumber_plot(result2)

review_lda <- LDA(review_dtm, k = 5, method = "GIBBS")
terms(review_lda, 10)

perplexity(review_lda)
