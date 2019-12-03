library(text2vec)
library(LDAvis)

load(("~/Downloads/americanRestaurantReviews.rda"))
reviews = americanRestaurantReviews
reviews = reviews[sample(nrow(reviews), 5000),]

tokens = reviews$text %>%
  tolower %>%
  word_tokenizer

it = itoken(tokens, ids = reviews$review_id)
v = create_vocabulary(it) %>%
  prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.2)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer, type = "lda_c")

# doc_topic_prior = alpha, topic_word_prior = eta
lda_model = LDA$new(n_topics = 30, vocabulary = v, 
                    doc_topic_prior = 1.7, topic_word_prior = 0.1)
doc_topic_distr = lda_model$fit_transform(dtm, n_iter = 1000, convergence_tol = 0.01, 
                                          check_convergence_every_n = 10)

lda_model$plot()
