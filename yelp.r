load(("~/Downloads/americanRestaurantReviews.rda"))

library(RTextTools)
library(topicmodels)
library(tm)
library(reshape2)
library(dplyr)
library(SnowballC)
reviews = americanRestaurantReviews
reviews = reviews[sample(nrow(reviews), 10000),]

# Create DocumentTermMatrix using reviews
mat = create_matrix(cbind(as.vector(reviews$business_id), as.vector(reviews$text)), 
                    language = "english", minWordLength = 3, removeNumbers = TRUE, removePunctuation = TRUE,
                    stemWords = TRUE, removeStopwords = TRUE,
                    weighting = weightTf)

# Make sure all of the reviews in the matrix have one or more words
rowTotals = apply(mat, 1, sum)
mat = mat[rowTotals > 0 & !is.na(rowTotals), ]

# Run LDA with 15 topics and find the posterior probabilities
lda = LDA(mat, k = 20, method = "Gibbs")
posterior_probs = posterior(lda)
posterior_probs_mat = posterior_probs$terms

# Function to extract words with 20 highest probabilities for each topic 
topics_df = function(col) {
  topic_df = add_rownames(as.data.frame(col), var = "word")
  colnames(topic_df) = c("word", "posteriorProb")
  topic_df = head(as.data.frame(topic_df[order(topic_df$posteriorProb, decreasing = TRUE), ]), n = 20)
  return(topic_df)
}

# Apply function to each topic and rbind dataframes in resulting list together 
lst = apply(posterior_probs_mat, 1, FUN = topics_df)
final_topics_df = do.call(rbind, lst)
final_topics_df$topic = rep(names(lst), sapply(lst, nrow))
final_topics_df$topic = as.numeric(final_topics_df$topic)










