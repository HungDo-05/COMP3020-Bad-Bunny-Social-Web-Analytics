
library(tidyverse)

# Load the existing reply dataset
replies <- read_csv(
  "data/replies.csv",
  show_col_types = FALSE
)

# Count replies by thread
replies %>%
  count(thread, name = "n_replies")

# Check missing and empty comments
replies %>%
  summarise(
    total_replies = n(),
    missing_text = sum(is.na(text)),
    empty_text = sum(
      is.na(text) | trimws(replace_na(text, "")) == ""
    ),
    unique_uris = n_distinct(uri),
    duplicate_uris = n() - n_distinct(uri)
  )

# Check text length by thread
replies %>%
  mutate(
    text_length = nchar(replace_na(text, ""))
  ) %>%
  group_by(thread) %>%
  summarise(
    n = n(),
    mean_length = mean(text_length),
    median_length = median(text_length),
    .groups = "drop"
  )

# ============================================================
# QUESTION 3(a): TEXT PREPROCESSING AND TF-IDF
# ============================================================

library(tidyverse)
library(tidytext)
library(Matrix)

# 1. Prepare the reply dataset
comments <- replies %>%
  filter(
    !is.na(uri),
    !is.na(thread),
    !is.na(text),
    str_trim(text) != ""
  ) %>%
  distinct(uri, .keep_all = TRUE) %>%
  mutate(
    comment_id = uri,
    thread = factor(thread, levels = c("T1", "T2", "T3")),
    text = str_to_lower(text)
  )

# 2. Tokenise comments into words
data("stop_words")

tokens <- comments %>%
  select(comment_id, thread, text) %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  anti_join(stop_words, by = "word") %>%
  filter(nchar(word) >= 3)

# 3. Calculate word frequencies
word_counts <- tokens %>%
  count(comment_id, word, name = "n")

# 4. Calculate TF-IDF
tfidf_data <- word_counts %>%
  bind_tf_idf(word, comment_id, n)

# 5. Construct Document Term Matrix
dtm <- tfidf_data %>%
  cast_sparse(comment_id, word, tf_idf)

# 6. Remove zero-vector documents
dtm <- dtm[
  Matrix::rowSums(dtm^2) > 0,
  ,
  drop = FALSE
]

# 7. Verify the matrix
cat("Documents:", nrow(dtm), "\n")
cat("Unique terms:", ncol(dtm), "\n")
cat("Nonzero entries:", length(dtm@x), "\n")

# 8. Preserve original thread labels
comment_metadata <- comments %>%
  select(comment_id, thread) %>%
  filter(comment_id %in% rownames(dtm)) %>%
  slice(match(rownames(dtm), comment_id))

stopifnot(
  identical(
    comment_metadata$comment_id,
    rownames(dtm)
  )
)

# 9. Save outputs
write_csv(
  tfidf_data,
  "data/q3_tfidf.csv"
)

write_csv(
  comment_metadata,
  "data/q3_comment_metadata.csv"
)

saveRDS(
  dtm,
  "data/q3_dtm.rds"
)

cat("Question 3(a) preprocessing completed.\n")

# ============================================================
# QUESTION 3(b): K-MEANS CLUSTERING
# ============================================================

library(tidyverse)
library(cluster)
library(Matrix)

# 1. Use the existing TF-IDF document-term matrix
stopifnot(nrow(dtm) == nrow(comment_metadata))

# 2. Convert to a numerical matrix
X <- as.matrix(dtm)

# 3. Normalise each document to unit length
row_lengths <- sqrt(rowSums(X^2))

stopifnot(all(row_lengths > 0))

X_norm <- sweep(X, 1, row_lengths, "/")

# 4. Calculate cosine distance
cosine_dist <- as.dist(
  1 - tcrossprod(X_norm)
)

# 5. Test different numbers of clusters
set.seed(42)

k_values <- 2:10

silhouette_results <- purrr::map_dfr(
  k_values,
  function(k) {
    
    set.seed(42)
    
    model <- kmeans(
      X_norm,
      centers = k,
      nstart = 25,
      iter.max = 300
    )
    
    sil <- silhouette(
      model$cluster,
      cosine_dist
    )
    
    tibble(
      k = k,
      avg_silhouette = mean(sil[, "sil_width"]),
      total_wss = model$tot.withinss
    )
  }
)

# 6. Display results
print(silhouette_results)

# 7. Identify the best k
best_k <- silhouette_results$k[
  which.max(silhouette_results$avg_silhouette)
]

cat("Optimal number of clusters:", best_k, "\n")

# 8. Plot silhouette scores
p_silhouette <- ggplot(
  silhouette_results,
  aes(x = k, y = avg_silhouette)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_vline(
    xintercept = best_k,
    linetype = "dashed"
  ) +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Question 3: Optimal Number of Clusters",
    subtitle = "Average silhouette score using cosine distance",
    x = "Number of clusters (k)",
    y = "Average silhouette score"
  ) +
  theme_minimal(base_size = 12)

print(p_silhouette)

# 9. Save results
write_csv(
  silhouette_results,
  "data/q3_silhouette_results.csv"
)

ggsave(
  "data/q3_silhouette_plot.png",
  plot = p_silhouette,
  width = 8,
  height = 5,
  dpi = 300
)

# ============================================================
# QUESTION 3(c): FINAL K-MEANS AND CLUSTER INTERPRETATION
# ============================================================

library(tidyverse)

# 1. Fit the final model using the selected k
set.seed(42)

final_kmeans <- kmeans(
  X_norm,
  centers = best_k,
  nstart = 50,
  iter.max = 300
)

# 2. Assign each comment to a cluster
cluster_assignments <- comment_metadata %>%
  mutate(
    cluster = factor(final_kmeans$cluster)
  )

# 3. Check the number of comments in each cluster
cluster_sizes <- cluster_assignments %>%
  count(cluster, name = "n_comments") %>%
  mutate(
    percentage = round(
      100 * n_comments / sum(n_comments), 2
    )
  )

print(cluster_sizes)

# 4. Compare cluster membership across threads
thread_clusters <- cluster_assignments %>%
  count(thread, cluster, name = "n_comments") %>%
  group_by(thread) %>%
  mutate(
    percentage = round(
      100 * n_comments / sum(n_comments), 2
    )
  ) %>%
  ungroup()

print(thread_clusters, n = Inf)

# 5. Identify top TF-IDF words for each cluster
cluster_terms <- tfidf_data %>%
  inner_join(
    cluster_assignments %>%
      select(comment_id, cluster),
    by = "comment_id"
  ) %>%
  group_by(cluster, word) %>%
  summarise(
    mean_tfidf = mean(tf_idf),
    .groups = "drop"
  ) %>%
  group_by(cluster) %>%
  slice_max(
    mean_tfidf,
    n = 10,
    with_ties = FALSE
  ) %>%
  arrange(cluster, desc(mean_tfidf)) %>%
  ungroup()

print(cluster_terms, n = Inf)

# 6. Save results
write_csv(
  cluster_assignments,
  "Data/q3_cluster_assignments.csv"
)

write_csv(
  cluster_sizes,
  "Data/q3_cluster_sizes.csv"
)

write_csv(
  thread_clusters,
  "Data/q3_thread_clusters.csv"
)

write_csv(
  cluster_terms,
  "Data/q3_cluster_top_terms.csv"
)

cat("Question 3(c) clustering completed.\n")

# ============================================================
# QUESTION 3(d): CLUSTER QUALITY DIAGNOSTICS
# ============================================================

library(tidyverse)
library(Matrix)

# 1. Confirm matrix dimensions and normalisation
cat("Matrix dimensions:\n")
print(dim(X_norm))

cat("\nVector length summary:\n")
print(summary(sqrt(rowSums(X_norm^2))))

# 2. Examine cluster sizes
print(cluster_sizes, n = Inf)

# 3. Examine cluster distribution across threads
cluster_thread_table <- table(
  cluster_assignments$thread,
  cluster_assignments$cluster
)

print(cluster_thread_table)

# 4. Inspect representative comments from each cluster
replies_original <- read_csv(
  "Data/replies.csv",
  show_col_types = FALSE
)

cluster_examples <- cluster_assignments %>%
  left_join(
    replies_original %>%
      transmute(
        comment_id = uri,
        text
      ),
    by = "comment_id"
  ) %>%
  group_by(cluster) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  select(cluster, thread, text)

print(cluster_examples, n = Inf, width = Inf)

# 5. Save diagnostic examples
write_csv(
  cluster_examples,
  "Data/q3_cluster_examples.csv"
)

cat("\nCluster diagnostics completed.\n")
