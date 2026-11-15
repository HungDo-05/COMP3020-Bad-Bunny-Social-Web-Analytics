
# ============================================================
# QUESTION 3b: COMPARE ALTERNATIVE CLUSTER SOLUTIONS
# ============================================================

library(tidyverse)

# 1. Test smaller numbers of clusters
k_values <- 2:5

comparison <- map_dfr(k_values, function(k) {
  
  set.seed(42)
  
  model <- kmeans(
    X_norm,
    centers = k,
    nstart = 50,
    iter.max = 300
  )
  
  sizes <- as.integer(table(
    factor(model$cluster, levels = seq_len(k))
  ))
  
  tibble(
    k = k,
    smallest_cluster = min(sizes),
    largest_cluster = max(sizes),
    largest_percentage = round(
      100 * max(sizes) / length(model$cluster),
      2
    ),
    total_wss = model$tot.withinss
  )
})

# 2. Display comparison
print(comparison, n = Inf)

# 3. Plot the largest cluster percentage
p_balance <- ggplot(
  comparison,
  aes(x = k, y = largest_percentage)
) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Question 3: Cluster Balance Comparison",
    x = "Number of clusters (k)",
    y = "Largest cluster (%)"
  ) +
  theme_minimal(base_size = 12)

print(p_balance)

# 4. Save comparison
write_csv(
  comparison,
  "Data/q3_cluster_comparison.csv"
)

ggsave(
  "Data/q3_cluster_balance.png",
  plot = p_balance,
  width = 8,
  height = 5,
  dpi = 300
)

cat("Cluster comparison completed.\n")

# Examine document lengths after preprocessing

document_lengths <- Matrix::rowSums(dtm > 0)

summary(document_lengths)

# Percentage of documents with 1 to 5 terms
mean(document_lengths <= 5) * 100

# Percentage of documents with 1 to 10 terms
mean(document_lengths <= 10) * 100

# Distribution of document lengths
hist(
  document_lengths,
  breaks = 30,
  main = "Number of Terms per Comment",
  xlab = "Number of unique terms",
  ylab = "Number of comments"
)
