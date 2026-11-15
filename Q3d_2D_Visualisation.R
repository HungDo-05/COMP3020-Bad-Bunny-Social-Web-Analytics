
# ============================================================
# QUESTION 3(d): 2D CLUSTER VISUALISATION
# ============================================================

library(tidyverse)
library(Matrix)

# 1. Load original TF-IDF matrix and cluster assignments

dtm <- readRDS("Data/q3_dtm.rds")

cluster_assignments <- read_csv(
  "Data/q3_cluster_assignments.csv",
  show_col_types = FALSE
)

# 2. Match comments to their original matrix rows

stopifnot(
  !is.null(rownames(dtm)),
  !anyDuplicated(rownames(dtm)),
  !anyDuplicated(cluster_assignments$comment_id)
)

plot_data <- cluster_assignments %>%
  slice(match(rownames(dtm), comment_id))

stopifnot(
  nrow(plot_data) == nrow(dtm),
  !anyNA(plot_data$comment_id),
  identical(
    plot_data$comment_id,
    rownames(dtm)
  )
)

# 3. Reduce TF-IDF data to two dimensions using SVD

# Install once if needed:
# install.packages("irlba")

if (!requireNamespace("irlba", quietly = TRUE)) {
  stop("Please install irlba using install.packages('irlba')")
}

set.seed(42)

svd_result <- irlba::irlba(
  dtm,
  nv = 2,
  nu = 2
)

coordinates <- sweep(
  svd_result$u,
  2,
  svd_result$d,
  "*"
)

# 4. Combine coordinates with cluster and thread labels

plot_data <- plot_data %>%
  mutate(
    SVD1 = coordinates[, 1],
    SVD2 = coordinates[, 2],
    cluster = factor(cluster),
    thread = factor(
      thread,
      levels = c("T1", "T2", "T3")
    )
  )

print(
  plot_data %>%
    count(thread, cluster),
  n = Inf
)

# 5. Create the 2D clustering plot

p_2d <- ggplot(
  plot_data,
  aes(
    x = SVD1,
    y = SVD2,
    colour = cluster,
    shape = thread
  )
) +
  geom_point(
    size = 2,
    alpha = 0.65
  ) +
  scale_shape_manual(
    values = c(
      T1 = 16,
      T2 = 17,
      T3 = 15
    )
  ) +
  labs(
    title = "Question 3(d): 2D Clustering Visualisation",
    subtitle = "TF-IDF projection using truncated SVD",
    x = "SVD Component 1",
    y = "SVD Component 2",
    colour = "Cluster",
    shape = "Original Thread"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right"
  )

print(p_2d)

# 6. Save the plot and coordinates

ggsave(
  "Data/q3_2d_clusters_threads.png",
  plot = p_2d,
  width = 10,
  height = 7,
  dpi = 300
)

write_csv(
  plot_data,
  "Data/q3_2d_coordinates.csv"
)

cat("\n2D clustering visualisation completed.\n")
cat("Comments:", nrow(plot_data), "\n")
cat(
  "Clusters:",
  n_distinct(plot_data$cluster),
  "\n"
)
cat("Results saved in Data folder.\n")
