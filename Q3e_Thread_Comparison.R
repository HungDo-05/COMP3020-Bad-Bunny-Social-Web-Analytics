
# ============================================================
# QUESTION 3(e): THREAD AND CLUSTER COMPARISON
# ============================================================

library(tidyverse)

# 1. Load original cluster assignments

cluster_assignments <- read_csv(
  "Data/q3_cluster_assignments.csv",
  show_col_types = FALSE
)

stopifnot(
  all(
    c("comment_id", "thread", "cluster") %in%
      names(cluster_assignments)
  ),
  !anyNA(cluster_assignments$thread),
  !anyNA(cluster_assignments$cluster),
  !anyDuplicated(cluster_assignments$comment_id)
)

# 2. Count comments by thread and cluster

thread_clusters <- cluster_assignments %>%
  mutate(
    thread = factor(
      thread,
      levels = c("T1", "T2", "T3")
    ),
    cluster = factor(
      cluster,
      levels = sort(unique(cluster))
    )
  ) %>%
  count(
    thread,
    cluster,
    name = "n_comments",
    .drop = FALSE
  ) %>%
  group_by(thread) %>%
  mutate(
    percentage = round(
      100 * n_comments / sum(n_comments),
      2
    )
  ) %>%
  ungroup()

print(thread_clusters, n = Inf)

# 3. Create a thread-cluster summary table

thread_summary <- thread_clusters %>%
  group_by(thread) %>%
  summarise(
    total_comments = sum(n_comments),
    largest_cluster = as.character(
      cluster[which.max(n_comments)]
    ),
    largest_cluster_pct = max(percentage),
    .groups = "drop"
  )

print(thread_summary)

# 4. Create a stacked percentage bar chart

p_threads <- ggplot(
  thread_clusters,
  aes(
    x = thread,
    y = percentage,
    fill = cluster
  )
) +
  geom_col(
    width = 0.7,
    colour = "white",
    linewidth = 0.2
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Question 3(e): Cluster Distribution by Thread",
    subtitle = "Percentage of comments in each cluster",
    x = "Original Thread",
    y = "Percentage of comments (%)",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right"
  )

print(p_threads)

# 5. Save the chart

ggsave(
  "Data/q3_thread_cluster_distribution.png",
  plot = p_threads,
  width = 9,
  height = 6,
  dpi = 300
)

# 6. Save the comparison results

write_csv(
  thread_clusters,
  "Data/q3_thread_clusters.csv"
)

write_csv(
  thread_summary,
  "Data/q3_thread_summary.csv"
)

# 7. Final verification

cat("\nThread-cluster comparison completed.\n")
cat(
  "Total comments:",
  nrow(cluster_assignments),
  "\n"
)
cat(
  "Threads:",
  n_distinct(cluster_assignments$thread),
  "\n"
)
cat(
  "Clusters:",
  n_distinct(cluster_assignments$cluster),
  "\n"
)
cat("Results saved in Data folder.\n")
