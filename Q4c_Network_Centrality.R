
# ============================================================
# COMP3020 - QUESTION 4(c)
# Degree Centrality and PageRank
# ============================================================

library(igraph)
library(tidyverse)

# 1. Load the directed network from Question 4(b)

g <- readRDS("Data/q4_network_graph.rds")

top_users <- read_csv(
  "Data/q4_top_three_users.csv",
  show_col_types = FALSE
)

edges <- read_csv(
  "Data/q4_network_edges.csv",
  show_col_types = FALSE
)

# 2. Create DID-to-handle lookup

handle_lookup <- bind_rows(
  edges %>% transmute(
    did = from,
    handle = from_handle
  ),
  edges %>% transmute(
    did = to,
    handle = to_handle
  ),
  top_users %>% transmute(
    did = author_did,
    handle = author_handle
  )
) %>%
  filter(
    !is.na(did),
    !is.na(handle),
    handle != ""
  ) %>%
  distinct(did, .keep_all = TRUE)

# 3. Calculate centrality measures

centrality_results <- tibble(
  did = V(g)$name,
  in_degree = degree(
    g,
    mode = "in"
  ),
  out_degree = degree(
    g,
    mode = "out"
  ),
  total_degree = degree(
    g,
    mode = "all"
  ),
  pagerank = page_rank(
    g,
    directed = TRUE,
    damping = 0.85
  )$vector
) %>%
  left_join(
    handle_lookup,
    by = "did"
  ) %>%
  mutate(
    handle = coalesce(handle, did)
  ) %>%
  select(
    did,
    handle,
    in_degree,
    out_degree,
    total_degree,
    pagerank
  )

write_csv(
  centrality_results,
  "Data/q4_centrality_results.csv"
)

# 4. Identify highest-ranking users

top_in_degree <- centrality_results %>%
  filter(in_degree == max(in_degree)) %>%
  arrange(handle)

top_out_degree <- centrality_results %>%
  filter(out_degree == max(out_degree)) %>%
  arrange(handle)

top_total_degree <- centrality_results %>%
  filter(total_degree == max(total_degree)) %>%
  arrange(handle)

top_pagerank <- centrality_results %>%
  filter(
    abs(pagerank - max(pagerank)) < 1e-12
  ) %>%
  arrange(handle)

# 5. Display results

cat("\n========== NETWORK SUMMARY ==========\n")
cat("Nodes:", vcount(g), "\n")
cat("Directed edges:", ecount(g), "\n")

cat("\n========== TOP IN-DEGREE ==========\n")
print(top_in_degree, n = Inf)

cat("\n========== TOP OUT-DEGREE ==========\n")
print(top_out_degree, n = Inf)

cat("\n========== TOP TOTAL DEGREE ==========\n")
print(top_total_degree, n = Inf)

cat("\n========== TOP PAGERANK ==========\n")
print(top_pagerank, n = Inf)

# 6. Compare highest total degree and PageRank

same_top_user <- any(
  top_total_degree$did %in% top_pagerank$did
)

cat("\n========== DEGREE VS PAGERANK ==========\n")
cat(
  "Do the highest total-degree and PageRank",
  "users overlap?",
  same_top_user,
  "\n"
)

# 7. Save top-10 tables

top10_degree <- centrality_results %>%
  arrange(desc(total_degree), handle) %>%
  slice_head(n = 10)

top10_pagerank <- centrality_results %>%
  arrange(desc(pagerank), handle) %>%
  slice_head(n = 10)

write_csv(
  top10_degree,
  "Data/q4_top10_degree.csv"
)

write_csv(
  top10_pagerank,
  "Data/q4_top10_pagerank.csv"
)

cat("\n========== TOP 10 BY TOTAL DEGREE ==========\n")
print(top10_degree, n = Inf)

cat("\n========== TOP 10 BY PAGERANK ==========\n")
print(top10_pagerank, n = Inf)

cat("\nQuestion 4(c) completed.\n")
