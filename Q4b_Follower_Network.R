
# COMP3020 - QUESTION 4(b)
# Directed Bluesky follower network

library(atrrr)
library(tidyverse)
library(igraph)

dir.create("Data", showWarnings = FALSE)

top_users <- read_csv(
  "Data/q4_top_three_users.csv",
  show_col_types = FALSE
)

# API request with retries
safe_get <- function(actor, type, limit = 50L) {
  
  for (attempt in 1:3) {
    
    result <- tryCatch(
      {
        if (type == "followers") {
          atrrr::get_followers(
            actor = actor,
            limit = limit,
            verbose = FALSE
          )
        } else {
          atrrr::get_follows(
            actor = actor,
            limit = limit,
            verbose = FALSE
          )
        }
      },
      error = function(e) {
        message(
          actor, " / ", type, ": ",
          conditionMessage(e)
        )
        NULL
      }
    )
    
    if (!is.null(result)) {
      return(list(
        data = head(result, limit),
        status = "success"
      ))
    }
    
    Sys.sleep(2^attempt)
  }
  
  list(
    data = tibble(),
    status = "failed"
  )
}

edges_list <- list()
request_log <- list()

for (i in seq_len(nrow(top_users))) {
  
  actor <- top_users$author_did[i]
  handle <- top_users$author_handle[i]
  
  cat("\nProcessing:", handle, "\n")
  
  for (type in c("followers", "following")) {
    
    response <- safe_get(actor, type)
    
    result <- response$data
    
    request_log[[length(request_log) + 1]] <-
      tibble(
        actor = actor,
        handle = handle,
        type = type,
        status = response$status,
        retrieved = nrow(result)
      )
    
    if (
      nrow(result) == 0 ||
      !all(c("did", "actor_handle") %in% names(result))
    ) {
      next
    }
    
    if (type == "followers") {
      
      edges <- tibble(
        from = result$did,
        to = actor,
        from_handle = result$actor_handle,
        to_handle = handle
      )
      
    } else {
      
      edges <- tibble(
        from = actor,
        to = result$did,
        from_handle = handle,
        to_handle = result$actor_handle
      )
    }
    
    edges_list[[length(edges_list) + 1]] <- edges
    
    Sys.sleep(1.5)
  }
}

log_df <- bind_rows(request_log)

write_csv(
  log_df,
  "Data/q4_network_collection_log.csv"
)

if (length(edges_list) == 0) {
  stop("No network edges retrieved.")
}

edges <- bind_rows(edges_list) %>%
  filter(
    !is.na(from),
    !is.na(to),
    from != to
  ) %>%
  distinct(from, to, .keep_all = TRUE)

write_csv(
  edges,
  "Data/q4_network_edges.csv"
)

# Build directed graph
g <- graph_from_data_frame(
  edges %>% select(from, to),
  directed = TRUE
)

# Remove isolated nodes only if present
g <- delete_vertices(
  g,
  which(degree(g, mode = "all") == 0)
)

# Identify three seed users
V(g)$is_seed <- V(g)$name %in%
  top_users$author_did

set.seed(42)

png(
  "Data/q4_network_graph.png",
  width = 1800,
  height = 1400,
  res = 180
)

plot(
  g,
  layout = layout_with_fr(g),
  vertex.size = ifelse(V(g)$is_seed, 12, 4),
  vertex.color = ifelse(
    V(g)$is_seed,
    "#E76F51",
    "#457B9D"
  ),
  vertex.label = ifelse(
    V(g)$is_seed,
    top_users$author_handle[
      match(V(g)$name, top_users$author_did)
    ],
    NA_character_
  ),
  vertex.label.cex = 0.65,
  vertex.label.color = "black",
  edge.arrow.size = 0.18,
  edge.color = adjustcolor(
    "grey45",
    alpha.f = 0.35
  ),
  main = "Directed Bluesky Follower Network"
)

dev.off()

saveRDS(
  g,
  "Data/q4_network_graph.rds"
)

cat("\n========== NETWORK SUMMARY ==========\n")
cat("Nodes:", vcount(g), "\n")
cat("Directed edges:", ecount(g), "\n")

cat("\n========== REQUEST LOG ==========\n")
print(log_df, n = Inf)

cat("\nQuestion 4(b) completed.\n")
