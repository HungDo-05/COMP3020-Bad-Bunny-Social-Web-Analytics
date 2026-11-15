
# ============================================================
# COMP3020 - QUESTION 4(a)
# Identify the top three most active Bluesky users
# Topic: Bad Bunny Super Bowl halftime show
# Publication period: 09-19 February 2026
# ============================================================

library(atrrr)
library(tidyverse)
library(lubridate)

dir.create("Data", showWarnings = FALSE)

# ------------------------------------------------------------
# 1. SEARCH SETTINGS
# ------------------------------------------------------------

start_date <- "2026-02-09"
end_date <- "2026-02-20"

search_queries <- c(
  '"Bad Bunny" halftime',
  '"Bad Bunny" "Super Bowl"',
  '"Benito" halftime',
  '"Bad Bunny"'
)

# Number of posts requested for each query
posts_per_query <- 1000L

# ------------------------------------------------------------
# 2. RETRIEVE POSTS
# ------------------------------------------------------------

all_results <- list()
search_log <- list()

for (query in search_queries) {
  
  cat("\nSearching:", query, "\n")
  
  result <- tryCatch(
    atrrr::search_post(
      q = query,
      since = start_date,
      until = end_date,
      sort = "latest",
      limit = posts_per_query
    ),
    error = function(e) {
      message("Search failed: ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(result)) {
    
    search_log[[length(search_log) + 1]] <-
      tibble(
        query = query,
        status = "failed",
        retrieved = 0L
      )
    
    next
  }
  
  search_log[[length(search_log) + 1]] <-
    tibble(
      query = query,
      status = "success",
      retrieved = nrow(result)
    )
  
  if (nrow(result) > 0) {
    result$search_query <- query
    all_results[[length(all_results) + 1]] <- result
  }
  
  Sys.sleep(2)
}

log_df <- bind_rows(search_log)

write_csv(
  log_df,
  "Data/q4_search_log.csv"
)

if (length(all_results) == 0) {
  stop("No posts retrieved. Check q4_search_log.csv.")
}

# ------------------------------------------------------------
# 3. COMBINE RESULTS
# ------------------------------------------------------------

raw_posts <- bind_rows(all_results)

# Retain columns required for analysis
raw_posts <- raw_posts %>%
  select(
    uri,
    author_handle,
    text,
    created_at,
    search_query
  )

write_csv(
  raw_posts,
  "Data/q4_raw_search_results.csv"
)

# ------------------------------------------------------------
# 4. DEDUPLICATE AND ENFORCE DATE WINDOW
# ------------------------------------------------------------

unique_posts <- raw_posts %>%
  filter(
    !is.na(uri),
    !is.na(author_handle),
    !is.na(created_at)
  ) %>%
  distinct(uri, .keep_all = TRUE) %>%
  mutate(
    created_at = as.POSIXct(
      created_at,
      tz = "UTC"
    )
  ) %>%
  filter(
    created_at >= as.POSIXct(
      "2026-02-09 00:00:00",
      tz = "UTC"
    ),
    created_at < as.POSIXct(
      "2026-02-20 00:00:00",
      tz = "UTC"
    )
  )

# ------------------------------------------------------------
# 5. TOPIC RELEVANCE FILTER
# ------------------------------------------------------------

topic_posts <- unique_posts %>%
  filter(
    !is.na(text),
    str_detect(
      text,
      regex(
        "bad\\s*bunny|benito",
        ignore_case = TRUE
      )
    ),
    str_detect(
      text,
      regex(
        "super\\s*bowl|half[ -]?time|sb60|sb lx",
        ignore_case = TRUE
      )
    )
  ) %>%
  mutate(
    author_did = str_extract(
      uri,
      "(?<=at://)did:[^/]+"
    )
  ) %>%
  filter(!is.na(author_did))

write_csv(
  topic_posts,
  "Data/q4_topic_posts.csv"
)

# ------------------------------------------------------------
# 6. COUNT POSTS PER AUTHOR
# ------------------------------------------------------------

author_activity <- topic_posts %>%
  group_by(author_did) %>%
  summarise(
    author_handle = first(author_handle),
    topic_post_count = n_distinct(uri),
    .groups = "drop"
  ) %>%
  arrange(
    desc(topic_post_count),
    author_did
  ) %>%
  mutate(rank = row_number()) %>%
  select(
    rank,
    author_did,
    author_handle,
    topic_post_count
  )

top_three_users <- author_activity %>%
  slice_head(n = 3)

write_csv(
  author_activity,
  "Data/q4_author_activity.csv"
)

write_csv(
  top_three_users,
  "Data/q4_top_three_users.csv"
)

# ------------------------------------------------------------
# 7. COLLECTION SUMMARY
# ------------------------------------------------------------

collection_summary <- tibble(
  metric = c(
    "Raw retrieved posts",
    "Unique posts in date range",
    "Topic-related posts",
    "Unique topic-related authors",
    "Successful search queries",
    "Failed search queries"
  ),
  value = c(
    nrow(raw_posts),
    nrow(unique_posts),
    nrow(topic_posts),
    nrow(author_activity),
    sum(log_df$status == "success"),
    sum(log_df$status == "failed")
  )
)

write_csv(
  collection_summary,
  "Data/q4_collection_summary.csv"
)

cat("\n========== COLLECTION SUMMARY ==========\n")
print(collection_summary, n = Inf)

cat("\n========== TOP THREE USERS ==========\n")
print(top_three_users, n = Inf)

cat("\n========== SEARCH LOG ==========\n")
print(log_df, n = Inf)

cat("\nQuestion 4(a) data collection completed.\n")

