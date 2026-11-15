# 02_collect_threads_network.R
# Run ONCE, after 01_collect_posts.R. Reads data/posts_clean.csv and collects:
#   Q2: all replies to the 3 most-replied posts  -> data/top3_posts.csv, data/replies.csv
#   Q4: up to 50 followers + 50 follows of the 3 most active users
#                                               -> data/top3_users.csv, data/edges.csv

library(httr2)
library(dplyr)
library(purrr)
library(readr)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Log in (same .Renviron as script 01) -----------------------------------
session <- request("https://bsky.social/xrpc/com.atproto.server.createSession") |>
  req_body_json(list(identifier = Sys.getenv("BSKY_HANDLE"),
                     password   = Sys.getenv("BSKY_APP_PASSWORD"))) |>
  req_perform() |>
  resp_body_json()
token <- session$accessJwt

# One GET request with retries; returns NULL (and a message) if it still fails,
# so one unavailable account or post does not stop the whole script.
api_get <- function(endpoint, ...) {
  res <- tryCatch(
    request(paste0("https://bsky.social/xrpc/", endpoint)) |>
      req_auth_bearer_token(token) |>
      req_url_query(...) |>
      req_retry(max_tries = 6,
                is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504),
                backoff = \(i) 2^i) |>
      req_perform() |>
      resp_body_json(),
    error = function(e) {
      message("  request failed (", endpoint, "): ", conditionMessage(e))
      NULL
    }
  )
  Sys.sleep(1)  # stay well inside rate limits
  res
}

posts <- read_csv("data/posts_clean.csv", show_col_types = FALSE)

# =============================================================================
# Q2: the three most-replied posts and their full reply threads
# =============================================================================
top3 <- posts |>
  arrange(desc(reply_count), desc(like_count)) |>
  slice_head(n = 3) |>
  mutate(thread = paste0("T", 1:3))
write_csv(top3, "data/top3_posts.csv")

# Walk the reply tree recursively (replies to replies are included).
# Deleted or blocked replies have no $post field and are skipped.
flatten_replies <- function(node, thread_id, depth = 1) {
  out <- list()
  for (r in node$replies %||% list()) {
    if (is.null(r$post)) next
    out[[length(out) + 1]] <- tibble(
      thread     = thread_id,
      depth      = depth,
      uri        = r$post$uri,
      handle     = r$post$author$handle,
      text       = r$post$record$text %||% "",
      created_at = r$post$record$createdAt %||% NA_character_,
      like_count = r$post$likeCount %||% 0L
    )
    out <- c(out, flatten_replies(r, thread_id, depth + 1))
  }
  out
}

replies <- map2_dfr(top3$uri, top3$thread, function(u, t) {
  res <- api_get("app.bsky.feed.getPostThread", uri = u, depth = 50, parentHeight = 0)
  if (is.null(res)) return(tibble())
  bind_rows(flatten_replies(res$thread, t))
})
write_csv(replies, "data/replies.csv")

cat("\nReplies retrieved vs reply_count shown on the post:\n")
print(replies |> count(thread, name = "retrieved") |>
        left_join(top3 |> select(thread, handle, reply_count), by = "thread"))

# =============================================================================
# Q4: three most active users and their follower / following network
# =============================================================================
top_users <- posts |>
  count(handle, name = "n_posts", sort = TRUE) |>
  slice_head(n = 3)
write_csv(top_users, "data/top3_users.csv")

# limit = 50 is within the API maximum of 100 per page, so one page gives
# "up to 50". Bluesky returns the most recent followers/follows first.
get_handles <- function(endpoint, actor, field) {
  res <- api_get(endpoint, actor = actor, limit = 50)
  if (is.null(res)) return(character(0))
  map_chr(res[[field]], \(x) x$handle)
}

# Edge direction: A -> B means "A follows B"
edges <- map_dfr(top_users$handle, function(h) {
  followers <- get_handles("app.bsky.graph.getFollowers", h, "followers")
  follows   <- get_handles("app.bsky.graph.getFollows",   h, "follows")
  message(h, ": ", length(followers), " followers, ", length(follows), " follows retrieved")
  bind_rows(
    tibble(from = followers, to = h,       relation = "follower_of_seed"),
    tibble(from = h,         to = follows, relation = "seed_follows")
  )
})
write_csv(edges, "data/edges.csv")

cat("\nTop 3 active users:\n"); print(top_users)
cat("Edges collected:", nrow(edges), "\n")
