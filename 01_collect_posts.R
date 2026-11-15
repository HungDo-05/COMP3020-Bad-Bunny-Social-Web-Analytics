# 01_collect_posts.R
# Collect Bluesky posts about Bad Bunny's Super Bowl LX halftime show
# Event split point: halftime start, approx. 8:00 pm ET on 8 Feb 2026 = 01:00 UTC 9 Feb 2026
# Window: 6 days before vs 6 days after (keeps the 1 Feb Grammys ceremony out of "Before")

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(lubridate)

# ---- 1. Settings ------------------------------------------------------------
event_time  <- ymd_hms("2026-02-09 01:00:00", tz = "UTC")
window_days <- 6
start_time  <- event_time - days(window_days)
end_time    <- event_time + days(window_days)

queries <- c('"Bad Bunny"', '"halftime show"')

slice_hours <- 6     # search in 6-hour slices so busy hours are not crowded out
max_pages   <- 10    # per query per slice; 10 x 100 = up to 1,000 posts
pause_sec   <- 1     # pause between requests to respect rate limits

dir.create("data", showWarnings = FALSE)

# ---- 2. Log in --------------------------------------------------------------
# Login details come from .Renviron (BSKY_HANDLE, BSKY_APP_PASSWORD)
session <- request("https://bsky.social/xrpc/com.atproto.server.createSession") |>
  req_body_json(list(identifier = Sys.getenv("BSKY_HANDLE"),
                     password   = Sys.getenv("BSKY_APP_PASSWORD"))) |>
  req_perform() |>
  resp_body_json()
token <- session$accessJwt

iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# ---- 3. Search one query within one time slice, following the cursor -------
search_slice <- function(q, since, until) {
  out <- list()
  cursor <- NULL
  for (page in seq_len(max_pages)) {
    body <- tryCatch(
      request("https://bsky.social/xrpc/app.bsky.feed.searchPosts") |>
        req_auth_bearer_token(token) |>
        req_url_query(q = q, since = iso(since), until = iso(until),
                      sort = "latest", limit = 100, cursor = cursor) |>
        req_retry(max_tries = 6,
                  is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504),
                  backoff = \(i) 2^i) |>
        req_perform() |>
        resp_body_json(),
      error = function(e) { message("  request failed: ", conditionMessage(e)); NULL }
    )
    if (is.null(body)) break
    out <- c(out, body$posts)
    cursor <- body$cursor
    if (is.null(cursor) || length(body$posts) == 0) break
    Sys.sleep(pause_sec)
  }
  out
}

# ---- 4. Loop over queries x time slices -------------------------------------
slice_starts <- seq(start_time, end_time - hours(slice_hours),
                    by = paste(slice_hours, "hours"))
raw <- list()
log <- list()

for (q in queries) {
  for (i in seq_along(slice_starts)) {
    s <- slice_starts[i]
    u <- s + hours(slice_hours)
    posts <- search_slice(q, s, u)
    capped <- length(posts) >= max_pages * 100
    message(q, " | ", iso(s), " -> ", length(posts), " posts", if (capped) "  (CAP REACHED)")
    raw <- c(raw, posts)
    log[[length(log) + 1]] <- tibble(query = q, since = iso(s), until = iso(u),
                                     n_returned = length(posts), capped = capped,
                                     collected_at = iso(now(tzone = "UTC")))
  }
}

write_json(raw, "data/posts_raw.json", auto_unbox = TRUE, null = "null")
log_df <- bind_rows(log)
write.csv(log_df, "data/collection_log.csv", row.names = FALSE)

# ---- 5. Flatten, de-duplicate, label periods --------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

posts_df <- map_dfr(raw, \(p) tibble(
  uri          = p$uri,
  handle       = p$author$handle,
  did          = p$author$did,
  text         = p$record$text %||% "",
  created_at   = p$record$createdAt %||% NA_character_,
  reply_count  = p$replyCount  %||% 0L,
  repost_count = p$repostCount %||% 0L,
  like_count   = p$likeCount   %||% 0L,
  quote_count  = p$quoteCount  %||% 0L,
  langs        = paste(unlist(p$record$langs), collapse = ",")
)) |>
  distinct(uri, .keep_all = TRUE) |>
  mutate(created_at = ymd_hms(created_at, tz = "UTC", quiet = TRUE),
         url = paste0("https://bsky.app/profile/", handle, "/post/", sub(".*/", "", uri))) |>
  filter(!is.na(created_at), created_at >= start_time, created_at < end_time) |>
  mutate(period = if_else(created_at < event_time, "Before", "After"))

write.csv(posts_df, "data/posts_clean.csv", row.names = FALSE)

# ---- 6. Pilot check ---------------------------------------------------------
cat("\nUnique posts:", nrow(posts_df), "\n")
print(table(posts_df$period))
cat("Slices that hit the page cap:", sum(log_df$capped), "of", nrow(log_df), "\n")
print(posts_df |> count(day = as_date(created_at)))
print(posts_df |> count(langs, sort = TRUE) |> head(5))
print(posts_df |> arrange(desc(reply_count)) |> select(handle, reply_count, like_count, url) |> head(5))
