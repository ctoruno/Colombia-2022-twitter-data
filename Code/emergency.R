batch.df <- search_tweets(candidates_query1, 
                          n = 17500, 
                          include_rts = F,
                          max_id = min.tweet[1],
                          lang = "es")
min.tweet <- batch.df %>%
  slice_min(created_at, n = 1) %>%
  pull(status_id)
print(paste0("Last tweet from previous extraction has the following ID: ", min.tweet))

print(Sys.time())

# raw_tweets <- batch.df
raw_tweets <- raw_tweets %>%
  bind_rows(batch.df)
