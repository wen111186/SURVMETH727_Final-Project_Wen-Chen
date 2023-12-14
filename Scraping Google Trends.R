gtrends_with_backoff <- function(keyword = NA,
                                 geo = "ch",
                                 time = "today+3-y",
                                 gprop = "web",
                                 category = "0",
                                 hl = "en-US",
                                 low_search_volume = FALSE,
                                 cookie_url = "http://trends.google.com/Cookies/NID",
                                 tz = 0,
                                 onlyInterest = FALSE,
                                 retry = 5,
                                 wait = 5,
                                 quiet = FALSE,
                                 attempt = 1) {
  msg <- function(...) {
    if (!quiet) {
      message(...)
    }
  }
  
  if (attempt > retry) {
    stop("Retries exhausted!")
  }
  
  if (attempt == 1) {
    msg("Downloading data for ", time)
  } else {
    msg("Attempt ", attempt, "/", retry)
  }
  tryCatch(
    gtrends(
      keyword = keyword, geo = geo, time = time, gprop = gprop,
      category = category, hl = hl,
      low_search_volume = low_search_volume, cookie_url = cookie_url,
      tz = tz, onlyInterest = onlyInterest
    ),
    error = function(e) {
      if (grepl("== 200 is not TRUE", e)) {
        msg("Server is not accepting requests")
      } else if (grepl("code\\:429", e)) {
        msg("Server response: 429 - too many requests")
      } else if (grepl("code\\:500", e)) {
        msg("Server response: 500 - internal server error")
      } else {
        stop(e)
      }
      
      t <- attempt * wait
      
      msg("Waiting for ", t, " seconds")
      Sys.sleep(t)
      msg("Retrying...")
      
      # Error handling by recursion
      gtrends_with_backoff(
        keyword,
        geo,
        time,
        gprop,
        category,
        hl,
        low_search_volume,
        cookie_url,
        tz,
        onlyInterest,
        retry,
        wait,
        quiet,
        attempt + 1
      )
    }
  )
}


# Call the function with the desired parameters
trends_150 <- gtrends_with_backoff(
  keyword = "Mass Effect Legendary Edition",   # the search term
  geo = "US",                  # Geographical area, set to US
  time = "2021-01-01 2023-12-01", # Time frame for the data
  gprop = "web",               # Type of search, 'web' for web searches
  retry = 5,                   # Number of retries
  wait = 5                     # Wait time in seconds between retries
)




calculate_average_monthly_interest <- function(trends) {
  # Convert the date column to Date type
  trends$interest_over_time$date <- as.Date(trends$interest_over_time$date)
  
  # Group by month and calculate average
  monthly_avg <- trends$interest_over_time %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarise(average_interest = mean(hits))
  
  # Calculate the overall average monthly interest
  avg_month_interest <- sum(monthly_avg$average_interest) / nrow(monthly_avg)
  
  return(avg_month_interest)
}

# Convert the date column to a date object if it's not already
interest_over_time_5<-trends_5$interest_over_time
interest_over_time_5$date <- as.Date(interest_over_time_5$date)

bigmouth <- function(interest_over_time) {
  monthly_interest <- interest_over_time %>%
    mutate(month = floor_date(as.Date(date), "month")) %>%
    group_by(month) %>%
    summarise(total_hits = sum(hits), .groups = 'drop')
  
  largest_month <- monthly_interest[which.max(monthly_interest$total_hits),]
  return(largest_month)
}

interest_over_time_150<-trends_150$interest_over_time
interest_over_time_150$date <- as.Date(interest_over_time_150$date)

avgmonth_150 <- calculate_average_monthly_interest(trends_150)

bigmonth_150 <- bigmouth(interest_over_time_150)$total_hits
# Pool the new varable avgmonth and bigmonth together to get the cleaned dataset



