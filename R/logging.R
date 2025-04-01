library(googlesheets4)

gs4_auth(path = here("keys/data-visualizer-logging.json"))  # authenticate with service account
sheet_id <- "1q7TG7OIs7goq8ul2rhxUjT6kEi5st3b2KiQMjXY0YCc"

log_to_gsheet <- function(event_type, details = "", user_id = Sys.getenv("SHINY_USER", unset = "anon")) {
  log_entry <- data.frame(
    timestamp = as.character(Sys.time()),
    user = user_id,
    event = event_type,
    details = details,
    stringsAsFactors = FALSE
  )
  sheets_append(ss = sheet_id, data = log_entry)
}
