library(tidyverse)
library(jsonlite)
library(lubridate)


if(file.exists(".Renviron")){
  print(1)
  readRenviron(".Renviron")
} else {
  print(2)
  readRenviron("../.Renviron")
}

api_key <- Sys.getenv("api_key")

dice_tower <- "UCiwBbXQlljGjKtKhcdMliRA"
david_robinson <- "UCeiiqmVK07qhY-wvg3IZiZQ"

lates_video <- function(channel_id) {
  json <- glue::glue("https://www.googleapis.com/youtube/v3/search?part=snippet&channelId={channel_id}&maxResults=1&order=date&type=video&key={api_key}")

  df_late <- tibble(
    key = read_json(json) %>% names(),
    json = read_json(json)
  ) %>%
    filter(key == "items") %>%
    unnest(json) %>%
    unnest_wider(json, names_sep = "_") %>%
    unnest_wider(json_id) %>%
    unnest_wider(json_snippet) %>%
    mutate(publishTime = ymd_hms(publishTime)) %>%
    mutate(url = glue::glue("https://www.youtube.com/watch?v={videoId}")) %>%
    unnest_wider(thumbnails, names_sep = "_") %>%
    unnest_wider(thumbnails_default, names_sep = "_") %>%
    unnest_wider(thumbnails_medium, names_sep = "_") %>%
    unnest_wider(thumbnails_high, names_sep = "_") %>%
    rename(kind_vid = kind)

  vid_details <- df_late %>%
    select(videoId) %>%
    mutate(json_url = glue::glue("https://www.googleapis.com/youtube/v3/videos?id={videoId}&part=contentDetails&key={api_key}"),
           json = map(json_url, read_json)) %>%
    unnest_wider(json) %>%
    unnest(items) %>%
    unnest_wider(items, names_sep = "_") %>%
    unnest_wider(items_contentDetails, names_sep = "_") %>%
    unnest_wider(pageInfo) %>%
    mutate(duration = str_remove_all(items_contentDetails_duration, "PT")) %>%
    mutate(duration = duration(duration)) %>%
    mutate(duration_seconds = seconds(duration))



  df <- df_late %>%
    left_join(
      vid_details
    )

  return(df)
}

dt <- lates_video(dice_tower)



dr <- lates_video(david_robinson)

prep_last <- function(df_latest){
  vid <- df_latest$url
  time <- df_latest$duration_seconds

  return(list("id" = vid, "duration" = time))

}

play_last <- function(prep){
  url <- prep[["id"]]
  time <- prep[["duration"]]

  browseURL(url)
  i <- 5
  while (i != 0) {
    print(i)
    Sys.sleep(1)
    i <- i - 1
  }
  KeyboardSimulator::keybd.press(" ")
  i <- time + 1
  while (i != 0) {
    print(i)
    Sys.sleep(1)
    i <- i - 1
  }
  KeyboardSimulator::keybd.press("ctrl+w")
}

dr %>%
  prep_last() %>%
  play_last()

