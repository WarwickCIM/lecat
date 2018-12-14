#' download_youtube_video_description
#'
#' Downloads a single YouTube video title, ID, publish time and description.
#'
#' @param video_id The id of the YouTube video
#' @param api_key An API key able to access the YouTube API v3
#' @param part The data you want returned. See the YouTube documentation for details. Defaults to 'snippet, statistics'.
#'
#' @return Data frame containing the descriptions and statistics of the YouTube video identified by the video_id parameter.
download_youtube_video_description <- function(video_id, api_key, part = 'snippet'){
  assertive::assert_is_character(video_id)
  assertive::assert_is_character(api_key)
  assertive::assert_is_character(part)
  api_url <- 'https://www.googleapis.com/youtube/v3/videos'
  api_options <- list(
    part = part,
    key = api_key,
    id = video_id
  )
  response <- plyr::ldply(
    httr::content(httr::GET(api_url, query = api_options))$items,
    data.frame,
    stringsAsFactors = FALSE
  )
  data.frame(
    id = response$id,
    published = response$snippet.publishedAt,
    title = response$snippet.title,
    description = response$snippet.description,
    stringsAsFactors = FALSE
  )
}
