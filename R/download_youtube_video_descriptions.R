#' download_youtube_video_descriptions
#'
#' Downloads the video descriptions of multiple YouTube videos using the V3 YouTube API.
#' @description Downloads the description of multiple videos using the YouTube V3 API.
#' Use the \link[lecat]{download_youtube_video_description} function for a single YouTube video.
#'
#' @param video_ids Character array containing multiple YouTube IDs
#' @param api_key An API key able to access the YouTube API v3
#' @param part The data you want returned. See the YouTube documentation for details. Defaults to 'snippet, statistics'
#' @param inShiny If inShiny is TRUE then shiny based notifications will be shown
#' @return Data frame containing YouTube ID, title, published and description
#'
download_youtube_video_descriptions <- function(video_ids, api_key, part = 'snippet', inShiny = FALSE){
  assertive::assert_is_character(video_ids)
  assertive::assert_is_character(api_key)
  assertive::assert_is_character(part)
  result <- NULL
  if (inShiny) {
    n <- length(video_ids)
    shiny::withProgress(message = 'Download YouTube descriptions', value = 0, {
      for (i in 1:length(video_ids)) {
        id <- video_ids[i]
        shiny::incProgress(1/n, detail = paste("Video", i))
        result <- rbind(
          result,
          download_youtube_video_description(video_id = id, api_key = api_key)
        )
      }
    })
  } else {
    pb <- utils::txtProgressBar(min = 1, max = length(video_ids), initial = 1)
    for (i in 1:length(video_ids)) {
      id <- video_ids[i]
      utils::setTxtProgressBar(pb, i)
      result <- rbind(
        result,
        download_youtube_video_description(video_id = id, api_key = api_key)
      )
    }
    close(pb)
  }
  result
}
