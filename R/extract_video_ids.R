#' extract_video_ids
#'
#' Extract YouTube IDs from URLs. The URL string is split and the segment contining
#' the video is extracted. Wrapper for the urltools function param_get.
#'
#' @param urls A character array of YouTube urls
#' @param resolve If resolve is TRUE then the function will attempt to resolve URLS which do not
#' appear to be of the format www.youtube.com/watch?v=xxxxxx
#' @return  A character array of YouTube video IDs are returned.
#' These can then be passed to the comment or description download functions.
extract_video_ids <- function(urls, resolve = TRUE){
  assertive::assert_is_character(urls)
  assertive::assert_is_logical(resolve)
  youtube_url_check <- grepl(pattern = 'www.youtube.com/watch?v=', x = urls, fixed = TRUE)
  message(paste(sum(youtube_url_check), 'of your URLs appear to be YouTube URLs'))
  message(paste(sum(!youtube_url_check), 'of your URLs appear not to be YouTube URLs'))
  good_urls <- urls[youtube_url_check]
  if (resolve & (sum(!youtube_url_check) > 0)) {
    message(paste('Trying to resolve', sum(!youtube_url_check), 'URLs'))
    bad_urls <- urls[!youtube_url_check]
    new_urls <- NULL
    for (url in bad_urls) {
      new_urls <- c(new_urls, resolve_url(url))
    }
    new_youtube_url_check <- grepl(pattern = 'www.youtube.com/watch?v=', x = new_urls, fixed = TRUE)
    message(paste(sum(new_youtube_url_check), 'of your URLs were able to be resolved to a YouTube URL'))
    if (sum(new_youtube_url_check) > 0) {
      good_urls <- c(good_urls, new_urls[new_youtube_url_check])
    }
  }
  assertive::assert_is_character(good_urls)
  urltools::param_get(good_urls, parameter_names = 'v')$v
}
