#' extract_video_ids
#'
#' Extract YouTube IDs from URLs. The URL string is split and the segment contining
#' the video is extracted. Wrapper for the urltools function param_get.
#'
#' @param urls A character array of YouTube urls
#' @param resolve If resolve is TRUE then the function will attempt to resolve URLS which do not
#' appear to be of the format www.youtube.com/watch?v=xxxxxx
#' @param inShiny If inShiny is TRUE then notifications will be shown on the shiny app
#' @return  A character array of YouTube video IDs are returned.
#' These can then be passed to the comment or description download functions.
extract_video_ids <- function(urls, resolve = TRUE, inShiny = FALSE){
  assertive::assert_is_character(urls)
  assertive::assert_is_logical(resolve)
  youtube_url_check <- grepl(pattern = 'www.youtube.com/watch?v=', x = urls, fixed = TRUE)
  nice_url_message <- paste(sum(youtube_url_check), 'of your URLs appear to be YouTube URLs')
  nasty_url_message <- paste(sum(!youtube_url_check), 'of your URLs appear not to be YouTube URLs')
  # display notification on shiny if app is running else through R console
  ifelse(inShiny, shiny::showNotification(nice_url_message, type = 'message', duration = 2), message(nice_url_message))
  ifelse(inShiny, shiny::showNotification(nasty_url_message, type = 'message', duration = 2), message(nasty_url_message))
  good_urls <- urls[youtube_url_check]
  if (resolve & (sum(!youtube_url_check) > 0)) {
    url_resolve_message <- paste('Trying to resolve', sum(!youtube_url_check), 'URLs')
    ifelse(inShiny, shiny::showNotification(url_resolve_message, type = 'message', duration = 2), message(url_resolve_message))
    bad_urls <- urls[!youtube_url_check]
    new_urls <- NULL
    if (inShiny){
      shiny::withProgress(message = 'Trying to resolve non-YouTube URLs', value = 0, {
        i <- 1
        n <- length(bad_urls)
        for (url in bad_urls) {
          i <- i + 1
          shiny::incProgress(1/n, detail = paste("Resolving url", i))
          new_urls <- c(new_urls, resolve_url(url))
        }
      })
    } else {
      for (url in bad_urls) {
        new_urls <- c(new_urls, resolve_url(url))
      }
    }
    new_youtube_url_check <- grepl(pattern = 'www.youtube.com/watch?v=', x = new_urls, fixed = TRUE)
    result_message <- paste(sum(new_youtube_url_check), 'of your URLs were able to be resolved to a YouTube URL')
    ifelse(inShiny, shiny::showNotification( result_message, type = 'message', duration = 2), message( result_message))
    if (sum(new_youtube_url_check) > 0) {
      good_urls <- c(good_urls, new_urls[new_youtube_url_check])
    }
  }
  assertive::assert_is_character(good_urls)
  urltools::param_get(good_urls, parameter_names = 'v')$v
}
