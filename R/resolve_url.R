#' resolve_url
#'
#' Attempt to resolve a URL using the CURL program
#'
#' @param url URL to try and resolve
resolve_url <- function(url){
  assertive::assert_is_character(url)
  tryCatch(
    curl::curl_fetch_memory(url, curl::new_handle(nobody = TRUE))$url,
    error = function(c) 'error'
  )
}
