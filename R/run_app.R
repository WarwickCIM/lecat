#' run_app
#'
#' Starts the Shiny App interface for the lecat package
#' @description The lecat package contains several functions and the run_app
#' function starts a Shiny app for interacting with these function. The Shiny app is a
#' web based interface suitable for students with little experience of R to use in place
#' of calling the function themselves. The app can run either locally - started by the
#' run_app function - or externally by placing the package and app.R file on an external server.
#'
#' @export
run_app <- function() {
  appDir <- system.file("shiny-apps", "lecat-app", package = "lecat")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `lecat`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
