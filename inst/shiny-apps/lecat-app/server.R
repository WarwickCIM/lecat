function(input, output, session) {

  # Reactive values for storing persistent data -----------------------

  # These objects are overwritten as the user interacts with the app.
  # Reactive values are used to dat can be accessed accross functions.
  data <- reactiveValues(
    youtube_urls = data.frame(
      urls = 'No URLs loaded',
      stringsAsFactors = FALSE
    ),
    youtube_descriptions = data.frame(
      descriptions = 'No descriptions donwloaded from YouTube',
      stringsAsFactors = FALSE
    )
  )

  # Render the current values of items in the data object
  output$youtube_urls <- DT::renderDataTable(DT::datatable(data$youtube_urls, options = list(pageLength = 5)))
  output$youtube_descriptions <- DT::renderDataTable(data$youtube_descriptions)

  # State flags for controlling UI -------------------------------------

  # Allows for a dynamic UI minimising User/App error.
  # Passed as a condition for the conditional Panel UI.

  # Define flags
  output$youtube_flag <- reactive(
    if (nrow(data$youtube_descriptions) > 1) {
      'descriptions_downloaded'
    } else if (sum(grepl(pattern = 'youtube.com', x = data$youtube_urls[,1], ignore.case = TRUE)) > 0) {
      'youtube_urls_uploaded'
    } else if (ncol(data$youtube_urls) > 1) {
      'more_than_one_col'
    } else {
      'initial_state'
    }
  )

  # Set flags as evaluated despite not being displayed in ui
  outputOptions(output, 'youtube_flag', suspendWhenHidden = FALSE)

  # Data manipulation in response to UI -------------------------------

  # When YouTube URL file is selected by user
  observeEvent(input$youtube_url_file, {
    req(input$youtube_url_file)
    tryCatch(
      {
        data$youtube_urls <- read.csv(input$youtube_url_file$datapath, stringsAsFactors = FALSE)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })

  # When user initiates YouTube description download
  observeEvent(input$youtube_description_download_button, {
    if (input$youtube_key == '') {
      shiny::showNotification('API key is empty', type = 'error')
    } else {
      shiny::showNotification('Checking API key with Google', type = 'message', duration = 2)
      # Check API key works
      api_url <- 'https://www.googleapis.com/youtube/v3/search'
      api_options <- list(
        part = 'snippet',
        key = input$youtube_key,
        type = 'video',
        q = 'YouTube Data API'
      )
      response <- plyr::ldply(
        httr::content(httr::GET(api_url, query = api_options))$error,
        data.frame,
        stringsAsFactors = FALSE
      )
      if (sum(grepl(pattern = 'Invalid', x = response)) > 0) {
        shiny::showNotification('API key is invalid', type = 'error')
      } else {
        resolved <- extract_video_ids(data$youtube_urls[,1], inShiny = TRUE)
        shiny::showNotification('Replacing new line and tabs with spaces', duration = 3, type = 'warning')
        x <- download_youtube_video_descriptions(resolved, api_key = input$youtube_key, inShiny = TRUE)
        x$description <- stringr::str_replace_all(string = x$description, pattern = "[\\r\\n\\t]+", replacement = ' ')
        data$youtube_descriptions <- x
      }
    }
  })

  # When user clicks on the Download YouTube Descriptions button
  output$youtube_data_file_download_button <- downloadHandler(
    filename = paste0("download_", Sys.Date(),".csv"),
    content = function(file) {
      write.table(x = data$youtube_descriptions, sep = '\t', file = file, row.names = FALSE)
    }
  )
}
