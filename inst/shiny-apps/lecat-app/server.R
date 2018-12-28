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
    'initial_state'
  )

  # Set flags as evaluated despite not being displayed in ui
  outputOptions(output, 'youtube_flag', suspendWhenHidden = FALSE)

  # Data manipulation in response to UI -------------------------------

  # When YouTube URL file is selected by user
  observeEvent(input$youtube_url_file, {
    req(input$youtube_url_file)
    tryCatch(
      {
        data$youtube_urls <- read.csv(input$youtube_url_file$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

  })
}
