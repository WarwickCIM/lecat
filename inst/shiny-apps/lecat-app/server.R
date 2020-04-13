require(lecat)
options(shiny.maxRequestSize=900*1024^2)
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
      descriptions = 'No descriptions downloaded from YouTube',
      stringsAsFactors = FALSE
    ),
    lecat_lexicon = data.frame(
      Type = 'No lexicon loaded yet',
      stringsAsFactors = FALSE
    ),
    lecat_corpus = data.frame(
      Type = 'No corpus loaded yet',
      stringsAsFactors = FALSE
    ),
    lecat_lookup_table = data.frame(
      Type = 'No lookup table loaded yet',
      stringsAsFactors = FALSE
    ),
    lecat_raw_result = data.frame(
      Type = 'LECAT not run yet',
      stringsAsFactors = FALSE
    ),
    lecat_diagnostics = data.frame(
      Type = 'No diagnostics created yet',
      stringsAsFactors = FALSE
    ),
    lecat_cotable = data.frame(
      Type = 'Cooccurrence table not generated yet',
      stringsAsFactors = FALSE
    ),
    lecat_network = data.frame(),
    corpus_loaded = FALSE,
    lexicon_loaded = FALSE,
    lookup_table_loaded = FALSE,
    lecat_analysis_complete = FALSE
  )

  # These objects are the example lecat files
  example_lexicon <- data.frame(
    Type = c('technology', 'influencers'),
    Category = c('Apple', 'CIM'),
    Query = c('iphone', 'Noortje Marres'),
    Query1 = c('iPad', 'James Tripp'),
    Query3 = c('imac', ''),
    stringsAsFactors = FALSE
  )

  example_corpus <- data.frame(
    id = c(1,2,3),
    title = c(' James Tripp talks about LE-CAT',
              ' Noortje Marres interview',
              'New iphone'),
    description = c('In this iphone and ipad delivered lecture James talks about a new tool.',
                    'An interesting interview',
                    'Apple has launched a series of iphones, ipads and imacs.'),
    stringsAsFactors = FALSE
  )

  example_lookup_table <- data.frame(
    Type = c('technology', 'influencers'),
    Column = c('description', 'title'),
    stringsAsFactors = FALSE)

  # Render the current values of items in the data object
  output$youtube_urls <- DT::renderDataTable(DT::datatable(data$youtube_urls, options = list(pageLength = 5, scrollX = TRUE)))
  output$youtube_descriptions <- DT::renderDataTable(data$youtube_descriptions, options = list(pageLength = 5, scrollX = TRUE))
  output$lecat_lexicon <- DT::renderDataTable(data$lecat_lexicon, options = list(pageLength = 5, scrollX = TRUE))
  output$lecat_corpus <- DT::renderDataTable(data$lecat_corpus, options = list(pageLength = 5, scrollX = TRUE))
  output$lecat_lookup_table <- DT::renderDataTable(data$lecat_lookup_table, options = list(pageLength = 5, scrollX = TRUE))
  output$lecat_raw_result <- DT::renderDataTable(data$lecat_raw_result, options = list(pageLength = 5, scrollX = TRUE))
  output$lecat_diagnostics <- DT::renderDataTable(data$lecat_diagnostics, options = list(pageLength = 5, scrollX = TRUE))
  output$lecat_cotable <- DT::renderDataTable(data$lecat_cotable, options = list(pageLength = 5, scrollX = TRUE))

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

  output$lecat_flag <- reactive(
    if (data$corpus_loaded & data$lexicon_loaded & data$lookup_table_loaded) {
      'lecat_ready'
    } else {
      'lecat_not_ready'
    }
  )

  output$lecat_analysis_flag <- reactive(
    if (data$lecat_analysis_complete) {
      'analysis_complete'
    }
  )

  # Set flags as evaluated despite not being displayed in ui
  outputOptions(output, 'youtube_flag', suspendWhenHidden = FALSE)
  outputOptions(output, 'lecat_flag', suspendWhenHidden = FALSE)
  outputOptions(output, 'lecat_analysis_flag', suspendWhenHidden = FALSE)

  # Data manipulation in response to UI -------------------------------

  # When YouTube URL file is selected by user
  observeEvent(input$youtube_url_file, {
    req(input$youtube_url_file)
    tryCatch(
      {
        data$youtube_urls <- read.csv(input$youtube_url_file$datapath,
                                      header = FALSE,
                                      stringsAsFactors = FALSE)
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
    filename = paste0("lecat_youtube_descriptions_", Sys.Date(),".csv"),
    content = function(file) {
      write.table(x = data$youtube_descriptions, sep = '\t', file = file, row.names = FALSE)
    }
  )

  ## LE-CAT Analysis

  # Example file download buttons
  # Buttons for downloading example files
  output$download_lecat_example<- downloadHandler(
    filename = function() {
      paste(input$lecat_example, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(lecat_example_file(), file, row.names = FALSE)
    }
  )

  # Switch for above
  lecat_example_file <- reactive({
    switch(input$lecat_example,
           "Lexicon" = example_lexicon,
           "Corpus" = example_corpus,
           "Lookup_Table" = example_lookup_table
    )
  })

  # Load lexicon, lookup table and corpus
  observeEvent(input$lecat_lexicon_file, {
    req(input$lecat_lexicon_file)
    shiny::showNotification('Formatting lexicon to long format', type = 'message', duration = 2)
    tryCatch(
      {
        x  <- read.csv(input$lecat_lexicon_file$datapath, stringsAsFactors = FALSE)
        at_lease_one_query <- sum(grepl(pattern = 'Query', x = names(x), ignore.case = FALSE)) > 0
        if(('Type' %in% names(x)) & ('Category' %in% names(x)) & at_lease_one_query) {
          data$lecat_lexicon <- parse_lexicon(x)
          data$lexicon_loaded <- TRUE
        } else {
          shiny::showNotification('Lexicon not loaded: Column names Type, Category and Query required', type = 'error')
        }
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })

  observeEvent(input$lecat_corpus_file, {
    req(input$lecat_corpus_file)
    if(grepl(x = input$lecat_corpus_file$datapath, pattern = '.csv')) {
      tryCatch(
        {
          data$lecat_corpus <- readr::read_csv(file = input$lecat_corpus_file$datapath)
          #data$lecat_corpus <- read.csv(input$lecat_corpus_file$datapath,
          #                              sep = input$corpus_sep,
          #                              stringsAsFactors = FALSE)
          data$corpus_loaded <- TRUE
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
    } else if(grepl(x = input$lecat_corpus_file$datapath, pattern = '.xlsx')) {
      tryCatch(
        {
          data$lecat_corpus <- openxlsx::read.xlsx(input$lecat_corpus_file$datapath)
          data$corpus_loaded <- TRUE
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
    } else {
      stop(safeError('File format not found'))
    }
  })

  observeEvent(input$lecat_lookup_table_file, {
    req(input$lecat_lookup_table_file)
    tryCatch(
      {
        lookup_table <- read.csv(input$lecat_lookup_table_file$datapath,
                                        stringsAsFactors = FALSE)
        if (('Type' %in% names(lookup_table)) & ('Column' %in% names(lookup_table))) {
          lookup_table <- unique(lookup_table[,c('Type', 'Column')])
          if (sum(!complete.cases(lookup_table)) > 0) { # if there are rows with missing Type or Column}
            n_incomplete_cases <- sum(!complete.cases(lookup_table))
            shiny::showNotification(paste('Removing', n_incomplete_cases, 'rows without Type or Column'), type = 'warning')
            lookup_table <- lookup_table[complete.cases(lookup_table),]
          }
          # remove any empty rows
          data$lecat_lookup_table <- lookup_table
          data$lookup_table_loaded <- TRUE
        } else {
          shiny::showNotification('Lookup Table not loaded: Column names Type and Column required', type = 'error')
        }
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })

  # When user selects to run a LE-CAT analysis
  observeEvent(input$lecat_run_analysis_button, {
    # Check that our various inputs match up
    lookup_table_types <- unique(data$lecat_lookup_table$Type)
    lexicon_types <- unique(data$lecat_lexicon$Type)
    message(lookup_table_types %in% lexicon_types)
    if (mean(lookup_table_types %in% lexicon_types) == 1) {
      shiny::showNotification('Running lecat analysis')
      x <- run_lecat_analysis(
        lexicon = data$lecat_lexicon,
        corpus = data$lecat_corpus,
        searches = data$lecat_lookup_table,
        regex_expression = input$lecat_analysis_regex,
        inShiny = TRUE,
        case_sensitive = input$lecat_case_sensitive
      )
      data$lecat_raw_result <- x
      shiny::showNotification('Generating diagnostics')
      data$lecat_diagnostics <- create_unique_total_diagnostics(x)
      data$lecat_analysis_complete <- TRUE
    } else {
      shiny::showNotification('Lookup table types do not match lexicon types. Please check these files', type = 'error')
    }
  })

  # When user select to generate the cotable and network graph
  observeEvent(input$lecat_generate_network_button, {
    shiny::showNotification('Generating cooccurrence table and network graph')
    x <- create_cooccurrence_graph(data$lecat_raw_result,
                                   level = input$lecat_network_level,
                                   inShiny = TRUE)
    data$lecat_cotable <- x$cotable
    data$lecat_network <- x$graph
  })

  # Buttons for downloading lecat output
  output$download_lecat_output<- downloadHandler(
    filename = function() {
      if (input$lecat_output == 'network') {
        paste0('lecat_', input$lecat_output, Sys.Date(), ".graphml", sep = "")
      } else {
        paste0('lecat_', input$lecat_output,Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      if (input$lecat_output == 'network') {
        igraph::write_graph(lecat_output_file(), file, format = 'graphml')
      } else {
        write.csv(lecat_output_file(), file, row.names = FALSE)
      }
    }
  )

  # Switch for above
  lecat_output_file <- reactive({
    switch(input$lecat_output,
           "raw" = data$lecat_raw_result,
           "cotable" = data$lecat_cotable,
           "diagnostics" = data$lecat_diagnostics,
           "network" = data$lecat_network)
  })

}
