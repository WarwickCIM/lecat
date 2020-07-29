# Load libraries ----
require(lecat)

# Option allowing large file uploads ----
options(shiny.maxRequestSize=9000*1024^2)

function(input, output, session) {

  # Reactive values for storing persistent data ----
  data <- reactiveValues(

    # Reactive value: lexicon ----
    lecat_lexicon = data.frame(
      Type = 'No lexicon loaded yet',
      stringsAsFactors = FALSE
    ),

    # Reactive value: corpus ----
    lecat_corpus = data.frame(
      Type = 'No corpus loaded yet',
      stringsAsFactors = FALSE
    ),

    # Reactive value: lookup table ----
    lecat_lookup_table = data.frame(
      Type = 'No lookup table loaded yet',
      stringsAsFactors = FALSE
    ),

    # Reactive value: raw results ----
    lecat_raw_result = data.frame(
      Type = 'LECAT not run yet',
      stringsAsFactors = FALSE
    ),

    # Reactive value: diagnostics ----
    lecat_diagnostics = data.frame(
      Type = 'No diagnostics created yet',
      stringsAsFactors = FALSE
    ),

    # Reactive value: cotable ----
    lecat_cotable = data.frame(
      Type = 'Cooccurrence table not generated yet',
      stringsAsFactors = FALSE
    ),

    # Reactive value: lecat network preallocation ----
    lecat_network = data.frame(),

    # Reactive value: flags used by conditional panels ----
    corpus_loaded = FALSE,
    lexicon_loaded = FALSE,
    lookup_table_loaded = FALSE,
    lecat_analysis_complete = FALSE
  )

  # These objects are the example lecat files

  # Example lexicon ----
  example_lexicon <- data.frame(
    Type = c('technology', 'influencers'),
    Category = c('Apple', 'CIM'),
    Query = c('iphone', 'Noortje Marres'),
    Query1 = c('iPad', 'James Tripp'),
    Query3 = c('imac', ''),
    stringsAsFactors = FALSE
  )

  # Example corpus ----
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

  # Example lookup table ----
  example_lookup_table <- data.frame(
    Type = c('technology', 'influencers'),
    Column = c('description', 'title'),
    stringsAsFactors = FALSE)

  # Render Data Table output for lexicon, lookup table and diagnostics ----
  output$lecat_lexicon <- DT::renderDataTable(data$lecat_lexicon, options = list(pageLength = 5, scrollX = TRUE))
  output$lecat_lookup_table <- DT::renderDataTable(data$lecat_lookup_table, options = list(pageLength = 5, scrollX = TRUE))
  output$lecat_diagnostics <- DT::renderDataTable(data$lecat_diagnostics, options = list(pageLength = 5, scrollX = TRUE))

  # Flag checking if lookup table, corpus and lexicon are loaded ----
  output$lecat_flag <- reactive(
    if (data$corpus_loaded & data$lexicon_loaded & data$lookup_table_loaded) {
      'lecat_ready'
    } else {
      'lecat_not_ready'
    }
  )

  # Flag checking if analysis has finished ---
  output$lecat_analysis_flag <- reactive(
    if (data$lecat_analysis_complete) {
      'analysis_complete'
    }
  )

  # Stop flags being make hidden when related ui items are not shown
  outputOptions(output, 'lecat_flag', suspendWhenHidden = FALSE)
  outputOptions(output, 'lecat_analysis_flag', suspendWhenHidden = FALSE)

  # Create output for download lecat example button ----
  output$download_lecat_example<- downloadHandler(
    filename = function() {
      paste(input$lecat_example, ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(lecat_example_file(), file, col_names = TRUE)
    }
  )

  # Extract file chosen by user in example drop down ----
  lecat_example_file <- reactive({
    switch(input$lecat_example,
           "Lexicon" = example_lexicon,
           "Corpus" = example_corpus,
           "Lookup_Table" = example_lookup_table
    )
  })

  # Event run when lexicon file uploaded ----
  observeEvent(input$lecat_lexicon_file, {

    # check there is an input
    req(input$lecat_lexicon_file)

    # inform the user
    shiny::showNotification('Formatting lexicon to long format', type = 'message', duration = 2)

    # try to load the lexicon file and parse it
    tryCatch(
      {
        # read excel file
        x  <- readxl::read_excel(input$lecat_lexicon_file$datapath)

        # Make sure there's at least one query
        at_least_one_query <- sum(grepl(pattern = 'Query', x = names(x), ignore.case = FALSE)) > 0

        # parse the lexicon if there are columns called Type, Category and at least one query
        if(('Type' %in% names(x)) & ('Category' %in% names(x)) & at_least_one_query) {
          data$lecat_lexicon <- parse_lexicon(x)
          data$lexicon_loaded <- TRUE

        } else {
          if (!at_least_one_query) {
            shiny::showNotification('Lexicon not loaded: Two or more queries required', type = 'error')
          } else {
            shiny::showNotification('Lexicon not loaded: Column names Type, Category and Query required.', type = 'error')
          }
        }
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        # stop(safeError(e))
        shiny::showNotification('Lexicon not loaded: error', type = 'error')
        #safeError(e)
      }
    )

  })

  # Event run when corpus file is uploaded ----
  observeEvent(input$lecat_corpus_file, {

    req(input$lecat_corpus_file)

      tryCatch(
        {
          data$lecat_corpus <- readxl::read_excel(input$lecat_corpus_file$datapath)
          data$corpus_loaded <- TRUE

        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          #stop(safeError(e))
          shiny::showNotification('Lexicon not loaded: error', type = 'error')
          #safeError(e)
        }
      )
  })

  # Event run when lookup table file is uploaded ----
  observeEvent(input$lecat_lookup_table_file, {

    req(input$lecat_lookup_table_file)

    tryCatch(
      {
        lookup_table <- readxl::read_excel(input$lecat_lookup_table_file$datapath)

        # check if lookup table has the Type and Column column names
        if (('Type' %in% names(lookup_table)) & ('Column' %in% names(lookup_table))) {

          # remove accidental duplicate rows
          lookup_table <- unique(lookup_table[,c('Type', 'Column')])

          # check for missing values
          if (sum(!complete.cases(lookup_table)) > 0) { # if there are rows with missing Type or Column}

            # count and remove incomplete cases
            n_incomplete_cases <- sum(!complete.cases(lookup_table))
            lookup_table <- lookup_table[complete.cases(lookup_table),]

            # notify the user of incomplete cases
            shiny::showNotification(paste('Removing', n_incomplete_cases, 'rows without Type or Column'), type = 'warning')
          }

          # put lookup table into reactive values (see top above for definition)
          data$lecat_lookup_table <- lookup_table

          # record the lookup table has been uploaded
          data$lookup_table_loaded <- TRUE

        } else {

          # Notify the user if the Type and Column column names are absent
          shiny::showNotification('Lookup Table not loaded: Column names Type and Column required', type = 'error')
        }
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        #safeError(e)
        #stop(safeError(e))
        shiny::showNotification('Lookup Table not loaded: error', type = 'error')
      }
    )
  })

  # Event run when lecat_run_analysis button pressed ----
  observeEvent(input$lecat_run_analysis_button, {

    # Get unique lookup table types
    lookup_table_types <- unique(data$lecat_lookup_table$Type)

    # Get lexicon types
    lexicon_types <- unique(data$lecat_lexicon$Type)

    # Check if the lexicon and lookup types are the same
    if (mean(lookup_table_types %in% lexicon_types) == 1) {

      shiny::showNotification('Running lecat analysis')

      # Run the analysis
      x <-
        run_lecat_analysis(
        lexicon = data$lecat_lexicon,
        corpus = data$lecat_corpus,
        searches = data$lecat_lookup_table,
        regex_expression = input$lecat_analysis_regex,
        inShiny = TRUE,
        case_sensitive = input$lecat_case_sensitive
      )

      # Copy over result to reactive value. Error if allocated directly, I think
      data$lecat_raw_result <- x

      # Notify user
      shiny::showNotification('Generating diagnostics. Please wait.')

      # Create diagnostic summary and assign into reactive value
      data$lecat_diagnostics <- create_unique_total_diagnostics(x, inShiny = TRUE)

      shiny::showNotification('Diagnostics generated')

      # Set flag indicating analysis complete for UI
      data$lecat_analysis_complete <- TRUE

    } else {

      # Notify the user if the lexicon and lookup types are different
      shiny::showNotification('Lookup table types do not match lexicon types. Please check these files', type = 'error')

    }
  })

  # Event run when generate_network_button is pressed ----
  observeEvent(input$lecat_generate_network_button, {

    shiny::showNotification('Generating cooccurrence table and network graph')

    # Create the network
    x <- create_cooccurrence_graph(data$lecat_raw_result,
                                   #level = input$lecat_network_level,
                                   inShiny = TRUE)

    # Assign the cotable and graph to the reactive values
    data$lecat_cotable <- x$cotable
    data$lecat_network <- x$graph
  })

  # Download handler for the output types ----
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

  # Extrac tthe output type requested by the user ----
  lecat_output_file <- reactive({
    switch(input$lecat_output,
           "raw" = data$lecat_raw_result,
           "cotable" = data$lecat_cotable,
           "diagnostics" = data$lecat_diagnostics,
           "network" = data$lecat_network)
  })

}
