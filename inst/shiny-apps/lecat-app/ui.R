# Define UI for LE-CAT app ----
fluidPage(
  # App title ----
  titlePanel("LE-CAT"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    # Sidebar panel for LE-CAT file upload and analysis buttons ----
    sidebarPanel(
      # Select input for downloading example files ----
      selectInput(
        "lecat_example",
        "Choose example file:",
        choices = c("Lexicon", "Corpus", "Lookup_Table")
      ),

      # Download button for examples files ----
      downloadButton("download_lecat_example", "Download"),

      hr(),
      tags$h5('Input'),

      # File input for lexicon excel file ----
      fileInput(
        "lecat_lexicon_file",
        "Choose Lexicon File (xlsx format)",
        multiple = FALSE,
        accept = ".xlsx"
      ),

      # File input for corpus excel file ----
      fileInput(
        "lecat_corpus_file",
        "Choose Corpus File (xlsx format)",
        multiple = FALSE,
        accept = ".xlsx"
      ),

      # File input for lookup table excel file ----
      fileInput(
        "lecat_lookup_table_file",
        "Choose Lookup Table File (xlsx format)",
        multiple = FALSE,
        accept = ".xlsx"
      ),

      # Conditional panel displayed when lecat is ready ----
      conditionalPanel(
        condition = "output.lecat_flag == 'lecat_ready'",

        tags$h5('Analysis'),

        # Help text for advanced mode tickbox ----
        tags$p(
          'Tick "Advanced Regex Mode" to use a custom regex pattern for each query term in your lexicon. Warning: LE-CAT will stop if the lexicon contains any malformed regex.'
        ),

        # Checkbox input for advanced mode ----
        checkboxInput(inputId = "lecat_advanced_mode",
                      label = "Advanced Regex Mode", FALSE),

        # Help text for normal mode options ----
        tags$p(
          'In Normal Mode, a default regex pattern will be applied to each query term (by substituting the word "query"). You may modify it below and/or select case sensitive searches.'
        ),

        # Text input for regex query ----
        textInput(
          inputId = 'lecat_analysis_regex',
          label = 'Regex',
          value = '(?<=\\W|^)query(?=\\W|$)'
        ),

        # Checkbox input for case sensitivity ----
        checkboxInput(inputId = "lecat_case_sensitive",
                      label = "Case sensitive", FALSE),

        # Action button to start lecat analysis ----
        actionButton(inputId = 'lecat_run_analysis_button',
                     label = 'Run LE-CAT analysis')
      ),

      # Conditional panel displaying output options once analysis is complete ----
      conditionalPanel(
        condition = "output.lecat_analysis_flag == 'analysis_complete'",
        br(),

        # Select input for network level  ----
        selectInput(inputId = 'lecat_network_level',
                    'Nodes:',
                    #choices = c('Type', 'Category', 'Query')
                    choices = 'Category'),

        # Action button to generate the cooccurence network ----
        actionButton(inputId = 'lecat_generate_network_button',
                     label = 'Calculate cooccurence'),

        br(),
        tags$h5('Output'),
        selectInput(
          "lecat_output",
          "Choose output file:",
          choices = c("raw", 'diagnostics', "cotable", "network")
        ),
        downloadButton("download_lecat_output", "Download")
      )
    ),

    # Main panel for the about, guide and data tabs ----
    mainPanel(
      # Tab set panel containing about, guide and data tabs ----
      tabsetPanel(
        type = "tabs",
        id = "main_tabs",

        # Tab containing LE-CAT project information ----
        tabPanel(
          'About',

          br(),
          h5('V1.9.2. Under GPL3.0 License.'),
          p(
            'LE-CAT is a Lexicon-based Categorization and Analysis Tool developed by the Centre
                              for Interdisciplinary Methodologies in collaboration with the',
            a(href = 'https://www.mediacoop.uni-siegen.de/en/', 'Media of Cooperation'),
            'Group at the University of Siegen.'
          ),
          p(
            'The tool allows you to apply a set of word queries associated with a category (a lexicon)
                              to a data set. LE-CAT determines the frequency of occurrence for each query and category
                              in the data, as well as the relations between categories (co-occurrence).'
          ),
          p(
            'The goal of this method is to automate and scale up data analysis on the basis of a
                              customized Lexicon created by the user. The quick iteration of analysis allows you to
                              refine a corpus and deeply analyse a given phenomenon.'
          ),
          p(
            'LE-CAT was coded by',
            a(href = 'https://github.com/jamestripp', 'James Tripp'),
            ', please send bugs to him. Additional modifications by ',
            a(href = 'https://github.com/ladelentes', 'Helena Suárez Val'),
            '. This application is written in R and the package - including this
                              app - can be downloaded from GitHub at',
            a(href = 'https://github.com/jamestripp/lecat', 'https://github.com/jamestripp/lecat'),
            '.'
          ),
          p(
            'Academic correspondence should be sent to ',
            a(mailto = 'n.marres@warwick.ac.uk', 'Noortje Marres'),
            '.'
          )
        ),

        # Tab containing the guide----
        tabPanel(
          "Guide",

          br(),
          p(
            'LE-CAT requires three files to work. All of these should be Excel (.xlsx) files.'
          ),
          tags$ul(
            tags$li(
              'A lexicon. The file contains queries you wish to search for in the corpus and
                                       the associated category and type.'
            ),
            tags$li('A corpus. The dataset in which to find your queries.'),
            tags$li(
              'A lookup table. A table instructing LE-CAT which column of the corpus to search
                                       for query types.'
            )
          ),
          strong('Walkthrough'),
          tags$ol(
            tags$li(
              'Download the example Lexicon, Corpus and Lookup Table. Modify these files to suite your needs.'
            ),
            tags$li(
              'Upload the modified files to LE-CAT by clicking Browse and choosing the file. The Analysis section will appear once all three files are uploaded.'
            ),
            tags$li(
              'Click on the Data tab to check the Lookup Table and Lexicon files appear correctly. The Lexicon will be altered to "long format" with 1 query per row.'
            ),
            tags$li(
              'Search the corpus for your queries by clicking on "Run LE-CAT Analysis". The LE-CAT diagnostics will be shown in the Data tab once the analysis is complete.'
            ),
            tags$li(
              'LE-CAT offers co-occurence analysis. You can investigate how often queries, types or categories co-occur. Select your desired level of co-occurrence then click "Calculate co-occurence". A co-occurence table and network file are now available to download.'
            ),
            tags$li(
              'All the output files are available to download. Click on the drop down menu to select a file and then press Download.'
            )
          )
        ),

        # Tab panel containing the data output ----
        tabPanel(
          "Data",
          h5('Lexicon'),

          # Data table showing lexicon ---
          DT::dataTableOutput('lecat_lexicon'),

          hr(),

          h5('Lookup Table'),

          # Data table showing lookup table---
          DT::dataTableOutput('lecat_lookup_table'),

          hr(),

          h5('LE-CAT diagnostics'),

          # Data table showing diagnostic information ---
          DT::dataTableOutput('lecat_diagnostics')
        )
      )
    )
  )
)
