# Define UI for LE-CAT app ----
fluidPage(

  # App title ----
  titlePanel("LE-CAT"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for LE-CAT file upload and analysis buttons ----
    sidebarPanel(

      # Select input for downloading example files ----
      selectInput("lecat_example", "Choose example file:",
                    choices = c("Lexicon", "Corpus", "Lookup_Table")),

      # Download button for examples files ----
      downloadButton("download_lecat_example", "Download"),

      hr(),
      tags$h5('Input'),

      # File input for lexicon excel file ----
      fileInput("lecat_lexicon_file", "Choose Lexicon File (xlsx format)",
                multiple = FALSE,
                accept = ".xlsx"),

      # File input for corpus excel file ----
      fileInput("lecat_corpus_file", "Choose Corpus File (xlsx format)",
                multiple = FALSE,
                accept = ".xlsx"),

      # File input for lookup table excel file ----
      fileInput("lecat_lookup_table_file", "Choose Lookup Table File (xlsx format)",
                multiple = FALSE,
                accept = ".xlsx"),

      # Conditional panel displayed when lecat is ready ----
      conditionalPanel(
        condition = "output.lecat_flag == 'lecat_ready'",

        tags$h5('Analysis'),

        # Text input for regex query ----
        textInput(
          inputId = 'lecat_analysis_regex',
          label = 'Regex',
          value = '\\Wquery\\W'
        ),

        # Checkbox input for case sensitivity ----
        checkboxInput(
          inputId = "lecat_case_sensitive",
          label = "Case sensitive", FALSE
          ),

        # Action button to start lecat analysis ----
        actionButton(
          inputId = 'lecat_run_analysis_button',
          label = 'Run LE-CAT analysis'
          ),

        br(),

        # Select input for network level  ----
        selectInput(
          inputId = 'lecat_network_level',
          'Nodes:',
          #choices = c('Type', 'Category', 'Query')
          choices = 'Query'
        ),

        # Action button to generate the cooccurence network ----
        actionButton(
          inputId = 'lecat_generate_network_button',
          label = 'Calculate cooccurence'
          )
        ),

        # Conditional panel displaying output options once analysis is complete ----
        conditionalPanel(
          condition = "output.lecat_analysis_flag == 'analysis_complete'",
          tags$h5('Output'),
          selectInput("lecat_output", "Choose output file:",
                      choices = c("raw", 'diagnostics', "cotable", "network")),
          downloadButton("download_lecat_output", "Download")
          )
        ),

    # Main panel for the about, guide and data tabs ----
    mainPanel(

      # Tab set panel containing about, guide and data tabs ----
      tabsetPanel(type = "tabs",

                  # Tab containing LE-CAT project information ----
                  tabPanel('About',

                           br(),
                           h5('V1.0. Under GPL3.0 License.'),
                           p('LE-CAT is a Lexicon-based Categorization and Analysis Tool developed by the Centre
                              for Interdisciplinary Methodologies in collaboration with the',
                           a(href = 'https://www.mediacoop.uni-siegen.de/en/','Media of Cooperation'),
                             'Group at the University of Siegen.'),
                           p('The tool allows you to apply a set of word queries associated with a category (a lexicon)
                              to a data set. LE-CAT determines the frequency of occurrence for each query and category
                              in the data, as well as the relations between categories (co-occurrence).'),
                           p('The goal of this method is to automate and scale up data analysis on the basis of a
                              customized Lexicon created by the user. The quick iteration of analysis allows you to
                              refine a corpus and deeply analyse a given phenomenon.'),
                           p('LE-CAT was coded by',
                           a(href = 'https://jamestripp.github.io', 'James Tripp'),
                              ', please send bugs to him. This application is written in R and the package - inlcuding this
                              app - can be downloaded from GitHub at',
                           a(href = 'https://github.com/jamestripp/lecat', 'https://github.com/jamestripp/lecat'),
                              '.'),
                           p('Academic correspondence should be sent to ',
                           a(mailto = 'n.marres@warwick.ac.uk', 'Noortje Marres'),
                             '.')
                           ),

                    # Tab containing the guide----
                    tabPanel("Guide",

                             br(),
                             p('LE-CAT requires three files to work.'),
                             tags$ul(
                               tags$li('A lexicon. The file contains queries you wish to search for in the corpus and
                                       the associated category and type.'),
                               tags$li('A corpus. The dataset in which to find your queries.'),
                               tags$li('A lookup table. A table instructing LE-CAT which column of the corpus to search
                                       for query types.')
                             ),
                             p('You can download templates for these files at the top of the left column of this page.
                               LE-CAT requires the files to be formatted in this way. Your corpus must have a column titled
                               id which identifies each row.'),
                             strong('Guide, Input, Output'),
                             p('The tabs above show either this guide, the contents of the currently loaded Input files
                               and the Output tables. If no file has been loaded then the you will placeholder text.'),
                             strong('Walkthrough'),
                             tags$ol(
                               tags$li('Load in the lexicon, corpus and lookup table. You can click on Browse or drag and
                                       drop the file. You can select how the corpus file is seperated. If the corpus does not
                                       look correct in the input tab then try a different seperator option (e.g., comma).
                                       The analysis section will appear below once all the files are uploaded.'),
                               tags$li('Click on the Run LE-CAT analysis button. You can set your own regex query. The default
                                       searches for the query term with a non-character either side. If you wish to specify your own
                                       then include the word query - the word query is replaced with the actual query when the analysis
                                       is run. The analysis may take some time.'),
                               tags$li('(Optional) Select which level you would like the cooccurrence graph and table calculated.
                                       You may wish to see cooccurence at the query, category or type level. Calculating
                                       cooccurence at the query level may take a long time if you have many queries. The output
                                       download options will be shown once the cooccurrences have been calculated.'),
                               tags$li('Select and download either the raw output, diagnostics, cooccurence table or network graph of
                                       cooccurences. The network graph can be viewed within Gephi.')
                               )

                             ),

                    # Tab panel containing the data output ----
                    tabPanel("Data",
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
