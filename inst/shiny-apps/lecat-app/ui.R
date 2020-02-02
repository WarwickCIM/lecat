navbarPage(theme = shinythemes::shinytheme("cerulean"),
  title = 'LE-CAT',
  tabPanel(
    'About',
    sidebarLayout(
      sidebarPanel(
        h2('Instructions'),
        p('Clicking on the navbar at the top of the page will take you to the sections of LE-CAT.'),
        p('1. If you do not already have a data file, then click on the YouTube section to download YouTube descriptions.'),
        p('2. Click on the LE-CAT analysis section. LE-CAT requires a corpus,
          lexicon and lookup table. You can download correctly formatted examples from the LE-CAT analysis section.'),
        p('3. Load your files within the interface and then click on the run LE-CAT analysis button and, optionally, the Calculate cooccurence button')
      ),
      mainPanel(
        h5('V1.0. Under GPL3.0 License.'),
        p('LE-CAT is a Lexicon-based Categorization and Analysis Tool developed by the Centre
          for Interdisciplinary Methodologies in collaboration with the',
          a(href = 'https://www.mediacoop.uni-siegen.de/en/','Media of Cooperation'),
          'Group at the University of Siegen.'),
        p('The tool allows you to apply a set of word queries associated with a category (a lexicon)
          to a data set. LE-CAT determines the frequency of occurrence for each query and category
          in the data, as well as the relations between categories (co-occurrence).'),
        p('The tool also lets you quickly generate the data for lexicon analysis, by extracting
          descriptions from the YouTube API for URLs provided by the user.'),
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
        )
      )
    ),
  tabPanel(
    'YouTube Data Collection',
    sidebarLayout(
      sidebarPanel(
        textInput(inputId = 'youtube_key', label = 'YouTube API key:', value = ''),
        br(),
        fileInput("youtube_url_file", "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
        ),
        hr(),
        actionButton(
          inputId = 'youtube_description_download_button',
          label = 'Get from YouTube'
        ),
        br(),
        br(),
          downloadButton("youtube_data_file_download_button",
                         "Download Output")
      ),
    mainPanel(
        condition = "output.youtube_flag == 'initial_state'",
        h5('Instructions'),
        p('Here you can download YouTube descriptions for use with LE-CAT.
          Please make sure you have the following at hand:'),
        tags$ul(
          tags$li('A CSV file containing one or more YouTube URLs.
          The URL should be similiar to https://www.youtube.com/watch?v=fjskfjdue'),
          tags$li('If you are using the app via R or Rstudio then a YouTube API key.
             Students using LE-CAT via the Warwick servers do not need a YouTube API key.')
        ),
        hr(),
        h5('YouTube URLs:'),
        DT::dataTableOutput('youtube_urls'),
        hr(),
        h5('YouTube Descriptions:'),
        DT::dataTableOutput('youtube_descriptions')
      )
    )
  ),
  tabPanel(
    'LE-CAT Data Analysis',
    sidebarLayout(
      sidebarPanel(
        selectInput("lecat_example", "Choose example file:",
                    choices = c("Lexicon", "Corpus", "Lookup_Table")),
        downloadButton("download_lecat_example", "Download"),
        hr(),
        tags$h5('Input'),
        fileInput("lecat_lexicon_file", "Choose Lexicon File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        fileInput("lecat_corpus_file", "Choose Corpus File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv",
                             ".xlsx")),
        radioButtons("corpus_sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        fileInput("lecat_lookup_table_file", "Choose Lookup Table File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        conditionalPanel(
          condition = "output.lecat_flag == 'lecat_ready'",
          tags$h5('Analysis'),
          textInput(
            inputId = 'lecat_analysis_regex',
            label = 'Regex',
            value = '\\Wquery\\W'
          ),
          checkboxInput(
            inputId = "lecat_case_sensitive",
            label = "Case sensitive", FALSE
            ),
          actionButton(
            inputId = 'lecat_run_analysis_button',
            label = 'Run LE-CAT analysis'
            ),
          br(),
          selectInput(
            inputId = 'lecat_network_level',
            'Nodes:',
            #choices = c('Type', 'Category', 'Query')
            choices = 'Query'
          ),
          actionButton(
            inputId = 'lecat_generate_network_button',
            label = 'Calculate cooccurence'
            )
          ),
        conditionalPanel(
          condition = "output.lecat_analysis_flag == 'analysis_complete'",
          tags$h5('Output'),
          selectInput("lecat_output", "Choose output file:",
                      choices = c("raw", 'diagnostics', "cotable", "network")),
          downloadButton("download_lecat_output", "Download")
        )
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
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
                    tabPanel("Input",
                             h5('Lexicon'),
                             DT::dataTableOutput('lecat_lexicon'),
                             hr(),
                             h5('Corpus'),
                             DT::dataTableOutput('lecat_corpus'),
                             hr(),
                             h5('Lookup Table'),
                             DT::dataTableOutput('lecat_lookup_table')
                             ),
                    tabPanel("Output",
                             h5('Raw LE-CAT output'),
                             DT::dataTableOutput('lecat_raw_result'),
                             hr(),
                             h5('LE-CAT diagnostics'),
                             DT::dataTableOutput('lecat_diagnostics'),
                             hr(),
                             h5('Cooccurence table'),
                             DT::dataTableOutput('lecat_cotable')
                             )
        )
        )
    )
  )
)
