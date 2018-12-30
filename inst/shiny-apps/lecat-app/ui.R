navbarPage(
  title = 'LE-CAT',
  theme = shinythemes::shinytheme('cosmo'),
  tabPanel(
    'About',
    sidebarLayout(
      sidebarPanel(
        h2('Instructions'),
        p('1.'),
        p('2.'),
        p(3.)
      ),
      mainPanel(
        h5('V0.5 Pre-release. Under GPL3.0 License.'),
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
        textInput(inputId = 'youtube_key', label = 'YouTube API key:', value = 'AIzaSyBGikBaMzNwzr7xIP-0PHqojYSyJvRboOU'),
        conditionalPanel(
          condition = "output.youtube_flag == 'initial_state'",
          fileInput("youtube_url_file", "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv"))
        ),
        conditionalPanel(
          condition = "output.youtube_flag == 'youtube_urls_uploaded'",
          actionButton(
            inputId = 'youtube_description_download_button',
            label = 'Download YouTube descriptions')
        ),
        conditionalPanel(
          condition = "output.youtube_flag == 'descriptions_downloaded'",
          downloadButton("youtube_data_file_download_button", "Download YouTube Descriptions")
        )
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
      sidebarPanel(),
      mainPanel()
    )
  )
)
