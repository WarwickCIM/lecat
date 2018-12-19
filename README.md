
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LE-CAT

LE-CAT is a Lexicon-based Categorization and Analysis Tool developed by
the Centre for Interdisciplinary Methodologies in collaboration with the
Media of Cooperation Group at the University of Siegen.

The tool allows you to apply a set of word queries associated with a
category (a lexicon) to a data set of textual sources (the corpus).
LE-CAT determines the frequency of occurrence for each query and
category in the corpus, as well as the relations between categories
(co-occurrence) by source.

The tool also allows you to quickly generate the data for lexicon-based
analysis, by extracting descriptions from the Youtube API for URLs
provided by the user.

The purpose of this technique is to automate and scale up user-led data
analysis as it allows the application of a custom-built Lexicon to large
data sets. The quick iteration of analysis allows the user to refine a
corpus and deeply analyse a given phenomenon.

LE-CAT was coded by James Tripp. It has been used to support the
workshop Youtube as Test Society (University of Siegen) and the Digital
Test of the News (University of Warwick) and will soon be tested by
students on the MA Module Digital Objects, Digital Methods.

Academic correspondence should be sent to Noortje Marres.

## Installation

You can install the released version of lecat from
[Github](https://github.com/) with:

``` r
install.packages("devtools")
library(devtools)
install_github("jamestripp/lecat")
```

## Example

### Downloading descriptions from YouTube

You may wish to create a corpus of video descriptions from YouTube. A
[recent
workshop](https://warwick.ac.uk/fac/cross_fac/cim/news/a-digital-test-of-the-news)
took this approach.

You should have a list of YouTube URLs and a YouTube data API key. You
can generate an API key by creating a Google account and going to the
[developers
console](https://accounts.google.com/ServiceLogin?service=cloudconsole&passive=1209600&osid=1&continue=https://console.developers.google.com/apis/api/youtube/&followup=https://console.developers.google.com/apis/api/youtube/&authuser=0).
Your YouTube URLs shoud be loaded as a character vector as below:

``` r
youtube_urls <- c(
  'https://www.youtube.com/watch?v=DWMPb8L4T78',
  'https://www.youtube.com/watch?v=a2x7AxdvEIU',
  'https://goo.gl/A2z1rm',
  'https://www.google.com',
  'https://www.youtube.com/watch?v=ehvz3iN8pp4'
)

youtube_key <- 'YOURKEY'
```

The extract\_ids function will attempt to extract the ids from your
URLs. The function will try to resolve any URLs and discard those which
cannot be resolved to a recognised format
(<https://www.youtube.com/watch?=>).

``` r
youtube_ids <- extract_video_ids(youtube_urls)
```

These ids can be passed to the download\_youtube\_video\_descriptions
function. The function returns a corpus dataframe with one video per
colum and the id, publishedAt, title and description of each
video.

``` r
corpus <- download_youtube_video_descriptions(video_ids = youtube_ids, api_key = youtube_key)
```

### Running a LE-CAT analysis

#### Lexicon

The LE-CAT lexicon should contain the queries you wish to look for, the
categories associated with the queries and the type of category. The
lexicon can be in a wide form such as

| Type       | Category | Query    | Query1    | Query2 |
| ---------- | -------- | -------- | --------- | ------ |
| Technology | Software | Windows  | MacOS     | Linux  |
| Technology | Hardware | Mac mini | Alienware | iMac   |
| Tree       | Hardwood | Oak      | Pine      | Maple  |

which is then converted to a long format using the function
parse\_lexicon

``` r
lexicon <- parse_lexicon(wide_lexicon = lexicon, query_column = 'Query')
```

where Query is the first column containing your queries and there is no
other type of data to the right. The parse\_lexicon function can handle
a wide lexicon containing differing numbers of queries per category,
such as:

| Type       | Category | Query   | Query1 | Query2 |
| ---------- | -------- | ------- | ------ | ------ |
| Technology | Software | Windows | MacOS  | Linux  |
| Technology | Hardware | iMac    |        |        |
| Tree       | Hardwood | Oak     | Pine   | Maple  |

The parse lexicon function retuns a long form lexicon which is used in
the lecat analysis. The long form lexicon looks like this

| Type       | Category | Query   |
| ---------- | -------- | ------- |
| Technology | Software | Windows |
| Technology | Software | MacOS   |
| Technology | Software | Linux   |
| Technology | Hardware | iMac    |

and, alternatively, one can use a lexicon already in a long form.

#### Query searching

LE-CAT searches for terms in the corpus text. You may wish to specify a
different search column for each Type. The search column for each type
is specified in a search data frame detailing each Type and the
corresponding search column. For example,

``` r
searches <- data.frame(
  Type = c('Technology', 'Tree'),
  Column = c('description', 'title'),
  stringsAsFactors = FALSE
)
```

where Technology queries should be searched for in the description
column of the corpus and Tree queries should be searched for in the
title column. You need to create this data frame to define the search
column for each Type.

To carry out your search of the corpus, pass the long form lexicon, the
search data frame and corpus to the run\_lecat\_analysis function.

``` r
lecat_result <- run_lecat_analysis(lexicon = lexicon,
                                   corpus = corpus,
                                   searches = searches,
                                   id = 'id',
                                   regex_expression = '\\Wquery\\W')
```

Note that you can pass your own regex expression. The function replaces
the text ‘query’ with the relevent search query. The above searches for
cases where there are non-word characters (e.g., spaces or periods)
located on either side of the search query.

The query result is a table like the below

| Type       | Category | Query   | Column\_examined | id1 | id2 |
| ---------- | -------- | ------- | ---------------- | --- | --- |
| Technology | Software | Windows | description      | 1   | 4   |
| Technology | Software | MacOS   | description      | 0   | 2   |
| Technology | Software | Linux   | description      | 3   | 1   |
| Technology | Hardware | iMac    | description      | 4   | 2   |

where id1 and id2 are the ids specified above for each entry in the
corpus.

#### Diagnostics

The LE-CAT raw count data shown above may be hard for one to process.
The above can be summarised into a diagnostic file using the
create\_unique\_total\_diagnostics function

``` r
diagnostics <- create_unique_total_diagnostics(lecat_result)
```

to create a table like
so

| Type            | Category                    | Queries                                         | Column\_examined |
| --------------- | --------------------------- | ----------------------------------------------- | ---------------- |
| Technology(2,2) | Software(2,2) Hardware(2,2) | Windows(2,2), MacOS(2,2), Linux(2,2), iMac(2,2) | description      |

where the first number in the brackets is the total occurance and second
is the number of corpus elements (e.g., individual YouTube videos) each
Type, Category and Query occur in.

#### Cooccurance

You may calculate the cooccurance of queries (irrespective of the
corresponding Type and Category) using the
create\_cooccurrence\_queries\_graph
function

``` r
cooccurrence <- create_cooccurrence_query_graph(lecat_result = lecat_result, 
                                                filename = 'result.graphml')
```

**Note** Creating the cooccurence graph can take a lot of time depending
on the size of corpus and lexicon.

If a filename is specified then the a graphml file is created. The Type,
Category and Column\_examined are attributes of the nodes and the weight
of the edges are the cooccurance of the queries. You may view graphml
network graphs using the excellent [Gephi](https://gephi.org) program.
**Note** if you have duplicate edges (where there are edges between
node1-node2 and node2-node1 then select merge first withing Gephi).

This function also returns a cooccurance table and the igraph object.
One can plot the graph using the igraph package by passing igraph object
to the plot function.

``` r
plot(cooccurrence$graph)
```

and view the cooccurance table like so

``` r
cooccurrence$cotable
```

## Bugs or feature requests

Please enter any bugs or feature requests via github.

[Dr James Tripp](https://jamestripp.github.io), Academic Technologist,
[CIM](https://www.warwick.ac.uk/cim)
