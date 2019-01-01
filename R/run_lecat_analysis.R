#' run_lecat_analysis
#'
#' The occurance frequency of queries in a corpus is recorded in preperation for
#' futher analysis by the diagnostics or coocurance functions.
#'
#' @param lexicon Lexicon dataframe as parsed by the \link[lecat]{parse_lexicon} function
#' @param corpus Corpus dataframe containing search columns present in the searches dataframe
#' @param searches Data frame with the columns 'Type' and 'Column'. Queries in each Type will be located in the corresponding corpus Column
#' @param id Column name to use for identifying differing corpus samples (e.g., YouTube video id)
#' @param regex_expression Regex expression defining search. String defining the regex expression where the string 'query' will be replaced by the actual query term
#' @param inShiny If inShiny is TRUE then shiny based notifications will be shown
#'
#' @return run_lecat_analysis returns a data frame containing the lexicon, the corresponding search column for the query type and the frequency of terms by corpus id
run_lecat_analysis <- function(lexicon, corpus, searches, id, regex_expression, inShiny = FALSE){
  assertive::assert_is_data.frame(lexicon)
  assertive::assert_is_data.frame(corpus)
  assertive::assert_is_data.frame(searches)
  assertive::assert_is_character(id)
  assertive::assert_is_character(regex_expression)
  run_search <- function(strings, query, regex, type, category, ids, column){
    this_pattern <- stringr::str_replace(string = regex, pattern = 'query', replacement = tolower(query))
    counts <- stringr::str_count(string = tolower(strings), pattern = this_pattern)
    result <- data.frame(Type = type, Category = category, Query = query, Column_examined = column, stringsAsFactors = FALSE)
    result <- cbind(result, as.data.frame(t(counts), stringsAsFactors = FALSE))
    names(result)[5:length(result)] <- ids
    result
  }
  out <- NULL
  if (inShiny) {
    n <- nrow(lexicon)
    shiny::withProgress(message = 'Searching corpus', value = 0, {
      for (i in 1:nrow(lexicon)) {
        shiny::incProgress(1/n, detail = paste("query", i))
        this_search_column <- searches$Column[lexicon$Type[i] == searches$Type]
        out <- rbind(out,
                     run_search(corpus[,this_search_column],
                                lexicon$Queries[i],
                                regex_expression, lexicon$Type[i],
                                lexicon$Category[i],
                                corpus[,id],
                                this_search_column)
        )
      }
    })
  } else {
    pb <- utils::txtProgressBar(min = 1, max = nrow(lexicon), initial = 1)
    for (i in 1:nrow(lexicon)) {
      utils::setTxtProgressBar(pb, i)
      this_search_column <- searches$Column[lexicon$Type[i] == searches$Type]
      out <- rbind(out,
                   run_search(corpus[,this_search_column],
                              lexicon$Queries[i],
                              regex_expression, lexicon$Type[i],
                              lexicon$Category[i],
                              corpus[,id],
                              this_search_column)
      )
    }
    close(pb)
  }

  out
}
