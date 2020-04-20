#' Searches a series of strings for a specific query. Used internally by run_lecat_analysis
#'
#' Search through a series for queries based on a regex query.
#'
#' @param strings Vector of strings to search through. Strings are from the chosen corpus.
#' @param query String query term to search for in the strings. Taken from the chosen lexicon.
#' @param regex String regular expression defining the strings should be searched. Defaults to searching for the query term with non word characters either side
#' @param type String query type. Derived from lexicon type.
#' @param category String query type. Derived from lexicon type.
#' @param ids id of the string
#' @param column String column name examined included in the returned dataframe
#'
#' @return dataframe with counts of the query in each string
run_search <- function(strings, query, regex = "\\Wquery\\W", type, category, ids, column){

  # correctly add backslash to any special character for regex search
  query <- stringr::str_replace_all(query, "([{\\[()|?$^*+.\\\\])", "\\$1")

  # replaces the word query in the regex with the query we're searching for
  this_pattern <- stringr::str_replace(string = regex, pattern = 'query', replacement = query)

  # count matches of the regex
  #counts <- stringr::str_count(string = strings, pattern = this_pattern)
  counts <- lapply(strings, stringr::str_count, pattern = this_pattern)

  # output dataframe
  result <-
    data.frame(
      Type = type,
      Category = category,
      Query = query,
      Column_examined = column,
      stringsAsFactors = FALSE
    )

  counts_df <- as.data.frame(matrix(data = counts[[1]], nrow = 1, ncol = nrow(strings)))
  result <- cbind(result, counts_df, stringsAsFactors = FALSE)

  result
}
