#' Searches a series of strings for a specific query. Used internally by \link[lecat]{run_lecat_analysis}
#'
#' Search through a series for queries based on a regex query.
#'
#' @param strings Vector of strings to search through. The values for the corresponding search column from the chosen corpus for a given lexicon type, as specified in the lookup table.
#' @param query String query term to search for in the strings. Derived from lexicon.
#' @param regex String regular expression defining the search pattern. Defaults to searching for the query term with non word characters either side or at the beginning or end of string. Look behind and look ahead impede characters outside the query term to be matched (correctly finds emoji or any other non-word query terms)
#' @param type String query type. Derived from lexicon type.
#' @param category String query type. Derived from lexicon category.
#' @param column String column name examined included in the returned dataframe
#' @param case_sensitive If case_sensitive is TRUE then the search will be case sensitive
#' @param advanced_mode If advanced_mode is TRUE then the search will not apply a regex, instead it will assume all query terms are well-formed regex patterns (this covers query terms with no regex pattern at all).
#'
#' @return dataframe with counts of the query in each string.
run_search <-
  function(strings,
           query,
           regex = "(?<=\\W|^)query(?=\\W|$)",
           type,
           category,
           column,
           case_sensitive,
           advanced_mode) {

    # if not on advanced mode, prepare the query by escaping special characters and adding the global regex
    if (!advanced_mode) {

      # correctly add backslash to any special character for regex search {}[]()|?$^*+.\
      query <-
        stringr::str_replace_all(query, "([{}\\[\\]()|?$^*+.\\\\])", "\\\\\\$1")

      # replaces the word query in the regex with the query we're searching for
      this_pattern <-
        stringr::str_replace(string = regex,
                             pattern = 'query',
                             replacement = query)

    } else {

      # the query term should already come as a regex pattern
      this_pattern <- query
    }

    # count matches of the regex - applies case sensitivity as per user selection
    counts <-
      lapply(
        strings,
        stringr::str_count,
        stringr::regex(this_pattern, ignore_case = !case_sensitive)
      )

    # output dataframe
    result <-
      data.frame(
        Type = type,
        Category = category,
        Query = query,
        Column_examined = column,
        stringsAsFactors = FALSE
      )

    # make strings into a data frame
    if (!is.data.frame(strings)) {
      strings <- as.data.frame(strings)
    }

    # create a matrix with the results, that can be appended to the whole
    counts_df <-
      as.data.frame(matrix(
        data = as.numeric(unlist(counts)),
        nrow = 1,
        ncol = nrow(strings)
      ))
    result <- cbind(result, counts_df, stringsAsFactors = FALSE)

    result
  }
