#' Searches for queries in a corpus using a specific regular expression. Uses \link[lecat]{run_search}.
#'
#' Each corpus element is checked for the presence of a query. The process is repeated for multiple queries. The result is a table of queries and number of matches for each corpus row.
#'
#' @param lexicon Lexicon dataframe as parsed by the \link[lecat]{parse_lexicon} function
#' @param corpus Corpus dataframe containing search columns present in the searches dataframe
#' @param searches Data frame with the columns 'Type' and 'Column'. Queries in each Type will be located in the corresponding corpus Column
#' @param regex_expression String regular expression defining search pattern. Defaults to searching for the query term with non word characters either side or at the beginning or end of string. Look behind and look ahead impede characters outside the query term to be matched (correctly finds emoji or any other non-word query terms)
#' @param inShiny If inShiny is TRUE then shiny based notifications will be shown
#' @param case_sensitive If case_sensitive is TRUE then the search will be case sensitive
#' @param advanced_mode If advanced_mode is TRUE then the search will not apply a regex, instead it will assume all query terms are well-formed regex patterns (this covers query terms with no regex pattern at all).
#'
#' @return run_lecat_analysis uses \link[lecat]{run_search} to return a data frame containing the lexicon, the corresponding search column for the query type, and the frequency of terms by corpus row (columns representing corpus rows are named after ID in corpus, if present)
run_lecat_analysis <-
  function(lexicon,
           corpus,
           searches,
           regex_expression = '(?<=\\W|^)query(?=\\W|$)',
           inShiny = FALSE,
           case_sensitive = FALSE,
           advanced_mode = FALSE) {
    # start building output
    result <-
      data.frame(
        Type = rep(NaN, nrow(lexicon)),
        Category = rep(NaN, nrow(lexicon)),
        Query = rep(NaN, nrow(lexicon)),
        Column_examined = rep(NaN, nrow(lexicon)),
        stringsAsFactors = FALSE
      )

    counts_df <-
      as.data.frame(matrix(
        data = rep(NaN, nrow(lexicon) * nrow(corpus)),
        nrow = nrow(lexicon),
        ncol = nrow(corpus)
      ))

    result <- cbind(result, counts_df, stringsAsFactors = FALSE)

    if (inShiny) {
      n <- nrow(lexicon)
      shiny::withProgress(message = 'Searching corpus',
                          detail = 'This  may take a while...',
                          value = 0,
                          {
                            # go through each row in the long lexicon
                            for (i in 1:nrow(lexicon)) {
                              shiny::incProgress(1 / n, detail = paste("Query", i, lexicon$Queries[i]))
                              # set the search column for this type, as specified in the lookup table
                              this_search_column <-
                                searches$Column[lexicon$Type[i] == searches$Type]
                              #out <- rbind(out,
                              #shiny::showNotification(paste('Query:', lexicon$Queries[i]))
                              # search for this query term
                              result[i, ] <- run_search(
                                corpus[, this_search_column],
                                lexicon$Queries[i],
                                regex_expression,
                                lexicon$Type[i],
                                lexicon$Category[i],
                                this_search_column,
                                case_sensitive,
                                advanced_mode
                              )
                              #)
                            }
                          })
    } else {
      pb <-
        utils::txtProgressBar(min = 1,
                              max = nrow(lexicon),
                              initial = 1)
      # go through each row in the long lexicon
      for (i in 1:nrow(lexicon)) {
        utils::setTxtProgressBar(pb, i)
        # set the search column for this type, as specified in the lookup table
        this_search_column <-
          searches$Column[lexicon$Type[i] == searches$Type]

        # search for this query term
        result[i, ] <-
          run_search(
            strings = corpus[, this_search_column],
            query = lexicon$Queries[i],
            regex = regex_expression,
            type = lexicon$Type[i],
            category = lexicon$Category[i],
            column = this_search_column,
            case_sensitive,
            advanced_mode
          )
      }
      close(pb)
    }

    # If corpus contains an ID column, use it to name columns in the raw matrix (after already named columns for lexicon fields)
    if ("id" %in% names(corpus)) {
      names(result)[5:ncol(result)] <- corpus$id
    }
    result
  }
