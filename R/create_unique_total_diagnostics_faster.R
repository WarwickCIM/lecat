#' create_unique_total_diagnostics
#'
#' Creates a file listing the total occurances of all queries and categories.
#'
#' @param lecat_result data frame output from the \link[lecat]{run_lecat_analysis} function
#'
#' @return Passing the output of the \link[lecat]{run_lecat_analysis} function will return a data frame with Type,
#' Category, Queries and Column_examined columns. In the output the unique and total occurances of Types, Category and Query
#' are reported in the format Term(total occurances, unique occurances).
create_unique_total_diagnostics_faster <- function(lecat_result){

  # TODO: keep counts as a matrix for efficiency reasons
  #       currently much code assumes it's a dataframe,
  #       so this will take a while

  # check input arguments
  assertive::assert_is_data.frame(lecat_result)

  # preallocate results dataframe
  result <- data.frame(Type = rep(NaN, length(unique(lecat_result$Type))),
                       Category = NaN,
                       Queries = NaN,
                       Column_examined = NaN,
                       stringsAsFactors = FALSE)
  # iterators
  i <- 1
  result_i <- 1

  # Create progress bar
  pb <- utils::txtProgressBar(min = 1, max = length(unique(lecat_result$Type)), initial = 1)

  count <- function(x) {
    totals <- colSums(x, na.rm = TRUE)
    paste('(', sum(totals), ',', sum(totals > 0), ')', sep = '')
  }

  n <- length(unique(lecat_result$Type))
  category_i <- 1
  shiny::withProgress(message = 'Generating diagnostics', value = 0, {

    # Loop though types
    for (type in unique(lecat_result$Type)) {

      shiny::incProgress(1/n, detail = paste("Type", category_i))

      # types, categories and queries
      these_types_categories_queries <-
        lecat_result[lecat_result$Type == type, 1:4]

      # pass frequencies to count function
      type_string <-
        paste(type,
              count(lecat_result[lecat_result$Type == type, 5:ncol(lecat_result)])
              , sep = '')

      # loop though categories in type
      for (category in unique(these_types_categories_queries$Category)) {

        category_i <- category_i + 1

        # categories and queries
        these_categories_queries <-
          lecat_result[lecat_result$Type == type & lecat_result$Category == category, 1:4]

        # pass frequencies to count function
        category_string <-
          paste(category,
                count(
                  lecat_result[lecat_result$Type == type & lecat_result$Category == category, 5:ncol(lecat_result)]
                ),
                sep = '')

        # preallocate query string
        query_strings <- ''

        # for each query in our category data
        for (query in unique(these_categories_queries$Query)) {

          # incriment the progress bar
          utils::setTxtProgressBar(pb, i)

          # pass frequencies to count function
          query_string <-
            paste(query,
                  count(
                    lecat_result[lecat_result$Type == type & lecat_result$Category == category & lecat_result$Query == query, 5:ncol(lecat_result)]
                  ),
                  sep = '')

          # add query string entry to existing query strings
          query_strings <- paste(query_strings, query_string)

        }

        # write result to preallocated dataframe
        result[result_i,] <- data.frame(Type = type_string,
                                        Category = category_string,
                                        Queries = query_strings,
                                        Column_examined = unique(these_categories_queries$Column_examined),
                                        stringsAsFactors = FALSE)
        # incriment result counter
        result_i <- result_i + 1
      }
      # incriment our i variable
      i <- i + 1
    }
  })
  close(pb)
  result
}
