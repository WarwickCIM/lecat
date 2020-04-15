#' create_unique_total_diagnostics
#'
#' Creates a file listing the total occurances of all queries and categories.
#'
#' @param lecat_result data frame output from the \link[lecat]{run_lecat_analysis} function
#'
#' @return Passing the output of the \link[lecat]{run_lecat_analysis} function will return a data frame with Type,
#' Category, Queries and Column_examined columns. In the output the unique and total occurances of Types, Category and Query
#' are reported in the format Term(total occurances, unique occurances).
create_unique_total_diagnostics <- function(lecat_result){

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
  pb <- utils::txtProgressBar(min = 1, max = nrow(lecat_result), initial = 1)

  # Loop though types
  for (type in unique(lecat_result$Type)) {

    # pick out data for this type
    this_result <- lecat_result[lecat_result$Type == type,]

    # calculate totals for this type by corpus element
    type_totals <- colSums(this_result[,5:ncol(this_result)], na.rm = TRUE)

    # total accross corpus elements
    type_total <- sum(type_totals, na.rm = TRUE)

    # count number of counts above 1 - in how many elements is the query found
    type_unique <- sum(type_totals > 0, na.rm = TRUE)

    # save string for this type
    type_string <- paste(type, '(', type_total, ',', type_unique, ')', sep = '')

    # loop though categories in type
    for (category in unique(this_result$Category)) {

      # pick out category data for this type
      category_data <- this_result[this_result$Category == category,]

      # sum accross corpus elements
      category_totals <- colSums(category_data[,5:ncol(category_data)])

      # count total number of occurences
      category_total <- sum(category_totals, na.rm = TRUE)

      # count total corpus elements for which the count > 0
      category_unique <- sum(category_totals > 0, na.rm = TRUE)

      # write save string summary
      category_string <- paste(category, '(', category_total, ',', category_unique, ')', sep = '')

      # preallocate query string
      query_strings <- ''

      # for each query in our category data
      for (query in unique(category_data$Query)) {

        # incriment the progress bar
        utils::setTxtProgressBar(pb, i)

        # pick out our query data
        query_data <- category_data[category_data$Query == query,]

        # take the totals here as we have no rows to sum down
        query_totals <- query_data[5:ncol(query_data)]

        # collect totals
        query_total <- sum(query_totals, na.rm = TRUE)

        # count number of corpus element where query occurs
        query_unique <- sum(query_totals > 0, na.rm = TRUE)

        # save query string
        query_string <- paste(query, '(', query_total, ',', query_unique, ')', sep = '')

        # add query string entry to existing query strings
        query_strings <- paste(query_strings, query_string)

        # incriment our i variable
        i <- i + 1
      }

        # write result to preallocated dataframe
        result[result_i,] <- data.frame(Type = type_string,
                                      Category = category_string,
                                      Queries = query_strings,
                                      Column_examined = unique(this_result$Column_examined),
                                      stringsAsFactors = FALSE)
       # incriment result counter
        result_i <- result_i + 1
    }
  }
  close(pb)
  result
}
