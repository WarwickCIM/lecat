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
  assertive::assert_is_data.frame(lecat_result)
  message('removing empty columns in the lecat_result')
  out <- NULL
  i <- 1
  pb <- utils::txtProgressBar(min = 1, max = nrow(lecat_result), initial = 1)
  for (type in unique(lecat_result$Type)) {
    this_result <- lecat_result[lecat_result$Type == type,]
    type_totals <- colSums(this_result[,5:ncol(this_result)], na.rm = TRUE)
    type_total <- sum(type_totals, na.rm = TRUE)
    type_unique <- sum(type_totals > 0, na.rm = TRUE)
    type_string <- paste(type, '(', type_total, ',', type_unique, ')', sep = '')
    for (category in unique(this_result$Category)) {
      category_data <- this_result[this_result$Category == category,]
      category_totals <- colSums(category_data[,5:ncol(category_data)])
      category_total <- sum(category_totals, na.rm = TRUE)
      category_unique <- sum(category_totals > 0, na.rm = TRUE)
      category_string <- paste(category, '(', category_total, ',', category_unique, ')', sep = '')
      query_strings <- ''
      for (query in unique(category_data$Query)) {
        utils::setTxtProgressBar(pb, i)
        query_data <- category_data[category_data$Query == query,]
        query_totals <- query_data[5:ncol(query_data)]
        query_total <- sum(query_totals, na.rm = TRUE)
        query_unique <- sum(query_totals > 0, na.rm = TRUE)
        query_string <- paste(query, '(', query_total, ',', query_unique, ')', sep = '')
        query_strings <- paste(query_strings, query_string)
        i <- i + 1
      }
      out <- rbind(out,
                   data.frame(Type = type_string,
                              Category = category_string,
                              Queries = query_strings,
                              Column_examined = unique(this_result$Column_examined),
                              stringsAsFactors = FALSE)
                   )
    }
  }
  close(pb)
  out
}
