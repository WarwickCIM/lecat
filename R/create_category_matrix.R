#' Generates a matrix for lexicon Category results
#'
#' Creates a file listing the total matches for each Category in the lexicon for each row in the corpus.
#' The file can be pasted as additional columns in the corpus, which allows for using filter and sorting functions to explore the data.
#' For example, to find all rows in an analysed corpus that match a specific lexicon category or combination of categories and how many times.
#'
#' @param lecat_result data frame output from the \link[lecat]{run_lecat_analysis} function
#' @param inShiny If inShiny is TRUE then shiny based notifications will be shown
#'
#' @return Passing the output of the \link[lecat]{run_lecat_analysis} function will return a data frame
create_category_matrix <-
  function(lecat_result, inShiny = FALSE) {
    # iterators
    categories <- unique(lecat_result$Category)
    i <- 1
    n <- nrow(categories)

    # start building the result matrix by adding row numbers (which are column names in the lecat_result data frame, from the fifth column) as the first column
    result <-
      cbind(colnames(lecat_result[, c(5:ncol(lecat_result))]))

    if (inShiny) {
      shiny::withProgress(message = 'Generating category matrix', detail = 'This is fairly fast', value = 0, {

        # go through each category
        for (x in categories) {

          # increment progress bar
          shiny::incProgress(1/n, detail = paste('Category:', x))
          i <- i + 1

          # subset the rows that match the category
          mm <- subset(lecat_result, Category == x)

          # start adding columns to the result dataframe (subtotals for ID columns - if a tweet has hit any of the query terms for the category the sum will be more than 0)
          result <- cbind(result, colSums(mm[, c(5:ncol(mm))]))
        }
      })
    } else {
      pb <- utils::txtProgressBar(
        min = 1,
        max = nrow(categories),
        initial = 1
      )
      # go through each category
      for (x in categories) {
        # increment progress bar
        utils::setTxtProgressBar(pb, i)
        i <- i + 1
        # subset the rows that match the category
        mm <- subset(lecat_result, Category == x)
        # start adding columns to the result dataframe (subtotals for ID columns - if a tweet has hit any of the query terms for the category the sum will be more than 0)
        result <- cbind(result, colSums(mm[, c(5:ncol(mm))]))
      }

    }
    # now let's add the column names so humans can read them
    colnames(result) <- c("Row ID", categories)

    # now return the result as a dataframe
    as.data.frame(result, stringsAsFactors = FALSE)

  }
