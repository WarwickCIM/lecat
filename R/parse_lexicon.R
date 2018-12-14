#' parse_lexicon
#'
#' Takes a wide format lexicon and creates a long form lexicon file.
#' The long form lexicon can then be passed to the LE-CAT search function.
#' Wide format lexicons should have the columns for type and category,
#' query terms should start at the query column and continue along the row.
#' Each category should have one row per category.
#'
#' @param wide_lexicon Data frame with one row per category containing type, category and the corresponding queries
#' @param type_column Column name in the wide_lexicon which contains the category type
#' @param category_column Column name in the wide_lexicon which contains the category of interest
#' @param query_column Column name in the wide_lexicon where the first query term is located. Additional queries should be in the subsequent columns until the end of the row.
#'
#' @return  If a wide format lexicon and the correct column names are passed
#' to this function then a long format lexicon is returned. The long format
#' lexicon has the columns Type, Category and Query.
parse_lexicon <- function(wide_lexicon, type_column = 'Type', category_column = 'Category', query_column = 'Query'){
  assertive::assert_is_character(type_column)
  assertive::assert_is_character(category_column)
  assertive::assert_is_character(query_column)
  assertive::assert_is_data.frame(wide_lexicon)
  long_lexicon <- NULL
  for (i in 1:nrow(wide_lexicon)){
    these_queries <- wide_lexicon[i, which(colnames(wide_lexicon) == query_column):length(names(wide_lexicon))]
    these_queries <- these_queries[!is.na(these_queries)]
    these_queries <- these_queries[these_queries != '']
    long_lexicon <- rbind(
      long_lexicon,
      data.frame(
        Type = wide_lexicon[i, type_column],
        Category = wide_lexicon[i, category_column],
        Queries = these_queries,
        stringsAsFactors = FALSE
      )
    )
  }
  long_lexicon
}
