#' create_cotable
#'
#' Generates a cooccurence table
#' @description Calculates a table of cooccurences
#'
#' @param this_mat matrix of occurences of one item. Rows are items (Category, Type or Query) and columns are corpus elements
#' @param that_mat matrix of occurences of the other items. Rows and columns as this_mat.
#' @param this_mat_attributes data frame containing the Type
#' @return data frame containing node1, node2 and the cooccurrence frequency
#'
create_cotable <- function(this_mat, that_mat, this_mat_attr, that_mat_attr, level) {
  assertive::assert_is_matrix(this_mat)
  assertive::assert_is_matrix(that_mat)
  assertive::assert_is_a_string(row.names(this_mat))
  assertive::assert_is_character(row.names(that_mat))
  cooc_matrix <- (that_mat > 0) *
    (do.call("rbind", rep(list(this_mat > 0), nrow(that_mat))))
  cooc_table <- data.frame(
    node1 = rep(row.names(this_mat), nrow(that_mat)),
    node2 = row.names(that_mat),
    cooccurence = rowSums(cooc_matrix),
    stringsAsFactors = FALSE
  )
  att_table <- switch(level,
                      Query = data.frame(
                        node1.Type = rep(this_mat_attr$Type, nrow(cooc_table)),
                        node1.Category = rep(this_mat_attr$Category, nrow(cooc_table)),
                        node2.Feature = that_mat_attr$Type,
                        node2.Category = that_mat_attr$Category,
                        stringsAsFactors = FALSE
                      ))
  cooc_table <- cbind(att_table, cooc_table)
  row.names(cooc_table) <- 1:nrow(cooc_table)
  cooc_table
}
