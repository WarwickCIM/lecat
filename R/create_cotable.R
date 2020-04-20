#' Create a cooccurence table. Used internally by create_coocurrence_graph
#'
#' Generates a cooccurence table
#' @description Calculates a table of cooccurences of a single node to all other nodes. Called by \link[lecat]{create_cooccurrence_graph} function for each node in turn.
#'
#' @param this_mat matrix of occurences of one item. Rows are nodes - differs to either Type, Category or Query depending on the level passed to \link[lecat]{create_cooccurrence_graph} - and columns are corpus elements
#' @param that_mat matrix of occurences of the other items. Rows and columns as this_mat
#' @param this_mat_attr data frame containing a single nodes attributes. Attributes present - Type or Category -
#' depends on the level parameter passed to \link[lecat]{create_cooccurrence_graph}
#' @param that_mat_attr data frame containing attribute of all other nodes. Attributes differ as above depending on level
#' @param level level of cooccurrence analysis. Can be either 'Type', 'Category' or 'Query'.
#'
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
  if (level == 'Query') {
    att_table <- data.frame(
      node1.Type = rep(this_mat_attr$Type, nrow(cooc_table)),
      node1.Category = rep(this_mat_attr$Category, nrow(cooc_table)),
      node2.Type = that_mat_attr$Type,
      node2.Category = that_mat_attr$Category,
      stringsAsFactors = FALSE
    )
  } else if (level == 'Category') {
    att_table <- data.frame(
      node1.Type = rep(this_mat_attr$Type, nrow(cooc_table)),
      node2.Type = that_mat_attr$Type,
      stringsAsFactors = FALSE
    )
  } else if (level == 'Type') {
    att_table <- data.frame(node1.Type = rep(NA, nrow(cooc_table)),
                            node2.Type = rep(NA, nrow(cooc_table)),
                            stringsAsFactors = FALSE)
  } else {
    stop('level parameter does not equal Query, Type or Category')
  }
  cooc_table <- cbind(att_table, cooc_table)
  row.names(cooc_table) <- 1:nrow(cooc_table)
  cooc_table
}
