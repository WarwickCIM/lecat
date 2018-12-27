#' create_cooccurence_graph
#'
#' Creates a cooccurence graph
#' @description Calculates the cooccurrence of either Types, Categories or Queries.
#' A graph object is then created using the \link[igraph]{make_graph} function for
#' visualising in Gephi or similiar software.
#'
#' @param lecat_result result from the \link[lecat]{run_lecat_analysis} function
#' @param filename filename of the generated graph file
#' @param level Level of cooccurence. Either 'Query', 'Category' or 'Type'
#' @return cooccurence table (At present.)
#'
create_cooccurrence_graph <- function(lecat_result, filename = 'result.graphml', level = 'Query') {
  assertive::assert_is_data.frame(lecat_result)
  assertive::assert_is_character(filename)
  assertive::assert_is_character(level)
  agg_data <-   switch(level,
                       Type = aggregate(. ~ Type,
                                        data = lecat_result[,c(1,5:ncol(lecat_result))],
                                        FUN = sum),
                       Category = aggregate(. ~ Type + Category,
                                            data = lecat_result[,c(1:2,5:ncol(lecat_result))],
                                            FUN = sum),
                       Query = aggregate(. ~ Type + Category + Query,
                                         data = lecat_result[,c(1:3,5:ncol(lecat_result))],
                                         FUN = sum),
                       stop('Level not found. Cannot summarise the data.')
  )
  number_columns <- as.logical(unlist(lapply(agg_data, FUN = is.numeric)))
  cotable <- NULL
  pb <- utils::txtProgressBar(min = 1, max = nrow(agg_data), initial = 1)
  for (i in 1:nrow(agg_data)) {
    this_mat <- as.matrix(agg_data[i, number_columns])
    that_mat <- as.matrix(agg_data[-i, number_columns])
    this_mat_attr <- agg_data[i, !number_columns]
    that_mat_attr <- agg_data[-i, !number_columns]
    row.names(this_mat) <- agg_data[i, level]
    row.names(that_mat) <- agg_data[-i, level]
    cotable <- rbind(
      cotable,
      create_cotable(this_mat, that_mat, this_mat_attr, that_mat_attr, level)
    )
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  graph <- igraph::make_empty_graph(directed = FALSE)
  if (level == 'Query') {
    node_list <- unique(cotable[,c(1:2,5)])
    node_list$id <- 1:nrow(node_list)
    head(node_list)
    graph <- igraph::add_vertices(
      graph = graph,
      nv = node_list$id,
      name = node_list$node1,
      label = node_list$node1,
      Type = node_list$node1.Type,
      Category = node_list$node1.Category
    )
  }
  list(
    cotable = cotable,
    graph = graph
  )
}
