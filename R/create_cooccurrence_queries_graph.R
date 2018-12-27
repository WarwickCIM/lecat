#' create_cooccurrence_query_graph
#'
#' Counts the co-occurrence of queries within each corpus element.
#' An graphml file is created and the co-occurrence table is returned.
#' Calculations are currently very inefficient and will be improved in the future.
#' @param lecat_result data frame output from the \link[lecat]{run_lecat_analysis} function
#' @param filename filename of the graphml graph file
#'
#' @return Returns a list containing a table of the co-occurrence of query terms
#' and a graph object when passed the output of the \link[lecat]{run_lecat_analysis} function. The nodes of the
#' undirected graph have the Type, Category and Column_examined attributes and edges are
#' weighted according to co-occurance. The graphi is written if a filename is specified.
create_cooccurrence_query_graph <- function(lecat_result, filename = NULL){
  assertive::assert_is_data.frame(lecat_result)
  cotable <- NULL
  n_col <- ncol(lecat_result)
  pb <- utils::txtProgressBar(min = 1, max = nrow(lecat_result), initial = 1)
  for (x in 1:nrow(lecat_result)) {
    #message('Row ', x, ' of ', nrow(lecat_result))
    this_x <- lecat_result$Query[x]
    for (y in 1:nrow(lecat_result)) {
      this_y <- lecat_result$Query[y]
      if (x != y){
        x_occurrence <- lecat_result[x, 5:n_col]
        y_occurrence <- lecat_result[y, 5:n_col]
        cooccurrence <- sum((x_occurrence > 0) * (y_occurrence > 0))
        cotable <- rbind(
          cotable,
          data.frame(
            node_nv_x = x,
            node_nv_y = y,
            x = this_x,
            y = this_y,
            cooccurrence = cooccurrence,
            stringsAsFactors = FALSE
          )
        )
      }
    }
    utils::setTxtProgressBar(pb, x)
  }
  close(pb)
  # TODO : Remove duplicate reverse x and y nodes from cotable
  # TODO : Increase speed of calculating the co-occurrence
  graph <- igraph::make_empty_graph(directed = FALSE)
  graph <- igraph::add_vertices(
    graph = graph,
    nv = nrow(lecat_result),
    name = lecat_result$Query,
    label = lecat_result$Query,
    Type = lecat_result$Type,
    Category = lecat_result$Category,
    Column_examined = lecat_result$Column_examined
    )

  message('Removing duplicate edges')
  graph <- igraph::simplify(graph)
  if (!is.null(filename)){
    assertive::assert_is_character(filename)
    igraph::write_graph(graph = graph, file = filename, format = 'graphml')
  }
  list(
    cotable = cotable[,c('x','y','cooccurrence')],
    graph = graph
  )
}
