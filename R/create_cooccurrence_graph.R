#' create_cooccurrence_graph
#'
#' Creates a cooccurrence graph
#' @description Calculates the cooccurrence of either Types, Categories or Queries.
#' A graph object is then created using the \link[igraph]{make_graph} function for
#' visualising in Gephi or similiar software.
#'
#' @param lecat_result result from the \link[lecat]{run_lecat_analysis} function
#' @param graph_filename filename of the generated graph file
#' @param cotable_filename filename of the generated cooccurrence table
#' @param level Level of cooccurence. Either 'Query', 'Category' or 'Type'
#' @return cooccurence table (At present.)
#'
create_cooccurrence_graph <- function(lecat_result, graph_filename = 'result.graphml', cotable_filename = 'cotable.csv', level = 'Query') {
  assertive::assert_is_data.frame(lecat_result)
  assertive::assert_is_character(graph_filename)
  assertive::assert_is_character(cotable_filename)
  assertive::assert_is_character(level)
  if (level == 'Query') {
    agg_data <- stats::aggregate(. ~ Type + Category + Query,
                                 data = lecat_result[,c(1:3,5:ncol(lecat_result))],
                                 FUN = sum)
  } else if (level == 'Type') {
    agg_data <- stats::aggregate(. ~ Type,
                                 data = lecat_result[,c(1,5:ncol(lecat_result))],
                                 FUN = sum)
  } else if (level == 'Category') {
    agg_data <- stats::aggregate(. ~ Type + Category,
                                 data = lecat_result[,c(1:2,5:ncol(lecat_result))],
                                 FUN = sum)
  } else {
    stop('level parameter does not equal Query, Type or Category')
  }
  number_columns <- as.logical(unlist(lapply(agg_data, FUN = is.numeric)))
  cotable <- NULL
  message('Calculating cooccurence table')
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
    message('Adding nodes to graph')
    graph <- igraph::add_vertices(
      graph = graph,
      nv = nrow(node_list),
      name = node_list$node1,
      label = node_list$node1,
      Type = node_list$node1.Type,
      Category = node_list$node1.Category
    )
    message('Adding edges to graph')
    edge_list <- cotable[cotable$cooccurence > 0,]
    pb <- utils::txtProgressBar(min = 1, max = nrow(edge_list), initial = 1)
    for (i in 1:nrow(edge_list)) {
      # lookup cotable nodes in node list
      type_idx_1 <- node_list$node1.Type == edge_list$node1.Type[i]
      category_idx_1 <- node_list$node1.Category == edge_list$node1.Category[i]
      query_idx_1 <- node_list$node1 == edge_list$node1[i]
      id_1 <- node_list$id[type_idx_1 & category_idx_1 & query_idx_1]

      type_idx_2 <- node_list$node1.Type == edge_list$node2.Type[i]
      category_idx_2 <- node_list$node1.Category == edge_list$node2.Category[i]
      query_idx_2 <- node_list$node1 == edge_list$node2[i]
      id_2 <- node_list$id[type_idx_2 & category_idx_2 & query_idx_2]

      # add edge to graph
      graph <- igraph::add_edges(graph = graph,
                                 edges = c(id_1, id_2),
                                 weight = edge_list$cooccurence[i])
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
  } else if (level == 'Category') {
    node_list <- unique(cotable[,c(1,3)])
    node_list$id <- 1:nrow(node_list)
    message('Adding nodes to graph')
    graph <- igraph::add_vertices(
      graph = graph,
      nv = nrow(node_list),
      name = node_list$node1,
      label = node_list$node1,
      Type = node_list$node1.Type
    )
    message('Adding edges to graph')
    edge_list <- cotable[cotable$cooccurence > 0,]
    pb <- utils::txtProgressBar(min = 1, max = nrow(edge_list), initial = 1)
    for (i in 1:nrow(edge_list)) {
      # lookup cotable nodes in node list
      type_idx_1 <- node_list$node1.Type == edge_list$node1.Type[i]
      category_idx_1 <- node_list$node1 == edge_list$node1[i]
      id_1 <- node_list$id[type_idx_1 & category_idx_1]

      type_idx_2 <- node_list$node1.Type == edge_list$node2.Type[i]
      category_idx_2 <- node_list$node1 == edge_list$node2[i]
      id_2 <- node_list$id[type_idx_2 & category_idx_2]

      # add edge to graph
      graph <- igraph::add_edges(graph = graph,
                                 edges = c(id_1, id_2),
                                 weight = edge_list$cooccurence[i])
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
  } else if (level == 'Type') {
    node_list <- data.frame(node1 = unique(cotable[,3]),
                            id = 1:length(unique(cotable[,3])),
                            stringsAsFactors = FALSE)
    message('Adding nodes to graph')
    graph <- igraph::add_vertices(
      graph = graph,
      nv = nrow(node_list),
      name = node_list$node1,
      label = node_list$node1
    )
    message('Adding edges to graph')
    edge_list <- cotable[cotable$cooccurence > 0,]
    pb <- utils::txtProgressBar(min = 1, max = nrow(edge_list), initial = 1)
    for (i in 1:nrow(edge_list)) {
      # lookup cotable nodes in node list
      id_1 <- node_list$id[node_list$node1 == edge_list$node1[i]]
      id_2 <- node_list$id[node_list$node1 == edge_list$node2[i]]

      # add edge to graph
      graph <- igraph::add_edges(graph = graph,
                                 edges = c(id_1, id_2),
                                 weight = edge_list$cooccurence[i])
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
  }
  message('Removing duplicate edges')
  graph <- igraph::simplify(graph)
  if (!is.null(graph_filename)){
    assertive::assert_is_character(graph_filename)
    igraph::write_graph(graph = graph, file = graph_filename, format = 'graphml')
  } else {
    stop('Parameter graph_filename is NULL.')
  }
  if (!is.null(cotable_filename)){
    assertive::assert_is_character(cotable_filename)
    utils::write.csv(x = cotable, file = cotable_filename, row.names = FALSE)
  } else {
    stop('Parameter cotable_filename is NULL.')
  }
  list(
    cotable = cotable,
    graph = graph
  )
}
