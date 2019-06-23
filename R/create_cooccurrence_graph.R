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
#' @param inShiny If inShiny is TRUE then shiny based notifications will be shown
#' @return cooccurence table (At present.)
#'
create_cooccurrence_graph <- function(lecat_result, graph_filename = 'result.graphml', cotable_filename = 'cotable.csv', level = 'Query', inShiny = FALSE) {
  assertive::assert_is_data.frame(lecat_result)
  assertive::assert_is_character(graph_filename)
  assertive::assert_is_character(cotable_filename)
  assertive::assert_is_character(level)
  lecat_result <- lecat_result[,colSums(is.na(lecat_result))<nrow(lecat_result)]
  # notify if user is trying to aggregate only one item (e.g., co-occurance of Type when there is only 1 type)
  if ((level == 'Query') & (length(unique(lecat_result$Query)) < 2) ) {
    if(inShiny) {
      shiny::showNotification('Cannot calculate co-occurence. Only 1 Query', type = "warning")
    } else {
      message('Cannot calculate co-occurence. Only 1 Query')
    }
    list(
      cotable = data.frame(warning = 'Cannot calculate co-occurence. Only 1 Query'),
      graph = data.frame(warning = 'Cannot calculate co-occurence. Only 1 Query')
    )
  } else if((level == 'Type') & (length(unique(lecat_result$Type)) < 2)) {
    if(inShiny) {
      shiny::showNotification('Cannot calculate co-occurence. Only 1 Type', type = "warning")
    } else {
      message('Cannot calculate co-occurence. Only 1 Type')
    }
    list(
      cotable = data.frame(warning = 'Cannot calculate co-occurence. Only 1 Query'),
      graph = data.frame(warning = 'Cannot calculate co-occurence. Only 1 Query')
    )
  } else if((level == 'Category') & (length(unique(lecat_result$Category)) < 2)) {
    if(inShiny) {
      shiny::showNotification('Cannot calculate co-occurence. Only 1 Category', type = "warning")
    } else {
      message('Cannot calculate co-occurence. Only 1 Category')
    }
    list(
      cotable = data.frame(warning = 'Cannot calculate co-occurence. Only 1 Query'),
      graph = data.frame(warning = 'Cannot calculate co-occurence. Only 1 Query')
    )
  } else { # If the selected level has more than 1 entry
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
    if (inShiny) {
      shiny::withProgress(message = 'Calculating cooccurnece', value = 0, {
        n <- nrow(agg_data)
        for (i in 1:nrow(agg_data)) {
          shiny::incProgress(1/n, detail = paste("node", i))
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
        }
      })
    } else {
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
    }

    graph <- igraph::make_empty_graph(directed = FALSE)
    if (level == 'Query') {
      node_list <- unique(cotable[,c(1:2,5)])
      node_list$id <- 1:nrow(node_list)
      if (inShiny){ shiny::showNotification('Adding nodes to graph')} else {message('Adding nodes to graph')}
      graph <- igraph::add_vertices(
        graph = graph,
        nv = nrow(node_list),
        name = node_list$node1,
        label = node_list$node1,
        Type = node_list$node1.Type,
        Category = node_list$node1.Category
      )
      edge_list <- cotable[cotable$cooccurence > 0,]
      if (inShiny) {
        shiny::withProgress(message = 'Adding edges to graph', value = 0, {
          n <- nrow(edge_list)
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
            shiny::incProgress(1/n, detail = paste("Edge", i))
          }
        })
      } else {
        message('Adding edges to graph')
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
      }
    } else if (level == 'Category') {
      node_list <- unique(cotable[,c(1,3)])
      node_list$id <- 1:nrow(node_list)
      if (inShiny){ shiny::showNotification('Adding nodes to graph')} else {message('Adding nodes to graph')}
      graph <- igraph::add_vertices(
        graph = graph,
        nv = nrow(node_list),
        name = node_list$node1,
        label = node_list$node1,
        Type = node_list$node1.Type
      )
      if (inShiny) { shiny::showNotification('Adding edges to graph')} else {message('Adding edges to graph')}
      edge_list <- cotable[cotable$cooccurence > 0,]
      if (inShiny) {
        n <- nrow(edge_list)
        shiny::withProgress(message = 'Adding edges', value = 0, {
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
            shiny::incProgress(1/n, detail = paste("Edge", i))
          }
        })
      } else {
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
      }
    } else if (level == 'Type') {
      node_list <- data.frame(node1 = unique(cotable[,3]),
                              id = 1:length(unique(cotable[,3])),
                              stringsAsFactors = FALSE)
      if (inShiny){ shiny::showNotification('Adding nodes to graph')} else {message('Adding nodes to graph')}
      graph <- igraph::add_vertices(
        graph = graph,
        nv = nrow(node_list),
        name = node_list$node1,
        label = node_list$node1
      )
      if (inShiny){ shiny::showNotification('Adding edges to graph')} else {message('Adding edges to graph')}
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
    if (inShiny){ shiny::showNotification('Removing duplicate edges')} else {message('Removing duplicate edges')}
    message('Removing duplicate edges')
    graph <- igraph::simplify(graph)
    if (!is.null(graph_filename)){
      assertive::assert_is_character(graph_filename)
      if (!inShiny) {
        igraph::write_graph(graph = graph, file = graph_filename, format = 'graphml')
      }
    } else {
      stop('Parameter graph_filename is NULL.')
    }
    if (!is.null(cotable_filename)){
      assertive::assert_is_character(cotable_filename)
      if (!inShiny) {
        utils::write.csv(x = cotable, file = cotable_filename, row.names = FALSE)
      }
    } else {
      stop('Parameter cotable_filename is NULL.')
    }
    list(
      cotable = cotable,
      graph = graph
    )
  }

}
