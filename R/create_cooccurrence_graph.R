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

  # Check argument types
  assertive::assert_is_data.frame(lecat_result)
  assertive::assert_is_character(graph_filename)
  assertive::assert_is_character(cotable_filename)
  assertive::assert_is_character(level)

  # prevent one node networks

  # Check more than 1 node ----
  # if only one query for query networks
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
    # if only 1 type for type networks
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
    # if only one category for category networks
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

    # Data aggregation if > 1 node ----
  } else {

    # Notification to user
    if (inShiny) {
      shiny::showNotification('Aggregating data to generate network. Please wait.',type = 'message')
    } else {
      message('Aggregating data to generate network. Please wait.')
    }

    # Aggregate the data according to level ----
    if (level == 'Query') {
      agg_data <- lecat_result[,c(1:3,5:ncol(lecat_result))] %>%
        dplyr::group_by(Type, Category, Query) %>%
        dplyr::summarise_all(sum)
    }
    if (level == 'Type') {
      agg_data <-  lecat_result[,c(1,5:ncol(lecat_result))] %>%
        dplyr::group_by(Type) %>%
        dplyr::summarise_all(sum)
    }
    if (level == 'Category') {
      agg_data <- lecat_result[,c(1:2,5:ncol(lecat_result))] %>%
        dplyr::group_by(Type, Category) %>%
        dplyr::summarise_all(sum)
    }

    # Create cooccurence table ----
    number_columns <- as.logical(unlist(lapply(agg_data, FUN = is.numeric)))
    cotable <- NULL
    n <- nrow(agg_data)
    if (inShiny) {
      shiny::withProgress(message = 'Calculating cooccurence', value = 0, {
        for (i in 1:nrow(agg_data)) {
          shiny::incProgress(1/n, detail = paste("node", i))

          # Current node
          # Name
          this_mat_attr <- agg_data[i, !number_columns]
          #Occurence accross stimuli
          that_mat <- as.matrix(agg_data[-i, number_columns])
          # Set rownames
          rownames(that_mat) <- dplyr::pull(agg_data[-i, level])

          # Other nodes
          # Names
          that_mat_attr <- agg_data[-i, !number_columns]
          # Occurences accross stimuli
          this_mat <- as.matrix(agg_data[i, number_columns])
          # Set rownames
          row.names(this_mat) <- dplyr::pull(agg_data[i, level])

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

        # Current node
        # Name
        this_mat_attr <- agg_data[i, !number_columns]
        #Occurence accross stimuli
        that_mat <- as.matrix(agg_data[-i, number_columns])
        # Set rownames
        rownames(that_mat) <- dplyr::pull(agg_data[-i, level])

        # Other nodes
        # Names
        that_mat_attr <- agg_data[-i, !number_columns]
        # Occurences accross stimuli
        this_mat <- as.matrix(agg_data[i, number_columns])
        # Set rownames
        row.names(this_mat) <- dplyr::pull(agg_data[i, level])

        cotable <- rbind(
          cotable,
          create_cotable(this_mat, that_mat, this_mat_attr, that_mat_attr, level)
        )
        utils::setTxtProgressBar(pb, i)
      }
      close(pb)
    }

    # Adding nodes to graph ----
    graph <- igraph::make_empty_graph(directed = FALSE)

    if (inShiny){
      shiny::showNotification('Adding nodes to graph')
    } else {
        message('Adding nodes to graph')
    }

    if (level == 'Query') {
      node_list <- unique(cotable[,c(1:2,5)])
      node_list$id <- 1:nrow(node_list)

      graph <- igraph::add_vertices(
        graph = graph,
        nv = nrow(node_list),
        name = node_list$node1,
        label = node_list$node1,
        Type = node_list$node1.Type,
        Category = node_list$node1.Category
      )

      edge_list <- cotable[cotable$cooccurence > 0,]
    }

    if (level == 'Type') {

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

    }

    if (level == 'Category') {
      node_list <- unique(cotable[,c(1,3)])
      node_list$id <- 1:nrow(node_list)

      graph <- igraph::add_vertices(
        graph = graph,
        nv = nrow(node_list),
        name = node_list$node1,
        label = node_list$node1,
        Type = node_list$node1.Type
      )

    }

    # Add edges to graph, if any
    edge_list <- cotable[cotable$cooccurence > 0,]


    # if there are no edges
    if (nrow(edge_list) < 1) {
      if (inShiny) {
        shiny::showNotification('No edges in graph. There are no cooccurences. Graph will only contain nodes.', type = 'message')
      } else {
        message('No edges in graph. There are no cooccurences. Graph will only contain nodes.')
      }
    }

    # Add edges to graph ----
    if (nrow(edge_list) > 0) {

      if (level == 'Query') {

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

      }

      if (level == 'Type') {
        if (inShiny){

          shiny::withProgress(message = 'Adding edges to graph', value = 0, {
            for (i in 1:nrow(edge_list)) {

              shiny::incProgress(1/n, detail = paste("edge", i))

              # lookup cotable nodes in node list
              id_1 <- node_list$id[node_list$node1 == edge_list$node1[i]]
              id_2 <- node_list$id[node_list$node1 == edge_list$node2[i]]

              # add edge to graph
              graph <- igraph::add_edges(graph = graph,
                                         edges = c(id_1, id_2),
                                         weight = edge_list$cooccurence[i])
            }
          })



        } else {
          message('Adding edges to graph')

          pb <- utils::txtProgressBar(min = 1, max = nrow(edge_list), initial = 1)

          for (i in 1:nrow(edge_list)) {
            # lookup cotable nodes in node list
            id_1 <- node_list$id[node_list$node1 == edge_list$node1[i]]
            id_2 <- node_list$id[node_list$node1 == edge_list$node2[i]]

            # add edge to graph
            graph <- igraph::add_edges(graph = graph,
                                       edges = c(id_1, id_2),
                                       weight = edge_list$cooccurence[i])

            # incriment the progress bar
            utils::setTxtProgressBar(pb, i)
          }

          close(pb)
      }
      }

      if (level == 'Category') {
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
      }

      if (inShiny){ shiny::showNotification('Removing duplicate edges')} else {message('Removing duplicate edges')}
      graph <- igraph::simplify(graph)

      }

      if (!is.null(graph_filename)){
        assertive::assert_is_character(graph_filename)
        if (!inShiny) {
          igraph::write_graph(graph = graph, file = graph_filename, format = 'graphml')
        }
      } else {
        if (inShiny){ shiny::showNotification('No graph filename')} else {message('No graph filename')}
      }

      if (!is.null(cotable_filename)){
        assertive::assert_is_character(cotable_filename)
        if (!inShiny) {
          utils::write.csv(x = cotable, file = cotable_filename, row.names = FALSE)
        }
      } else {
        if (inShiny){ shiny::showNotification('No cotable filename')} else {message('No cotable filename')}
      }
      list(
        cotable = cotable,
        graph = graph
      )
    }
  }
