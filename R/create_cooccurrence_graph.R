#' Create a graph representation of the cooccurences of Categories
#'
#' Creates a cooccurrence graph
#' @description Calculates the cooccurrence of Categories.
#' A graph object is then created using the \link[igraph]{make_graph} function for
#' visualising in Gephi or similiar software.
#'
#' @param result result from the \link[lecat]{run_lecat_analysis} function
#' @param graph_filename filename of the generated graph file
#' @param cotable_filename filename of the generated cooccurrence table
# @param level Level of cooccurence. Either 'Query', 'Category' or 'Type'
#' @param inShiny If inShiny is TRUE then shiny based notifications will be shown
#' @return cooccurence table (At present.)
#'
#create_cooccurrence_graph <- function(lecat_result, graph_filename = 'result.graphml', cotable_filename = 'cotable.csv', level = 'Query', inShiny = FALSE) {
create_cooccurrence_graph <- function(result, graph_filename = 'result.graphml', cotable_filename = 'cotable.csv',  inShiny = FALSE) {

  # Trim matrix by removing the 0 columns
  frequencies <- result[,5:ncol(result)]
  non_zero <- frequencies[, which(colSums(frequencies) != 0)]
  non_zero_result <- cbind(result[,1:4], non_zero)

  # compress rows by category
  result <- data.frame()
  counts_matrix <-
    matrix(nrow = length(unique(non_zero_result$Category)), ncol = length(5:ncol(non_zero_result)))

  # run through each category
  for (cat_idx in 1:length(unique(non_zero_result$Category))) {

    category <- unique(non_zero_result$Category)[cat_idx]

    message(paste('Current category:', category))
    this_data <- non_zero_result[non_zero_result$Category == category,]

    if (ncol(this_data) == 5){
      counts_matrix[cat_idx,] <- matrix(ncol = 1, data = sum(this_data[,5]))
    } else {
      counts_matrix[cat_idx,] <- colSums(this_data[,5:ncol(this_data)])
    }

  }

  # NOT WORKING: remove non-zero columns from counts matrix
  # If only one non_zero category then cannot set rownames
  #non_zero_counts <- counts_matrix[, which(colSums(counts_matrix) != 0)]
  non_zero_counts <- counts_matrix

  rownames(non_zero_counts) <- unique(non_zero_result$Category)

  # create co-occurrence table
  cotable <-
    tidyr::crossing(
      node1 = unique(non_zero_result$Category),
      node2 = unique(non_zero_result$Category)
    )

  cotable$n <- NaN

  for (i in 1:nrow(cotable)) { # for each category

    # Do not calculate cooccurence for same categories

    if (cotable$node1[i] != cotable$node2[i]) {

      notification <- paste('Calculating ', cotable$node1[i], ' vs ', cotable$node2[i])

      if (inShiny){ shiny::showNotification(notification)} else {notification}

      cat1_idx <- which(rownames(non_zero_counts) == cotable$node1[i])
      cat2_idx <- which(rownames(non_zero_counts) == cotable$node2[i])

      # calculate cooccurence
      cat1 <- non_zero_counts[cat1_idx,]
      cat2 <- non_zero_counts[cat2_idx,]

      cooc_present_idx <- as.logical((cat1 > 0) * (cat2 > 0) > 0)

      # The results of this are not correct for self category calculations
      # So we don't look at those (see if statement above)
      n_cooc_sample <-
        apply(rbind(cat1[cooc_present_idx], cat2[cooc_present_idx]), FUN = min, MARGIN = 2)

      # prod might inflate observations
      #n_cooc_sample <-
      #  apply(rbind(cat1[cooc_present_idx], cat2[cooc_present_idx]), FUN = prod, MARGIN = 2)

      n_cooc <- sum(n_cooc_sample)

      cotable$n[i] <- n_cooc
    }
  }

  cotable

  # add types back in
  cotable$node1.Type <- NaN
  cotable$node2.Type <- NaN

  for (i in 1:nrow(cotable)) {

    cotable$node1.Type[i] <-
      unique(non_zero_result$Type[non_zero_result$Category == cotable$node1[i]])
    cotable$node2.Type[i] <-
      unique(non_zero_result$Type[non_zero_result$Category == cotable$node2[i]])

  }

  cotable$Weight <- cotable$n

  graph <- igraph::make_empty_graph()
  graph <-
    igraph::graph_from_data_frame(d = cotable[, c('node1', 'node2', 'Weight')],
                                  vertices = unique(c(cotable$node1, cotable$node2)),
                                  directed = FALSE)

  if (inShiny){ shiny::showNotification('Removing duplicate edges')} else {message('Removing duplicate edges')}

  graph <- igraph::simplify(graph)

  if (!is.null(graph_filename)){
    if (!inShiny) {
      if(!exists('do_not_write_files')){
        igraph::write_graph(graph = graph, file = graph_filename, format = 'graphml')
      }
    }
  } else {
    if (inShiny){ shiny::showNotification('No graph filename')} else {message('No graph filename')}
  }

  if (!is.null(cotable_filename)){
    if (!inShiny) {
      if(!exists('do_not_write_files')){
        utils::write.csv(x = cotable, file = cotable_filename, row.names = FALSE)
      }
    }
  } else {
    if (inShiny){ shiny::showNotification('No cotable filename')} else {message('No cotable filename')}
  }
  list(
    cotable = cotable,
    graph = graph
  )
}
