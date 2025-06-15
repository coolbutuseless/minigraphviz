

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Conditionally add double quotes if there is a space or a non-word
#' character in a string
#'
#' @param x character vector
#' @return any strings with spaces or non-word characters gets double-quoted
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cquote <- function(x) {
  ifelse(grepl("[^\\w]", x, perl = TRUE), dQuote(x, FALSE), x)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
graph_att_to_txt <- function(att) {
  if (length(att) == 0) return(NULL)
  paste(cquote(names(att)), cquote(unname(att)), sep = "=")
}


# per-node and per-edge attributes
att_to_txt <- function(att) {
  if (length(att) == 0) return(NULL)
  txt <- paste(cquote(names(att)), cquote(unname(att)), sep = "=", collapse = ", ")
  txt <- paste0("[", txt, "]")
  txt
}


# global node attribute to text
node_att_to_txt <- function(att) {
  if (length(att) == 0) return(NULL)
  txt <- att_to_txt(att)
  paste("node", txt)
}

# global edge attribute to text
edge_att_to_txt <- function(att) {
  if (length(att) == 0) return(NULL)
  txt <- att_to_txt(att)
  paste("edge", txt)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
node_to_txt <- function(node) {
  if (length(node$att) == 0) return(NULL)
  txt <- cquote(node$name)
  att <- att_to_txt(node$att)
  txt <- paste(c(txt, att), collapse = " ")
  txt
}

nodes_to_txt <- function(nodes) {
  ll <- lapply(nodes, node_to_txt)
  ll <- Filter(Negate(is.null), ll)
  ll
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
edge_to_txt <- function(edge) {
  txt <- paste(cquote(edge$a), "->", cquote(edge$b))
  att <- att_to_txt(edge$att)
  txt <- paste(c(txt, att), collapse = " ")
  txt
}


edges_to_txt <- function(edges) {
  ll <- lapply(edges, edge_to_txt)
  ll <- Filter(Negate(is.null), ll)
  ll
}


subgraphs_to_txt <- function(sgs) {
  if (length(sgs) == 0) return(NULL)
  
  txt <- lapply(sgs, as.character)
  unlist(txt)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' convert a dot graph to character
#' 
#' @param x dot graph
#' @param ...  ignored
#' @return character string
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.dot <- function(x, ...) {
  
  if (x$is_subgraph) {
    type <- 'subgraph'
  } else {
    type <- 'digraph'
  }
  
  bits <- c(
    graph_att_to_txt(x$graph_att),
    node_att_to_txt(x$node_att),
    edge_att_to_txt(x$edge_att),
    nodes_to_txt(x$nodes),
    edges_to_txt(x$edges),
    subgraphs_to_txt(x$subgraphs)
  )
  
  bits <- paste(" ", bits)
  
  bits <- c(
    paste(type, x$name, "{"),
    bits, 
    "}"
  )
  
  if (x$is_subgraph) {
    return(bits)
  }
  
  bits <- c(bits, "")
  paste(bits, collapse = "\n")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print dot graph to console
#' @param x dot graph
#' @param ... ignored
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.dot <- function(x, ...) {
  cat(as.character(x, ...))
}




if (FALSE) {
  
  d <- create_graph(color = 'blue') |>
    add_node('a', color = 'green') |>
    add_edge('a', 'b', arrowhead = 'diamond')
  
  d
  
}
