

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a dot graph
#' 
#' @param ... named attributes
#' @param name graph name. Default: NULL
#' @inheritParams add_graph_attr
#' @return 'dot' object
#' @examples
#' create_graph() |>
#'    add_node('a', color = 'blue', shape = 'rect') |>
#'    add_edge('a', 'b')
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_graph <- function(name = NULL, 
                         area, bb, bgcolor, center, charset, class, clusterrank, 
                         color, colorscheme, comment, compound, concentrate, 
                         Damping, defaultdist, dim, dimen, diredgeconstraints, 
                         dpi, epsilon, esep, fillcolor, fontcolor, fontname, 
                         fontnames, fontpath, fontsize, forcelabels, 
                         gradientangle, href, id, imagepath, inputscale, K, 
                         label, label_scheme, labeljust, labelloc, landscape,
                         layer, layerlistsep, layers, layerselect, layersep, 
                         layout, levels, levelsgap, lheight, lp, lwidth, 
                         margin, maxiter, mclimit, mindist, mode, model,
                         mosek, newrank, nodesep, nojustify, normalize, 
                         notranslate, nslimit, nslimit1, ordering, orientation,
                         outputorder, overlap, overlap_scaling, overlap_shrink, 
                         pack, packmode, pad, page, pagedir, pencolor, penwidth, 
                         peripheries, quadtree, quantum, rankdir, ranksep, 
                         ratio, remincross, repulsiveforce, resolution, 
                         root, rotate, rotation, scale, searchsize, sep, 
                         showboxes, size, smoothing, sortv, splines, start,
                         style, stylesheet, target, tooltip, truecolor, URL,
                         viewport, voro_margin, xdotversion, ...) {
  d <- new.env()
  class(d) <- 'dot'
  
  d$name        <- name
  d$nodes       <- list()  # add_node()
  d$edges       <- list()  # add_edge()
  d$subgraphs   <- list()  # add_subgraph()
  d$is_subgraph <- FALSE
  
  d$node_att    <- list()  # add_node_attr()
  d$edge_att    <- list()  # add_edge_attr()
  
  # Create named list of global graph attributes
  att      <- find_args(...)
  att$name <- NULL
  assert_named(att)
  d$graph_att <- att

    
  d
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add node to a graph, if it already exists, it will be overwritten
#' 
#' @param d dot graph
#' @param name node name
#' @inheritParams add_node_attr
#' @return 'dot' graph
#' @examples
#' create_graph() |>
#'    add_node('a', color = 'blue', shape = 'rect') |>
#'    add_edge('a', 'b')
#' @export 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_node <- function(d, name, 
                     area, class, color, colorscheme, comment, distortion, 
                     fillcolor, fixedsize, fontcolor, fontname, fontsize, 
                     gradientangle, group, height, href, id, image, 
                     imagepos, imagescale, label, labelloc, layer, margin, 
                     nojustify, ordering, orientation, penwidth, 
                     peripheries, pin, pos, rects, regular, root, 
                     samplepoints, shape, shapefile, showboxes, sides, 
                     skew, sortv, style, target, tooltip, URL, vertices, 
                     width, xlabel, xlp, z, ...) {
  assert_dot(d)
  
  # Create named list of attributes
  att      <- find_args(...)
  att$d    <- NULL
  att$name <- NULL
  assert_named(att)
  
  d$nodes[[name]] <- list(name = name, att = att)
  d
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add edge to a graph
#' 
#' @param d dot graph
#' @param a,b starting and ending node names for this edge
#' @inheritParams add_edge_attr
#' @return 'dot' graph
#' @examples
#' create_graph() |>
#'    add_node('a', color = 'blue', shape = 'rect') |>
#'    add_edge('a', 'b')
#' @export 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_edge <- function(d, a, b, 
                     arrowhead, arrowsize, arrowtail, class, color, 
                     colorscheme, comment, constraint, decorate, dir, 
                     edgehref, edgetarget, edgetooltip, edgeURL, fillcolor, 
                     fontcolor, fontname, fontsize, head_lp, headclip, 
                     headhref, headlabel, headport, headtarget, headtooltip, 
                     headURL, href, id, label, labelangle, labeldistance, 
                     labelfloat, labelfontcolor, labelfontname, 
                     labelfontsize, labelhref, labeltarget, labeltooltip,
                     labelURL, layer, len, lhead, lp, ltail, minlen, 
                     nojustify, penwidth, pos, samehead, sametail, 
                     showboxes, style, tail_lp, tailclip, tailhref, 
                     taillabel, tailport, tailtarget, tailtooltip, 
                     tailURL, target, tooltip, URL, weight, xlabel,
                     xlp, ...) {
  assert_dot(d)
  name <- paste(a, b, sep="~")
  
  # Create named list of attributes
  att      <- find_args(...)
  att$d    <- NULL
  att$a    <- NULL
  att$b    <- NULL
  assert_named(att)
  
  d$edges[[name]] <- list(a = a, b = b, att = att)
  d
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add edge to a graph
#' 
#' @param d dot graph
#' @param sg subgraph
#' @return 'dot' graph
#' @examples
#' g <- create_graph()
#' 
#' # First subgraph
#' sg1 <- create_graph('cluster_1') |>
#'   add_node('a', color = 'blue', shape = 'rect') |>
#'   add_edge('a', 'b')
#' 
#' # Second subgraph
#' sg2 <- create_graph('cluster_2') |>
#'   add_node('d', style = 'filled', fillcolor = 'lightblue') |>
#'   add_edge('c', 'd') |>
#'   add_edge('c', 'e') |>
#'   add_edge('c', 'f') |>
#'   add_edge('d', 'g')    
#' 
#' # Add two subgraphs to main graph
#' g <- g |> 
#'   add_subgraph(sg1) |> 
#'   add_subgraph(sg2) |>
#'   add_edge('a', 'd')
#' 
#' g
#' @export 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_subgraph <- function(d, sg) {
  assert_dot(d)
  assert_dot(sg)
  sg$is_subgraph <- TRUE
  d$subgraphs <- append(d$subgraphs, sg)
  d
}


