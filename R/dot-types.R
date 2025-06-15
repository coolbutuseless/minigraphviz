
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Some standard values for various types for different attributes
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dot_types <- list(
  shapes = c(  
    "box",
    "polygon",
    "ellipse",
    "oval",
    "circle",
    "point",
    "egg",
    "triangle",
    "none",
    "plaintext",
    "plain",
    "diamond",
    "trapezium",
    "parallelogram",
    "house",
    "pentagon",
    "hexagon",
    "septagon",
    "octagon",
    "note",
    "tab",
    "folder",
    "box3d",
    "component",
    "cylinder",
    "rect",
    "rectangle",
    "square",
    "star",
    "doublecircle",
    "doubleoctagon",
    "tripleoctagon",
    "invtriangle",
    "invtrapezium",
    "invhouse",
    "underline",
    "Mdiamond",
    "Msquare",
    "Mcircle",
    # biological circuit shapes
    # gene expression symbols
    "promoter",
    "cds",
    "terminator",
    "utr",
    "insulator",
    "ribosite",
    "rnastab",
    "proteasesite",
    "proteinstab",
    # dna construction symbols
    "primersite",
    "restrictionsite",
    "fivepoverhang",
    "threepoverhang",
    "noverhang",
    "assembly",
    "signature",
    "rpromoter",
    "larrow",
    "rarrow",
    "lpromoter"
  ),
  arrowType = c(
    'box', 'crow', 'curve', 'diamond', 'dot', 'icurve', 'inv', 'none', 
    'normal', 'tee', 'vee'
  ),
  arrowType_modifiers = c('l', 'r', 'o'),
  clusterMode = c('local', 'global', 'none'),
  dirType = c('forward', 'back', 'both', 'none'),
  outputMode = c('breadthfirst', 'nodesfirst', 'edgesfirst'),
  packMode = c(
    'node', 'cluster', 'graph', 'array(_flags)?(%d)?'
  ),
  pagedir = c(
    'BL', 'BR', 'TL', 'TR', 'RB', 'RT', 'LB', 'LT'
  ),
  portPos = c('n', 'ne', 'e', 'se', 's', 'sw', 'w', 'nw', 'c', '_'),
  quadType = c('normal', 'fast', 'none'),
  rankdir = c('TB', 'LR', 'BT', 'RL'),
  rankType = c('same', 'min', 'source', 'max', 'sink'),
  rect = "l,b,r,t",
  smoothType = c('none', 'avg_dist', 'graph_dist', 'power_dist', 
                 'rng', 'spring', 'triangle')
)