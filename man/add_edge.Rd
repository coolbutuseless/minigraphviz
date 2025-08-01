% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dot-core.R
\name{add_edge}
\alias{add_edge}
\title{Add edge to a graph}
\usage{
add_edge(
  d,
  a,
  b,
  arrowhead,
  arrowsize,
  arrowtail,
  class,
  color,
  colorscheme,
  comment,
  constraint,
  decorate,
  dir,
  edgehref,
  edgetarget,
  edgetooltip,
  edgeURL,
  fillcolor,
  fontcolor,
  fontname,
  fontsize,
  head_lp,
  headclip,
  headhref,
  headlabel,
  headport,
  headtarget,
  headtooltip,
  headURL,
  href,
  id,
  label,
  labelangle,
  labeldistance,
  labelfloat,
  labelfontcolor,
  labelfontname,
  labelfontsize,
  labelhref,
  labeltarget,
  labeltooltip,
  labelURL,
  layer,
  len,
  lhead,
  lp,
  ltail,
  minlen,
  nojustify,
  penwidth,
  pos,
  samehead,
  sametail,
  showboxes,
  style,
  tail_lp,
  tailclip,
  tailhref,
  taillabel,
  tailport,
  tailtarget,
  tailtooltip,
  tailURL,
  target,
  tooltip,
  URL,
  weight,
  xlabel,
  xlp,
  ...
)
}
\arguments{
\item{d}{dot graph}

\item{a, b}{starting and ending node names for this edge}

\item{arrowhead}{Type: [arrowType]. Default: [normal].

       Style of arrowhead on the head node of an edge.
       This will only appear if the dir attribute
       is "forward" or "both".
       See the limitation.}

\item{arrowsize}{Type: [double]. Default: [1.0].

       Multiplicative scale factor for arrowheads.}

\item{arrowtail}{Type: [arrowType]. Default: [normal].

       Style of arrowhead on the tail node of an edge.
       This will only appear if the dir attribute
       is "back" or "both".
       See the limitation.}

\item{class}{Type: [string]. Default: [""].

       Classnames to attach to the node, edge, graph, or cluster's SVG element.
       Combine with stylesheet for styling SVG output
       using CSS classnames.
       
       Multiple space-separated classes are supported.
       
       
       digraph G \{
         graph [class="cats"];
       
         subgraph cluster_big \{
           graph [class="big_cats"];
       
           "Lion" [class="yellow social"];
           "Snow Leopard" [class="white solitary"];
         \};
       \}}

\item{color}{Type: [color/colorList]. Default: [black].

       Basic drawing color for graphics, not text. For the latter, use the
       fontcolor attribute.
       
       For edges, the value
       can either be a single color or a colorList.
       In the latter case, if colorList has no fractions,
       the edge is drawn using parallel splines or lines,
       one for each color in the list, in the order given.
       The head arrow, if any, is drawn using the first color in the list,
       and the tail arrow, if any, the second color. This supports the common
       case of drawing opposing edges, but using parallel splines instead of
       separately routed multiedges. 
       If any fraction is used, the colors are drawn in series, with each color
       being given roughly its specified fraction of the edge.
       For example, the graph
       
       digraph G \{
         a -> b [dir=both color="red:blue"]
         c -> d [dir=none color="green:red;0.25:blue"]
       \}
       
       yields}

\item{colorscheme}{Type: [string]. Default: [""].

       This attribute specifies a color scheme namespace. If defined, it specifies
       the context for interpreting color names. In particular, if a
       color value has form \code{"xxx"} or \code{"//xxx"},
       then the
       color \code{xxx} will be evaluated according to the current color scheme.
       If no color scheme is set, the standard X11 naming is used.
       For example, if \code{colorscheme=bugn9}, then \code{color=7}
       is interpreted as \code{"/bugn9/7"}.}

\item{comment}{Type: [string]. Default: [""].

       Comments are inserted into output. Device-dependent}

\item{constraint}{Type: [bool]. Default: [true].

       If false, the edge is not used in ranking the nodes. For example,
       in the graph
       
       digraph G \{
         a -> c;
         a -> b;
         b -> c [constraint=false];
       \}
       
       the edge b -> c does not add a constraint during rank
       assignment, so the only constraints are that a be above b and c,
       yielding the graph:}

\item{decorate}{Type: [bool]. Default: [false].

       If true, attach edge label to edge by a 2-segment
       polyline, underlining the label, then going to the closest point of spline.}

\item{dir}{Type: [dirType]. Default: [forward(directed)/none(undirected)].

       Set edge type for drawing arrowheads. This indicates which ends of the
       edge should be decorated with an arrowhead. The actual style of the
       arrowhead can be specified using the arrowhead
       and arrowtail attributes.
       See limitation.}

\item{edgehref}{Type: [escString]. Default: [""].

       Synonym for edgeURL.}

\item{edgetarget}{Type: [escString]. Default: [].

       If the edge has a URL or edgeURL
       attribute, this attribute determines which window of the
       browser is used
       for the URL attached to the non-label part of the edge.
       Setting it to "_graphviz" will open a new window if it
       doesn't already exist, or reuse it if it does.
       If undefined, the value of the target is used.}

\item{edgetooltip}{Type: [escString]. Default: [""].

       Tooltip annotation attached to the non-label part of an edge.
       This is used only if the edge has a URL
       or edgeURL attribute.}

\item{edgeURL}{Type: [escString]. Default: [""].

       If \code{edgeURL} is defined, this is the link used for the non-label
       parts of an edge. This value overrides any URL
       defined for the edge.
       Also, this value is used near the head or tail node unless overridden
       by a headURL or tailURL value,
       respectively.
       See limitation.}

\item{fillcolor}{Type: [color/colorList]. Default: [lightgrey(nodes)/black(clusters)].

       Color used to fill the background of a node or cluster
       assuming style=filled, or a filled arrowhead.
       If \code{fillcolor} is not defined, color is
       used. (For clusters, if \code{color} is not defined,
       bgcolor is used.) If this is not defined,
       the default is used, except for
       \code{shape}=point or when the output
       format is MIF,
       which use black by default.
       
       If the value is a colorList, a gradient fill is
       used. By default, this is a linear fill; setting \code{style=radial} will
       cause a radial fill. At present, only two colors are used. If the second
       color (after a colon) is missing, the default color is used for it.
       See also the gradientangle attribute
       for setting the gradient angle. 
       
       Note that a cluster inherits the root graph's attributes if defined.
       Thus, if the root graph has defined a \code{fillcolor}, this will override a
       \code{color} or \code{bgcolor} attribute set for the cluster.}

\item{fontcolor}{Type: [color]. Default: [black].

       Color used for text.}

\item{fontname}{Type: [string]. Default: ["Times-Roman"].

       Font used for text. This very much depends on the output format and, for
       non-bitmap output such as PostScript or SVG, the availability of the font
       when the graph is displayed or printed. As such, it is best to rely on
       font faces that are generally available, such as Times-Roman, Helvetica or
       Courier.
       
       How font names are resolved also depends on the underlying library that handles
       font name resolution.
       If Graphviz was built using the
       fontconfig library, 
       the latter library will be used to search for the font. 
       See the commands \code{fc-list}, \code{fc-match} and the other fontconfig commands for how
       names are resolved and which fonts are available.
       Other systems may provide their own font package, such as Quartz for OS X.
       
       Note that various font attributes, such as weight and slant, can be built into the
       font name. Unfortunately, the syntax varies depending on which font system is dominant. 
       Thus, using \code{fontname="times bold italic"} will produce a bold, slanted Times font 
       using Pango, the usual main font library. Alternatively,  
       \code{fontname="times:italic"} will produce a slanted
       Times font from fontconfig, while \code{fontname="times-bold"} will resolve to a bold
       Times using Quartz. You will need
       to ascertain which package is used by your Graphviz system and refer to the relevant
       documentation.
       
       If Graphviz is not built with a high-level font library, \code{fontname} will be
       considered the name of a Type 1 or True Type font file.
       If you specify \code{fontname=schlbk}, the tool will look for a
       file named  \code{schlbk.ttf} or \code{schlbk.pfa} or \code{schlbk.pfb}
       in one of the directories specified by
       the fontpath attribute.
       The lookup does support various aliases for the common fonts.}

\item{fontsize}{Type: [double]. Default: [14.0].

       Font size, in points, used for text.}

\item{head_lp}{Type: [point]. Default: [NA].

       Position of an edge's head label, in points.
       The position indicates the center of the label.}

\item{headclip}{Type: [bool]. Default: [true].

       If true, the head of an edge is clipped to the boundary of the head node;
       otherwise, the end of the edge goes to the center of the node, or the
       center of a port, if applicable.}

\item{headhref}{Type: [escString]. Default: [""].

       Synonym for headURL.}

\item{headlabel}{Type: [lblString]. Default: [""].

       Text label to be placed near head of edge.
       See limitation.}

\item{headport}{Type: [portPos]. Default: [center].

       Indicates where on the head node to attach the head of the edge.
       In the default case, the edge is aimed towards the center of the node,
       and then clipped at the node boundary.
       See limitation.}

\item{headtarget}{Type: [escString]. Default: [].

       If the edge has a headURL,
       this attribute determines which window of the
       browser is used
       for the URL. Setting it to "_graphviz" will open a new window if it
       doesn't already exist, or reuse it if it does.
       If undefined, the value of the target is used.}

\item{headtooltip}{Type: [escString]. Default: [""].

       Tooltip annotation attached to the head of an edge. This is used only
       if the edge has a headURL attribute.}

\item{headURL}{Type: [escString]. Default: [""].

       If \code{headURL} is defined, it is
       output as part of the head label of the edge.
       Also, this value is used near the head node, overriding any
       URL value.
       See limitation.}

\item{href}{Type: [escString]. Default: [""].

       Synonym for URL.}

\item{id}{Type: [escString]. Default: [""].

       Allows the graph author to provide an id for graph objects which is to be included in the output.
       Normal "&#92;N", "&#92;E", "&#92;G" substitutions are applied.
       If provided, it is the responsibility of the provider to keep
       its values sufficiently unique for its intended downstream use.
       Note, in particular, that "&#92;E" does not provide a unique id for multi-edges.
       If no id attribute is provided, then a unique internal id is used. However, 
       this value is unpredictable by the graph writer.
       An externally provided id is not used internally.
       
       If the graph provides an id attribute, this will be used as a prefix for
       internally generated attributes. By making these distinct, the user
       can include multiple image maps in the same document.}

\item{label}{Type: [lblString]. Default: [].

       Text label attached to objects.
       If a node's shape is record, then the label can
       have a special format
       which describes the record layout.
       
       Note that a node's default label is "", so the node's name or ID becomes
       its label. Technically, a node's name can be an HTML string but this will not
       mean that the node's label will be interpreted as an HTML-like label. This is
       because the node's actual label is an ordinary string, which will be replaced 
       by the raw bytes stored in the node's name.
       To get an HTML-like label, the label attribute value itself must be an HTML string.}

\item{labelangle}{Type: [double]. Default: [-25.0].

       This, along with labeldistance, determine
       where the
       headlabel (taillabel) are placed with respect to the head (tail)
       in polar coordinates. The origin in the coordinate system is
       the point where the edge touches the node. The ray of 0 degrees
       goes from the origin back along the edge, parallel to the edge
       at the origin.
       
       The angle, in degrees, specifies the rotation from the 0 degree ray,
       with positive angles moving counterclockwise and negative angles
       moving clockwise.}

\item{labeldistance}{Type: [double]. Default: [1.0].

       Multiplicative scaling factor adjusting the distance that
       the headlabel(taillabel) is from the head(tail) node.
       The default distance is 10 points. See labelangle
       for more details.}

\item{labelfloat}{Type: [bool]. Default: [false].

       If true, allows edge labels to be less constrained in position.
       In particular, it may appear on top of other edges.}

\item{labelfontcolor}{Type: [color]. Default: [black].

       Color used for headlabel and taillabel.
       If not set, defaults to edge's fontcolor.}

\item{labelfontname}{Type: [string]. Default: ["Times-Roman"].

       Font used for headlabel and taillabel.
       If not set, defaults to edge's fontname.}

\item{labelfontsize}{Type: [double]. Default: [14.0].

       Font size, in points, used for headlabel and taillabel.
       If not set, defaults to edge's fontsize.}

\item{labelhref}{Type: [escString]. Default: [""].

       Synonym for labelURL.}

\item{labeltarget}{Type: [escString]. Default: [].

       If the edge has a URL or labelURL
       attribute, this attribute determines which window of the
       browser is used
       for the URL attached to the label.
       Setting it to "_graphviz" will open a new window if it
       doesn't already exist, or reuse it if it does.
       If undefined, the value of the target is used.}

\item{labeltooltip}{Type: [escString]. Default: [""].

       Tooltip annotation attached to label of an edge.
       This is used only if the edge has a URL
       or labelURL attribute.}

\item{labelURL}{Type: [escString]. Default: [""].

       If \code{labelURL} is defined, this is the link used for the label
       of an edge. This value overrides any URL
       defined for the edge.}

\item{layer}{Type: [layerRange]. Default: [""].

       Specifies layers in which the node, edge or cluster is present.}

\item{len}{Type: [double]. Default: [1.0(neato)/0.3(fdp)].

       Preferred edge length, in inches.}

\item{lhead}{Type: [string]. Default: [""].

       Logical head of an edge. When \code{compound} is true,
       if \code{lhead} is defined and is the name of a cluster containing
       the real head,
       the edge is clipped to the boundary of the cluster.
       See limitation.}

\item{lp}{Type: [point]. Default: [NA].

       Label position, in points.
       The position indicates the center of the label.}

\item{ltail}{Type: [string]. Default: [""].

       Logical tail of an edge. When \code{compound} is true,
       if \code{ltail} is defined and is the name of a cluster
       containing the real tail,
       the edge is clipped to the boundary of the cluster.
       See limitation.}

\item{minlen}{Type: [int]. Default: [1].

       Minimum edge length (rank difference between head and tail).}

\item{nojustify}{Type: [bool]. Default: [false].

       By default, the justification of multi-line labels is done within the
       largest context that makes sense. Thus, in the label of a polygonal
       node, a left-justified line will align with the left side of the node
       (shifted by the prescribed margin).
       In record nodes, left-justified
       line will line up with the left side of the enclosing column of fields.
       If \code{nojustify} is \code{"true"}, multi-line labels will be justified
       in the context of itself. For example, if the attribute is set,
       the first label line is long, and the second is shorter and left-justified,
       the second will align with the left-most character in the first line,
       regardless of  how large the node might be.}

\item{penwidth}{Type: [double]. Default: [1.0].

       Specifies the width of the pen, in points, used to draw lines and curves,
       including the boundaries of edges and clusters. The value is inherited
       by subclusters.
       It has no effect on text.
       
       Previous to 31 January 2008, the effect of \code{penwidth=W}
       was achieved by including \code{setlinewidth(W)}
       as part of a \code{style} specification.
       If both are used, \code{penwidth} will be used.}

\item{pos}{Type: [point/splineType]. Default: [NA].

       Position of node, or spline control points.
       For nodes, the position indicates the center of the node.
       On output, the coordinates are in points.
       
       In neato and fdp, pos can be used to set the initial position of a node.
       By default, the coordinates are assumed to be in inches. However, the
       -s command line flag can be used to specify
       different units. As the output coordinates are in points, 
       feeding the output of a graph laid out by a Graphviz program into
       neato or fdp will almost always require the -s flag.
       
       When the -n command line flag is used with
       neato, it is assumed the positions have been set by one of the layout
       programs, and are therefore in points. Thus, \code{neato -n} can accept
       input correctly without requiring a \code{-s} flag and, in fact,
       ignores any such flag.}

\item{samehead}{Type: [string]. Default: [""].

       Edges with the same head and the same \code{samehead} value are aimed
       at the same point on the head. This has no effect on loops.
       Each node can have at most 5 unique samehead values.
       See limitation.}

\item{sametail}{Type: [string]. Default: [""].

       Edges with the same tail and the same \code{sametail} value are aimed
       at the same point on the tail. This has no effect on loops.
       Each node can have at most 5 unique sametail values.
       See limitation.}

\item{showboxes}{Type: [int]. Default: [0].

       Print guide boxes in PostScript at the beginning of
       routesplines if 1, or at the end if 2. (Debugging, TB mode only!)}

\item{style}{Type: [style]. Default: [""].

       Set style information for components of the graph. For cluster subgraphs, if \code{style="filled"}, the
       cluster box's background is filled.
       
       If the default style attribute has been set for a component, an individual component can use
       \code{style=""} to revert to the normal default. For example, if the graph has
       
       \code{edge [style="invis"]}
       
       making all edges invisible, a specific edge can overrride this via:
       
       \code{a -> b [style=""]}
       
       Of course, the component can also explicitly set its \code{style} attribute to the desired value.}

\item{tail_lp}{Type: [point]. Default: [NA].

       Position of an edge's tail label, in points.
       The position indicates the center of the label.}

\item{tailclip}{Type: [bool]. Default: [true].

       If true, the tail of an edge is clipped to the boundary of the tail node;
       otherwise, the end of the edge goes to the center of the node, or the
       center of a port, if applicable.}

\item{tailhref}{Type: [escString]. Default: [""].

       Synonym for tailURL.}

\item{taillabel}{Type: [lblString]. Default: [""].

       Text label to be placed near tail of edge.
       See limitation.}

\item{tailport}{Type: [portPos]. Default: [center].

       Indicates where on the tail node to attach the tail of the edge.
       See limitation.}

\item{tailtarget}{Type: [escString]. Default: [].

       If the edge has a tailURL,
       this attribute determines which window of the
       browser is used
       for the URL. Setting it to "_graphviz" will open a new window if it
       doesn't already exist, or reuse it if it does.
       If undefined, the value of the target is used.}

\item{tailtooltip}{Type: [escString]. Default: [""].

       Tooltip annotation attached to the tail of an edge. This is used only
       if the edge has a tailURL attribute.}

\item{tailURL}{Type: [escString]. Default: [""].

       If \code{tailURL} is defined, it is
       output as part of the tail label of the edge.
       Also, this value is used near the tail node, overriding any
       URL value.
       See limitation.}

\item{target}{Type: [escString/string]. Default: [].

       If the object has a URL, this attribute determines which window
       of the browser is used for the URL.
       See W3C documentation.}

\item{tooltip}{Type: [escString]. Default: [""].

       Tooltip annotation attached to the node or edge. If unset, Graphviz
       will use the object's label if defined.
       Note that if the label is a record specification or an HTML-like
       label, the resulting tooltip may be unhelpful. In this case, if
       tooltips will be generated, the user should set a \code{tooltip}
       attribute explicitly.}

\item{URL}{Type: [escString]. Default: [].

       Hyperlinks incorporated into device-dependent output.
       At present, used in ps2, cmap, i*map and svg formats.
       For all these formats, URLs can be attached to nodes, edges and
       clusters. URL attributes can also be attached to the root graph in ps2,
       cmap and i*map formats. This serves as the base URL for relative URLs in the
       former, and as the default image map file in the latter.
       
       For svg, cmapx and imap output, the active area for a node is its
       visible image.
       For example, an unfilled
       node with no drawn boundary will only be active on its label.
       For other output, the active area is its bounding box.
       The active area for a cluster is its bounding box.
       For edges, the active areas are small circles where the edge contacts its head
       and tail nodes. In addition, for svg, cmapx and imap, the active area
       includes a thin polygon approximating the edge. The circles may
       overlap the related node, and the edge URL dominates.
       If the edge has a label, this will also be active.
       Finally, if the edge has a head or tail label, this will also be active.
       
       Note that, for edges, the attributes headURL,
       tailURL, labelURL and
       edgeURL allow control of various parts of an
       edge.
       Also note that, if active areas of two edges overlap, it is unspecified
       which area dominates.}

\item{weight}{Type: [int/double]. Default: [1].

       Weight of edge. In dot, the heavier the weight, the shorter,
       straighter and more vertical the edge is.
       \code{N.B.} Weights in dot must be integers.
       For twopi, a weight of 0 indicates the edge should not be used in constructing a
       spanning tree from the root.
       For other layouts, a larger weight encourages the layout to 
       make the edge length closer to that specified by the 
       len attribute.}

\item{xlabel}{Type: [lblString]. Default: [""].

       External label for a node or edge. For nodes, the label will be placed outside of the
       node but near it. For edges, the label will be placed near the center of the edge.
       This can be useful in dot to avoid the occasional problem when the use of edge labels
       distorts the layout. For other layouts, the xlabel attribute can be viewed as a synonym
       for the \code{label} attribute.
       
       These labels are added after all nodes and edges have been placed. The labels will be placed
       so that they do not overlap any node or label. This means it may not be possible to place all
       of them. To force placing all of them, use the  \code{forcelabels} attribute.}

\item{xlp}{Type: [point]. Default: [NA].

       Position of an exterior label, in points.
       The position indicates the center of the label.}

\item{...}{attributes}
}
\value{
'dot' graph
}
\description{
Add edge to a graph
}
\examples{
create_graph() |>
   add_node('a', color = 'blue', shape = 'rect') |>
   add_edge('a', 'b')
}
