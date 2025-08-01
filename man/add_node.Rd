% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dot-core.R
\name{add_node}
\alias{add_node}
\title{Add node to a graph, if it already exists, it will be overwritten}
\usage{
add_node(
  d,
  name,
  area,
  class,
  color,
  colorscheme,
  comment,
  distortion,
  fillcolor,
  fixedsize,
  fontcolor,
  fontname,
  fontsize,
  gradientangle,
  group,
  height,
  href,
  id,
  image,
  imagepos,
  imagescale,
  label,
  labelloc,
  layer,
  margin,
  nojustify,
  ordering,
  orientation,
  penwidth,
  peripheries,
  pin,
  pos,
  rects,
  regular,
  root,
  samplepoints,
  shape,
  shapefile,
  showboxes,
  sides,
  skew,
  sortv,
  style,
  target,
  tooltip,
  URL,
  vertices,
  width,
  xlabel,
  xlp,
  z,
  ...
)
}
\arguments{
\item{d}{dot graph}

\item{name}{node name}

\item{area}{Type: [double]. Default: [1.0].

       Indicates the preferred area for a node or empty cluster when laid out by patchwork.}

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

\item{distortion}{Type: [double]. Default: [0.0].

       Distortion factor for \code{shape}=polygon.
       Positive values cause top part to
       be larger than bottom; negative values do the opposite.}

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

\item{fixedsize}{Type: [bool/string]. Default: [false].

       If false, the size of a node is determined by smallest width and height
       needed to contain its label and image, if any, with a margin specified by
       the \code{margin} attribute. The width
       and height must also be at least as large as the sizes specified by the
       \code{width} and
       \code{height} attributes, which specify
       the minimum values for these parameters. 
       
       If true, the node size is specified by the values of the
       \code{width}
       and \code{height} attributes only
       and is not expanded to contain the text label.
       There will be a warning if the label (with margin)
       cannot fit within these limits.
       
       If the \code{fixedsize} attribute is set 
       to shape, the \code{width} and
       \code{height} attributes also determine the size
       of the node shape, but the label can be much larger. Both the label and
       shape sizes are used when avoiding node overlap, but all edges to the
       node ignore the label and only contact the node shape. No warning is given
       if the label is too large.}

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

\item{gradientangle}{Type: [int]. Default: [""].

       If a gradient fill is being used, this determines the angle of the fill. For linear fills, the colors transform
       along a line specified by the angle and the center of the object. For radial fills, a value of zero causes the
       colors to transform radially from the center; for non-zero values, the colors transform from a point near the
       object's periphery as specified by the value.
       
       If unset, the default angle is 0.}

\item{group}{Type: [string]. Default: [""].

       If the end points of an edge belong to the same group, i.e., have the
       same group attribute, parameters are set to avoid crossings and keep
       the edges straight.}

\item{height}{Type: [double]. Default: [0.5].

       Height of node, in inches. This is taken as the initial, minimum height
       of the node. If \code{fixedsize} is true, this
       will be the final height of the node. Otherwise, if the node label
       requires more height to fit, the node's height will be increased to
       contain the label. Note also that, if the output format is dot, the
       value given to \code{height} will be the final value.
       
       If the node shape is regular, the width and height are made identical.
       In this case, if either the width or the height is set explicitly,
       that value is used.
       In this case, if both the width or the height are set explicitly,
       the maximum of the two values is used.
       If neither is set explicitly, the minimum of the two default values
       is used.}

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

\item{image}{Type: [string]. Default: [""].

       Gives the name of a file containing an image to be displayed inside
       a node. The image file must be in one of the recognized 
       formats,
       typically JPEG, PNG, GIF, BMP, SVG or Postscript, and be able to be converted
       into the desired output format.
       
       The file must contain the image size information. This is usually trivially
       true for the bitmap formats. For PostScript, the file must contain a
       line starting with \code{BoundingBox: } followed by four integers
       specifying the lower left x and y coordinates and the upper right x and y
       coordinates of the bounding box for the image, the coordinates being in
       points. An SVG image file must contain \code{width} and \code{height}
       attributes, typically as part of the \code{svg} element.
       The values for these should have the form of a floating point number,
       followed by optional units, e.g., \code{width="76pt"}. 
       Recognized units are \code{in}, \code{px},
       \code{pc}, \code{pt}, \code{cm} and \code{mm} for inches, pixels,
       picas, points, centimeters and millimeters, respectively.
       The default unit is points.
       
       Unlike with the shapefile attribute,
       the image is treated as node
       content rather than the entire node. In particular, an image can
       be contained in a node of any shape, not just a rectangle.}

\item{imagepos}{Type: [string]. Default: ["mc"].

       Attribute controlling how an image is positioned within its containing node.  This
       only has an effect when the image is smaller than the containing node.  The
       default is to be centered both horizontally and vertically.  Valid values:
       
       tlTop Left
       tcTop Centered
       trTop Right
       mlMiddle Left
       mcMiddle Centered (the default)
       mrMiddle Right
       blBottom Left
       bcBottom Centered
       brBottom Right}

\item{imagescale}{Type: [bool/string]. Default: [false].

       Attribute controlling how an image fills its
       containing node. In general, the image is given its natural size,
       (cf. dpi),
       and the node size is made large enough to contain its image, its
       label, its margin, and its peripheries.
       Its width and height will also be at least as large as its
       minimum width and height.
       If, however, \code{fixedsize=true},
       the width and height attributes specify the exact size of the node.
       
       During rendering, in the default case (\code{imagescale=false}),
       the image retains its natural size.
       If \code{imagescale=true},
       the image is uniformly scaled (i.e., its aspect ratio is
       preserved) to fit inside the node.
       At least one dimension of the image will be as large as possible
       given the size of the node.
       When \code{imagescale=width},
       the width of the image is scaled to fill the node width.
       The corresponding property holds when \code{imagescale=height}.
       When \code{imagescale=both},
       both the height and the width are scaled separately to fill the node.
       
       In all cases, if a dimension of the image is larger than the
       corresponding dimension of the node, that dimension of the
       image is scaled down to fit the node. As with the case of
       expansion, if  \code{imagescale=true}, width and height are
       scaled uniformly.}

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

\item{labelloc}{Type: [string]. Default: ["t"(clusters)/"b"(root graphs)/"c"(nodes)].

       Vertical placement of labels for nodes, root graphs and clusters.
       
       For graphs and clusters, only "t" and "b" are allowed, corresponding
       to placement at the top and bottom, respectively.
       By default, root
       graph labels go on the bottom and cluster labels go on the top.
       Note that a subgraph inherits attributes from its parent. Thus, if
       the root graph sets \code{labelloc} to "b", the subgraph inherits
       this value.
       
       For nodes, this attribute is used only when the height of the node
       is larger than the height of its label. 
       If \code{labelloc} is set to "t", "c", or "b", the label is aligned
       with the top, centered, or aligned with the bottom of the node, respectively.
       In the default case, the label is vertically centered.}

\item{layer}{Type: [layerRange]. Default: [""].

       Specifies layers in which the node, edge or cluster is present.}

\item{margin}{Type: [double/point]. Default: [].

       For graphs, this sets x and y margins of canvas, in inches. If the margin
       is a single double, both margins are set equal to the given value.
       
       Note that the margin is not part of the drawing but just empty space
       left around the drawing. It basically corresponds to a translation of
       drawing, as would be necessary to center a drawing on a page. Nothing
       is actually drawn in the margin. To actually extend the background of
       a drawing, see the pad attribute.
       
       For clusters, this specifies the space between the nodes in the cluster and
       the cluster bounding box. By default, this is 8 points.
       
       For nodes, this attribute specifies space left around the node's label.
       By default, the value is \code{0.11,0.055}.}

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

\item{ordering}{Type: [string]. Default: [""].

       If the value of the attribute is "out", then
       the outedges of a node, that is, edges with the node as its tail node,
       must appear
       left-to-right in the same order in which they are defined in
       the input.
       If the value of the attribute is "in", then
       the inedges of a node must appear
       left-to-right in the same order in which they are defined in
       the input.
       If defined as a graph or subgraph attribute, the value is applied to all nodes
       in the graph or subgraph. Note that the graph attribute takes
       precedence over the node attribute.}

\item{orientation}{Type: [double/string]. Default: [0.0/""].

       When used on nodes: Angle, in degrees, used to rotate polygon node shapes. For any number of polygon sides, 0 degrees rotation results in a flat base.
       
       When used on graphs: If "[lL]*", set graph orientation to landscape.
       Used only if \code{rotate} is not defined.}

\item{penwidth}{Type: [double]. Default: [1.0].

       Specifies the width of the pen, in points, used to draw lines and curves,
       including the boundaries of edges and clusters. The value is inherited
       by subclusters.
       It has no effect on text.
       
       Previous to 31 January 2008, the effect of \code{penwidth=W}
       was achieved by including \code{setlinewidth(W)}
       as part of a \code{style} specification.
       If both are used, \code{penwidth} will be used.}

\item{peripheries}{Type: [int]. Default: [shape default(nodes)/1(clusters)].

       Set number of peripheries used in polygonal shapes and cluster
       boundaries. Note that
       user-defined shapes are treated as a
       form of box shape, so the default
       peripheries value is 1 and the user-defined shape will be drawn in
       a bounding rectangle. Setting \code{peripheries=0} will turn this off.
       Also, 1 is the maximum peripheries value for clusters.}

\item{pin}{Type: [bool]. Default: [false].

       If true and the node has a pos attribute on input, neato or fdp prevents the
       node from moving from the input position. This property can also be specified
       in the pos attribute itself (cf. the point type).
       
       \code{Note:} Due to an artifact of the implementation, 
       previous to 27 Feb 2014, final coordinates
       are translated to the origin. Thus, if you look at the output coordinates
       given in the (x)dot or plain format, pinned nodes will not have the same
       output coordinates as were given on input. If this is important, a
       simple workaround is to maintain the coordinates of a pinned node. The vector
       difference between the old and new coordinates will give the translation,
       which can then be subtracted from all of the appropriate coordinates.
       
       After 27 Feb 2014, this translation can be avoided in neato by setting the 
       notranslate to TRUE. However, if the graph
       specifies node overlap removal or a change in 
       aspect ratio, node coordinates may still change.}

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

\item{rects}{Type: [rect]. Default: [NA].

       Rectangles for fields of records, in points.}

\item{regular}{Type: [bool]. Default: [false].

       If true, force polygon to be regular, i.e., the vertices of the
       polygon will lie on a circle whose center is the center of the node.}

\item{root}{Type: [string/bool]. Default: [(graphs)/false(nodes)].

       This specifies nodes to be used as the center of the
       layout and the root of the generated spanning tree. As a graph attribute,
       this gives the name of the node. As a node attribute, it
       specifies that the node should be used as a central node. In twopi,
       this will actually be the central node. In circo, the block containing
       the node will be central in the drawing of its connected component.
       If not defined,
       twopi will pick a most central node, and circo will pick a random node.
       
       If the root attribute is defined as the empty string, twopi will reset it
       to name of the node picked as the root node.
       
       For twopi, it is possible to have multiple roots, presumably one for each
       component. If more than one node in a component is marked as the root, twopi
       will pick one.}

\item{samplepoints}{Type: [int]. Default: [8(output)/20(overlap and image maps)].

       If the input graph defines the \code{vertices}
       attribute, and output is dot or xdot, this gives
       the number of points used for a node whose shape is a circle or ellipse.
       It plays the same role in neato, when adjusting the layout to avoid
       overlapping nodes, and in image maps.}

\item{shape}{Type: [shape]. Default: [ellipse].

       Set the shape of a node.}

\item{shapefile}{Type: [string]. Default: [""].

       (Deprecated) If defined, shapefile specifies a file containing user-supplied node content.
       The \code{shape} of the node is set to box.
       The image in the shapefile must be
       rectangular. The image formats supported as well as the precise semantics of
       how the file is used depends on the
       output format.  For further details, see
       Image Formats and
       External PostScript files.
       
       There is one exception to this usage.
       If \code{shape} is set to "epsf", shapefile gives
       a filename containing a definition of the node in PostScript.
       The graphics defined must be contain all of the
       node content, including any desired boundaries.
       For further details, see
       
       External PostScript files.}

\item{showboxes}{Type: [int]. Default: [0].

       Print guide boxes in PostScript at the beginning of
       routesplines if 1, or at the end if 2. (Debugging, TB mode only!)}

\item{sides}{Type: [int]. Default: [4].

       Number of sides if \code{shape}=polygon.}

\item{skew}{Type: [double]. Default: [0.0].

       Skew factor for \code{shape}=polygon. Positive values
       skew top of polygon to right; negative to left.}

\item{sortv}{Type: [int]. Default: [0].

       If packmode indicates an array packing, 
       this attribute specifies an
       insertion order among the components, with smaller values inserted first.}

\item{style}{Type: [style]. Default: [""].

       Set style information for components of the graph. For cluster subgraphs, if \code{style="filled"}, the
       cluster box's background is filled.
       
       If the default style attribute has been set for a component, an individual component can use
       \code{style=""} to revert to the normal default. For example, if the graph has
       
       \code{edge [style="invis"]}
       
       making all edges invisible, a specific edge can overrride this via:
       
       \code{a -> b [style=""]}
       
       Of course, the component can also explicitly set its \code{style} attribute to the desired value.}

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

\item{vertices}{Type: [pointList]. Default: [NA].

       If the input graph defines this attribute, the node is polygonal,
       and output is dot or xdot, this attribute provides the
       coordinates of the vertices of the node's polygon, in inches.
       If the node is an ellipse or circle, the
       samplepoints attribute affects
       the output.}

\item{width}{Type: [double]. Default: [0.75].

       Width of node, in inches. This is taken as the initial, minimum width
       of the node. If \code{fixedsize} is true, this
       will be the final width of the node. Otherwise, if the node label
       requires more width to fit, the node's width will be increased to
       contain the label. Note also that, if the output format is dot, the
       value given to \code{width} will be the final value.
       
       If the node shape is regular, the width and height are made identical.
       In this case, if either the width or the height is set explicitly,
       that value is used.
       In this case, if both the width or the height are set explicitly,
       the maximum of the two values is used.
       If neither is set explicitly, the minimum of the two default values
       is used.}

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

\item{z}{Type: [double]. Default: [0.0].

       \code{Deprecated:}Use \code{pos} attribute, along
       with \code{dimen} and/or \code{dim}
       to specify dimensions.
       
       Provides z coordinate value for 3D layouts and displays. If the
       graph has \code{dim} set to 3 (or more),
       neato will use a node's \code{z} value
       for the z coordinate of its initial position if
       its \code{pos} attribute is also defined.
       
       Even if no \code{z} values are specified in the input, it is necessary to
       declare a \code{z} attribute for nodes, e.g, using \code{node[z=""]}
       in order to get z values on output.
       Thus, setting \code{dim=3} but not declaring \code{z} will
       cause \code{neato -Tvrml} to
       layout the graph in 3D but project the layout onto the xy-plane
       for the rendering. If the \code{z} attribute is declared, the final rendering
       will be in 3D.}

\item{...}{attributes}
}
\value{
'dot' graph
}
\description{
Add node to a graph, if it already exists, it will be overwritten
}
\examples{
create_graph() |>
   add_node('a', color = 'blue', shape = 'rect') |>
   add_edge('a', 'b')
}
