
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add a global attribute for the graph or all nodes or all edges
#' 
#' @param d dot graph
#' @param ... attributes
#' @param Damping Type: [double]. Default: [0.99].
#'
#'        Factor damping force motions. On each iteration, a nodes movement
#'        is limited to this factor of its potential motion. By being less than
#'        1.0, the system tends to ``cool'', thereby preventing cycling.
#'
#' @param URL Type: [escString]. Default: [].
#'
#'        Hyperlinks incorporated into device-dependent output.
#'        At present, used in ps2, cmap, i*map and svg formats.
#'        For all these formats, URLs can be attached to nodes, edges and
#'        clusters. URL attributes can also be attached to the root graph in ps2,
#'        cmap and i*map formats. This serves as the base URL for relative URLs in the
#'        former, and as the default image map file in the latter.
#'        
#'        For svg, cmapx and imap output, the active area for a node is its
#'        visible image.
#'        For example, an unfilled
#'        node with no drawn boundary will only be active on its label.
#'        For other output, the active area is its bounding box.
#'        The active area for a cluster is its bounding box.
#'        For edges, the active areas are small circles where the edge contacts its head
#'        and tail nodes. In addition, for svg, cmapx and imap, the active area
#'        includes a thin polygon approximating the edge. The circles may
#'        overlap the related node, and the edge URL dominates.
#'        If the edge has a label, this will also be active.
#'        Finally, if the edge has a head or tail label, this will also be active.
#'        
#'        Note that, for edges, the attributes headURL,
#'        tailURL, labelURL and
#'        edgeURL allow control of various parts of an
#'        edge.
#'        Also note that, if active areas of two edges overlap, it is unspecified
#'        which area dominates.
#'
#' @param area Type: [double]. Default: [1.0].
#'
#'        Indicates the preferred area for a node or empty cluster when laid out by patchwork.
#'
#' @param bb Type: [rect]. Default: [NA].
#'
#'        Bounding box of drawing in points.
#'
#' @param bgcolor Type: [color/colorList]. Default: [].
#'
#'        When attached to the root graph, this color is used as the background for
#'        entire canvas. When a cluster attribute, it is used as the initial
#'        background for the cluster. If a cluster has a filled
#'        style, the
#'        cluster's fillcolor will overlay the
#'        background color.
#'        
#'        If the value is a colorList, a gradient fill is
#'        used. By default, this is a linear fill; setting \code{style=radial} will
#'        cause a radial fill. At present, only two colors are used. If the second
#'        color (after a colon) is missing, the default color is used for it.
#'        See also the gradientangle attribute
#'        for setting the gradient angle. 
#'        
#'        For certain output formats, such as PostScript, no fill is done for
#'        the root graph unless
#'        bgcolor is explicitly set. For bitmap formats, however, 
#'        the bits need to be
#'        initialized to something, so the canvas is filled with white by default.
#'        This means that if the bitmap output is included in some other
#'        document, all of the bits within the bitmap's bounding box will be
#'        set, overwriting whatever color or graphics were already on the page.
#'        If this effect is not desired, and you only want to set bits explicitly
#'        assigned in drawing the graph, set \code{bgcolor}="transparent".
#'
#' @param center Type: [bool]. Default: [false].
#'
#'        If true, the drawing is centered in the output canvas.
#'
#' @param charset Type: [string]. Default: ["UTF-8"].
#'
#'        Specifies the character encoding used when interpreting string input
#'        as a text label. The default value is \code{"UTF-8"}.
#'        The other legal value is \code{"iso-8859-1"} or,
#'        equivalently,
#'        \code{"Latin1"}. The \code{charset} attribute is case-insensitive.
#'        Note that if the character encoding used in the input does not
#'        match the \code{charset} value, the resulting output may be very strange.
#'
#' @param class Type: [string]. Default: [""].
#'
#'        Classnames to attach to the node, edge, graph, or cluster's SVG element.
#'        Combine with stylesheet for styling SVG output
#'        using CSS classnames.
#'        
#'        Multiple space-separated classes are supported.
#'        
#'        
#'        digraph G \{
#'          graph [class="cats"];
#'        
#'          subgraph cluster_big \{
#'            graph [class="big_cats"];
#'        
#'            "Lion" [class="yellow social"];
#'            "Snow Leopard" [class="white solitary"];
#'          \};
#'        \}
#'        
#'
#' @param clusterrank Type: [clusterMode]. Default: [local].
#'
#'        Mode used for handling clusters. If \code{clusterrank} is "local", a
#'        subgraph whose name begins with "cluster" is given special treatment.
#'        The subgraph is laid out separately, and then integrated as a unit into
#'        its parent graph, with a bounding rectangle drawn about it.
#'        If the cluster has a label parameter, this label
#'        is displayed within the rectangle.
#'        Note also that there can be clusters within clusters.
#'        At present, the modes "global" and "none"
#'        appear to be identical, both turning off the special cluster processing.
#'
#' @param color Type: [color/colorList]. Default: [black].
#'
#'        Basic drawing color for graphics, not text. For the latter, use the
#'        fontcolor attribute.
#'        
#'        For edges, the value
#'        can either be a single color or a colorList.
#'        In the latter case, if colorList has no fractions,
#'        the edge is drawn using parallel splines or lines,
#'        one for each color in the list, in the order given.
#'        The head arrow, if any, is drawn using the first color in the list,
#'        and the tail arrow, if any, the second color. This supports the common
#'        case of drawing opposing edges, but using parallel splines instead of
#'        separately routed multiedges. 
#'        If any fraction is used, the colors are drawn in series, with each color
#'        being given roughly its specified fraction of the edge.
#'        For example, the graph
#'        
#'        digraph G \{
#'          a -> b [dir=both color="red:blue"]
#'          c -> d [dir=none color="green:red;0.25:blue"]
#'        \}
#'        
#'        yields
#'        
#'
#' @param colorscheme Type: [string]. Default: [""].
#'
#'        This attribute specifies a color scheme namespace. If defined, it specifies
#'        the context for interpreting color names. In particular, if a
#'        color value has form \code{"xxx"} or \code{"//xxx"},
#'        then the
#'        color \code{xxx} will be evaluated according to the current color scheme.
#'        If no color scheme is set, the standard X11 naming is used.
#'        For example, if \code{colorscheme=bugn9}, then \code{color=7}
#'        is interpreted as \code{"/bugn9/7"}.
#'
#' @param comment Type: [string]. Default: [""].
#'
#'        Comments are inserted into output. Device-dependent
#'
#' @param compound Type: [bool]. Default: [false].
#'
#'        If true, allow edges between clusters. (See lhead
#'        and ltail below.)
#'
#' @param concentrate Type: [bool]. Default: [false].
#'
#'        If true, use edge concentrators.
#'        This merges multiedges into a single edge and causes partially parallel
#'        edges to share part of their paths. The latter feature is not yet available
#'        outside of dot.
#'
#' @param defaultdist Type: [double]. Default: [1+(avg. len)*sqrt(|V|)].
#'
#'        This specifies the distance between nodes in separate connected
#'        components. If set too small, connected components may overlap.
#'        Only applicable if pack=false.
#'
#' @param dim Type: [int]. Default: [2].
#'
#'        Set the number of dimensions used for the layout. The maximum value
#'        allowed is 10.
#'
#' @param dimen Type: [int]. Default: [2].
#'
#'        Set the number of dimensions used for rendering. 
#'        The maximum value allowed is 10.
#'        If both \code{dimen} and \code{dim} are set, the latter specifies
#'        the dimension used for layout, and the former for rendering.
#'        If only \code{dimen} is set, this is used for both layout and rendering
#'        dimensions.
#'        
#'        Note that, at present, all aspects of rendering are 2D. This includes
#'        the shape and size of nodes, overlap removal, and edge routing. Thus,
#'        for \code{dimen &gt; 2}, the only valid information is the \code{pos}
#'        attribute of the nodes.
#'        All other coordinates will be 2D and, at best, will reflect a projection
#'        of a higher-dimensional point onto the plane.
#'
#' @param diredgeconstraints Type: [string/bool]. Default: [false].
#'
#'        Only valid when mode="ipsep".
#'        If true, constraints are generated for each edge in the largest (heuristic)
#'        directed acyclic subgraph such that the edge must point downwards.
#'        If "hier", generates level constraints similar to those used with
#'        mode="hier". The main difference is that, in the latter
#'        case, only these constraints are involved, so a faster solver can be used.
#'
#' @param dpi Type: [double]. Default: [96.0/0.0].
#'
#'        This specifies the expected number of pixels per inch on a display device.
#'        For bitmap output, this guarantees that text rendering will be
#'        done more accurately, both in size and in placement. For SVG output,
#'        it is used to guarantee that the dimensions in the output correspond to
#'        the correct number of points or inches.
#'
#' @param epsilon Type: [double]. Default: [.0001 * # nodes(mode == KK)/.0001(mode == major)/.01(mode == sgd)].
#'
#'        Terminating condition. If the length squared of all energy gradients are
#'        &lt; \code{epsilon}, the algorithm stops.
#'
#' @param esep Type: [addDouble/addPoint]. Default: [+3].
#'
#'        Margin used around polygons for purposes of spline edge routing.
#'        The interpretation is the same as given for sep.
#'        This should normally be strictly less than sep.
#'
#' @param fillcolor Type: [color/colorList]. Default: [lightgrey(nodes)/black(clusters)].
#'
#'        Color used to fill the background of a node or cluster
#'        assuming style=filled, or a filled arrowhead.
#'        If \code{fillcolor} is not defined, color is
#'        used. (For clusters, if \code{color} is not defined,
#'        bgcolor is used.) If this is not defined,
#'        the default is used, except for
#'        \code{shape}=point or when the output
#'        format is MIF,
#'        which use black by default.
#'        
#'        If the value is a colorList, a gradient fill is
#'        used. By default, this is a linear fill; setting \code{style=radial} will
#'        cause a radial fill. At present, only two colors are used. If the second
#'        color (after a colon) is missing, the default color is used for it.
#'        See also the gradientangle attribute
#'        for setting the gradient angle. 
#'        
#'        Note that a cluster inherits the root graph's attributes if defined.
#'        Thus, if the root graph has defined a \code{fillcolor}, this will override a
#'        \code{color} or \code{bgcolor} attribute set for the cluster.
#'
#' @param fontcolor Type: [color]. Default: [black].
#'
#'        Color used for text.
#'
#' @param fontnames Type: [string]. Default: [""].
#'
#'        Allows user control of how basic fontnames are represented in SVG output.
#'        If \code{fontnames} is undefined or \code{"svg"},
#'        the output will try to use known SVG fontnames. For example, the
#'        default font  \code{"Times-Roman"} will be mapped to the
#'        basic SVG font \code{"serif"}. This can be overridden by setting
#'        \code{fontnames} to  \code{"ps"} or  \code{"gd"}.
#'        In the former case, known PostScript font names such as
#'        \code{"Times-Roman"} will be used in the output.
#'        In the latter case, the fontconfig font conventions
#'        are used. Thus, \code{"Times-Roman"} would be treated as
#'        \code{"Nimbus Roman No9 L"}. These last two options are useful
#'        with SVG viewers that support these richer fontname spaces.
#'
#' @param fontname Type: [string]. Default: ["Times-Roman"].
#'
#'        Font used for text. This very much depends on the output format and, for
#'        non-bitmap output such as PostScript or SVG, the availability of the font
#'        when the graph is displayed or printed. As such, it is best to rely on
#'        font faces that are generally available, such as Times-Roman, Helvetica or
#'        Courier.
#'        
#'        How font names are resolved also depends on the underlying library that handles
#'        font name resolution.
#'        If Graphviz was built using the
#'        fontconfig library, 
#'        the latter library will be used to search for the font. 
#'        See the commands \code{fc-list}, \code{fc-match} and the other fontconfig commands for how
#'        names are resolved and which fonts are available.
#'        Other systems may provide their own font package, such as Quartz for OS X.
#'        
#'        Note that various font attributes, such as weight and slant, can be built into the
#'        font name. Unfortunately, the syntax varies depending on which font system is dominant. 
#'        Thus, using \code{fontname="times bold italic"} will produce a bold, slanted Times font 
#'        using Pango, the usual main font library. Alternatively,  
#'        \code{fontname="times:italic"} will produce a slanted
#'        Times font from fontconfig, while \code{fontname="times-bold"} will resolve to a bold
#'        Times using Quartz. You will need
#'        to ascertain which package is used by your Graphviz system and refer to the relevant
#'        documentation.
#'        
#'        If Graphviz is not built with a high-level font library, \code{fontname} will be
#'        considered the name of a Type 1 or True Type font file.
#'        If you specify \code{fontname=schlbk}, the tool will look for a
#'        file named  \code{schlbk.ttf} or \code{schlbk.pfa} or \code{schlbk.pfb}
#'        in one of the directories specified by
#'        the fontpath attribute.
#'        The lookup does support various aliases for the common fonts.
#'
#' @param fontpath Type: [string]. Default: [system-dependent].
#'
#'        Directory list used by libgd to search for bitmap fonts if Graphviz
#'        was not built with the fontconfig library.
#'        If \code{fontpath} is not set, the environment
#'        variable \code{DOTFONTPATH} is checked.
#'        If that is not set, \code{GDFONTPATH} is checked.
#'        If not set, libgd uses its compiled-in font path.
#'        Note that fontpath is an attribute of the root graph.
#'
#' @param fontsize Type: [double]. Default: [14.0].
#'
#'        Font size, in points, used for text.
#'
#' @param forcelabels Type: [bool]. Default: [true].
#'
#'        If true, all  \code{xlabel} attributes are placed, even if there is some overlap with nodes
#'        or other labels.
#'
#' @param gradientangle Type: [int]. Default: [""].
#'
#'        If a gradient fill is being used, this determines the angle of the fill. For linear fills, the colors transform
#'        along a line specified by the angle and the center of the object. For radial fills, a value of zero causes the
#'        colors to transform radially from the center; for non-zero values, the colors transform from a point near the
#'        object's periphery as specified by the value.
#'        
#'        If unset, the default angle is 0.
#'
#' @param href Type: [escString]. Default: [""].
#'
#'        Synonym for URL.
#'
#' @param id Type: [escString]. Default: [""].
#'
#'        Allows the graph author to provide an id for graph objects which is to be included in the output.
#'        Normal "&#92;N", "&#92;E", "&#92;G" substitutions are applied.
#'        If provided, it is the responsibility of the provider to keep
#'        its values sufficiently unique for its intended downstream use.
#'        Note, in particular, that "&#92;E" does not provide a unique id for multi-edges.
#'        If no id attribute is provided, then a unique internal id is used. However, 
#'        this value is unpredictable by the graph writer.
#'        An externally provided id is not used internally.
#'        
#'        If the graph provides an id attribute, this will be used as a prefix for
#'        internally generated attributes. By making these distinct, the user
#'        can include multiple image maps in the same document.
#'
#' @param imagepath Type: [string]. Default: [""].
#'
#'        Specifies a list of directories in which to look for image files as specified by the
#'        image attribute or using the \code{IMG} element in
#'        HTML-like labels.
#'        The string should be a list of (absolute or relative) pathnames, each separated by
#'        a semicolon (for Windows) or a colon (all other OS). 
#'        The first directory in which a file of the given name is found will be used to
#'        load the image. If imagepath is not set, relative pathnames for the image
#'        file will be interpreted with respect to the current working directory.
#'
#' @param inputscale Type: [double]. Default: [].
#'
#'        For layout algorithms that support initial input positions (specified by the \code{pos} attribute),
#'        this attribute can be used to appropriately scale the values. By default, fdp and neato interpret
#'        the x and y values of pos as being in inches. (\code{NOTE}: neato -n(2) treats the coordinates as
#'        being in points, being the unit used by the layout algorithms for the pos attribute.) Thus, if
#'        the graph has pos attributes in points, one should set \code{inputscale=72}.
#'        This can also be set on the command line using the \code{-s} flag flag.
#'        
#'        If not set, no scaling is done and the units on input are treated as inches.
#'        A value of 0 is equivalent to \code{inputscale=72}.
#'
#' @param K Type: [double]. Default: [0.3].
#'
#'        Spring constant used in virtual physical model. It roughly corresponds
#'        to an ideal edge length (in inches), in that increasing K tends to
#'        increase the distance between nodes.
#'        Note that the edge attribute len can be used to
#'        override this value for adjacent nodes.
#'
#' @param label Type: [lblString]. Default: [].
#'
#'        Text label attached to objects.
#'        If a node's shape is record, then the label can
#'        have a special format
#'        which describes the record layout.
#'        
#'        Note that a node's default label is "", so the node's name or ID becomes
#'        its label. Technically, a node's name can be an HTML string but this will not
#'        mean that the node's label will be interpreted as an HTML-like label. This is
#'        because the node's actual label is an ordinary string, which will be replaced 
#'        by the raw bytes stored in the node's name.
#'        To get an HTML-like label, the label attribute value itself must be an HTML string.
#'
#' @param labeljust Type: [string]. Default: ["c"].
#'
#'        Justification for cluster labels. If "r", the label
#'        is right-justified within bounding rectangle; if "l", left-justified;
#'        else the label is centered.
#'        Note that a subgraph inherits attributes from its parent. Thus, if
#'        the root graph sets \code{labeljust} to "l", the subgraph inherits
#'        this value.
#'
#' @param labelloc Type: [string]. Default: ["t"(clusters)/"b"(root graphs)/"c"(nodes)].
#'
#'        Vertical placement of labels for nodes, root graphs and clusters.
#'        
#'        For graphs and clusters, only "t" and "b" are allowed, corresponding
#'        to placement at the top and bottom, respectively.
#'        By default, root
#'        graph labels go on the bottom and cluster labels go on the top.
#'        Note that a subgraph inherits attributes from its parent. Thus, if
#'        the root graph sets \code{labelloc} to "b", the subgraph inherits
#'        this value.
#'        
#'        For nodes, this attribute is used only when the height of the node
#'        is larger than the height of its label. 
#'        If \code{labelloc} is set to "t", "c", or "b", the label is aligned
#'        with the top, centered, or aligned with the bottom of the node, respectively.
#'        In the default case, the label is vertically centered.
#'
#' @param label_scheme Type: [int]. Default: [0].
#'
#'        The value indicates
#'        whether to treat a node whose name has the form |edgelabel|* as a special node representing an edge label. 
#'        The default (0) produces no effect.
#'        If the attribute is set to 1, sfdp uses a penalty-based method to make that kind of node close to the 
#'        center of its neighbor. With a value of 2, sfdp uses a penalty-based method to make that kind of node 
#'        close to the old center of its neighbor. Finally, a value of 3 invokes a two-step process of overlap 
#'        removal and straightening.
#'
#' @param landscape Type: [bool]. Default: [false].
#'
#'        If true, the graph is rendered in landscape mode. Synonymous with
#'        \code{rotate=90} or
#'        \code{orientation=landscape}.
#'
#' @param layer Type: [layerRange]. Default: [""].
#'
#'        Specifies layers in which the node, edge or cluster is present.
#'
#' @param layers Type: [layerList]. Default: [""].
#'
#'        Specifies a linearly ordered list of layer names attached to the graph
#'        The graph is then output in separate layers. Only those components
#'        belonging to the current output layer appear. For more information,
#'        see the page How to use drawing layers (overlays).
#'
#' @param layerlistsep Type: [string]. Default: [","].
#'
#'        Specifies the separator characters used to split
#'        an attribute of type layerRange into a list of ranges.
#'
#' @param layerselect Type: [layerRange]. Default: [""].
#'
#'        Selects a list of layers to be emitted.
#'
#' @param layersep Type: [string]. Default: [" ].
#'
#'        Specifies the separator characters used to split the
#'        layers attribute into a list of layer names.
#'
#' @param layout Type: [string]. Default: [""].
#'
#'        Specifies the name of the layout algorithm to use, such as "dot"
#'        or "neato". Normally, graphs should be kept independent of a type of
#'        layout. In some cases, however, it can be convenient to embed the type
#'        of layout desired within the graph. For example, a graph containing
#'        position information from a layout might want to record what the
#'        associated layout algorithm was.
#'        
#'        This attribute takes precedence over 
#'        the -K flag 
#'        or the actual command name used.
#'
#' @param levels Type: [int]. Default: [MAXINT].
#'
#'        Number of levels allowed in the multilevel scheme.
#'
#' @param levelsgap Type: [double]. Default: [0.0].
#'
#'        Specifies strictness of level constraints in neato
#'        when \code{mode="ipsep" or "hier"}.
#'        Larger positive values mean stricter constraints, which demand more
#'        separation between levels. On the other hand, negative values will relax
#'        the constraints by allowing some overlap between the levels.
#'
#' @param lheight Type: [double]. Default: [NA].
#'
#'        Height of graph or cluster label, in inches.
#'
#' @param lp Type: [point]. Default: [NA].
#'
#'        Label position, in points.
#'        The position indicates the center of the label.
#'
#' @param lwidth Type: [double]. Default: [NA].
#'
#'        Width of graph or cluster label, in inches.
#'
#' @param margin Type: [double/point]. Default: [].
#'
#'        For graphs, this sets x and y margins of canvas, in inches. If the margin
#'        is a single double, both margins are set equal to the given value.
#'        
#'        Note that the margin is not part of the drawing but just empty space
#'        left around the drawing. It basically corresponds to a translation of
#'        drawing, as would be necessary to center a drawing on a page. Nothing
#'        is actually drawn in the margin. To actually extend the background of
#'        a drawing, see the pad attribute.
#'        
#'        For clusters, this specifies the space between the nodes in the cluster and
#'        the cluster bounding box. By default, this is 8 points.
#'        
#'        For nodes, this attribute specifies space left around the node's label.
#'        By default, the value is \code{0.11,0.055}.
#'
#' @param maxiter Type: [int]. Default: [100 * # nodes(mode == KK)/200(mode == major)/30(mode == sgd)/600(fdp)].
#'
#'        Sets the number of iterations used.
#'
#' @param mclimit Type: [double]. Default: [1.0].
#'
#'        Multiplicative scale factor used to alter the MinQuit (default = 8)
#'        and MaxIter (default = 24) parameters used during crossing
#'        minimization. These correspond to the
#'        number of tries without improvement before quitting and the
#'        maximum number of iterations in each pass.
#'
#' @param mindist Type: [double]. Default: [1.0].
#'
#'        Specifies the minimum separation between all nodes.
#'
#' @param mode Type: [string]. Default: [major].
#'
#'        Technique for optimizing the layout. For neato, if \code{mode} is \code{"major"},
#'        neato uses stress majorization. If \code{mode} is \code{"KK"},
#'        neato uses a version of the gradient descent method. The only advantage
#'        to the latter technique is that it is sometimes appreciably faster for
#'        small (number of nodes &lt; 100) graphs. A significant disadvantage is that
#'        it may cycle. If \code{mode} is \code{"sgd"}, neato uses a version of the
#'        stochastic gradient descent method. The advantage of sgd is faster and more
#'        reliable convergence than both the previous methods, while the disadvantage
#'        is that it runs in a fixed number of iterations and may require larger
#'        values of \code{"maxiter"} in some graphs.
#'        
#'        There are two experimental modes in neato, "hier", which adds a top-down
#'        directionality similar to the layout used in dot, and "ipsep", which
#'        allows the graph to specify minimum vertical and horizontal distances
#'        between nodes. (See the sep attribute.)
#'        
#'        For sfdp, the default \code{mode} is \code{"spring"}, which corresponds to using
#'        a spring-electrical model. Setting \code{mode} to \code{"maxent"} causes a similar
#'        model to be run but one that also takes into account edge lengths specified by the
#'        \code{"len"} attribute.
#'
#' @param model Type: [string]. Default: [shortpath].
#'
#'        This value specifies how the distance matrix is computed for the input
#'        graph. The distance matrix specifies the ideal distance between every
#'        pair of nodes. neato attemps to find a layout which best achieves
#'        these distances. By default, it uses the length of the shortest path,
#'        where the length of each edge is given by its len
#'        attribute. If \code{model} is \code{"circuit"}, neato uses the
#'        circuit resistance
#'        model to compute the distances. This tends to emphasize clusters. If
#'        \code{model} is \code{"subset"}, neato uses the subset model. This sets the
#'        edge length to be the number of nodes that are neighbors of exactly one
#'        of the end points, and then calculates the shortest paths. This helps
#'        to separate nodes with high degree.
#'        
#'        For more control of distances, one can use \code{model=mds}. In this
#'        case, the len of an edge is used as the ideal distance
#'        between its vertices. A shortest path calculation is only used for
#'        pairs of nodes not connected by an edge. Thus, by supplying a complete
#'        graph, the input can specify all of the relevant distances.
#'
#' @param mosek Type: [bool]. Default: [false].
#'
#'        If Graphviz is built with MOSEK defined, mode=ipsep and mosek=true,
#'        the Mosek software (www.mosek.com) is use to solve the ipsep constraints.
#'
#' @param newrank Type: [bool]. Default: [false].
#'
#'        The original ranking algorithm in dot is recursive on clusters. This can produce fewer ranks
#'        and a more compact layout, but sometimes at the cost of a head node being place on a higher
#'        rank than the tail node. It also assumes that a node is not constrained in separate, 
#'        incompatible subgraphs. For example, a node cannot be in a cluster and also be constrained by
#'        \code{rank=same} with a node not in the cluster.
#'        
#'        If \code{newrank=true}, the ranking algorithm does a single global ranking, ignoring clusters.
#'        This allows nodes to be subject to multiple constraints. Rank constraints will usually take
#'        precedence over edge constraints.
#'
#' @param nodesep Type: [double]. Default: [0.25].
#'
#'        In dot, this specifies the minimum space between two adjacent nodes in the same rank, in inches.
#'        
#'        For other layouts, this affects the spacing between loops on a single node, or multiedges between
#'        a pair of nodes.
#'
#' @param nojustify Type: [bool]. Default: [false].
#'
#'        By default, the justification of multi-line labels is done within the
#'        largest context that makes sense. Thus, in the label of a polygonal
#'        node, a left-justified line will align with the left side of the node
#'        (shifted by the prescribed margin).
#'        In record nodes, left-justified
#'        line will line up with the left side of the enclosing column of fields.
#'        If \code{nojustify} is \code{"true"}, multi-line labels will be justified
#'        in the context of itself. For example, if the attribute is set,
#'        the first label line is long, and the second is shorter and left-justified,
#'        the second will align with the left-most character in the first line,
#'        regardless of  how large the node might be.
#'
#' @param normalize Type: [double/bool]. Default: [false].
#'
#'        If set, normalize coordinates of final
#'        layout so that the first point is at the origin, and then rotate the
#'        layout so that the angle of the first edge is specified by the value of \code{normalize} in degrees.
#'        If  \code{normalize} is not a number, it is evaluated as a  \code{bool}, with true
#'        corresponding to 0 degrees. \code{NOTE:} Since the attribute is evaluated first as a number,
#'        0 and 1 cannot be used for false and true.
#'
#' @param notranslate Type: [bool]. Default: [false].
#'
#'        By default, the final layout is translated so that the lower-left corner of the bounding box is
#'        at the origin. This can be annoying if some nodes are pinned or if the user runs \code{neato -n}. 
#'        To avoid this translation, set \code{notranslate} to true.
#'
#' @param nslimit Type: [double]. Default: [NA].
#'
#'        Used to set number of iterations in
#'        network simplex applications. \code{nslimit} is used in
#'        computing node x coordinates.
#'        If defined, # iterations =  \code{nslimit} * # nodes;
#'        otherwise,  # iterations = MAXINT.
#'
#' @param nslimit1 Type: [double]. Default: [NA].
#'
#'        Used to set number of iterations in
#'        network simplex applications. 
#'        \code{nslimit1} is used for ranking nodes.
#'        If defined, # iterations =  \code{nslimit1} * # nodes;
#'        otherwise,  # iterations = MAXINT.
#'
#' @param ordering Type: [string]. Default: [""].
#'
#'        If the value of the attribute is "out", then
#'        the outedges of a node, that is, edges with the node as its tail node,
#'        must appear
#'        left-to-right in the same order in which they are defined in
#'        the input.
#'        If the value of the attribute is "in", then
#'        the inedges of a node must appear
#'        left-to-right in the same order in which they are defined in
#'        the input.
#'        If defined as a graph or subgraph attribute, the value is applied to all nodes
#'        in the graph or subgraph. Note that the graph attribute takes
#'        precedence over the node attribute.
#'
#' @param orientation Type: [double/string]. Default: [0.0/""].
#'
#'        When used on nodes: Angle, in degrees, used to rotate polygon node shapes. For any number of polygon sides, 0 degrees rotation results in a flat base.
#'        
#'        When used on graphs: If "[lL]*", set graph orientation to landscape.
#'        Used only if \code{rotate} is not defined.
#'
#' @param outputorder Type: [outputMode]. Default: [breadthfirst].
#'
#'        Specify order in which nodes and edges are drawn.
#'
#' @param overlap Type: [string/bool]. Default: [true].
#'
#'        Determines if and how node overlaps should be removed. Nodes are first
#'        enlarged using the \code{sep} attribute.
#'        If "true" , overlaps are retained.
#'        If the value is "scale", overlaps are removed by uniformly scaling in x and y.
#'        If the value converts to "false", and it is available, Prism, a proximity graph-based algorithm, is
#'        used to remove node overlaps.
#'        This can also be invoked explicitly with "overlap=prism".
#'        This technique starts with a
#'        small scaling up, controlled by the 
#'        \code{overlap_scaling} attribute,
#'        which can remove a significant portion of the overlap.
#'        The prism option also accepts an optional non-negative integer suffix.
#'        This can be used to control the number of attempts made at overlap
#'        removal. By default, \code{overlap="prism"} is equivalent to
#'        \code{overlap="prism1000"}. Setting \code{overlap="prism0"}
#'        causes only the scaling phase to be run.
#'        
#'        If Prism is not available, or the version of Graphviz is earlier than 2.28, "overlap=false"
#'        uses a Voronoi-based technique.
#'        This can always be invoked explicitly with "overlap=voronoi".
#'        
#'        If the value is "scalexy", x and y are separately
#'        scaled to remove overlaps.
#'        
#'        If the value is "compress", the layout will be scaled down as much as
#'        possible without introducing any overlaps, obviously assuming there are
#'        none to begin with.
#'        
#'        \code{N.B.}The remaining allowed values of \code{overlap}
#'        correspond to algorithms which, at present, can produce bad aspect ratios.
#'        In addition, we deprecate the use of the "ortho*" and "portho*".
#'        
#'        If the value is "vpsc", overlap removal is done as a
#'        quadratic optimization to minimize node displacement while removing
#'        node overlaps.
#'        
#'        If the value is "orthoxy" or "orthoyx", overlaps
#'        are moved by optimizing two constraint problems, one for the x axis and
#'        one for the y. The suffix indicates which axis is processed first.
#'        If the value is "ortho", the technique is similar to "orthoxy" except a
#'        heuristic is used to reduce the bias between the two passes.
#'        If the value is "ortho_yx", the technique is the same as "ortho", except
#'        the roles of x and y are reversed.
#'        The values "portho", "porthoxy", "porthoxy", and "portho_yx" are similar
#'        to the previous four, except only pseudo-orthogonal ordering is
#'        enforced.
#'        
#'        If the layout is done by neato with mode="ipsep",
#'        then one can use \code{overlap=ipsep}.
#'        In this case, the overlap removal constraints are
#'        incorporated into the layout algorithm itself.
#'        N.B. At present, this only supports one level of clustering.
#'        
#'        Except for fdp and sfdp, the layouts assume \code{overlap="true"} as the default.
#'        Fdp first uses a number of passes using a built-in, force-directed technique
#'        to try to remove overlaps. Thus, fdp accepts \code{overlap} with an integer
#'        prefix followed by a colon, specifying the number of tries. If there is
#'        no prefix, no initial tries will be performed. If there is nothing following
#'        a colon, none of the above methods will be attempted. By default, fdp
#'        uses \code{overlap="9:prism"}. Note that \code{overlap="true"},
#'        \code{overlap="0:true"} and \code{overlap="0:"} all turn off all overlap
#'        removal.
#'        
#'        By default, sfdp uses \code{overlap="prism0"}.
#'        
#'        Except for the Voronoi and prism methods, all of these transforms preserve the
#'        orthogonal ordering of the original layout. That is, if the x coordinates
#'        of two nodes are originally the same, they will remain the same, and if
#'        the x coordinate of one node is originally less than the x coordinate of
#'        another, this relation will still hold in the transformed layout. The
#'        similar properties hold for the y coordinates.
#'        This is not quite true for the "porth*" cases. For these, orthogonal
#'        ordering is only preserved among nodes related by an edge.
#'
#' @param overlap_scaling Type: [double]. Default: [-4].
#'
#'        When \code{overlap=prism}, the layout is scaled by this factor, thereby
#'        removing a fair amount of node overlap, and making node overlap removal
#'        faster and better able to retain the graph's shape.
#'        
#'        If \code{overlap_scaling} is negative, the layout is scaled by
#'        \code{-1*overlap_scaling} times the average label size.
#'        If \code{overlap_scaling} is positive, the layout is scaled by
#'        \code{overlap_scaling}.
#'        If \code{overlap_scaling} is zero, no scaling is done.
#'
#' @param overlap_shrink Type: [bool]. Default: [true].
#'
#'        If true, the overlap removal algorithm will perform a compression pass to reduce the
#'        size of the layout.
#'
#' @param pack Type: [bool/int]. Default: [false].
#'
#'        This is true if the value of pack is "true" (case-insensitive) or a
#'        non-negative integer. If true, each connected component of the graph is
#'        laid out separately, and then the graphs are packed together.
#'        If pack has an integral value, this is used as the size,
#'        in points, of
#'        a margin around each part; otherwise, a default margin of 8 is used.
#'        If pack is interpreted as false, the entire graph is laid out together.
#'        The granularity and method of packing is influenced by the
#'        packmode attribute.
#'        
#'        For layouts which always do packing, such a twopi, the \code{pack}
#'        attribute is just used to set the margin.
#'
#' @param packmode Type: [packMode]. Default: [node].
#'
#'        This indicates how connected components should be packed
#'        (cf. packMode). Note that defining
#'        \code{packmode} will automatically turn on packing as though one had
#'        set \code{pack=true}.
#'
#' @param pad Type: [double/point]. Default: [0.0555 (4 points)].
#'
#'        The pad attribute specifies how much, in inches, to extend the
#'        drawing area around the minimal area needed to draw the graph.
#'        If the pad is a single double, both the x and y pad values are set
#'        equal to the given value. This area is part of the
#'        drawing and will be filled with the background color, if appropriate.
#'        
#'        Normally, a small pad is used for aesthetic reasons, especially when
#'        a background color is used, to avoid having nodes and edges abutting
#'        the boundary of the drawn region.
#'
#' @param page Type: [double/point]. Default: [NA].
#'
#'        Width and height of output pages, in inches. If only a single value
#'        is given, this is used for both the width and height.
#'        
#'        If this is set and is
#'        smaller than the size of the layout, a rectangular array of pages of
#'        the specified page size is overlaid on the layout, with origins
#'        aligned in the lower-left corner, thereby partitioning the layout
#'        into pages. The pages are then produced one at a time, in
#'        pagedir order.
#'        
#'        At present, this only works for PostScript output. For other types of
#'        output, one should use another tool to split the output into multiple
#'        output files. Or use the viewport to generate
#'        multiple files.
#'
#' @param pagedir Type: [pagedir]. Default: [BL].
#'
#'        If the page attribute is set and applicable,
#'        this attribute specifies the order in which the pages are emitted.
#'        This is limited to one of the 8 row or column major orders.
#'
#' @param pencolor Type: [color]. Default: [black].
#'
#'        Color used to draw the bounding box around a cluster.
#'        If \code{pencolor} is not defined, \code{color} is
#'        used. If this is not defined, bgcolor is used.
#'        If this is not defined, the default is used.
#'        
#'        Note that a cluster inherits the root graph's attributes if defined.
#'        Thus, if the root graph has defined a \code{pencolor}, this will override a
#'        \code{color} or \code{bgcolor} attribute set for the cluster.
#'
#' @param penwidth Type: [double]. Default: [1.0].
#'
#'        Specifies the width of the pen, in points, used to draw lines and curves,
#'        including the boundaries of edges and clusters. The value is inherited
#'        by subclusters.
#'        It has no effect on text.
#'        
#'        Previous to 31 January 2008, the effect of \code{penwidth=W}
#'        was achieved by including \code{setlinewidth(W)}
#'        as part of a \code{style} specification.
#'        If both are used, \code{penwidth} will be used.
#'
#' @param peripheries Type: [int]. Default: [shape default(nodes)/1(clusters)].
#'
#'        Set number of peripheries used in polygonal shapes and cluster
#'        boundaries. Note that
#'        user-defined shapes are treated as a
#'        form of box shape, so the default
#'        peripheries value is 1 and the user-defined shape will be drawn in
#'        a bounding rectangle. Setting \code{peripheries=0} will turn this off.
#'        Also, 1 is the maximum peripheries value for clusters.
#'
#' @param quadtree Type: [quadType/bool]. Default: [normal].
#'
#'        Quadtree scheme to use.
#'        
#'        A TRUE bool value corresponds to "normal";
#'        a FALSE bool value corresponds to "none".
#'        As a slight exception to the normal interpretation of bool,
#'        a value of "2" corresponds to "fast".
#'
#' @param quantum Type: [double]. Default: [0.0].
#'
#'        If \code{quantum} > 0.0, node label dimensions
#'        will be rounded to integral multiples of the quantum.
#'
#' @param rankdir Type: [rankdir]. Default: [TB].
#'
#'        Sets direction of graph layout. For example, if \code{rankdir}="LR",
#'        and barring cycles, an edge T -> H; will go
#'        from left to right. By default, graphs are laid out from top to bottom.
#'        
#'        This attribute also has a side-effect in determining how record nodes
#'        are interpreted. See record shapes.
#'
#' @param ranksep Type: [double/doubleList]. Default: [0.5(dot)/1.0(twopi)].
#'
#'        In dot, this gives the desired rank separation, in inches. This is
#'        the minimum vertical distance between the bottom of the nodes in one
#'        rank and the tops of nodes in the next. If the value
#'        contains "equally", the centers of all ranks are spaced equally apart.
#'        Note that both
#'        settings are possible, e.g., ranksep = "1.2 equally".
#'        
#'        In twopi, this attribute specifies the radial separation of concentric circles.
#'        For twopi, \code{ranksep} can also be a list of doubles. The first double specifies
#'        the radius of the inner circle; the second double specifies the increase in
#'        radius from the first circle to the second; etc. If there are more circles than
#'        numbers, the last number is used as the increment for the remainder.
#'
#' @param ratio Type: [double/string]. Default: [NA].
#'
#'        Sets the aspect ratio (drawing height/drawing width) for the drawing.
#'        Note that this is adjusted before
#'        the \code{size} attribute constraints are enforced.
#'        In addition, the calculations usually ignore the node sizes, so the
#'        final drawing size may only approximate what is desired.
#'        
#'        If \code{ratio} is numeric, it is taken as the desired aspect ratio.
#'        Then, if the actual aspect ratio is less than the desired ratio,
#'        the drawing height is scaled up to achieve the
#'        desired ratio; if the actual ratio is greater than that desired ratio,
#'        the drawing width is scaled up.
#'        
#'        If \code{ratio} = "fill" and the \code{size}
#'        attribute is set, node positions are scaled, separately in both x
#'        and y, so that the final drawing exactly fills the specified size.
#'        If both \code{size} values exceed the width
#'        and height of the drawing, then both coordinate values of each
#'        node are scaled up accordingly. However, if either size dimension
#'        is smaller than the corresponding dimension in the drawing, one
#'        dimension is scaled up so that the final drawing has the same aspect
#'        ratio as specified by \code{size}. 
#'        Then, when rendered, the layout will be
#'        scaled down uniformly in both dimensions to fit the given
#'        \code{size}, which may cause nodes and text
#'        to shrink as well. This may not be what the user
#'        wants, but it avoids the hard problem of how to reposition the
#'        nodes in an acceptable fashion to reduce the drawing size.
#'        
#'        If \code{ratio} = "compress" and the \code{size}
#'        attribute is set, dot attempts to compress the initial layout to fit
#'        in the given size. This achieves a tighter packing of nodes but
#'        reduces the balance and symmetry. This feature only works in dot.
#'        
#'        If \code{ratio} = "expand", the \code{size}
#'        attribute is set, and both the width and the height of the graph are
#'        less than the value in  \code{size}, node positions are scaled
#'        uniformly until at least
#'        one dimension fits \code{size} exactly.
#'        Note that this is distinct from using \code{size} as the
#'        desired size, as here the drawing is expanded before edges are generated and
#'        all node and text sizes remain unchanged.
#'        
#'        If \code{ratio} = "auto", the \code{page}
#'        attribute is set and the graph cannot be drawn on a single page,
#'        then \code{size} is set to an ``ideal'' value.
#'        In particular, the size in a given dimension will be the smallest integral
#'        multiple of the page size in that dimension which is at least half the
#'        current size. The two dimensions are then scaled independently to the
#'        new size. This feature only works in dot.
#'
#' @param remincross Type: [bool]. Default: [true].
#'
#'        If true and there are multiple clusters, run crossing
#'        minimization a second time.
#'
#' @param repulsiveforce Type: [double]. Default: [1.0].
#'
#'        The power of the repulsive force used in an extended Fruchterman-Reingold 
#'        force directed model. Values larger than 1 tend to reduce 
#'        the warping effect at the expense of less clustering.
#'
#' @param resolution Type: [double]. Default: [96.0/0.0].
#'
#'        This is a synonym for the dpi attribute.
#'
#' @param root Type: [string/bool]. Default: [(graphs)/false(nodes)].
#'
#'        This specifies nodes to be used as the center of the
#'        layout and the root of the generated spanning tree. As a graph attribute,
#'        this gives the name of the node. As a node attribute, it
#'        specifies that the node should be used as a central node. In twopi,
#'        this will actually be the central node. In circo, the block containing
#'        the node will be central in the drawing of its connected component.
#'        If not defined,
#'        twopi will pick a most central node, and circo will pick a random node.
#'        
#'        If the root attribute is defined as the empty string, twopi will reset it
#'        to name of the node picked as the root node.
#'        
#'        For twopi, it is possible to have multiple roots, presumably one for each
#'        component. If more than one node in a component is marked as the root, twopi
#'        will pick one.
#'
#' @param rotate Type: [int]. Default: [0].
#'
#'        If 90, set drawing orientation to landscape.
#'
#' @param rotation Type: [double]. Default: [0].
#'
#'        Causes the final layout to be rotated counter-clockwise by the specified number of degrees.
#'
#' @param scale Type: [double/point]. Default: [NA].
#'
#'        If set, after the initial layout, the layout is scaled by the given factors.
#'        If only a single number is given, this is used for both factors.
#'
#' @param searchsize Type: [int]. Default: [30].
#'
#'        During network simplex, maximum number of edges with negative cut values
#'        to search when looking for one with minimum cut value.
#'
#' @param sep Type: [addDouble/addPoint]. Default: [+4].
#'
#'        Specifies margin to leave around nodes when removing node overlap.
#'        This guarantees a minimal non-zero distance between nodes.
#'        
#'        If the attribute begins with a plus sign '+', an additive margin is
#'        specified. That is, "+w,h" causes the node's bounding box to be increased 
#'        by w
#'        points on the left and right sides, and by h points on the top and bottom.
#'        Without a plus sign, the node is scaled by 1 + w in the x coordinate
#'        and 1 + h in the y coordinate.
#'        
#'        If only a single number is given, this is used for both dimensions.
#'        
#'        If unset but esep is defined, the sep values
#'        will be set to the esep values divided by 0.8. 
#'        If esep is unset, the default value is used.
#'
#' @param showboxes Type: [int]. Default: [0].
#'
#'        Print guide boxes in PostScript at the beginning of
#'        routesplines if 1, or at the end if 2. (Debugging, TB mode only!)
#'
#' @param size Type: [double/point]. Default: [NA].
#'
#'        Maximum width and height of drawing, in inches.
#'        If only a single number is given, this is used for both the width
#'        and the height.
#'        
#'        If defined and the drawing is larger than the given size, 
#'        the drawing is uniformly
#'        scaled down so that it fits within the given size.
#'        
#'        If \code{size} ends in an exclamation point (\code{!}),
#'        then it is taken to be
#'        the desired size. In this case, if both dimensions of the drawing are
#'        less than \code{size}, the drawing is scaled up uniformly until at
#'        least one dimension equals its dimension in \code{size}.
#'        
#'        Note that there is some interaction between the \code{size} and
#'        \code{ratio} attributes.
#'
#' @param smoothing Type: [smoothType]. Default: ["none"].
#'
#'        Specifies a post-processing step used to smooth out an uneven distribution 
#'        of nodes.
#'
#' @param sortv Type: [int]. Default: [0].
#'
#'        If packmode indicates an array packing, 
#'        this attribute specifies an
#'        insertion order among the components, with smaller values inserted first.
#'
#' @param splines Type: [bool/string]. Default: [NA].
#'
#'        Controls how, and if, edges are represented. If true, edges are drawn as
#'        splines routed around nodes; if false, edges are drawn as line segments.
#'        If set to none or "", no edges are drawn at all.
#'        
#'        (1 March 2007) The values \code{line} and \code{spline} can be
#'        used as synonyms for \code{false} and \code{true}, respectively.
#'        In addition, the value \code{polyline} specifies that edges should be
#'        drawn as polylines.
#'        
#'        (28 Sep 2010) The value \code{ortho} specifies edges should be
#'        routed as polylines of axis-aligned segments. Currently, the routing
#'        does not handle ports or, in dot, edge labels.
#'        
#'        (25 Sep 2012) The value \code{curved} specifies edges should be
#'        drawn as curved arcs.
#'        
#'        
#'        
#'             
#'            
#'        
#'        
#'            splines=nonesplines=""
#'            splines=linesplines=false
#'        
#'        
#'            
#'            
#'        
#'        
#'            splines=polyline
#'            splines=curved
#'        
#'        
#'            
#'            
#'        
#'        
#'            splines=ortho
#'            splines=splinesplines=true
#'        
#'        
#'        
#'        
#'        By default, the attribute is unset. How this is interpreted depends on
#'        the layout. For dot, the default is to draw edges as splines. For all
#'        other layouts, the default is to draw edges as line segments. Note that
#'        for these latter layouts, if \code{splines="true"}, this
#'        requires non-overlapping nodes (cf. \code{overlap}).
#'        If fdp is used for layout and \code{splines="compound"}, then the edges are
#'        drawn to avoid clusters as well as nodes.
#'
#' @param start Type: [startType]. Default: [""].
#'
#'        Parameter used to determine the initial layout of nodes. If unset, the
#'        nodes are randomly placed in a unit square with
#'        the same seed is always used for the random number generator, so the
#'        initial placement is repeatable.
#'
#' @param style Type: [style]. Default: [""].
#'
#'        Set style information for components of the graph. For cluster subgraphs, if \code{style="filled"}, the
#'        cluster box's background is filled.
#'        
#'        If the default style attribute has been set for a component, an individual component can use
#'        \code{style=""} to revert to the normal default. For example, if the graph has
#'        
#'        \code{edge [style="invis"]}
#'        
#'        making all edges invisible, a specific edge can overrride this via:
#'        
#'        \code{a -> b [style=""]}
#'        
#'        Of course, the component can also explicitly set its \code{style} attribute to the desired value.
#'
#' @param stylesheet Type: [string]. Default: [""].
#'
#'        A URL or pathname specifying an XML style sheet, used in SVG output. Combine
#'        with class to style elements using CSS selectors.
#'
#' @param target Type: [escString/string]. Default: [].
#'
#'        If the object has a URL, this attribute determines which window
#'        of the browser is used for the URL.
#'        See W3C documentation.
#'
#' @param tooltip Type: [escString]. Default: [""].
#'
#'        Tooltip annotation attached to the node or edge. If unset, Graphviz
#'        will use the object's label if defined.
#'        Note that if the label is a record specification or an HTML-like
#'        label, the resulting tooltip may be unhelpful. In this case, if
#'        tooltips will be generated, the user should set a \code{tooltip}
#'        attribute explicitly.
#'
#' @param truecolor Type: [bool]. Default: [].
#'
#'        If set explicitly to true or false, the value determines whether or not
#'        internal bitmap rendering relies on a truecolor color model or uses
#'        a color palette.
#'        If the attribute is unset, truecolor is not used
#'        unless there is a shapefile property
#'        for some node in the graph.
#'        The output model will use the input model when possible.
#'        
#'        Use of color palettes results in less memory usage during creation of the
#'        bitmaps and smaller output files.
#'        
#'        Usually, the only time it is necessary to specify the truecolor model
#'        is if the graph uses more than 256 colors.
#'        However, if one uses bgcolor=transparent with
#'        a color palette, font
#'        antialiasing can show up as a fuzzy white area around characters.
#'        Using \code{truecolor}=true avoids this problem.
#'
#' @param xdotversion Type: [string]. Default: [].
#'
#'        For xdot output, if this attribute is set, this determines the version of xdot used in output.
#'        If not set, the attribute will be set to the xdot version used for output.
#'
#' @param viewport Type: [viewPort]. Default: [""].
#'
#'        Clipping window on final drawing. Note that this attribute supersedes any
#'        size attribute. The width and height of the viewport specify
#'        precisely the final size of the output. 
#'
#' @param voro_margin Type: [double]. Default: [0.05].
#'
#'        Factor to scale up drawing to allow margin for expansion in
#'        Voronoi technique. dim' = (1+2*margin)*dim.
#'
#' @return original dot graph
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_graph_attr <- function(d, 
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
  assert_dot(d)
  att <- find_args(...)
  assert_named(att)
  
  d$graph_att <- modifyList(d$graph_att, att)
  
  d
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add global node attributes 
#' @inheritParams add_graph_attr
#' @param URL Type: [escString]. Default: [].
#'
#'        Hyperlinks incorporated into device-dependent output.
#'        At present, used in ps2, cmap, i*map and svg formats.
#'        For all these formats, URLs can be attached to nodes, edges and
#'        clusters. URL attributes can also be attached to the root graph in ps2,
#'        cmap and i*map formats. This serves as the base URL for relative URLs in the
#'        former, and as the default image map file in the latter.
#'        
#'        For svg, cmapx and imap output, the active area for a node is its
#'        visible image.
#'        For example, an unfilled
#'        node with no drawn boundary will only be active on its label.
#'        For other output, the active area is its bounding box.
#'        The active area for a cluster is its bounding box.
#'        For edges, the active areas are small circles where the edge contacts its head
#'        and tail nodes. In addition, for svg, cmapx and imap, the active area
#'        includes a thin polygon approximating the edge. The circles may
#'        overlap the related node, and the edge URL dominates.
#'        If the edge has a label, this will also be active.
#'        Finally, if the edge has a head or tail label, this will also be active.
#'        
#'        Note that, for edges, the attributes headURL,
#'        tailURL, labelURL and
#'        edgeURL allow control of various parts of an
#'        edge.
#'        Also note that, if active areas of two edges overlap, it is unspecified
#'        which area dominates.
#'
#' @param area Type: [double]. Default: [1.0].
#'
#'        Indicates the preferred area for a node or empty cluster when laid out by patchwork.
#'
#' @param class Type: [string]. Default: [""].
#'
#'        Classnames to attach to the node, edge, graph, or cluster's SVG element.
#'        Combine with stylesheet for styling SVG output
#'        using CSS classnames.
#'        
#'        Multiple space-separated classes are supported.
#'        
#'        
#'        digraph G \{
#'          graph [class="cats"];
#'        
#'          subgraph cluster_big \{
#'            graph [class="big_cats"];
#'        
#'            "Lion" [class="yellow social"];
#'            "Snow Leopard" [class="white solitary"];
#'          \};
#'        \}
#'        
#'
#' @param color Type: [color/colorList]. Default: [black].
#'
#'        Basic drawing color for graphics, not text. For the latter, use the
#'        fontcolor attribute.
#'        
#'        For edges, the value
#'        can either be a single color or a colorList.
#'        In the latter case, if colorList has no fractions,
#'        the edge is drawn using parallel splines or lines,
#'        one for each color in the list, in the order given.
#'        The head arrow, if any, is drawn using the first color in the list,
#'        and the tail arrow, if any, the second color. This supports the common
#'        case of drawing opposing edges, but using parallel splines instead of
#'        separately routed multiedges. 
#'        If any fraction is used, the colors are drawn in series, with each color
#'        being given roughly its specified fraction of the edge.
#'        For example, the graph
#'        
#'        digraph G \{
#'          a -> b [dir=both color="red:blue"]
#'          c -> d [dir=none color="green:red;0.25:blue"]
#'        \}
#'        
#'        yields
#'        
#'
#' @param colorscheme Type: [string]. Default: [""].
#'
#'        This attribute specifies a color scheme namespace. If defined, it specifies
#'        the context for interpreting color names. In particular, if a
#'        color value has form \code{"xxx"} or \code{"//xxx"},
#'        then the
#'        color \code{xxx} will be evaluated according to the current color scheme.
#'        If no color scheme is set, the standard X11 naming is used.
#'        For example, if \code{colorscheme=bugn9}, then \code{color=7}
#'        is interpreted as \code{"/bugn9/7"}.
#'
#' @param comment Type: [string]. Default: [""].
#'
#'        Comments are inserted into output. Device-dependent
#'
#' @param distortion Type: [double]. Default: [0.0].
#'
#'        Distortion factor for \code{shape}=polygon.
#'        Positive values cause top part to
#'        be larger than bottom; negative values do the opposite.
#'
#' @param fillcolor Type: [color/colorList]. Default: [lightgrey(nodes)/black(clusters)].
#'
#'        Color used to fill the background of a node or cluster
#'        assuming style=filled, or a filled arrowhead.
#'        If \code{fillcolor} is not defined, color is
#'        used. (For clusters, if \code{color} is not defined,
#'        bgcolor is used.) If this is not defined,
#'        the default is used, except for
#'        \code{shape}=point or when the output
#'        format is MIF,
#'        which use black by default.
#'        
#'        If the value is a colorList, a gradient fill is
#'        used. By default, this is a linear fill; setting \code{style=radial} will
#'        cause a radial fill. At present, only two colors are used. If the second
#'        color (after a colon) is missing, the default color is used for it.
#'        See also the gradientangle attribute
#'        for setting the gradient angle. 
#'        
#'        Note that a cluster inherits the root graph's attributes if defined.
#'        Thus, if the root graph has defined a \code{fillcolor}, this will override a
#'        \code{color} or \code{bgcolor} attribute set for the cluster.
#'
#' @param fixedsize Type: [bool/string]. Default: [false].
#'
#'        If false, the size of a node is determined by smallest width and height
#'        needed to contain its label and image, if any, with a margin specified by
#'        the \code{margin} attribute. The width
#'        and height must also be at least as large as the sizes specified by the
#'        \code{width} and
#'        \code{height} attributes, which specify
#'        the minimum values for these parameters. 
#'        
#'        If true, the node size is specified by the values of the
#'        \code{width}
#'        and \code{height} attributes only
#'        and is not expanded to contain the text label.
#'        There will be a warning if the label (with margin)
#'        cannot fit within these limits.
#'        
#'        If the \code{fixedsize} attribute is set 
#'        to shape, the \code{width} and
#'        \code{height} attributes also determine the size
#'        of the node shape, but the label can be much larger. Both the label and
#'        shape sizes are used when avoiding node overlap, but all edges to the
#'        node ignore the label and only contact the node shape. No warning is given
#'        if the label is too large.
#'
#' @param fontcolor Type: [color]. Default: [black].
#'
#'        Color used for text.
#'
#' @param fontname Type: [string]. Default: ["Times-Roman"].
#'
#'        Font used for text. This very much depends on the output format and, for
#'        non-bitmap output such as PostScript or SVG, the availability of the font
#'        when the graph is displayed or printed. As such, it is best to rely on
#'        font faces that are generally available, such as Times-Roman, Helvetica or
#'        Courier.
#'        
#'        How font names are resolved also depends on the underlying library that handles
#'        font name resolution.
#'        If Graphviz was built using the
#'        fontconfig library, 
#'        the latter library will be used to search for the font. 
#'        See the commands \code{fc-list}, \code{fc-match} and the other fontconfig commands for how
#'        names are resolved and which fonts are available.
#'        Other systems may provide their own font package, such as Quartz for OS X.
#'        
#'        Note that various font attributes, such as weight and slant, can be built into the
#'        font name. Unfortunately, the syntax varies depending on which font system is dominant. 
#'        Thus, using \code{fontname="times bold italic"} will produce a bold, slanted Times font 
#'        using Pango, the usual main font library. Alternatively,  
#'        \code{fontname="times:italic"} will produce a slanted
#'        Times font from fontconfig, while \code{fontname="times-bold"} will resolve to a bold
#'        Times using Quartz. You will need
#'        to ascertain which package is used by your Graphviz system and refer to the relevant
#'        documentation.
#'        
#'        If Graphviz is not built with a high-level font library, \code{fontname} will be
#'        considered the name of a Type 1 or True Type font file.
#'        If you specify \code{fontname=schlbk}, the tool will look for a
#'        file named  \code{schlbk.ttf} or \code{schlbk.pfa} or \code{schlbk.pfb}
#'        in one of the directories specified by
#'        the fontpath attribute.
#'        The lookup does support various aliases for the common fonts.
#'
#' @param fontsize Type: [double]. Default: [14.0].
#'
#'        Font size, in points, used for text.
#'
#' @param gradientangle Type: [int]. Default: [""].
#'
#'        If a gradient fill is being used, this determines the angle of the fill. For linear fills, the colors transform
#'        along a line specified by the angle and the center of the object. For radial fills, a value of zero causes the
#'        colors to transform radially from the center; for non-zero values, the colors transform from a point near the
#'        object's periphery as specified by the value.
#'        
#'        If unset, the default angle is 0.
#'
#' @param group Type: [string]. Default: [""].
#'
#'        If the end points of an edge belong to the same group, i.e., have the
#'        same group attribute, parameters are set to avoid crossings and keep
#'        the edges straight.
#'
#' @param height Type: [double]. Default: [0.5].
#'
#'        Height of node, in inches. This is taken as the initial, minimum height
#'        of the node. If \code{fixedsize} is true, this
#'        will be the final height of the node. Otherwise, if the node label
#'        requires more height to fit, the node's height will be increased to
#'        contain the label. Note also that, if the output format is dot, the
#'        value given to \code{height} will be the final value.
#'        
#'        If the node shape is regular, the width and height are made identical.
#'        In this case, if either the width or the height is set explicitly,
#'        that value is used.
#'        In this case, if both the width or the height are set explicitly,
#'        the maximum of the two values is used.
#'        If neither is set explicitly, the minimum of the two default values
#'        is used.
#'
#' @param href Type: [escString]. Default: [""].
#'
#'        Synonym for URL.
#'
#' @param id Type: [escString]. Default: [""].
#'
#'        Allows the graph author to provide an id for graph objects which is to be included in the output.
#'        Normal "&#92;N", "&#92;E", "&#92;G" substitutions are applied.
#'        If provided, it is the responsibility of the provider to keep
#'        its values sufficiently unique for its intended downstream use.
#'        Note, in particular, that "&#92;E" does not provide a unique id for multi-edges.
#'        If no id attribute is provided, then a unique internal id is used. However, 
#'        this value is unpredictable by the graph writer.
#'        An externally provided id is not used internally.
#'        
#'        If the graph provides an id attribute, this will be used as a prefix for
#'        internally generated attributes. By making these distinct, the user
#'        can include multiple image maps in the same document.
#'
#' @param image Type: [string]. Default: [""].
#'
#'        Gives the name of a file containing an image to be displayed inside
#'        a node. The image file must be in one of the recognized 
#'        formats,
#'        typically JPEG, PNG, GIF, BMP, SVG or Postscript, and be able to be converted
#'        into the desired output format.
#'        
#'        The file must contain the image size information. This is usually trivially
#'        true for the bitmap formats. For PostScript, the file must contain a
#'        line starting with \code{BoundingBox: } followed by four integers
#'        specifying the lower left x and y coordinates and the upper right x and y
#'        coordinates of the bounding box for the image, the coordinates being in
#'        points. An SVG image file must contain \code{width} and \code{height}
#'        attributes, typically as part of the \code{svg} element.
#'        The values for these should have the form of a floating point number,
#'        followed by optional units, e.g., \code{width="76pt"}. 
#'        Recognized units are \code{in}, \code{px},
#'        \code{pc}, \code{pt}, \code{cm} and \code{mm} for inches, pixels,
#'        picas, points, centimeters and millimeters, respectively.
#'        The default unit is points.
#'        
#'        Unlike with the shapefile attribute,
#'        the image is treated as node
#'        content rather than the entire node. In particular, an image can
#'        be contained in a node of any shape, not just a rectangle.
#'
#' @param imagepos Type: [string]. Default: ["mc"].
#'
#'        Attribute controlling how an image is positioned within its containing node.  This
#'        only has an effect when the image is smaller than the containing node.  The
#'        default is to be centered both horizontally and vertically.  Valid values:
#'        
#'        tlTop Left
#'        tcTop Centered
#'        trTop Right
#'        mlMiddle Left
#'        mcMiddle Centered (the default)
#'        mrMiddle Right
#'        blBottom Left
#'        bcBottom Centered
#'        brBottom Right
#'        
#'
#' @param imagescale Type: [bool/string]. Default: [false].
#'
#'        Attribute controlling how an image fills its
#'        containing node. In general, the image is given its natural size,
#'        (cf. dpi),
#'        and the node size is made large enough to contain its image, its
#'        label, its margin, and its peripheries.
#'        Its width and height will also be at least as large as its
#'        minimum width and height.
#'        If, however, \code{fixedsize=true},
#'        the width and height attributes specify the exact size of the node.
#'        
#'        During rendering, in the default case (\code{imagescale=false}),
#'        the image retains its natural size.
#'        If \code{imagescale=true},
#'        the image is uniformly scaled (i.e., its aspect ratio is
#'        preserved) to fit inside the node.
#'        At least one dimension of the image will be as large as possible
#'        given the size of the node.
#'        When \code{imagescale=width},
#'        the width of the image is scaled to fill the node width.
#'        The corresponding property holds when \code{imagescale=height}.
#'        When \code{imagescale=both},
#'        both the height and the width are scaled separately to fill the node.
#'        
#'        In all cases, if a dimension of the image is larger than the
#'        corresponding dimension of the node, that dimension of the
#'        image is scaled down to fit the node. As with the case of
#'        expansion, if  \code{imagescale=true}, width and height are
#'        scaled uniformly.
#'
#' @param label Type: [lblString]. Default: [].
#'
#'        Text label attached to objects.
#'        If a node's shape is record, then the label can
#'        have a special format
#'        which describes the record layout.
#'        
#'        Note that a node's default label is "", so the node's name or ID becomes
#'        its label. Technically, a node's name can be an HTML string but this will not
#'        mean that the node's label will be interpreted as an HTML-like label. This is
#'        because the node's actual label is an ordinary string, which will be replaced 
#'        by the raw bytes stored in the node's name.
#'        To get an HTML-like label, the label attribute value itself must be an HTML string.
#'
#' @param labelloc Type: [string]. Default: ["t"(clusters)/"b"(root graphs)/"c"(nodes)].
#'
#'        Vertical placement of labels for nodes, root graphs and clusters.
#'        
#'        For graphs and clusters, only "t" and "b" are allowed, corresponding
#'        to placement at the top and bottom, respectively.
#'        By default, root
#'        graph labels go on the bottom and cluster labels go on the top.
#'        Note that a subgraph inherits attributes from its parent. Thus, if
#'        the root graph sets \code{labelloc} to "b", the subgraph inherits
#'        this value.
#'        
#'        For nodes, this attribute is used only when the height of the node
#'        is larger than the height of its label. 
#'        If \code{labelloc} is set to "t", "c", or "b", the label is aligned
#'        with the top, centered, or aligned with the bottom of the node, respectively.
#'        In the default case, the label is vertically centered.
#'
#' @param layer Type: [layerRange]. Default: [""].
#'
#'        Specifies layers in which the node, edge or cluster is present.
#'
#' @param margin Type: [double/point]. Default: [].
#'
#'        For graphs, this sets x and y margins of canvas, in inches. If the margin
#'        is a single double, both margins are set equal to the given value.
#'        
#'        Note that the margin is not part of the drawing but just empty space
#'        left around the drawing. It basically corresponds to a translation of
#'        drawing, as would be necessary to center a drawing on a page. Nothing
#'        is actually drawn in the margin. To actually extend the background of
#'        a drawing, see the pad attribute.
#'        
#'        For clusters, this specifies the space between the nodes in the cluster and
#'        the cluster bounding box. By default, this is 8 points.
#'        
#'        For nodes, this attribute specifies space left around the node's label.
#'        By default, the value is \code{0.11,0.055}.
#'
#' @param nojustify Type: [bool]. Default: [false].
#'
#'        By default, the justification of multi-line labels is done within the
#'        largest context that makes sense. Thus, in the label of a polygonal
#'        node, a left-justified line will align with the left side of the node
#'        (shifted by the prescribed margin).
#'        In record nodes, left-justified
#'        line will line up with the left side of the enclosing column of fields.
#'        If \code{nojustify} is \code{"true"}, multi-line labels will be justified
#'        in the context of itself. For example, if the attribute is set,
#'        the first label line is long, and the second is shorter and left-justified,
#'        the second will align with the left-most character in the first line,
#'        regardless of  how large the node might be.
#'
#' @param ordering Type: [string]. Default: [""].
#'
#'        If the value of the attribute is "out", then
#'        the outedges of a node, that is, edges with the node as its tail node,
#'        must appear
#'        left-to-right in the same order in which they are defined in
#'        the input.
#'        If the value of the attribute is "in", then
#'        the inedges of a node must appear
#'        left-to-right in the same order in which they are defined in
#'        the input.
#'        If defined as a graph or subgraph attribute, the value is applied to all nodes
#'        in the graph or subgraph. Note that the graph attribute takes
#'        precedence over the node attribute.
#'
#' @param orientation Type: [double/string]. Default: [0.0/""].
#'
#'        When used on nodes: Angle, in degrees, used to rotate polygon node shapes. For any number of polygon sides, 0 degrees rotation results in a flat base.
#'        
#'        When used on graphs: If "[lL]*", set graph orientation to landscape.
#'        Used only if \code{rotate} is not defined.
#'
#' @param penwidth Type: [double]. Default: [1.0].
#'
#'        Specifies the width of the pen, in points, used to draw lines and curves,
#'        including the boundaries of edges and clusters. The value is inherited
#'        by subclusters.
#'        It has no effect on text.
#'        
#'        Previous to 31 January 2008, the effect of \code{penwidth=W}
#'        was achieved by including \code{setlinewidth(W)}
#'        as part of a \code{style} specification.
#'        If both are used, \code{penwidth} will be used.
#'
#' @param peripheries Type: [int]. Default: [shape default(nodes)/1(clusters)].
#'
#'        Set number of peripheries used in polygonal shapes and cluster
#'        boundaries. Note that
#'        user-defined shapes are treated as a
#'        form of box shape, so the default
#'        peripheries value is 1 and the user-defined shape will be drawn in
#'        a bounding rectangle. Setting \code{peripheries=0} will turn this off.
#'        Also, 1 is the maximum peripheries value for clusters.
#'
#' @param pin Type: [bool]. Default: [false].
#'
#'        If true and the node has a pos attribute on input, neato or fdp prevents the
#'        node from moving from the input position. This property can also be specified
#'        in the pos attribute itself (cf. the point type).
#'        
#'        \code{Note:} Due to an artifact of the implementation, 
#'        previous to 27 Feb 2014, final coordinates
#'        are translated to the origin. Thus, if you look at the output coordinates
#'        given in the (x)dot or plain format, pinned nodes will not have the same
#'        output coordinates as were given on input. If this is important, a
#'        simple workaround is to maintain the coordinates of a pinned node. The vector
#'        difference between the old and new coordinates will give the translation,
#'        which can then be subtracted from all of the appropriate coordinates.
#'        
#'        After 27 Feb 2014, this translation can be avoided in neato by setting the 
#'        notranslate to TRUE. However, if the graph
#'        specifies node overlap removal or a change in 
#'        aspect ratio, node coordinates may still change. 
#'
#' @param pos Type: [point/splineType]. Default: [NA].
#'
#'        Position of node, or spline control points.
#'        For nodes, the position indicates the center of the node.
#'        On output, the coordinates are in points.
#'        
#'        In neato and fdp, pos can be used to set the initial position of a node.
#'        By default, the coordinates are assumed to be in inches. However, the
#'        -s command line flag can be used to specify
#'        different units. As the output coordinates are in points, 
#'        feeding the output of a graph laid out by a Graphviz program into
#'        neato or fdp will almost always require the -s flag.
#'        
#'        When the -n command line flag is used with
#'        neato, it is assumed the positions have been set by one of the layout
#'        programs, and are therefore in points. Thus, \code{neato -n} can accept
#'        input correctly without requiring a \code{-s} flag and, in fact,
#'        ignores any such flag.
#'
#' @param rects Type: [rect]. Default: [NA].
#'
#'        Rectangles for fields of records, in points.
#'
#' @param regular Type: [bool]. Default: [false].
#'
#'        If true, force polygon to be regular, i.e., the vertices of the
#'        polygon will lie on a circle whose center is the center of the node.
#'
#' @param root Type: [string/bool]. Default: [(graphs)/false(nodes)].
#'
#'        This specifies nodes to be used as the center of the
#'        layout and the root of the generated spanning tree. As a graph attribute,
#'        this gives the name of the node. As a node attribute, it
#'        specifies that the node should be used as a central node. In twopi,
#'        this will actually be the central node. In circo, the block containing
#'        the node will be central in the drawing of its connected component.
#'        If not defined,
#'        twopi will pick a most central node, and circo will pick a random node.
#'        
#'        If the root attribute is defined as the empty string, twopi will reset it
#'        to name of the node picked as the root node.
#'        
#'        For twopi, it is possible to have multiple roots, presumably one for each
#'        component. If more than one node in a component is marked as the root, twopi
#'        will pick one.
#'
#' @param samplepoints Type: [int]. Default: [8(output)/20(overlap and image maps)].
#'
#'        If the input graph defines the \code{vertices}
#'        attribute, and output is dot or xdot, this gives
#'        the number of points used for a node whose shape is a circle or ellipse.
#'        It plays the same role in neato, when adjusting the layout to avoid
#'        overlapping nodes, and in image maps.
#'
#' @param shape Type: [shape]. Default: [ellipse].
#'
#'        Set the shape of a node.
#'
#' @param shapefile Type: [string]. Default: [""].
#'
#'        (Deprecated) If defined, shapefile specifies a file containing user-supplied node content.
#'        The \code{shape} of the node is set to box.
#'        The image in the shapefile must be
#'        rectangular. The image formats supported as well as the precise semantics of
#'        how the file is used depends on the
#'        output format.  For further details, see
#'        Image Formats and
#'        External PostScript files.
#'        
#'        There is one exception to this usage.
#'        If \code{shape} is set to "epsf", shapefile gives
#'        a filename containing a definition of the node in PostScript.
#'        The graphics defined must be contain all of the
#'        node content, including any desired boundaries.
#'        For further details, see
#'        
#'        External PostScript files.
#'
#' @param showboxes Type: [int]. Default: [0].
#'
#'        Print guide boxes in PostScript at the beginning of
#'        routesplines if 1, or at the end if 2. (Debugging, TB mode only!)
#'
#' @param sides Type: [int]. Default: [4].
#'
#'        Number of sides if \code{shape}=polygon.
#'
#' @param skew Type: [double]. Default: [0.0].
#'
#'        Skew factor for \code{shape}=polygon. Positive values
#'        skew top of polygon to right; negative to left.
#'
#' @param sortv Type: [int]. Default: [0].
#'
#'        If packmode indicates an array packing, 
#'        this attribute specifies an
#'        insertion order among the components, with smaller values inserted first.
#'
#' @param style Type: [style]. Default: [""].
#'
#'        Set style information for components of the graph. For cluster subgraphs, if \code{style="filled"}, the
#'        cluster box's background is filled.
#'        
#'        If the default style attribute has been set for a component, an individual component can use
#'        \code{style=""} to revert to the normal default. For example, if the graph has
#'        
#'        \code{edge [style="invis"]}
#'        
#'        making all edges invisible, a specific edge can overrride this via:
#'        
#'        \code{a -> b [style=""]}
#'        
#'        Of course, the component can also explicitly set its \code{style} attribute to the desired value.
#'
#' @param target Type: [escString/string]. Default: [].
#'
#'        If the object has a URL, this attribute determines which window
#'        of the browser is used for the URL.
#'        See W3C documentation.
#'
#' @param tooltip Type: [escString]. Default: [""].
#'
#'        Tooltip annotation attached to the node or edge. If unset, Graphviz
#'        will use the object's label if defined.
#'        Note that if the label is a record specification or an HTML-like
#'        label, the resulting tooltip may be unhelpful. In this case, if
#'        tooltips will be generated, the user should set a \code{tooltip}
#'        attribute explicitly.
#'
#' @param vertices Type: [pointList]. Default: [NA].
#'
#'        If the input graph defines this attribute, the node is polygonal,
#'        and output is dot or xdot, this attribute provides the
#'        coordinates of the vertices of the node's polygon, in inches.
#'        If the node is an ellipse or circle, the
#'        samplepoints attribute affects
#'        the output.
#'
#' @param width Type: [double]. Default: [0.75].
#'
#'        Width of node, in inches. This is taken as the initial, minimum width
#'        of the node. If \code{fixedsize} is true, this
#'        will be the final width of the node. Otherwise, if the node label
#'        requires more width to fit, the node's width will be increased to
#'        contain the label. Note also that, if the output format is dot, the
#'        value given to \code{width} will be the final value.
#'        
#'        If the node shape is regular, the width and height are made identical.
#'        In this case, if either the width or the height is set explicitly,
#'        that value is used.
#'        In this case, if both the width or the height are set explicitly,
#'        the maximum of the two values is used.
#'        If neither is set explicitly, the minimum of the two default values
#'        is used.
#'
#' @param xlabel Type: [lblString]. Default: [""].
#'
#'        External label for a node or edge. For nodes, the label will be placed outside of the
#'        node but near it. For edges, the label will be placed near the center of the edge.
#'        This can be useful in dot to avoid the occasional problem when the use of edge labels
#'        distorts the layout. For other layouts, the xlabel attribute can be viewed as a synonym
#'        for the \code{label} attribute.
#'        
#'        These labels are added after all nodes and edges have been placed. The labels will be placed
#'        so that they do not overlap any node or label. This means it may not be possible to place all
#'        of them. To force placing all of them, use the  \code{forcelabels} attribute.
#'
#' @param xlp Type: [point]. Default: [NA].
#'
#'        Position of an exterior label, in points.
#'        The position indicates the center of the label.
#'
#' @param z Type: [double]. Default: [0.0].
#'
#'        \code{Deprecated:}Use \code{pos} attribute, along
#'        with \code{dimen} and/or \code{dim}
#'        to specify dimensions.
#'        
#'        Provides z coordinate value for 3D layouts and displays. If the
#'        graph has \code{dim} set to 3 (or more),
#'        neato will use a node's \code{z} value
#'        for the z coordinate of its initial position if
#'        its \code{pos} attribute is also defined.
#'        
#'        Even if no \code{z} values are specified in the input, it is necessary to
#'        declare a \code{z} attribute for nodes, e.g, using \code{node[z=""]}
#'        in order to get z values on output.
#'        Thus, setting \code{dim=3} but not declaring \code{z} will
#'        cause \code{neato -Tvrml} to
#'        layout the graph in 3D but project the layout onto the xy-plane
#'        for the rendering. If the \code{z} attribute is declared, the final rendering
#'        will be in 3D.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_node_attr <- function(d, 
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
  assert_named(att)
  
  d$node_att <- modifyList(d$node_att, att)
  
  d
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set global edge attributes
#' 
#' @inheritParams add_graph_attr
#' @param URL Type: [escString]. Default: [].
#'
#'        Hyperlinks incorporated into device-dependent output.
#'        At present, used in ps2, cmap, i*map and svg formats.
#'        For all these formats, URLs can be attached to nodes, edges and
#'        clusters. URL attributes can also be attached to the root graph in ps2,
#'        cmap and i*map formats. This serves as the base URL for relative URLs in the
#'        former, and as the default image map file in the latter.
#'        
#'        For svg, cmapx and imap output, the active area for a node is its
#'        visible image.
#'        For example, an unfilled
#'        node with no drawn boundary will only be active on its label.
#'        For other output, the active area is its bounding box.
#'        The active area for a cluster is its bounding box.
#'        For edges, the active areas are small circles where the edge contacts its head
#'        and tail nodes. In addition, for svg, cmapx and imap, the active area
#'        includes a thin polygon approximating the edge. The circles may
#'        overlap the related node, and the edge URL dominates.
#'        If the edge has a label, this will also be active.
#'        Finally, if the edge has a head or tail label, this will also be active.
#'        
#'        Note that, for edges, the attributes headURL,
#'        tailURL, labelURL and
#'        edgeURL allow control of various parts of an
#'        edge.
#'        Also note that, if active areas of two edges overlap, it is unspecified
#'        which area dominates.
#'
#' @param arrowhead Type: [arrowType]. Default: [normal].
#'
#'        Style of arrowhead on the head node of an edge.
#'        This will only appear if the dir attribute
#'        is "forward" or "both".
#'        See the limitation.
#'
#' @param arrowsize Type: [double]. Default: [1.0].
#'
#'        Multiplicative scale factor for arrowheads.
#'
#' @param arrowtail Type: [arrowType]. Default: [normal].
#'
#'        Style of arrowhead on the tail node of an edge.
#'        This will only appear if the dir attribute
#'        is "back" or "both".
#'        See the limitation.
#'
#' @param class Type: [string]. Default: [""].
#'
#'        Classnames to attach to the node, edge, graph, or cluster's SVG element.
#'        Combine with stylesheet for styling SVG output
#'        using CSS classnames.
#'        
#'        Multiple space-separated classes are supported.
#'        
#'        
#'        digraph G \{
#'          graph [class="cats"];
#'        
#'          subgraph cluster_big \{
#'            graph [class="big_cats"];
#'        
#'            "Lion" [class="yellow social"];
#'            "Snow Leopard" [class="white solitary"];
#'          \};
#'        \}
#'        
#'
#' @param color Type: [color/colorList]. Default: [black].
#'
#'        Basic drawing color for graphics, not text. For the latter, use the
#'        fontcolor attribute.
#'        
#'        For edges, the value
#'        can either be a single color or a colorList.
#'        In the latter case, if colorList has no fractions,
#'        the edge is drawn using parallel splines or lines,
#'        one for each color in the list, in the order given.
#'        The head arrow, if any, is drawn using the first color in the list,
#'        and the tail arrow, if any, the second color. This supports the common
#'        case of drawing opposing edges, but using parallel splines instead of
#'        separately routed multiedges. 
#'        If any fraction is used, the colors are drawn in series, with each color
#'        being given roughly its specified fraction of the edge.
#'        For example, the graph
#'        
#'        digraph G \{
#'          a -> b [dir=both color="red:blue"]
#'          c -> d [dir=none color="green:red;0.25:blue"]
#'        \}
#'        
#'        yields
#'        
#'
#' @param colorscheme Type: [string]. Default: [""].
#'
#'        This attribute specifies a color scheme namespace. If defined, it specifies
#'        the context for interpreting color names. In particular, if a
#'        color value has form \code{"xxx"} or \code{"//xxx"},
#'        then the
#'        color \code{xxx} will be evaluated according to the current color scheme.
#'        If no color scheme is set, the standard X11 naming is used.
#'        For example, if \code{colorscheme=bugn9}, then \code{color=7}
#'        is interpreted as \code{"/bugn9/7"}.
#'
#' @param comment Type: [string]. Default: [""].
#'
#'        Comments are inserted into output. Device-dependent
#'
#' @param constraint Type: [bool]. Default: [true].
#'
#'        If false, the edge is not used in ranking the nodes. For example,
#'        in the graph
#'        
#'        digraph G \{
#'          a -> c;
#'          a -> b;
#'          b -> c [constraint=false];
#'        \}
#'        
#'        the edge b -> c does not add a constraint during rank
#'        assignment, so the only constraints are that a be above b and c,
#'        yielding the graph:
#'        
#'
#' @param decorate Type: [bool]. Default: [false].
#'
#'        If true, attach edge label to edge by a 2-segment
#'        polyline, underlining the label, then going to the closest point of spline.
#'
#' @param dir Type: [dirType]. Default: [forward(directed)/none(undirected)].
#'
#'        Set edge type for drawing arrowheads. This indicates which ends of the
#'        edge should be decorated with an arrowhead. The actual style of the
#'        arrowhead can be specified using the arrowhead
#'        and arrowtail attributes.
#'        See limitation.
#'
#' @param edgeURL Type: [escString]. Default: [""].
#'
#'        If \code{edgeURL} is defined, this is the link used for the non-label
#'        parts of an edge. This value overrides any URL
#'        defined for the edge.
#'        Also, this value is used near the head or tail node unless overridden
#'        by a headURL or tailURL value,
#'        respectively.
#'        See limitation.
#'
#' @param edgehref Type: [escString]. Default: [""].
#'
#'        Synonym for edgeURL.
#'
#' @param edgetarget Type: [escString]. Default: [].
#'
#'        If the edge has a URL or edgeURL
#'        attribute, this attribute determines which window of the
#'        browser is used
#'        for the URL attached to the non-label part of the edge.
#'        Setting it to "_graphviz" will open a new window if it
#'        doesn't already exist, or reuse it if it does.
#'        If undefined, the value of the target is used.
#'
#' @param edgetooltip Type: [escString]. Default: [""].
#'
#'        Tooltip annotation attached to the non-label part of an edge.
#'        This is used only if the edge has a URL
#'        or edgeURL attribute.
#'
#' @param fillcolor Type: [color/colorList]. Default: [lightgrey(nodes)/black(clusters)].
#'
#'        Color used to fill the background of a node or cluster
#'        assuming style=filled, or a filled arrowhead.
#'        If \code{fillcolor} is not defined, color is
#'        used. (For clusters, if \code{color} is not defined,
#'        bgcolor is used.) If this is not defined,
#'        the default is used, except for
#'        \code{shape}=point or when the output
#'        format is MIF,
#'        which use black by default.
#'        
#'        If the value is a colorList, a gradient fill is
#'        used. By default, this is a linear fill; setting \code{style=radial} will
#'        cause a radial fill. At present, only two colors are used. If the second
#'        color (after a colon) is missing, the default color is used for it.
#'        See also the gradientangle attribute
#'        for setting the gradient angle. 
#'        
#'        Note that a cluster inherits the root graph's attributes if defined.
#'        Thus, if the root graph has defined a \code{fillcolor}, this will override a
#'        \code{color} or \code{bgcolor} attribute set for the cluster.
#'
#' @param fontcolor Type: [color]. Default: [black].
#'
#'        Color used for text.
#'
#' @param fontname Type: [string]. Default: ["Times-Roman"].
#'
#'        Font used for text. This very much depends on the output format and, for
#'        non-bitmap output such as PostScript or SVG, the availability of the font
#'        when the graph is displayed or printed. As such, it is best to rely on
#'        font faces that are generally available, such as Times-Roman, Helvetica or
#'        Courier.
#'        
#'        How font names are resolved also depends on the underlying library that handles
#'        font name resolution.
#'        If Graphviz was built using the
#'        fontconfig library, 
#'        the latter library will be used to search for the font. 
#'        See the commands \code{fc-list}, \code{fc-match} and the other fontconfig commands for how
#'        names are resolved and which fonts are available.
#'        Other systems may provide their own font package, such as Quartz for OS X.
#'        
#'        Note that various font attributes, such as weight and slant, can be built into the
#'        font name. Unfortunately, the syntax varies depending on which font system is dominant. 
#'        Thus, using \code{fontname="times bold italic"} will produce a bold, slanted Times font 
#'        using Pango, the usual main font library. Alternatively,  
#'        \code{fontname="times:italic"} will produce a slanted
#'        Times font from fontconfig, while \code{fontname="times-bold"} will resolve to a bold
#'        Times using Quartz. You will need
#'        to ascertain which package is used by your Graphviz system and refer to the relevant
#'        documentation.
#'        
#'        If Graphviz is not built with a high-level font library, \code{fontname} will be
#'        considered the name of a Type 1 or True Type font file.
#'        If you specify \code{fontname=schlbk}, the tool will look for a
#'        file named  \code{schlbk.ttf} or \code{schlbk.pfa} or \code{schlbk.pfb}
#'        in one of the directories specified by
#'        the fontpath attribute.
#'        The lookup does support various aliases for the common fonts.
#'
#' @param fontsize Type: [double]. Default: [14.0].
#'
#'        Font size, in points, used for text.
#'
#' @param headURL Type: [escString]. Default: [""].
#'
#'        If \code{headURL} is defined, it is
#'        output as part of the head label of the edge.
#'        Also, this value is used near the head node, overriding any
#'        URL value.
#'        See limitation.
#'
#' @param headclip Type: [bool]. Default: [true].
#'
#'        If true, the head of an edge is clipped to the boundary of the head node;
#'        otherwise, the end of the edge goes to the center of the node, or the
#'        center of a port, if applicable.
#'
#' @param headhref Type: [escString]. Default: [""].
#'
#'        Synonym for headURL.
#'
#' @param headlabel Type: [lblString]. Default: [""].
#'
#'        Text label to be placed near head of edge.
#'        See limitation.
#'
#' @param head_lp Type: [point]. Default: [NA].
#'
#'        Position of an edge's head label, in points.
#'        The position indicates the center of the label.
#'
#' @param headport Type: [portPos]. Default: [center].
#'
#'        Indicates where on the head node to attach the head of the edge.
#'        In the default case, the edge is aimed towards the center of the node,
#'        and then clipped at the node boundary.
#'        See limitation.
#'
#' @param headtarget Type: [escString]. Default: [].
#'
#'        If the edge has a headURL,
#'        this attribute determines which window of the
#'        browser is used
#'        for the URL. Setting it to "_graphviz" will open a new window if it
#'        doesn't already exist, or reuse it if it does.
#'        If undefined, the value of the target is used.
#'
#' @param headtooltip Type: [escString]. Default: [""].
#'
#'        Tooltip annotation attached to the head of an edge. This is used only
#'        if the edge has a headURL attribute.
#'
#' @param href Type: [escString]. Default: [""].
#'
#'        Synonym for URL.
#'
#' @param id Type: [escString]. Default: [""].
#'
#'        Allows the graph author to provide an id for graph objects which is to be included in the output.
#'        Normal "&#92;N", "&#92;E", "&#92;G" substitutions are applied.
#'        If provided, it is the responsibility of the provider to keep
#'        its values sufficiently unique for its intended downstream use.
#'        Note, in particular, that "&#92;E" does not provide a unique id for multi-edges.
#'        If no id attribute is provided, then a unique internal id is used. However, 
#'        this value is unpredictable by the graph writer.
#'        An externally provided id is not used internally.
#'        
#'        If the graph provides an id attribute, this will be used as a prefix for
#'        internally generated attributes. By making these distinct, the user
#'        can include multiple image maps in the same document.
#'
#' @param label Type: [lblString]. Default: [].
#'
#'        Text label attached to objects.
#'        If a node's shape is record, then the label can
#'        have a special format
#'        which describes the record layout.
#'        
#'        Note that a node's default label is "", so the node's name or ID becomes
#'        its label. Technically, a node's name can be an HTML string but this will not
#'        mean that the node's label will be interpreted as an HTML-like label. This is
#'        because the node's actual label is an ordinary string, which will be replaced 
#'        by the raw bytes stored in the node's name.
#'        To get an HTML-like label, the label attribute value itself must be an HTML string.
#'
#' @param labelURL Type: [escString]. Default: [""].
#'
#'        If \code{labelURL} is defined, this is the link used for the label
#'        of an edge. This value overrides any URL
#'        defined for the edge.
#'
#' @param labelangle Type: [double]. Default: [-25.0].
#'
#'        This, along with labeldistance, determine
#'        where the
#'        headlabel (taillabel) are placed with respect to the head (tail)
#'        in polar coordinates. The origin in the coordinate system is
#'        the point where the edge touches the node. The ray of 0 degrees
#'        goes from the origin back along the edge, parallel to the edge
#'        at the origin.
#'        
#'        The angle, in degrees, specifies the rotation from the 0 degree ray,
#'        with positive angles moving counterclockwise and negative angles
#'        moving clockwise.
#'
#' @param labeldistance Type: [double]. Default: [1.0].
#'
#'        Multiplicative scaling factor adjusting the distance that
#'        the headlabel(taillabel) is from the head(tail) node.
#'        The default distance is 10 points. See labelangle
#'        for more details.
#'
#' @param labelfloat Type: [bool]. Default: [false].
#'
#'        If true, allows edge labels to be less constrained in position.
#'        In particular, it may appear on top of other edges.
#'
#' @param labelfontcolor Type: [color]. Default: [black].
#'
#'        Color used for headlabel and taillabel.
#'        If not set, defaults to edge's fontcolor.
#'
#' @param labelfontname Type: [string]. Default: ["Times-Roman"].
#'
#'        Font used for headlabel and taillabel.
#'        If not set, defaults to edge's fontname.
#'
#' @param labelfontsize Type: [double]. Default: [14.0].
#'
#'        Font size, in points, used for headlabel and taillabel.
#'        If not set, defaults to edge's fontsize.
#'
#' @param labelhref Type: [escString]. Default: [""].
#'
#'        Synonym for labelURL.
#'
#' @param labeltarget Type: [escString]. Default: [].
#'
#'        If the edge has a URL or labelURL
#'        attribute, this attribute determines which window of the
#'        browser is used
#'        for the URL attached to the label.
#'        Setting it to "_graphviz" will open a new window if it
#'        doesn't already exist, or reuse it if it does.
#'        If undefined, the value of the target is used.
#'
#' @param labeltooltip Type: [escString]. Default: [""].
#'
#'        Tooltip annotation attached to label of an edge.
#'        This is used only if the edge has a URL
#'        or labelURL attribute.
#'
#' @param layer Type: [layerRange]. Default: [""].
#'
#'        Specifies layers in which the node, edge or cluster is present.
#'
#' @param len Type: [double]. Default: [1.0(neato)/0.3(fdp)].
#'
#'        Preferred edge length, in inches.
#'
#' @param lhead Type: [string]. Default: [""].
#'
#'        Logical head of an edge. When \code{compound} is true,
#'        if \code{lhead} is defined and is the name of a cluster containing
#'        the real head,
#'        the edge is clipped to the boundary of the cluster.
#'        See limitation.
#'
#' @param lp Type: [point]. Default: [NA].
#'
#'        Label position, in points.
#'        The position indicates the center of the label.
#'
#' @param ltail Type: [string]. Default: [""].
#'
#'        Logical tail of an edge. When \code{compound} is true,
#'        if \code{ltail} is defined and is the name of a cluster
#'        containing the real tail,
#'        the edge is clipped to the boundary of the cluster.
#'        See limitation.
#'
#' @param minlen Type: [int]. Default: [1].
#'
#'        Minimum edge length (rank difference between head and tail).
#'
#' @param nojustify Type: [bool]. Default: [false].
#'
#'        By default, the justification of multi-line labels is done within the
#'        largest context that makes sense. Thus, in the label of a polygonal
#'        node, a left-justified line will align with the left side of the node
#'        (shifted by the prescribed margin).
#'        In record nodes, left-justified
#'        line will line up with the left side of the enclosing column of fields.
#'        If \code{nojustify} is \code{"true"}, multi-line labels will be justified
#'        in the context of itself. For example, if the attribute is set,
#'        the first label line is long, and the second is shorter and left-justified,
#'        the second will align with the left-most character in the first line,
#'        regardless of  how large the node might be.
#'
#' @param penwidth Type: [double]. Default: [1.0].
#'
#'        Specifies the width of the pen, in points, used to draw lines and curves,
#'        including the boundaries of edges and clusters. The value is inherited
#'        by subclusters.
#'        It has no effect on text.
#'        
#'        Previous to 31 January 2008, the effect of \code{penwidth=W}
#'        was achieved by including \code{setlinewidth(W)}
#'        as part of a \code{style} specification.
#'        If both are used, \code{penwidth} will be used.
#'
#' @param pos Type: [point/splineType]. Default: [NA].
#'
#'        Position of node, or spline control points.
#'        For nodes, the position indicates the center of the node.
#'        On output, the coordinates are in points.
#'        
#'        In neato and fdp, pos can be used to set the initial position of a node.
#'        By default, the coordinates are assumed to be in inches. However, the
#'        -s command line flag can be used to specify
#'        different units. As the output coordinates are in points, 
#'        feeding the output of a graph laid out by a Graphviz program into
#'        neato or fdp will almost always require the -s flag.
#'        
#'        When the -n command line flag is used with
#'        neato, it is assumed the positions have been set by one of the layout
#'        programs, and are therefore in points. Thus, \code{neato -n} can accept
#'        input correctly without requiring a \code{-s} flag and, in fact,
#'        ignores any such flag.
#'
#' @param samehead Type: [string]. Default: [""].
#'
#'        Edges with the same head and the same \code{samehead} value are aimed
#'        at the same point on the head. This has no effect on loops.
#'        Each node can have at most 5 unique samehead values.
#'        See limitation.
#'
#' @param sametail Type: [string]. Default: [""].
#'
#'        Edges with the same tail and the same \code{sametail} value are aimed
#'        at the same point on the tail. This has no effect on loops.
#'        Each node can have at most 5 unique sametail values.
#'        See limitation.
#'
#' @param showboxes Type: [int]. Default: [0].
#'
#'        Print guide boxes in PostScript at the beginning of
#'        routesplines if 1, or at the end if 2. (Debugging, TB mode only!)
#'
#' @param style Type: [style]. Default: [""].
#'
#'        Set style information for components of the graph. For cluster subgraphs, if \code{style="filled"}, the
#'        cluster box's background is filled.
#'        
#'        If the default style attribute has been set for a component, an individual component can use
#'        \code{style=""} to revert to the normal default. For example, if the graph has
#'        
#'        \code{edge [style="invis"]}
#'        
#'        making all edges invisible, a specific edge can overrride this via:
#'        
#'        \code{a -> b [style=""]}
#'        
#'        Of course, the component can also explicitly set its \code{style} attribute to the desired value.
#'
#' @param tailURL Type: [escString]. Default: [""].
#'
#'        If \code{tailURL} is defined, it is
#'        output as part of the tail label of the edge.
#'        Also, this value is used near the tail node, overriding any
#'        URL value.
#'        See limitation.
#'
#' @param tailclip Type: [bool]. Default: [true].
#'
#'        If true, the tail of an edge is clipped to the boundary of the tail node;
#'        otherwise, the end of the edge goes to the center of the node, or the
#'        center of a port, if applicable.
#'
#' @param tailhref Type: [escString]. Default: [""].
#'
#'        Synonym for tailURL.
#'
#' @param taillabel Type: [lblString]. Default: [""].
#'
#'        Text label to be placed near tail of edge.
#'        See limitation.
#'
#' @param tailport Type: [portPos]. Default: [center].
#'
#'        Indicates where on the tail node to attach the tail of the edge.
#'        See limitation.
#'
#' @param tail_lp Type: [point]. Default: [NA].
#'
#'        Position of an edge's tail label, in points.
#'        The position indicates the center of the label.
#'
#' @param tailtarget Type: [escString]. Default: [].
#'
#'        If the edge has a tailURL,
#'        this attribute determines which window of the
#'        browser is used
#'        for the URL. Setting it to "_graphviz" will open a new window if it
#'        doesn't already exist, or reuse it if it does.
#'        If undefined, the value of the target is used.
#'
#' @param tailtooltip Type: [escString]. Default: [""].
#'
#'        Tooltip annotation attached to the tail of an edge. This is used only
#'        if the edge has a tailURL attribute.
#'
#' @param target Type: [escString/string]. Default: [].
#'
#'        If the object has a URL, this attribute determines which window
#'        of the browser is used for the URL.
#'        See W3C documentation.
#'
#' @param tooltip Type: [escString]. Default: [""].
#'
#'        Tooltip annotation attached to the node or edge. If unset, Graphviz
#'        will use the object's label if defined.
#'        Note that if the label is a record specification or an HTML-like
#'        label, the resulting tooltip may be unhelpful. In this case, if
#'        tooltips will be generated, the user should set a \code{tooltip}
#'        attribute explicitly.
#'
#' @param weight Type: [int/double]. Default: [1].
#'
#'        Weight of edge. In dot, the heavier the weight, the shorter,
#'        straighter and more vertical the edge is.
#'        \code{N.B.} Weights in dot must be integers.
#'        For twopi, a weight of 0 indicates the edge should not be used in constructing a
#'        spanning tree from the root.
#'        For other layouts, a larger weight encourages the layout to 
#'        make the edge length closer to that specified by the 
#'        len attribute.
#'
#' @param xlabel Type: [lblString]. Default: [""].
#'
#'        External label for a node or edge. For nodes, the label will be placed outside of the
#'        node but near it. For edges, the label will be placed near the center of the edge.
#'        This can be useful in dot to avoid the occasional problem when the use of edge labels
#'        distorts the layout. For other layouts, the xlabel attribute can be viewed as a synonym
#'        for the \code{label} attribute.
#'        
#'        These labels are added after all nodes and edges have been placed. The labels will be placed
#'        so that they do not overlap any node or label. This means it may not be possible to place all
#'        of them. To force placing all of them, use the  \code{forcelabels} attribute.
#'
#' @param xlp Type: [point]. Default: [NA].
#'
#'        Position of an exterior label, in points.
#'        The position indicates the center of the label.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_edge_attr <- function(d, 
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
  
  # Create named list of attributes
  att      <- find_args(...)
  att$d    <- NULL
  assert_named(att)
  
  d$edge_att <- modifyList(d$edge_att, att)
  
  d
}



