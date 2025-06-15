
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minigraphviz

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
[![CRAN](https://www.r-pkg.org/badges/version/minigraphviz)](https://CRAN.R-project.org/package=minigraphviz)
[![R-CMD-check](https://github.com/coolbutuseless/minigraphviz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coolbutuseless/minigraphviz/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`{minigraphviz}` is a package for building input files used by
[graphiviz](https://graphviz.org) for visualising graphs
(i.e. collections of nodes and edges)

This package is for building documents only. The rendering from the
graph specification to an image must currently be done manually -
usually with the command-line graphviz commands e.g. `dot`, `fdp`,
`neato`, etc

## Installation

<!-- This package can be installed from CRAN -->

<!-- ``` r -->

<!-- install.packages('minigraphviz') -->

<!-- ``` -->

You can install the latest development version from
[GitHub](https://github.com/coolbutuseless/minigraphviz) with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/minigraphviz')
```

<!-- Pre-built source/binary versions can also be installed from -->

<!-- [R-universe](https://r-universe.dev) -->

<!-- ``` r -->

<!-- install.packages('minigraphviz', repos = c('https://coolbutuseless.r-universe.dev', 'https://cloud.r-project.org')) -->

<!-- ``` -->

## A simple graph

``` r
library(minigraphviz)

d <- create_graph() |>
  add_node('a', color = 'green') |>
  add_edge('a', 'b', arrowhead = 'diamond')

d
```

    digraph  {
      a [color=green]
      a -> b [arrowhead=diamond]
    }

#### Rendering with `dot`

``` r
writeLines(as.character(d), "man/figures/basic.dot")
system("dot -Tpng -O man/figures/basic.dot")
```

![](man/figures/basic.dot.png)

## Visualising Lyrics

``` r
lyrics <- r"(Mary had a little lamb
its fleece was white as snow
and everywhere that Mary went
the lamb was sure to go)"

# Split lyrics into words
lyrics <- gsub("\\(|\\)|,", "", lyrics)
words <- strsplit(lyrics, "\\s+")[[1]]
words <- words[words != ""]
cols <- rainbow(length(words))

# Create the main lyrics graph
d <- create_graph(name = 'mary', rankdir = 'LR') |>
  add_node_attr(shape = 'rect')

# Create edges which link one word with the next
for (i in seq_len(length(words) - 1)) {
  d |> add_edge(words[[i]], words[[i + 1]], color = cols[i])
}

# Create a subgraph
sg <-create_graph(name = 'cluster_sheep') |>
  add_edge('Lamb', 'Sheep', label = 'growth', color = 'red')

# Add the subgraph to the main graph and link two nodes in those graphs
d <- add_subgraph(d, sg) |> 
  add_edge('lamb', 'Lamb')

# Explicitly set some parameters for the 'Mary' node
d <- d |>
  add_node("Mary", style = "filled", fillcolor = 'lightblue')


d
```

    digraph mary {
      rankdir=LR
      node [shape=rect]
      Mary [fillcolor=lightblue, style=filled]
      Mary -> had [color="#FF0000"]
      had -> a [color="#FF4600"]
      a -> little [color="#FF8B00"]
      little -> lamb [color="#FFD100"]
      lamb -> its [color="#E8FF00"]
      its -> fleece [color="#A2FF00"]
      fleece -> was [color="#5DFF00"]
      was -> white [color="#17FF00"]
      white -> as [color="#00FF2E"]
      as -> snow [color="#00FF74"]
      snow -> and [color="#00FFB9"]
      and -> everywhere [color="#00FFFF"]
      everywhere -> that [color="#00B9FF"]
      that -> Mary [color="#0074FF"]
      Mary -> went [color="#002EFF"]
      went -> the [color="#1700FF"]
      the -> lamb [color="#5D00FF"]
      lamb -> was [color="#A200FF"]
      was -> sure [color="#E800FF"]
      sure -> to [color="#FF00D1"]
      to -> go [color="#FF008B"]
      lamb -> Lamb
      subgraph cluster_sheep {
        Lamb -> Sheep [color=red, label=growth]
      }
    }

#### Rendering with `dot`

``` r
writeLines(as.character(d), "man/figures/mary.dot")
system("dot -Tpng -O man/figures/mary.dot")
```

![](man/figures/mary.dot.png)

#### Alternate rendering with `fdp` instead of `dot`

``` r
system("fdp -Tpng -oman/figures/mary2.dot.png man/figures/mary.dot")
```

![](man/figures/mary2.dot.png)
