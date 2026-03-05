# Draw SVG graph from a chent object using RDKit

Draw SVG graph from a chent object using RDKit

## Usage

``` r
draw_svg.chent(
  x,
  width = 300,
  height = 150,
  filename = paste0(names(x$identifier), ".svg"),
  subdir = "svg"
)
```

## Arguments

- x:

  The chent object to be plotted

- width:

  The desired width in pixels

- height:

  The desired height in pixels

- filename:

  The filename

- subdir:

  The path to which the file should be written
