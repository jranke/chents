% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chent.R
\name{draw_svg.chent}
\alias{draw_svg.chent}
\title{Draw SVG graph from a chent object using RDKit}
\usage{
draw_svg.chent(
  x,
  width = 300,
  height = 150,
  filename = paste0(names(x$identifier), ".svg"),
  subdir = "svg"
)
}
\arguments{
\item{x}{The chent object to be plotted}

\item{width}{The desired width in pixels}

\item{height}{The desired height in pixels}

\item{filename}{The filename}

\item{subdir}{The path to which the file should be written}
}
\description{
Draw SVG graph from a chent object using RDKit
}
