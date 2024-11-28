# chents

[![Online documentation](https://img.shields.io/badge/docs-jrwb.de-blue.svg)](https://pkgdown.jrwb.de/chents/)
[![R-Universe status](https://jranke.r-universe.dev/badges/chents)](https://jranke.r-universe.dev/chents)
[![Build Status](https://app.travis-ci.com/jranke/chents.svg?token=Sq9VuYWyRz2FbBLxu6DK&branch=main)](https://app.travis-ci.com/jranke/chents)
[![codecov](https://codecov.io/github/jranke/chents/branch/main/graphs/badge.svg)](https://app.codecov.io/gh/jranke/chents) 

The R package **chents** provides some utilities for working with chemical 
entities in R.

## Features

- Some chemical information is retrieved from the PubChem website using the webchem 
package
- If Python and RDKit (> 2015.03) are installed and configured for use with
  'reticulate', some basic chemoinformatics functions some additional chemical
  information is computed and a 2D graph can be plotted
- Additional information can be read from a local .yaml file

## Installation

You can conveniently install chents from the repository kindly made available by the
R-Universe project:

```
install.packages("chents",
  repos = c("https://jranke.r-universe.dev", "https://cran.r-project.org"))
```

In order to profit from the chemoinformatics, you need to install RDKit and its
python bindings. On a Debian type Linux distribution, just use

```
sudo apt install python3-rdkit
```
If you use this package on Windows or MacOS, I would be happy to include
installation instructions here if you share them with me, e.g. via a Pull
Request.

## Configuration of the Python version to use

On Debian type Linux distributions, you can use the following line in your
global or project specific `.Rprofile` file to tell the `reticulate` package to
use the system Python version that will find the RDKit installed in the system
location.

```
Sys.setenv(RETICULATE_PYTHON="/usr/bin/python3")
```

## Examples

Some examples are available from the 
[reference on jrwb.de](https://pkgdown.jrwb.de/chents/reference). For example,
in the [example code section of the chent object docs](https://pkgdown.jrwb.de/chents/reference/chent.html#ref-examples)
you can see how to generate an R object for caffeine, show some of the information
retrieved from PubChem and plot it by virtue of RDKit.



