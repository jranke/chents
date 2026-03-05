# Plot method for chent objects

Plot method for chent objects

## Usage

``` r
# S3 method for class 'chent'
plot(x, ...)
```

## Arguments

- x:

  The chent object to be plotted

- ...:

  Further arguments passed to
  [grImport::grid.picture](https://rdrr.io/pkg/grImport/man/grid.picture.html)

## Examples

``` r
# Don't run examples per default, as PubChem may be unavailable
# \dontrun{
caffeine <- chent$new("caffeine")
#> Querying PubChem for name caffeine ...
#> Get chemical information from RDKit using PubChem SMILES
#> CN1C=NC2=C1C(=O)N(C(=O)N2C)C
print(caffeine)
#> <chent>
#> Identifier $identifier caffeine 
#> InChI Key $inchikey RYYVLZVUVIJVGH-UHFFFAOYSA-N 
#> SMILES string $smiles:
#>                        PubChem 
#> "CN1C=NC2=C1C(=O)N(C(=O)N2C)C" 
#> Molecular weight $mw: 194.2 
#> PubChem synonyms (up to 10):
#>  [1] "caffeine"                "58-08-2"                
#>  [3] "Guaranine"               "1,3,7-Trimethylxanthine"
#>  [5] "Methyltheobromine"       "Theine"                 
#>  [7] "Thein"                   "Cafeina"                
#>  [9] "Caffein"                 "Cafipel"                
if (!is.null(caffeine$Picture)) {
  plot(caffeine)
}

# }
```
