# An R6 class for pesticidal active ingredients and associated data

This class is derived from
[chent](https://pkgdown.jrwb.de/chents/reference/chent.md). It makes it
easy to create a
[chent](https://pkgdown.jrwb.de/chents/reference/chent.md) from the ISO
common name of a pesticide active ingredient, and additionally stores
the ISO name as well as the complete result of querying the BCPC
compendium using
[bcpc_query](https://docs.ropensci.org/webchem/reference/bcpc_query.html).

## Format

An [R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) generator
object

## Super class

[`chents::chent`](https://pkgdown.jrwb.de/chents/reference/chent.md) -\>
`pai`

## Public fields

- `iso`:

  ISO common name of the active ingredient according to ISO 1750

- `bcpc`:

  Information retrieved from the BCPC compendium available online at
  \<pesticidecompendium.bcpc.org\>

## Methods

### Public methods

- [`pai$new()`](#method-pai-new)

- [`pai$clone()`](#method-pai-clone)

Inherited methods

- [`chents::chent$add_PUF()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-add_PUF)
- [`chents::chent$add_TP()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-add_TP)
- [`chents::chent$add_cwsat()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-add_cwsat)
- [`chents::chent$add_p0()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-add_p0)
- [`chents::chent$add_soil_degradation()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-add_soil_degradation)
- [`chents::chent$add_soil_ff()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-add_soil_ff)
- [`chents::chent$add_soil_sorption()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-add_soil_sorption)
- [`chents::chent$add_transformation()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-add_transformation)
- [`chents::chent$emf()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-emf)
- [`chents::chent$get_chyaml()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-get_chyaml)
- [`chents::chent$get_pubchem()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-get_pubchem)
- [`chents::chent$get_rdkit()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-get_rdkit)
- [`chents::chent$pdf()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-pdf)
- [`chents::chent$png()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-png)
- [`chents::chent$try_pubchem()`](https://pkgdown.jrwb.de/chents/reference/chent.html#method-try_pubchem)

------------------------------------------------------------------------

### Method `new()`

Create a new pai object

#### Usage

    pai$new(
      iso,
      identifier = iso,
      smiles = NULL,
      inchikey = NULL,
      bcpc = TRUE,
      pubchem = TRUE,
      pubchem_from = "auto",
      rdkit = TRUE,
      template = NULL,
      chyaml = FALSE
    )

#### Arguments

- `iso`:

  The ISO common name to be used in the query of the BCPC compendium

- `identifier`:

  Alternative identifier used for querying pubchem

- `smiles`:

  Optional user provided SMILES code

- `inchikey`:

  Optional user provided InChI Key

- `bcpc`:

  Should the BCPC compendium be queried?

- `pubchem`:

  Should an attempt be made to retrieve chemical information from
  PubChem via the webchem package?

- `pubchem_from`:

  Possibility to select the argument that is used to query pubchem

- `rdkit`:

  Should an attempt be made to retrieve chemical information from a
  local rdkit installation via python and the reticulate package?

- `template`:

  An optional SMILES code to be used as template for RDKit

- `chyaml`:

  Should we look for a identifier.yaml file in the working

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    pai$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Don't run examples per default, as PubChem may be unavailable
# \dontrun{
atr <- pai$new("atrazine")
#> Querying BCPC for atrazine ...
#> Querying PubChem for inchikey MXWJVTOOROXGIU-UHFFFAOYSA-N ...
#> Get chemical information from RDKit using PubChem SMILES
#> CCNC1=NC(=NC(=N1)Cl)NC(C)C
print(atr)
#> <pai> with ISO common name $iso atrazine 
#> <chent>
#> Identifier $identifier atrazine 
#> InChI Key $inchikey MXWJVTOOROXGIU-UHFFFAOYSA-N 
#> SMILES string $smiles:
#>                      PubChem 
#> "CCNC1=NC(=NC(=N1)Cl)NC(C)C" 
#> Molecular weight $mw: 215.7 
#> PubChem synonyms (up to 10):
#>  [1] "atrazine"     "1912-24-9"    "Gesaprim"     "Aatrex"       "Atranex"     
#>  [6] "Atrazin"      "Oleogesaprim" "Atazinax"     "Atrasine"     "Chromozin"   
if (!is.null(atr$Picture)) {
  plot(atr)
}

# We can also define pais that are not found on the BCPC site
decanol <- pai$new("1-Decanol")
#> Querying BCPC for 1-Decanol ...
#> Common name 1-Decanol is not known at the BCPC compendium, trying PubChem
#> Querying PubChem for name 1-Decanol ...
#> Get chemical information from RDKit using PubChem SMILES
#> CCCCCCCCCCO
print(decanol)
#> <pai> without ISO common name
#> <chent>
#> Identifier $identifier 1-Decanol 
#> InChI Key $inchikey MWKFXSUHUHTGQN-UHFFFAOYSA-N 
#> SMILES string $smiles:
#>       PubChem 
#> "CCCCCCCCCCO" 
#> Molecular weight $mw: 158.3 
#> PubChem synonyms (up to 10):
#>  [1] "1-DECANOL"       "Decan-1-ol"      "Decyl alcohol"   "112-30-1"       
#>  [5] "n-Decyl alcohol" "n-Decanol"       "Capric alcohol"  "Nonylcarbinol"  
#>  [9] "Antak"           "Royaltac"       
# }
```
