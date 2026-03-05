# An R6 class for chemical entities with associated data

The class is initialised with an identifier. Chemical information is
retrieved from the internet. Additionally, it can be generated using
RDKit if RDKit and its python bindings are installed.

## Format

An [R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) generator
object

## Public fields

- `identifier`:

  (`character(1)`)  
  The identifier that was used to initiate the object, with attribute
  'source'

- `inchikey`:

  (`character(1)`)  
  InChI Key, with attribute 'source'

- `smiles`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  SMILES code(s), with attribute 'source'

- `mw`:

  (`numeric(1)`)  
  Molecular weight, with attribute 'source'

- `pubchem`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of information retrieved from PubChem

- `rdkit`:

  List of information obtained with RDKit

- `mol`:

  \<rdkit.Chem.rdchem.Mol\> object

- `svg`:

  SVG code

- `Picture`:

  Graph as a
  [grImport::Picture](https://rdrr.io/pkg/grImport/man/Picture-class.html)
  object obtained using the grImport package

- `Pict_font_size`:

  Font size as extracted from the intermediate PostScript file

- `pdf_height`:

  Height of the MediaBox in the pdf after cropping

- `p0`:

  Vapour pressure in Pa

- `cwsat`:

  Water solubility in mg/L

- `PUF`:

  Plant uptake factor

- `chyaml`:

  List of information obtained from a YAML file

- `TPs`:

  List of transformation products as chent objects

- `transformations`:

  Data frame of observed transformations

- `soil_degradation`:

  Dataframe of modelling DT50 values

- `soil_ff`:

  Dataframe of formation fractions

- `soil_sorption`:

  Dataframe of soil sorption data

## Methods

### Public methods

- [`chent$new()`](#method-chent-new)

- [`chent$try_pubchem()`](#method-chent-try_pubchem)

- [`chent$get_pubchem()`](#method-chent-get_pubchem)

- [`chent$get_rdkit()`](#method-chent-get_rdkit)

- [`chent$get_chyaml()`](#method-chent-get_chyaml)

- [`chent$add_p0()`](#method-chent-add_p0)

- [`chent$add_cwsat()`](#method-chent-add_cwsat)

- [`chent$add_PUF()`](#method-chent-add_PUF)

- [`chent$add_TP()`](#method-chent-add_TP)

- [`chent$add_transformation()`](#method-chent-add_transformation)

- [`chent$add_soil_degradation()`](#method-chent-add_soil_degradation)

- [`chent$add_soil_ff()`](#method-chent-add_soil_ff)

- [`chent$add_soil_sorption()`](#method-chent-add_soil_sorption)

- [`chent$pdf()`](#method-chent-pdf)

- [`chent$png()`](#method-chent-png)

- [`chent$emf()`](#method-chent-emf)

- [`chent$clone()`](#method-chent-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    chent$new(
      identifier,
      smiles = NULL,
      inchikey = NULL,
      pubchem = TRUE,
      pubchem_from = c("name", "smiles", "inchikey"),
      rdkit = TRUE,
      template = NULL,
      chyaml = FALSE
    )

#### Arguments

- `identifier`:

  Identifier to be stored in the object

- `smiles`:

  Optional user provided SMILES code

- `inchikey`:

  Optional user provided InChI Key

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

  Should we look for a identifier.yaml file in the working directory?

------------------------------------------------------------------------

### Method `try_pubchem()`

Try to get chemical information from PubChem

#### Usage

    chent$try_pubchem(query = self$identifier, from = "name")

#### Arguments

- `query`:

  Query string to be passed to
  [get_cid](https://docs.ropensci.org/webchem/reference/get_cid.html)

- `from`:

  Passed to
  [get_cid](https://docs.ropensci.org/webchem/reference/get_cid.html)

------------------------------------------------------------------------

### Method `get_pubchem()`

Get chemical information from PubChem for a known PubChem CID

#### Usage

    chent$get_pubchem(pubchem_cid)

#### Arguments

- `pubchem_cid`:

  CID

------------------------------------------------------------------------

### Method `get_rdkit()`

Get chemical information from RDKit if available

#### Usage

    chent$get_rdkit(template = NULL)

#### Arguments

- `template`:

  An optional SMILES code to be used as template for RDKit

------------------------------------------------------------------------

### Method `get_chyaml()`

Obtain information from a YAML file

#### Usage

    chent$get_chyaml(
      repo = c("wd", "local", "web"),
      chyaml = paste0(URLencode(self$identifier), ".yaml")
    )

#### Arguments

- `repo`:

  Should the file be looked for in the current working directory, a
  local git repository under `~/git/chyaml`, or from the web (not
  implemented).

- `chyaml`:

  The filename to be looked for

------------------------------------------------------------------------

### Method `add_p0()`

Add a vapour pressure

#### Usage

    chent$add_p0(p0, T = NA, source = NA, page = NA, remark = "")

#### Arguments

- `p0`:

  The vapour pressure in Pa

- `T`:

  Temperature

- `source`:

  An acronym specifying the source of the information

- `page`:

  The page from which the information was taken

- `remark`:

  A remark

------------------------------------------------------------------------

### Method `add_cwsat()`

Add a water solubility

#### Usage

    chent$add_cwsat(cwsat, T = NA, pH = NA, source = NA, page = NA, remark = "")

#### Arguments

- `cwsat`:

  The water solubility in mg/L

- `T`:

  Temperature

- `pH`:

  pH value

- `source`:

  An acronym specifying the source of the information

- `page`:

  The page from which the information was taken

- `remark`:

  A remark

------------------------------------------------------------------------

### Method `add_PUF()`

Add a plant uptake factor

#### Usage

    chent$add_PUF(
      PUF = 0,
      source = "focus_generic_gw_2014",
      page = 41,
      remark = "Conservative default value"
    )

#### Arguments

- `PUF`:

  The plant uptake factor, a number between 0 and 1

- `source`:

  An acronym specifying the source of the information

- `page`:

  The page from which the information was taken

- `remark`:

  A remark

------------------------------------------------------------------------

### Method `add_TP()`

Add a transformation product to the internal list

#### Usage

    chent$add_TP(x, smiles = NULL, pubchem = FALSE)

#### Arguments

- `x`:

  A chent object, or an identifier to generate a chent object

- `smiles`:

  Optional user provided SMILES code

- `pubchem`:

  Should chemical information be obtained from PubChem?

------------------------------------------------------------------------

### Method `add_transformation()`

Add a line in the internal dataframe holding observed transformations

#### Usage

    chent$add_transformation(
      study_type,
      TP_identifier,
      max_occurrence,
      remark = "",
      source = NA,
      pages = NA
    )

#### Arguments

- `study_type`:

  A characterisation of the study type

- `TP_identifier`:

  An identifier of one of the transformation products in `self$TPs`

- `max_occurrence`:

  The maximum observed occurrence of the transformation product,
  expressed as a fraction of the amount that would result from
  stochiometric transformation

- `remark`:

  A remark

- `source`:

  An acronym specifying the source of the information

- `pages`:

  The pages from which the information was taken

------------------------------------------------------------------------

### Method `add_soil_degradation()`

Add a line in the internal dataframe holding modelling DT50 values

#### Usage

    chent$add_soil_degradation(
      soils,
      DT50_mod,
      DT50_mod_ref,
      type = NA,
      country = NA,
      pH_orig = NA,
      pH_medium = NA,
      pH_H2O = NA,
      perc_OC = NA,
      temperature = NA,
      moisture = NA,
      category = "lab",
      formulation = NA,
      model = NA,
      chi2 = NA,
      remark = "",
      source,
      page = NA
    )

#### Arguments

- `soils`:

  Names of the soils

- `DT50_mod`:

  The modelling DT50 in the sense of regulatory pesticide fate modelling

- `DT50_mod_ref`:

  The normalised modelling DT50 in the sense of regulatory pesticide
  fate modelling

- `type`:

  The soil type

- `country`:

  The country (mainly for field studies)

- `pH_orig`:

  The pH stated in the study

- `pH_medium`:

  The medium in which this pH was measured

- `pH_H2O`:

  The pH extrapolated to pure water

- `perc_OC`:

  The percentage of organic carbon in the soil

- `temperature`:

  The temperature during the study in degrees Celsius

- `moisture`:

  The moisture during the study

- `category`:

  Is it a laboratory ('lab') or field study ('field')

- `formulation`:

  Name of the formulation applied, if it was not the technical active
  ingredient

- `model`:

  The degradation model used for deriving `DT50_mod`

- `chi2`:

  The relative error as defined in FOCUS kinetics

- `remark`:

  A remark

- `source`:

  An acronym specifying the source of the information

- `page`:

  The page from which the information was taken

------------------------------------------------------------------------

### Method `add_soil_ff()`

Add one or more formation fractions for degradation in soil

#### Usage

    chent$add_soil_ff(target, soils, ff = 1, remark = "", source, page = NA)

#### Arguments

- `target`:

  The identifier(s) of the transformation product

- `soils`:

  The soil name(s) in which the transformation was observed

- `ff`:

  The formation fraction(s)

- `remark`:

  A remark

- `source`:

  An acronym specifying the source of the information

- `page`:

  The page from which the information was taken

------------------------------------------------------------------------

### Method `add_soil_sorption()`

Add soil sorption data

#### Usage

    chent$add_soil_sorption(
      soils,
      Kf,
      Kfoc,
      N,
      type = NA,
      pH_orig = NA,
      pH_medium = NA,
      pH_H2O = NA,
      perc_OC = NA,
      perc_clay = NA,
      CEC = NA,
      remark = "",
      source,
      page = NA
    )

#### Arguments

- `soils`:

  Names of the soils

- `Kf`:

  The sorption constant in L/kg, either linear (then `N` is 1) or
  according to Freundlich

- `Kfoc`:

  The constant from above, normalised to soil organic carbon

- `N`:

  The Freundlich exponent

- `type`:

  The soil type

- `pH_orig`:

  The pH stated in the study

- `pH_medium`:

  The medium in which this pH was measured

- `pH_H2O`:

  The pH extrapolated to pure water

- `perc_OC`:

  The percentage of organic carbon in the soil

- `perc_clay`:

  The percentage of clay in the soil

- `CEC`:

  The cation exchange capacity

- `remark`:

  A remark

- `source`:

  An acronym specifying the source of the information

- `page`:

  The page from which the information was taken

------------------------------------------------------------------------

### Method [`pdf()`](https://rdrr.io/r/grDevices/pdf.html)

Write a PDF image of the structure

#### Usage

    chent$pdf(
      file = paste0(self$identifier, ".pdf"),
      dir = "structures/pdf",
      template = NULL
    )

#### Arguments

- `file`:

  The file to write to

- `dir`:

  The directory to write the file to

- `template`:

  An optional SMILES code to be used as template for RDKit

------------------------------------------------------------------------

### Method [`png()`](https://rdrr.io/r/grDevices/png.html)

Write a PNG image of the structure

#### Usage

    chent$png(
      file = paste0(self$identifier, ".png"),
      dir = "structures/png",
      antialias = "gray"
    )

#### Arguments

- `file`:

  The file to write to

- `dir`:

  The directory to write the file to

- `antialias`:

  Passed to [png](https://rdrr.io/r/grDevices/png.html)

------------------------------------------------------------------------

### Method `emf()`

Write an EMF image of the structure using
[emf](https://rdrr.io/pkg/devEMF/man/emf.html)

#### Usage

    chent$emf(file = paste0(self$identifier, ".emf"), dir = "structures/emf")

#### Arguments

- `file`:

  The file to write to

- `dir`:

  The directory to write the file to

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    chent$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

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

oct <- chent$new("1-octanol", smiles = "CCCCCCCCO", pubchem = FALSE)
#> Get chemical information from RDKit using user SMILES
#> CCCCCCCCO
print(oct)
#> <chent>
#> Identifier $identifier 1-octanol 
#> InChI Key $inchikey NA 
#> SMILES string $smiles:
#>        user 
#> "CCCCCCCCO" 
#> Molecular weight $mw: 130.2 
# }
```
