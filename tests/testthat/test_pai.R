# For manual use of this file
require(chents)
require(testthat)

context("Generation of pai objects")

test_that("a pai object is correctly generated from an ambiguous name, with warning", {
  expect_warning(glyphosate <- pai$new("glyphosate", chyaml = FALSE))
  expect_warning(pai$new("benzalkonium chloride", chyaml = FALSE))

  expect_equivalent(glyphosate$alanwood$cas, "1071-83-6")
  expect_equivalent(glyphosate$alanwood$formula, "C3H8NO5P")
  expect_equivalent(glyphosate$alanwood$iupac_name, "N-(phosphonomethyl)glycine")
  expect_equal(names(glyphosate$identifier), "glyphosate")
  ik = "XDDAORKBJWWYJS-UHFFFAOYSA-N"
  attr(ik, "source") <- c("alanwood", "pubchem")
  expect_equal(glyphosate$inchikey, ik)
  expect_equivalent(round(glyphosate$mw, 2), 169.07) 
  smiles <- "C(C(=O)O)[NH2+]CP(=O)(O)[O-]"
  expect_equal(glyphosate$smiles[["PubChem_Canonical"]], smiles)
})
