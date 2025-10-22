# For manual use of this file
require(chents)
require(testthat)

context("Generation of pai objects")

test_that("a pai object is correctly generated", {
  skip_on_travis() # server certificate verification failed in curl_fetch_memory()
  glyphosate <- pai$new("glyphosate")
  
  # This did not give results at BCPC in previous times, so it was used to test
  # the corresponding warning.
  #bc <- pai$new("benzalkonium chloride") 

  expect_equivalent(glyphosate$bcpc$cas, "1071-83-6")
  expect_equivalent(glyphosate$bcpc$formula, "C3H8NO5P")
  expect_equivalent(glyphosate$bcpc$iupac_name, "N-(phosphonomethyl)glycine")
  expect_equal(names(glyphosate$identifier), "glyphosate")
  ik = "XDDAORKBJWWYJS-UHFFFAOYSA-N"
  attr(ik, "source") <- c("bcpc", "pubchem")
  expect_equal(glyphosate$inchikey, ik)
  expect_equivalent(round(glyphosate$mw, 2), 169.07)
  smiles <- "C(C(=O)O)NCP(=O)(O)O"
  expect_equal(glyphosate$smiles[["PubChem"]], smiles)
})
