context("Generation of pai objects")

glyphosate <- pai$new("glyphosate", chyaml = FALSE)

test_that("We can generate a pai object from its ISO common name", {
  expect_equivalent(glyphosate$alanwood$cas, "1071-83-6")
  expect_equivalent(glyphosate$alanwood$formula, "C3H8NO5P")
  expect_equivalent(glyphosate$alanwood$iupac_name, "N-(phosphonomethyl)glycine")
  expect_equal(names(glyphosate$identifier), "glyphosate")
  ik = "XDDAORKBJWWYJS-UHFFFAOYSA-N"
  attr(ik, "source") <- "alanwood"
  expect_equal(glyphosate$inchikey, ik)
})

test_that("RDKit information was added", {
  expect_equivalent(glyphosate$rdkit$mw, 169.073)
})

test_that("PubChem information was added via webchem", {
  expect_equivalent(round(glyphosate$mw, 2), 169.07) 
  smiles <- "C(C(=O)O)NCP(=O)(O)O"
  expect_equal(glyphosate$smiles[["PubChem_Canonical"]], smiles)
})
