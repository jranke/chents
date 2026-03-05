test_that("a pai object is correctly generated", {

  # Check availability of BCPC and PubChem immediately before use
  bcpc_up <- webchem::ping_service("bcpc")
  pc_up <- webchem::ping_service("pc")

  skip_if_not(bcpc_up & pc_up)

  {glyphosate <- pai$new("glyphosate", rdkit = FALSE)} |>
    expect_message("Querying BCPC for glyphosate") |>
    expect_message("Querying PubChem for inchikey ")

  # Check BCPC results
  expect_equal(glyphosate$bcpc$cas, "1071-83-6")
  expect_equal(glyphosate$bcpc$formula, "C3H8NO5P")
  expect_equal(glyphosate$bcpc$iupac_name, "N-(phosphonomethyl)glycine")

  expect_equal(names(glyphosate$identifier), "glyphosate")

  # Check PubChem results
  ik = "XDDAORKBJWWYJS-UHFFFAOYSA-N"
  expect_equal(glyphosate$inchikey, structure("XDDAORKBJWWYJS-UHFFFAOYSA-N", source = c("bcpc", "pubchem")))
  expect_equal(round(glyphosate$mw, 2), structure(169.07, source = "pubchem"))
  expect_equal(glyphosate$smiles[["PubChem"]], "C(C(=O)O)NCP(=O)(O)O")
})
