test_that("a pai object is correctly generated", {
  skip_on_travis() # server certificate verification failed in curl_fetch_memory()
  {glyphosate <- pai$new("glyphosate")} |> 
    expect_message("Querying BCPC for glyphosate") |> 
    expect_message("Querying PubChem for inchikey ") |> 
    expect_message("Get chemical information from RDKit using PubChem SMILES")
  
  expect_equal(glyphosate$bcpc$cas, "1071-83-6")
  expect_equal(glyphosate$bcpc$formula, "C3H8NO5P")
  expect_equal(glyphosate$bcpc$iupac_name, "N-(phosphonomethyl)glycine")
  expect_equal(names(glyphosate$identifier), "glyphosate")
  ik = "XDDAORKBJWWYJS-UHFFFAOYSA-N"
  expect_equal(glyphosate$inchikey, structure("XDDAORKBJWWYJS-UHFFFAOYSA-N", source = c("bcpc", "pubchem")))
  expect_equal(round(glyphosate$mw, 2), structure(169.07, source = "pubchem"))
  expect_equal(glyphosate$smiles[["PubChem"]], "C(C(=O)O)NCP(=O)(O)O")
})
