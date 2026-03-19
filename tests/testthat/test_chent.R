test_that("We can initialise an object and add information", {
  oct <- chent$new("1-octanol", smiles = "CCCCCCCCO", rdkit = FALSE, pubchem = FALSE, chyaml = FALSE)
  expect_equal(oct$identifier, c(X1.octanol = "1-octanol")) # The name of the identifier is generated using make.names()
  expect_equal(oct$inchikey, structure(NA, source = "user"))
  expect_equal(oct$smiles, c(user = "CCCCCCCCO"))

  if (pc_up) {
    oct$try_pubchem() |> 
      expect_message("Querying PubChem for name 1-octanol")
    expect_equal(oct$inchikey, structure("KBPLFHHGFOOTCA-UHFFFAOYSA-N", source = "pubchem"))
    expect_equal(oct$smiles[["PubChem"]], "CCCCCCCCO")
    expect_equal(round(oct$mw, 2), structure(130.23, source = "pubchem"))
  }

  skip_if_no_rdkit()
  oct$get_rdkit() |> 
    expect_message("Get chemical information from RDKit using user SMILES")

  skip_if_not(pc_up)
  expect_snapshot(print(oct))
})
