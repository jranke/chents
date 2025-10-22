# Check if we can use RDKit
skip_if_no_rdkit <- function() {
  if (!chents:::rdkit_available) skip("RDKit is not available via reticulate")
}

test_that("We can initialise an object and add information", {
  oct <- chent$new("1-octanol", smiles = "CCCCCCCCO", rdkit = FALSE, pubchem = FALSE, chyaml = FALSE)
  expect_equal(oct$identifier, c(X1.octanol = "1-octanol")) # The name of the identifier is generated using make.names()
  expect_equal(oct$inchikey, structure(NA, source = "user"))
  expect_equal(oct$smiles, c(user = "CCCCCCCCO"))
  oct$try_pubchem() |> 
    expect_message("Querying PubChem for name 1-octanol") |> 
    expect_warning("Overwriting uninitialized InChIKey")
  expect_equal(oct$inchikey, structure("KBPLFHHGFOOTCA-UHFFFAOYSA-N", source = "pubchem"))
  expect_equal(oct$smiles[["PubChem"]], "CCCCCCCCO")
  skip_if_no_rdkit()
  oct$get_rdkit() |> 
    expect_message("Get chemical information from RDKit using user SMILES")
  expect_equal(round(oct$mw, 2), structure(130.23, source = "pubchem"))
  expect_snapshot(print(oct))
})
