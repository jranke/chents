context("Generation of chent objects")

# Check if we can use RDKit
skip_if_no_rdkit <- function() {
  if (!chents:::rdkit_available) skip("RDKit is not available via reticulate")
}

oct <- chent$new("1-octanol", smiles = "CCCCCCCCO", rdkit = FALSE, pubchem = FALSE, chyaml = FALSE)

test_that("We can initialise an object only with identifier and SMILES code", {
  expect_equal(oct$identifier, c(X1.octanol = "1-octanol")) # The name of the identifier is generated using make.names()
  expect_equal(oct$inchikey, structure(NA, source = "user"))
  expect_equal(oct$smiles, c(user = "CCCCCCCCO"))
})

test_that("We can generate a chent object from SMILES using RDKit", {
  skip_if_no_rdkit()
  oct$get_rdkit()
  expect_equivalent(round(oct$mw, 2), 130.23)
})

test_that("We can add information retrieved from PubChem via webchem", {
  expect_warning(oct$try_pubchem(), "Overwriting uninitialized InChIKey")
  expect_equivalent(round(oct$mw, 2), 130.23)
  ik = "KBPLFHHGFOOTCA-UHFFFAOYSA-N"
  attr(ik, "source") <- "pubchem"
  expect_equal(oct$inchikey, ik)
  expect_equal(oct$smiles[["PubChem"]], "CCCCCCCCO")
})
