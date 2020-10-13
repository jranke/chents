context("Generation of chent objects")

# Check if we can use RDKit
skip_if_no_rdkit <- function() {
  if (!chents:::rdkit_available()) skip("RDKit is not available via PythonInR")
}

oct <- chent$new("1-octanol", smiles = "CCCCCCCCO", pubchem = FALSE, chyaml = FALSE)

test_that("We can generate a chent object from SMILES using RDKit", {
  skip_if_no_rdkit()
  expect_equivalent(round(oct$mw, 2), 130.23)
  expect_equal(names(oct$identifier), "X1.octanol")
  expect_equal(oct$smiles[["user"]], "CCCCCCCCO")
})

test_that("We can add information retrieved from PubChem via webchem", {
  oct$try_pubchem()
  expect_equivalent(round(oct$mw, 2), 130.23)
  ik = "KBPLFHHGFOOTCA-UHFFFAOYSA-N"
  attr(ik, "source") <- "pubchem"
  expect_equal(oct$inchikey, ik)
  expect_equal(oct$smiles[["PubChem_Canonical"]], "CCCCCCCCO")
})
