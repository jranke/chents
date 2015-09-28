context("Generation of chent objects")

oct <- chent$new("1-octanol", smiles = "CCCCCCCCO")

test_that("We can generate a chent object from SMILES using RDKit", {
  expect_equivalent(round(oct$mw, 2), 130.23) 
  expect_equal(names(oct$identifier), "X1.octanol")
  expect_equal(oct$smiles, "CCCCCCCCO")
})

test_that("We can add information retrieved from PubChem via webchem", {
  oct$try_pubchem()
  expect_equivalent(round(oct$mw, 2), 130.23) 
  ik = "KBPLFHHGFOOTCA-UHFFFAOYSA-N"
  attr(ik, "source") <- "pubchem"
  expect_equal(oct$inchikey, ik)
  smiles <- "CCCCCCCCO"
  attr(smiles, "source") <- "pubchem"
  attr(smiles, "type") <- "canonical"
  expect_equal(oct$smiles, smiles)
})
