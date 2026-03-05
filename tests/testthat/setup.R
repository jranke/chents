# Check if we can use RDKit
skip_if_no_rdkit <- function() {
  if (!chents:::rdkit_available) skip("RDKit is not available via reticulate")
}
