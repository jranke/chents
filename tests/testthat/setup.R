# Check if we can use RDKit
skip_if_no_rdkit <- function() {
  if (!chents:::rdkit_available) skip("RDKit is not available via reticulate")
}

# Check availability of BCPC and PubChem
bcpc_up <- webchem::ping_service("bcpc")
pc_up <- webchem::ping_service("pc")
