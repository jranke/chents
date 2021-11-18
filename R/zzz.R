.onLoad = function(libname, pkgname) {
  rdkit_available <- FALSE
  if(requireNamespace("reticulate", quietly = TRUE)) {
    rdkit_module <- try(reticulate::import("rdkit"))
    if (!inherits(rdkit_module, "try-error")) {
      rdkit_available <- TRUE
    }
  }
  assign('rdkit_available', rdkit_available, envir = topenv())
  if (rdkit_available) {
    assign('rdkit_module', rdkit_module, envir = topenv())
  }
}
