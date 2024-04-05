.onLoad = function(libname, pkgname) {
  rdkit_available <- FALSE
  rdkit_module <- try(
    reticulate::import("rdkit", delay_load = TRUE), 
    silent = TRUE)
  if (!inherits(rdkit_module, "try-error")) {
    rdkit_available <- TRUE
  }
  assign('rdkit_available', rdkit_available, envir = topenv())
  assign('rdkit_module', rdkit_module, envir = topenv())
}
