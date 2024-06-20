.onLoad = function(libname, pkgname) {
  conf <- reticulate::py_discover_config("rdkit")
  rdkit_available <- reticulate::py_module_available("rdkit")
  rdkit_module <- try(
    reticulate::import("rdkit"), 
    silent = TRUE)
  assign('rdkit_available', rdkit_available, envir = topenv())
  assign('rdkit_module', rdkit_module, envir = topenv())
}
