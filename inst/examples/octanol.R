oct <- chent$new("1-octanol", smiles = "CCCCCCCCO")
print(oct)
if (!is.null(oct$Picture)) {
  plot(oct)
}
