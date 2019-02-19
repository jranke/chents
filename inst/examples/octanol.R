oct <- chent$new("1-octanol", smiles = "CCCCCCCCO")
print(oct)
if (requireNamespace("grConvert")) {
  plot(oct)
}
