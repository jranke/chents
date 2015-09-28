caffeine <- chent$new("caffeine", source = "pubchem")
print(caffeine)
caffeine$get_rdkit()
plot(caffeine)
