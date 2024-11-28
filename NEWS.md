## version 0.3.7

- R/chent.R: Do not attempt to load a chyaml file per default, as the format of such a file and the resulting chyaml list object is not documented and would need to be inferred from its use in the pfm package.

## version 0.3.6

- R/chent.R: Set the fields `smiles`, `inchikey` and `mw` to NA if these fields are still NULL at the end of the initialization, with `source` attribute set to "user", assuming that the initialisation was carefully done and `pubchem` and `rdkit` were skipped because the structure is not well-defined.

Please refer to the ChangeLog for commit messages up to November 2021, where I changed the GNUmakefile not to include commit messages
in that file any more.
