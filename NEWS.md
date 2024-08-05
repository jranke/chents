## version 0.3.6

- R/chent.R: Set the fields `smiles`, `inchikey` and `mw` to NA if these fields are still NULL at the end of the initialization, with `source` attribute set to "user", assuming that the initialisation was carefully done and `pubchem` and `rdkit` were skipped because the structure is not well-defined.

