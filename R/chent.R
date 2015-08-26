# Copyright (C) 2015  Johannes Ranke
# Contact: jranke@uni-bremen.de
# This file is part of the R package chents

# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>

#' An R6 class for chemical entities with associated data
#' 
#' The class is initialised with an identifier. Chemical information is retrieved
#' from the internet.
#'
#' @docType class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @importFrom R6 R6Class
#' @importFrom webchem get_cid cid_compinfo
#' @field identifier The identifier that was used to initiate the object, with attribute 'source'
#' @field inchikey InChI Key, with attribute 'source'
#' @field smiles SMILES code, with attribute 'source'
#' @field mw Molecular weight, with attribute 'source'
#' @field pubchem List of information retreived from PubChem
#' @example inst/examples/chents.R
#' @keywords data

chent <- R6Class("chent",
  public <- list(
    identifier = NULL,
    inchikey = NULL,
    smiles = NULL,
    mw = NULL,
    pubchem = NULL,
    initialize = function(identifier, type = c("name", "smiles"), 
                          source = c("pubchem")) {
      self$identifier <- identifier
      type = match.arg(type)
      attr(self$identifier, "type") <- type
      source = match.arg(source)      
      switch(source,
        pubchem = {
          self$try_pubchem(identifier)
        }
      )
      invisible(self)
    },
    try_pubchem = function(identifier) {
      pubchem_cids = webchem::get_cid(identifier)

        if (is.na(pubchem_cids[1])) {
          stop("Query ", identifier, " did not give results at PubChem")
        } else {
          message("Found ", length(pubchem_cids), " entries in PubChem, using the first one.")
          self$get_pubchem(pubchem_cids[1])
        }
    },
    get_pubchem = function(pubchem_cid) {
      self$pubchem = webchem::cid_compinfo(pubchem_cid)

      self$smiles = self$pubchem$CanonicalSmiles
      attr(self$smiles, "source") <- "pubchem"
      attr(self$smiles, "type") <- "canonical"

      self$mw = as.numeric(self$pubchem$MolecularWeight)
      attr(self$mw, "source") <- "pubchem"

      if (is.null(self$inchikey)) {
        self$inchikey <- self$pubchem$InChIKey
        attr(self$inchikey, "source") <- "pubchem"
      } else {
        if (self$pubchem$InChIKey != self$inchikey) {
          stop("InChiKey of PubChem record does not the one retreived from ", 
               attr(self$inchi, "source"))
        }
      }
    },
    show = function() {
      cat("<chent> built using $identifier", self$identifier, "\n")
      cat ("InChI Key $inchikey", self$inchikey, "\n")
      cat ("SMILES string $smiles", self$smiles, "\n")
      if (!is.null(self$mw)) cat ("Molecular weight $mw:", round(self$mw, 1), "\n")
      if (!is.null(self$pubchem)) {
        cat ("PubChem synonyms (first 10):\n")
        print(head(self$pubchem$synonyms, n = 10L))
      }
    }
  )
)

#' An R6 class for pesticidal active ingredients and associated data
#' 
#' The class is initialised with an identifier which is generally an ISO common name.
#' Additional chemical information is retrieved from the internet.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @field iso ISO common name according to ISO 1750 as retreived from www.alanwood.net/pesticides
#' @field alanwood List of information retreived from www.alanwood.net/pesticides
#' @example inst/examples/ai.R
#' @keywords data

pai <- R6Class("pai",
  inherit = chent,
  public <- list(
    iso = NULL,
    alanwood = NULL,
    initialize = function(identifier, type = c("name", "smiles"), 
                          source = c("alanwood", "pubchem")) {
      self$identifier <- identifier
      type = match.arg(type)
      attr(self$identifier, "type") <- type
      source = match.arg(source)      
      switch(source,
        alanwood = {
          self$alanwood = webchem::alanwood(identifier, type = "commonname")
          if (is.na(self$alanwood[1])) {
            message("Common name ", identifier, " is not known at www.alanwood.net, trying PubChem")
            self$try_pubchem(identifier)
          } else {
            self$iso = self$alanwood$cname
            attr(self$iso, "source") <- "alanwood"
            attr(self$iso, "status") <- self$alanwood$status
            self$inchikey = self$alanwood$inchikey
            attr(self$inchikey, "source") <- "alanwood"

            # Get additional information from PubChem
            pubchem_cids = get_cid(identifier)
            self$get_pubchem(pubchem_cids[[1]])
          }
        },
        pubchem = {
          self$try_pubchem(identifier)
        }
      )
      invisible(self)
    },
    print = function() {
      cat("<pai> with ISO common name $iso", self$iso, "\n")
      super$show()
    }
  )
)
# vim: set ts=2 sw=2 expandtab:
