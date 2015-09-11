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
#' @field rdkit List of information obtained with RDKit
#' @example inst/examples/chents.R
#' @keywords data

chent <- R6Class("chent",
  public <- list(
    identifier = NULL,
    inchikey = NULL,
    smiles = NULL,
    mw = NULL,
    pubchem = NULL,
    rdkit = NULL,
    initialize = function(identifier, smiles = NULL,
                          source = c("rdkit", "pubchem")) {
      self$identifier <- identifier
      names(self$identifier) <- make.names(identifier)
      source = match.arg(source)
      switch(source,
        pubchem = {
          self$try_pubchem(identifier)
        },
        rdkit = {
          if (is.null(smiles)) {
            stop("rdkit needs smiles as input")
          } else {
            self$smiles <- smiles
            self$get_rdkit()
            self$mw <- self$rdkit$mw
            attr(self$mw, "source") <- "rdkit"
          }
        }
      )
      invisible(self)
    },
    try_pubchem = function(identifier) {
      if (missing(identifier)) identifier <- self$identifier
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
    get_rdkit = function() {
      if (require(PythonInR)) {
        id <- names(self$identifier)
        try_rdkit <- try(pyImport("Chem", from = "rdkit"))
        if (inherits(try_rdkit, "try-error")) {
          message("Could not import RDKit in Python session")
        } else {
          self$rdkit <- list()
          pyImport("Descriptors", from = "rdkit.Chem")
          pyExec(paste0("mol = Chem.MolFromSmiles('", self$smiles, "')"))
          self$rdkit$mw <- pyExecg("mw = Descriptors.MolWt(mol)", "mw")
          if (!is.null(self$mw)) {
            if (round(self$rdkit$mw, 1) != round(self$mw, 1)) {
              message("RDKit mw is ", self$rdkit$mw)
              message("mw is ", self$mw)
            }
          }
        }
      } else {
        stop("rdkit not available as PythonInR is not installed")
      }
    },
    TPs = list(),
    add_TP = function(x, smiles = NULL) {
      if (inherits(x, "chent")) {
        id <- names(x$identifier)
        chent <- x
      } else {
        id <- make.names(x)
        chent <- chent$new(x, smiles) 
      }
      self$TPs[[id]] <- chent
    },
    transformations = data.frame(study_type = character(0),
                                 TP_identifier = character(0), 
                                 max_occurrence = numeric(0), 
                                 source = character(0), 
                                 pages = character(0),
                                 stringsAsFactors = FALSE),
    add_transformation = function(study_type, TP_identifier, max_occurrence, 
                                  comment = "", source = NA, pages = NA) {
      TP_name = make.names(TP_identifier)
      if (!inherits(self$TPs[[TP_name]], "chent")) {
        stop(paste("Please add the TP", TP_identifier, "first using chent$add_TP()"))
      }
      TP_chent <- self$TPs[TP_name]
      if (is.numeric(pages)) pages <- paste(pages, collapse = ", ")
      cn <- colnames(self$transformations)
      self$transformations <- rbind(self$transformations, 
                                    data.frame(study_type = study_type, 
                                               TP_identifier = TP_identifier, 
                                               max_occurrence = max_occurrence, 
                                               comment = comment, 
                                               source = source,
                                               pages = pages))
    },
    soil_degradation_endpoints = data.frame(destination = character(0), 
                                            DT50 = numeric(0),
                                            comment = character(0),
                                            pages = character(0),
                                            stringsAsFactors = FALSE),
    add_soil_degradation_endpoints = function(destination, DT50 = NA,
                                              comment = "", pages = NA) {
      i <- nrow(self$soil_degradation_endpoints) + 1
      self$soil_degradation_endpoints[i, c("destination", "comment", "pages")] <- 
        c(destination, comment, pages)
      self$soil_degradation_endpoints[i, "DT50"] <- DT50
    }
  )
)

#' Printing method for chent objects
#'
#' @param x The chent object to be printed
#' @param ... Further arguments for compatibility with the S3 method
#' @export
print.chent = function(x, ...) {
  cat("<chent>\n")
  cat("Identifier $identifier", x$identifier, "\n")
  cat ("InChI Key $inchikey", x$inchikey, "\n")
  cat ("SMILES string $smiles", x$smiles, "\n")
  if (!is.null(x$mw)) cat ("Molecular weight $mw:", round(x$mw, 1), "\n")
  if (!is.null(x$pubchem)) {
    cat ("PubChem synonyms (first 10):\n")
    print(head(x$pubchem$synonyms, n = 10L))
  }
}

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
      names(self$identifier) <- make.names(identifier)
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
            self$get_rdkit()
          }
        },
        pubchem = {
          self$try_pubchem(identifier)
          self$get_rdkit()
        }
      )
      invisible(self)
    }
  )
)

#' Printing method for pai objects (pesticidal active ingredients)
#'
#' @param x The chent object to be printed
#' @param ... Further arguments for compatibility with the S3 method
#' @export
print.pai = function(x, ...) {
  cat("<pai> with ISO common name $iso", x$iso, "\n")
  print.chent(x)
  if (length(x$TPs) > 0) {
    cat("\nTransformation products:\n")
    print(x$TPs)
  }
  if (nrow(x$transformations) > 0) {
    cat("\nTransformations:\n")
    print(x$transformations)
  }
}

#' R6 class for holding a product with at least one active ingredient
#'
#' An R6 class for holding information about a product with at least one active ingredient
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object.
#' @field name The name of the product
#' @field ais A list of active ingredients
#' @field concentrations The concentration of the ais
#' @field concentration_units Defaults to g/L
#' @keywords data

pp <- R6Class("pp",
  public <- list(
    name = NULL,
    ais = list(),
    concentrations = NULL,
    concentration_units = NULL,
    density = NULL,
    density_units = "g/L",
    initialize = function(name, ..., concentrations, concentration_units = "g/L",
                          density = 1000, density_units = "g/L") {
      self$name <- name
      self$ais <- list(...)
      self$concentrations <- concentrations
      self$density <- density
      self$density_units <- density_units
      names(self$concentrations) <- names(self$ais)
      self$concentration_units <- concentration_units
    },
    print = function() {
      cat("<pp> named", self$name, "\n")
    }
  )
)
# vim: set ts=2 sw=2 expandtab:
