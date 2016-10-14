# Copyright (C) 2016  Johannes Ranke
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
#' The class is initialised with an identifier. Chemical information is retrieved from
#' the internet. Additionally, it can be generated using RDKit if RDKit and its
#' python bindings are installed and configured for use with PythonInR.
#'
#' @docType class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @importFrom R6 R6Class
#' @importFrom webchem get_cid cid_compinfo
#' @importFrom grImport PostScriptTrace readPicture
#' @importFrom yaml yaml.load_file
#' @field identifier The identifier that was used to initiate the object, with attribute 'source'
#' @field inchikey InChI Key, with attribute 'source'
#' @field smiles SMILES code, with attribute 'source'
#' @field mw Molecular weight, with attribute 'source'
#' @field pubchem List of information retreived from PubChem
#' @field rdkit List of information obtained with RDKit, if installed and
#'   configured for use with PythonInR
#' @field Picture Graph as a \code{\link{picture}} object obtained using grImport
#' @field Pict_font_size Font size as extracted from the intermediate PostScript file
#' @field pdf_height Height of the MediaBox in the pdf after cropping
#' @field chyaml List of information obtained from a YAML file
#' @field degradation List of degradation endpoints
#' @example inst/examples/octanol.R
#' @example inst/examples/caffeine.R
#' @keywords data

chent <- R6Class("chent",
  public <- list(
    identifier = NULL,
    inchikey = NULL,
    smiles = NULL,
    mw = NULL,
    pubchem = NULL,
    rdkit = NULL,
    Picture = NULL,
    Pict_font_size = NULL,
    pdf_height = NULL,
    chyaml = NULL,
    degradation = NULL,
    initialize = function(identifier, smiles = NULL, smiles_source = 'user',
                          inchikey = NULL, inchikey_source = 'user',
                          pubchem = TRUE, pubchem_from = c('name', 'smiles', 'inchikey'),
                          rdkit = TRUE, 
                          chyaml = TRUE) {

      self$identifier <- identifier
      names(self$identifier) <- make.names(identifier)
      pubchem_from = match.arg(pubchem_from)

      self$smiles <- c(user = smiles)

      if (pubchem) {
        if (pubchem_from == 'name') {
          query = identifier
        } else {
          query = get(pubchem_from)
        }
        self$try_pubchem(query, from = pubchem_from)
      }

      if (rdkit) {
        if(requireNamespace("PythonInR", quietly = TRUE)) {
          if (is.null(self$smiles)) {
            message("RDKit would need a SMILES code")
          } else {
            message("Trying to get chemical information from RDKit using ",
                    names(self$smiles)[1], " SMILES\n",
                    self$smiles[1])
            self$get_rdkit()
            self$mw <- self$rdkit$mw
            attr(self$mw, "source") <- "rdkit"
          }
        }
      }

      if (chyaml) {
        self$get_chyaml()
      }
      invisible(self)
    },
    try_pubchem = function(query, from = 'name') {
      message("PubChem:")
      if (missing(query)) query <- self$identifier
      pubchem_result = webchem::get_cid(query, from = from)

      if (is.na(pubchem_result[[1]][1])) {
        message("Query ", query, " did not give results at PubChem")
      } else {
        n_results = length(pubchem_result[[1]])
        if (n_results > 1) {
          warning("Found ", n_results, " entries in PubChem, using the first one.")
        }
        self$get_pubchem(pubchem_result[[1]][1])
      }
    },
    get_pubchem = function(pubchem_cid) {
      self$pubchem = as.list(webchem::pc_prop(pubchem_cid, from = "cid",
        properties = c("MolecularFormula", "MolecularWeight", 
                       "CanonicalSMILES", "IsomericSMILES", 
                       "InChI", "InChIKey", "IUPACName", 
                       "XLogP", "TPSA", "Complexity", "Charge",
                       "HBondDonorCount", "HBondAcceptorCount")))
      self$pubchem$synonyms = webchem::pc_synonyms(pubchem_cid, from ="cid")[[1]]

      self$smiles["PubChem_Canonical"] <- self$pubchem$CanonicalSMILES

      if (self$pubchem$IsomericSMILES != self$pubchem$CanonicalSMILES) {
          self$smiles["PubChem_Isomeric"] <- self$pubchem$IsomericSMILES
      }

      self$mw = as.numeric(self$pubchem$MolecularWeight)
      attr(self$mw, "source") <- "pubchem"

      if (is.null(self$inchikey)) {
        self$inchikey <- self$pubchem$InChIKey
        attr(self$inchikey, "source") <- "pubchem"
      } else {
        if (length(self$inchikey) > 1) {
          message("InChIKey ", self$inchikey, " retreived from ",
                  attr(self$inchikey, "source"), 
                  " has length > 1, using PubChem InChIKey")
          self$inchikey <- self$pubchem$InChIKey
          attr(self$inchikey, "source") <- "pubchem"
        } else {
          if (self$pubchem$InChIKey != self$inchikey) {
            message("InChiKey ", self$pubchem$InChIKey, " from PubChem record does not match\n",
                    "InChiKey ", self$inchikey, " retreived from ",
                    attr(self$inchikey, "source"))
          } else {
            attr(self$inchikey, "source") <- c(attr(self$inchikey, "source"), "pubchem")
          }
        }
      }
    },
    get_rdkit = function() {
      if (!requireNamespace("PythonInR"))
        stop("PythonInR can not be loaded")
      id <- names(self$identifier)
      if (!PythonInR::pyIsConnected()) {
        PythonInR::pyConnect()
      }
      try_rdkit <- try(PythonInR::pyImport("Chem", from = "rdkit"))
      if (inherits(try_rdkit, "try-error")) {
        message("Could not import RDKit in Python session")
      } else {
        self$rdkit <- list()
        PythonInR::pyImport("Descriptors", from = "rdkit.Chem")
        PythonInR::pyExec(paste0("mol = Chem.MolFromSmiles('", self$smiles[1], "')"))
        self$rdkit$mw <- PythonInR::pyExecg("mw = Descriptors.MolWt(mol)", "mw")
        if (!is.null(self$mw)) {
          if (round(self$rdkit$mw, 1) != round(self$mw, 1)) {
            message("RDKit mw is ", self$rdkit$mw)
            message("mw is ", self$mw)
          }
        }

        # Create a grImport Picture 
        PythonInR::pyImport("Draw", from = "rdkit.Chem")
        psfile <- tempfile(fileext = ".ps")
        xmlfile <- tempfile(fileext = ".xml")
        cmd <- paste0("Draw.MolToFile(mol, '", psfile, "')")
        PythonInR::pyExec(cmd)
        ps_font_line <- grep("Tm$", readLines(psfile), value = TRUE)[1]
        ps_font_size <- gsub(" .*$", "", ps_font_line)

        self$Pict_font_size = as.numeric(ps_font_size)
        PostScriptTrace(psfile, outfilename = xmlfile)
        unlink(paste0("capture", basename(psfile)))
        self$Picture <- readPicture(xmlfile)
      }
    },
    get_chyaml = function(repo = c("wd", "local", "web"), 
                          chyaml = paste0(URLencode(self$identifier), ".yaml")) {
      repo = match.arg(repo)
      paths = c(wd = ".", 
                local = file.path("~", "git/chyaml"))

      chyaml_handlers = list(
        expr = function(x) NULL, # To avoid security risks from reading chyaml files
        dataframe = function(x) 
          eval(parse(text = paste0("data.frame(", x, ", stringsAsFactors = FALSE)"))))

      if (repo %in% c("wd", "local")) {
        path = paths[repo]
        full = file.path(path, chyaml)
        if (!file.exists(full)) {
          message("Did not find chyaml file ", full)
        } else {
          if (is(try(self$chyaml <- yaml.load_file(chyaml, handlers = chyaml_handlers)), 
                 "try-error")) {
            message("Could not load ", full)
          } else {
            message("Loaded ", full)
          }
        }
      } else {
        message("web repositories not implemented")
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
                                               pages = pages,
                                               stringsAsFactors = FALSE))
    },
    soil_degradation_endpoints = data.frame(destination = character(0), 
                                            DT50 = numeric(0),
                                            comment = character(0),
                                            pages = character(0),
                                            stringsAsFactors = FALSE),
    add_soil_degradation_endpoints = function(destination, DT50 = NA,
                                              comment = "", pages = NA) {
      if (length(pages) > 1) pages = paste(pages, collapse = ", ")
      i <- nrow(self$soil_degradation_endpoints) + 1
      self$soil_degradation_endpoints[i, c("destination", "comment", "pages")] <- 
        c(destination, comment, pages)
      self$soil_degradation_endpoints[i, "DT50"] <- DT50
    },
    ff = data.frame(from = character(0), to = character(0), ff = numeric(0),
                    comment = character(0), pages = character(0),
                    stringsAsFactors = FALSE),
    add_ff = function(from = "parent", to, ff = 1, comment = "", pages = NA) {
      i <- nrow(self$ff) + 1
      if (from != "parent") {
        if (!exists(from, self$TPs)) stop(from, " was not found in TPs")
      }
      if (!exists(to, self$TPs)) stop(to, " was not found in TPs")
      self$ff[i, ] <- c(from, to, ff, comment, pages)
    },
    pdf = function(file = paste0(self$identifier, ".pdf"), dir = "structures/pdf") {
      if (!dir.exists(dir)) {
        message("Directory '", dir, "' does not exist")
        message("Trying to create directory '", dir, "'")
        dir.create(dir, recursive = TRUE)
      }
      path = file.path(dir, file)
      message("Creating file '", path, "'")
      pdf(path)
      plot(self)
      dev.off()
      message("Cropping file '", path, "' using pdfcrop")
      system(paste("pdfcrop --margin 10", path, path, "> /dev/null"))

      # Get the height of the MediaBox
      head <- readLines(path, n = 20, skipNul = TRUE)
      m_line <- suppressWarnings(grep("MediaBox", head, value = TRUE))
      self$pdf_height <- as.numeric(gsub("/MediaBox \\[.* (.*)\\]", "\\1", m_line))
    },
    png = function(file = paste0(self$identifier, ".png"), dir = "structures/png",
                   antialias = 'gray') {
      if (!dir.exists(dir)) {
        message("Directory '", dir, "' does not exist")
        message("Trying to create directory '", dir, "'")
        dir.create(dir, recursive = TRUE)
      }
      path = file.path(dir, file)
      message("Creating file '", path, "'")
      png(path, antialias = antialias)
      plot(self)
      dev.off()
    }
  )
)

#' Printing method for chent objects
#'
#' @param x The chent object to be printed
#' @param ... Further arguments for compatibility with the S3 method
#' @importFrom utils head
#' @export
print.chent = function(x, ...) {
  cat("<chent>\n")
  cat("Identifier $identifier", x$identifier, "\n")
  cat ("InChI Key $inchikey", x$inchikey, "\n")
  cat ("SMILES string $smiles:\n")
  print(x$smiles)
  if (!is.null(x$mw)) cat ("Molecular weight $mw:", round(x$mw, 1), "\n")
  if (!is.null(x$pubchem$synonyms)) {
    cat ("PubChem synonyms (up to 10):\n")
    print(head(x$pubchem$synonyms, n = 10L))
  }
}

#' Draw SVG graph from a chent object using RDKit
#'
#' @param x The chent object to be plotted
#' @param width The desired width in pixels
#' @param height The desired height in pixels
#' @param filename The filename
#' @param subdir The path to which the file should be written
#' @export
draw_svg.chent = function(x, width = 300, height = 150,
                          filename = paste0(names(x$identifier), ".svg"),
                          subdir = "svg") {
  if (!PythonInR::pyIsConnected()) {
    PythonInR::pyConnect()
  }
  try_rdkit <- try(PythonInR::pyImport("Chem", from = "rdkit"))
  if (inherits(try_rdkit, "try-error")) {
    message("Could not import RDKit in Python session")
  } else {
    if (!dir.exists(subdir)) dir.create(subdir)
    PythonInR::pyExec(paste0("mol = Chem.MolFromSmiles('", x$smiles, "')"))
    PythonInR::pyImport("Draw", from = "rdkit.Chem")
    cmd <- paste0("Draw.MolToFile(mol, '", file.path(subdir, filename), 
                  "', size = (", width, ", ", height, "))")
    PythonInR::pyExec(cmd)
  }
}

#' Plot method for chent objects
#'
#' @importFrom grImport grid.picture
#' @param x The chent object to be plotted
#' @param ... Further arguments passed to \code{\link{grid.picture}}
#' @example inst/examples/caffeine.R
#' @export
plot.chent = function(x, ...) {
  grid.picture(x$Picture)
}

#' An R6 class for pesticidal active ingredients and associated data
#' 
#' The class is initialised with an identifier which is generally an ISO common name.
#' Additional chemical information is retrieved from the internet if available. 
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @field iso ISO common name according to ISO 1750 as retreived from www.alanwood.net/pesticides
#' @field alanwood List of information retreived from www.alanwood.net/pesticides
#' @example inst/examples/pai.R
#' @keywords data

pai <- R6Class("pai",
  inherit = chent,
  public <- list(
    iso = NULL,
    alanwood = NULL,
    initialize = function(iso, identifier = iso, 
                          smiles = NULL, smiles_source = 'user',
                          inchikey = NULL, inchikey_source = 'user',
                          alanwood = TRUE, 
                          pubchem = TRUE, pubchem_from = 'auto',
                          rdkit = TRUE, chyaml = TRUE)
    {

      if (!is.null(inchikey)) {
        self$inchikey = inchikey
        attr(self$inchikey, "source") <- "user"
      }

      if (!missing(iso) & alanwood) {
        message("alanwood.net:")
        aw_result = webchem::aw_query(identifier, type = "commonname")

        # Use first element of list, as we passed a query of length one
        if (is.na(aw_result[[1]][1])) {
          message("Common name ", identifier, " is not known at www.alanwood.net, trying PubChem")
        } else {
          self$alanwood = aw_result[[1]]
          self$iso = self$alanwood$cname
          attr(self$iso, "source") <- "alanwood"
          attr(self$iso, "status") <- self$alanwood$status
          aw_ik = self$alanwood$inchikey
          if (length(aw_ik) == 1 && nchar(aw_ik) == 27 && !is.na(aw_ik)) {
            if (is.null(self$inchikey)) {
              self$inchikey = self$alanwood$inchikey
              attr(self$inchikey, "source") <- "alanwood"
            } else {
              if (aw_ik == self$inchikey) {
                attr(self$inchikey, "source") = c(attr(self$inchikey, "source"), "alanwood")
              } else {
                warning("InChIKey ", self$inchikey, " differs from ", aw_ik, " obtained from alanwood.net")
              }
            }
          }
        }
      } 

      # Set pubchem_from if not specified
      if (pubchem_from == 'auto') {
        pubchem_from = 'name'
        if (!is.null(self$inchikey)) {
          pubchem_from = 'inchikey'
        }
      }

      super$initialize(identifier = identifier, 
                       smiles = smiles, smiles_source = smiles_source,
                       inchikey = self$inchikey,
                       pubchem = pubchem, pubchem_from = pubchem_from,
                       rdkit = rdkit, chyaml = chyaml)

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
