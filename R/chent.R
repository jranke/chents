# Copyright (C) 2016-2021  Johannes Ranke
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
#' python bindings are installed.
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @importFrom R6 R6Class
#' @importFrom webchem get_cid cid_compinfo
#' @importFrom grImport PostScriptTrace readPicture
#' @importFrom yaml yaml.load_file
#' @importFrom rsvg rsvg_ps
#' @field identifier The identifier that was used to initiate the object, with attribute 'source'
#' @field inchikey InChI Key, with attribute 'source'
#' @field smiles SMILES code, with attribute 'source'
#' @field mw Molecular weight, with attribute 'source'
#' @field pubchem List of information retreived from PubChem
#' @field rdkit List of information obtained with RDKit
#' @field mol <rdkit.Chem.rdchem.Mol> object
#' @field svg SVG code
#' @field Picture Graph as a \code{\link{picture}} object obtained using grImport
#' @field Pict_font_size Font size as extracted from the intermediate PostScript file
#' @field pdf_height Height of the MediaBox in the pdf after cropping
#' @field p0 Vapour pressure in Pa
#' @field cwsat Water solubility in mg/L
#' @field chyaml List of information obtained from a YAML file
#' @field soil_degradation Dataframe of modelling DT50 values
#' @field soil_ff Dataframe of formation fractions
#' @field soil_sorption Dataframe of soil sorption data
#' @field PUF Plant uptake factor
#' @keywords data
#' @examples
#' oct <- chent$new("1-octanol", smiles = "CCCCCCCCO")
#' print(oct)
#' if (!is.null(oct$Picture)) {
#'   plot(oct)
#' }
#'
#' caffeine <- chent$new("caffeine")
#' print(caffeine)
#' if (!is.null(caffeine$Picture)) {
#'   plot(caffeine)
#' }

chent <- R6Class("chent",
  public <- list(
    identifier = NULL,
    inchikey = NULL,
    smiles = NULL,
    mw = NULL,
    pubchem = NULL,
    rdkit = NULL,
    mol = NULL,
    svg = NULL,
    Picture = NULL,
    Pict_font_size = NULL,
    pdf_height = NULL,
    p0 = NULL,
    cwsat = NULL,
    PUF = NULL,
    chyaml = NULL,
    initialize = function(identifier, smiles = NULL, smiles_source = 'user',
                          inchikey = NULL, inchikey_source = 'user',
                          pubchem = TRUE, pubchem_from = c('name', 'smiles', 'inchikey'),
                          rdkit = TRUE, template = NULL,
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
        if(rdkit_available) {
          if (is.null(self$smiles)) {
            message("RDKit would need a SMILES code")
          } else {
            message("Trying to get chemical information from RDKit using ",
                    names(self$smiles)[1], " SMILES\n",
                    self$smiles[1])
            self$get_rdkit(template = template)
            self$mw <- self$rdkit$mw
            attr(self$mw, "source") <- "rdkit"
          }
        } else {
          message("RDKit is not available")
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
      pubchem_result = webchem::get_cid(query, from = from, match = "first")

      if (is.na(pubchem_result[[1, "cid"]])) {
        message("Query ", query, " did not give results at PubChem")
      } else {
        self$get_pubchem(pubchem_result[[1, "cid"]])
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
    get_rdkit = function(template = NULL) {
      if(!rdkit_available) {
        stop("RDKit is not available")
      }
      self$rdkit <- list()
      self$mol <- rdkit_module$Chem$MolFromSmiles(self$smiles[1])
      self$rdkit$mw <- rdkit_module$Chem$Descriptors$MolWt(self$mol)
      if (!is.null(self$mw)) {
        if (round(self$rdkit$mw, 1) != round(self$mw, 1)) {
          message("RDKit mw is ", self$rdkit$mw)
          message("mw is ", self$mw)
        }
      }

      # Create an SVG representation
      rdkit_module$Chem$rdDepictor$Compute2DCoords(self$mol)
      if (!is.null(template)) {
        rdkit_template <- rdkit_module$Chem$MolFromSmiles(template)
        rdkit_module$Chem$rdDepictor$Compute2DCoords(template)
        rdkit$Chem$AllChem$GenerateDepictionMatching2DStructure(self$mol, template)
      }
      d2d <- rdkit_module$Chem$Draw$rdMolDraw2D$MolDraw2DSVG(400L, 400L)
      d2d$DrawMolecule(self$mol)
      d2d$FinishDrawing()
      self$svg <- d2d$GetDrawingText()
      svgfile <- tempfile(fileext = ".svg")
      psfile <- tempfile(fileext = ".ps")
      writeLines(self$svg, svgfile)
      rsvg::rsvg_ps(svgfile, psfile)

      # Get size properties useful for plotting
      ps_font_line <- grep("Tm$", readLines(psfile), value = TRUE)[1]
      ps_font_size <- gsub(" .*$", "", ps_font_line)
      self$Pict_font_size = as.numeric(ps_font_size)

      # Read in to create Picture
      xmlfile <- tempfile(fileext = ".xml")
      PostScriptTrace(psfile, outfilename = xmlfile)
      unlink(paste0("capture", basename(psfile)))
      self$Picture <- readPicture(xmlfile)
      unlink(c(xmlfile, psfile, svgfile))
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
    add_p0 = function(p0, T = NA, source = NA, page = NA, remark = "") {
      self$p0 <- p0
      attr(self$p0, "T") <- T
      attr(self$p0, "source") <- source
      attr(self$p0, "page") <- page
      attr(self$p0, "remark") <- remark
    },
    add_cwsat = function(cwsat, T = NA, pH = NA, source = NA, page = NA, remark = "") {
      self$cwsat <- cwsat
      attr(self$cwsat, "T") <- T
      attr(self$cwsat, "pH") <- pH
      attr(self$cwsat, "source") <- source
      attr(self$cwsat, "page") <- page
      attr(self$cwsat, "remark") <- remark
    },
    add_PUF = function(PUF = 0, source = "focus_generic_gw_2014", page = 41, remark = "Conservative default value") {
      self$PUF <- PUF
      attr(self$PUF, "source") <- source
      attr(self$PUF, "page") <- page
      attr(self$PUF, "remark") <- remark
    },
    TPs = list(),
    add_TP = function(x, smiles = NULL, pubchem = FALSE) {
      if (inherits(x, "chent")) {
        id <- names(x$identifier)
        chent <- x
      } else {
        id <- make.names(x)
        chent <- chent$new(x, smiles, pubchem = pubchem)
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
                                  remark = "", source = NA, pages = NA) {
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
                                               remark = remark,
                                               source = source,
                                               pages = pages,
                                               stringsAsFactors = FALSE))
    },
    soil_degradation = NULL,
    add_soil_degradation = function(soils, DT50_mod, DT50_mod_ref,
                                    type = NA, country = NA,
                                    pH_orig = NA, pH_medium = NA, pH_H2O = NA,
                                    perc_OC = NA,
                                    temperature = NA, moisture = NA,
                                    category = "lab", formulation = NA,
                                    model = NA, chi2 = NA,
                                    remark = "", source, page = NA) {
      new_soil_degradation = data.frame(
        soil = soils,
        DT50_mod = DT50_mod,
        DT50_mod_ref = DT50_mod_ref,
        type = type,
        country = country,
        pH_orig = pH_orig,
        pH_medium = pH_medium,
        pH_H2O = pH_H2O,
        perc_OC = perc_OC,
        temperature = temperature,
        moisture = moisture,
        category = category,
        formulation = formulation,
        model = model,
        chi2 = chi2,
        remark = remark,
        source = source,
        page = page,
        stringsAsFactors = FALSE)
      if (is.null(self$soil_degradation)) {
        self$soil_degradation <- new_soil_degradation
      } else {
        self$soil_degradation <- rbind(self$soil_degradation, new_soil_degradation)
      }
    },
    soil_ff = NULL,
    add_soil_ff = function(target, soils, ff = 1, remark = "", source, page = NA) {
      new_soil_ff = data.frame(
        target = target,
        soil = soils,
        ff = ff,
        remark = remark,
        source = source,
        page = page,
        stringsAsFactors = FALSE)
      if (is.null(self$soil_ff)) {
        self$soil_ff <- new_soil_ff
      } else {
        self$soil_ff <- rbind(self$soil_ff, new_soil_ff)
      }
    },
    soil_sorption = NULL,
    add_soil_sorption = function(soils, Kf, Kfoc, N,
                                 type = NA,
                                 pH_orig = NA, pH_medium = NA,
                                 pH_H2O = NA,
                                 perc_OC = NA, perc_clay = NA, CEC = NA,
                                 remark = "", source, page = NA) {
      new_soil_sorption = data.frame(
        soils = soils,
        Kf = Kf, Kfoc = Kfoc, N = N,
        type = type,
        pH_orig = pH_orig,
        pH_medium = pH_medium,
        pH_H2O = pH_H2O,
        perc_OC = perc_OC, perc_clay = perc_clay, CEC = CEC,
        remark = remark,
        source = source,
        page = page,
        stringsAsFactors = FALSE)
      if (is.null(self$soil_sorption)) {
        self$soil_sorption <- new_soil_sorption
      } else {
        self$soil_sorption <- rbind(self$soil_sorption, new_soil_sorption)
      }
    },
    pdf = function(file = paste0(self$identifier, ".pdf"), dir = "structures/pdf",
                   template = NULL) {
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
      bash_path <- shQuote(path)
      system(paste("pdfcrop --margin 10", bash_path, bash_path, "> /dev/null"))

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
    },
    emf = function(file = paste0(self$identifier, ".emf"), dir = "structures/emf") {
      if (!requireNamespace("devEMF")) {
        stop("You need to have the devEMF package installed for this function")
      }
      if (!dir.exists(dir)) {
        message("Directory '", dir, "' does not exist")
        message("Trying to create directory '", dir, "'")
        dir.create(dir, recursive = TRUE)
      }
      path = file.path(dir, file)
      message("Creating file '", path, "'")
      devEMF::emf(path)
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
  if (!rdkit_available) {
    stop("RDkit is not available via reticulate")
  } else {
    if (!dir.exists(subdir)) dir.create(subdir)
    mol <- rdkit_module$Chem$MolFromSmiles(x$smiles)

    rdkit_module$Chem$Draw$MolToFile(mol, file.path(subdir, filename),
      size = c(as.integer(width), as.integer(height)))
  }
}

#' Plot method for chent objects
#'
#' @importFrom grImport grid.picture
#' @param x The chent object to be plotted
#' @param ... Further arguments passed to \code{\link{grid.picture}}
#' @export
plot.chent = function(x, ...) {
  if (is.null(x$Picture)) stop("No Picture object in chent, was RDKit available during creation?")
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
#' @field iso ISO common name according to ISO 1750 as retreived from pesticidecompendium.bcpc.org
#' @field bcpc List of information retrieved from pesticidecompendium.bcpc.org
#' @keywords data
#' @examples
#' atr <- pai$new("atrazine")
#' print(atr)
#' if (!is.null(atr$Picture)) {
#'   plot(atr)
#' }

pai <- R6Class("pai",
  inherit = chent,
  public <- list(
    iso = NULL,
    bcpc = NULL,
    initialize = function(iso, identifier = iso,
                          smiles = NULL, smiles_source = 'user',
                          inchikey = NULL, inchikey_source = 'user',
                          bcpc = TRUE,
                          pubchem = TRUE, pubchem_from = 'auto',
                          rdkit = TRUE, template = NULL,
                          chyaml = TRUE)
    {

      if (!is.null(inchikey)) {
        self$inchikey = inchikey
        attr(self$inchikey, "source") <- "user"
      }

      if (!missing(iso) & bcpc) {
        message("BCPC:")
        bcpc_result = webchem::bcpc_query(identifier, from = "name")

        # Use first element of list, as we passed a query of length one
        if (is.na(bcpc_result[[1]][1])) {
          message("Common name ", identifier, " is not known at the BCPC compendium, trying PubChem")
        } else {
          self$bcpc = bcpc_result[[1]]
          self$iso = self$bcpc$cname
          attr(self$iso, "source") <- "bcpc"
          attr(self$iso, "status") <- self$bcpc$status
          bcpc_ik = self$bcpc$inchikey
          if (length(bcpc_ik) == 1 && nchar(bcpc_ik) == 27 && !is.na(bcpc_ik)) {
            if (is.null(self$inchikey)) {
              self$inchikey = self$bcpc$inchikey
              attr(self$inchikey, "source") <- "bcpc"
            } else {
              if (bcpc_ik == self$inchikey) {
                attr(self$inchikey, "source") = c(attr(self$inchikey, "source"), "bcpc")
              } else {
                warning("InChIKey ", self$inchikey, " differs from ", bcpc_ik, " obtained from bcpc.org")
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
                       rdkit = rdkit, template = template, chyaml = chyaml)

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
