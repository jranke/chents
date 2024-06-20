#' @title An R6 class for chemical entities with associated data
#'
#' @description The class is initialised with an identifier. Chemical
#' information is retrieved from the internet. Additionally, it can be
#' generated using RDKit if RDKit and its python bindings are installed.
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @importFrom R6 R6Class
#' @importFrom utils URLencode
#' @importFrom webchem get_cid cid_compinfo
#' @importFrom grImport PostScriptTrace readPicture
#' @importFrom yaml yaml.load_file
#' @importFrom rsvg rsvg_ps
#' @examples
#' oct <- chent$new("1-octanol", smiles = "CCCCCCCCO", pubchem = FALSE)
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
  public = list(
    #' @field identifier (`character(1)`)\cr
    #' The identifier that was used to initiate the object, with attribute 'source'
    identifier = NULL,

    #' @field inchikey (`character(1)`)\cr
    #' InChI Key, with attribute 'source'
    inchikey = NULL,

    #' @field smiles (`character()`)\cr
    #' SMILES code(s), with attribute 'source'
    smiles = NULL,

    #' @field mw (`numeric(1)`)\cr
    #' Molecular weight, with attribute 'source'
    mw = NULL,

    #' @field pubchem (`list()`)\cr
    #' List of information retrieved from PubChem
    pubchem = NULL,

    #' @field rdkit
    #' List of information obtained with RDKit
    rdkit = NULL,

    #' @field mol <rdkit.Chem.rdchem.Mol> object
    mol = NULL,

    #' @field svg SVG code
    svg = NULL,

    #' @field Picture Graph as a \code{\link{picture}} object obtained using grImport
    Picture = NULL,

    #' @field Pict_font_size Font size as extracted from the intermediate PostScript file
    Pict_font_size = NULL,

    #' @field pdf_height Height of the MediaBox in the pdf after cropping
    pdf_height = NULL,

    #' @field p0 Vapour pressure in Pa
    p0 = NULL,

    #' @field cwsat Water solubility in mg/L
    cwsat = NULL,

    #' @field PUF Plant uptake factor
    PUF = NULL,

    #' @field chyaml List of information obtained from a YAML file
    chyaml = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param identifier Identifier to be stored in the object
    #' @param smiles Optional user provided SMILES code
    #' @param inchikey Optional user provided InChI Key
    #' @param pubchem Should an attempt be made to retrieve chemical
    #' information from PubChem via the webchem package?
    #' @param pubchem_from Possibility to select the argument
    #' that is used to query pubchem
    #' @param rdkit Should an attempt be made to retrieve chemical
    #' information from a local rdkit installation via python
    #' and the reticulate package?
    #' @param template An optional SMILES code to be used as template for RDKit
    #' @param chyaml Should we look for a identifier.yaml file in the working
    #' directory?
    initialize = function(identifier, smiles = NULL, inchikey = NULL,
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
            available_smiles <- names(self$smiles)
            smiles_preference <- c("user", "PubChem_Isomeric", "PubChem_Canonical")
            smiles_preferred_i <- min(match(available_smiles, smiles_preference))
            smiles_preferred <- smiles_preference[smiles_preferred_i]

            message("Trying to get chemical information from RDKit using ",
                    smiles_preferred, " SMILES\n",
                    self$smiles[smiles_preferred])
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

    #' Try to get chemical information from PubChem
    #' @param query Query string to be passed to [get_cid][webchem::get_cid]
    #' @param from Passed to [get_cid][webchem::get_cid]
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

    #' Get chemical information from PubChem for a known PubChem CID
    #' @param pubchem_cid CID
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

    #' Get chemical information from RDKit if available
    #' @param template Optional template specified as a SMILES code
    get_rdkit = function(template = NULL) {
      if (!rdkit_available) {
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

    #' Obtain information from a YAML file
    #' @param repo Should the file be looked for in the current working
    #' directory, a local git repository under `~/git/chyaml`, or from
    #' the web (not implemented).
    #' @param chyaml The filename to be looked for
    get_chyaml = function(repo = c("wd", "local", "web"),
      chyaml = paste0(URLencode(self$identifier), ".yaml"))
    {
      repo = match.arg(repo)
      paths = c(
        wd = ".",
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

    #' Add a vapour pressure
    #' @param p0 The vapour pressure in Pa
    #' @param T Temperature
    #' @param source An acronym specifying the source of the information
    #' @param page The page from which the information was taken
    #' @param remark A remark
    add_p0 = function(p0, T = NA, source = NA, page = NA, remark = "") {
      self$p0 <- p0
      attr(self$p0, "T") <- T
      attr(self$p0, "source") <- source
      attr(self$p0, "page") <- page
      attr(self$p0, "remark") <- remark
    },

    #' Add a water solubility
    #' @param cwsat The water solubility in mg/L
    #' @param T Temperature
    #' @param pH The pH value
    #' @param source An acronym specifying the source of the information
    #' @param page The page from which the information was taken
    #' @param remark A remark
    add_cwsat = function(cwsat, T = NA, pH = NA,
      source = NA, page = NA, remark = "")
    {
      self$cwsat <- cwsat
      attr(self$cwsat, "T") <- T
      attr(self$cwsat, "pH") <- pH
      attr(self$cwsat, "source") <- source
      attr(self$cwsat, "page") <- page
      attr(self$cwsat, "remark") <- remark
    },

    #' Add a plant uptake factor
    #' @param PUF The plant uptake factor, a number between 0 and 1
    #' @param source An acronym specifying the source of the information
    #' @param page The page from which the information was taken
    #' @param remark A remark
    add_PUF = function(PUF = 0,
      source = "focus_generic_gw_2014", page = 41,
      remark = "Conservative default value")
    {
      self$PUF <- PUF
      attr(self$PUF, "source") <- source
      attr(self$PUF, "page") <- page
      attr(self$PUF, "remark") <- remark
    },

    #' @field TPs List of transformation products as chent objects
    TPs = list(),

    #' Add a transformation product to the internal list
    #' @param x A [chent] object, or an identifier to generate a [chent] object
    #' @param smiles A SMILES code for defining a [chent] object
    #' @param pubchem Should chemical information be obtained from PubChem?
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

    #' @field transformations Data frame of observed transformations
    transformations = data.frame(study_type = character(0),
      TP_identifier = character(0),
      max_occurrence = numeric(0),
      source = character(0),
      page = character(0),
      stringsAsFactors = FALSE),

    #' Add a line in the internal dataframe holding observed transformations
    #' @param study_type A characterisation of the study type
    #' @param TP_identifier An identifier of one of the transformation products
    #' in `self$TPs`
    #' @param max_occurrence The maximum observed occurrence of the
    #' transformation product, expressed as a fraction of the amount that would
    #' result from stochiometric transformation
    #' @param source An acronym specifying the source of the information
    #' @param pages The page from which the information was taken
    #' @param remark A remark
    add_transformation = function(study_type, TP_identifier, max_occurrence,
      remark = "", source = NA, pages = NA)
    {
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
          page = page,
          stringsAsFactors = FALSE))
    },

    #' @field soil_degradation Dataframe of modelling DT50 values
    soil_degradation = NULL,

    #' Add a line in the internal dataframe holding modelling DT50 values
    #' @param soils Names of the soils
    #' @param DT50_mod The modelling DT50 in the sense of regulatory pesticide
    #' fate modelling
    #' @param DT50_mod_ref The normalised modelling DT50 in the sense of
    #' regulatory pesticide fate modelling
    #' @param type The soil type
    #' @param country The country (mainly for field studies)
    #' @param pH_orig The pH stated in the study
    #' @param pH_medium The medium in which this pH was measured
    #' @param pH_H2O The pH extrapolated to pure water
    #' @param perc_OC The percentage of organic carbon in the soil
    #' @param temperature The temperature during the study in degrees Celsius
    #' @param moisture The moisture during the study
    #' @param category Is it a laboratory ('lab') or field study ('field')
    #' @param formulation Name of the formulation applied, if it was not
    #' the technical active ingredient
    #' @param model The degradation model used for deriving `DT50_mod`
    #' @param chi2 The relative error as defined in FOCUS kinetics
    #' @param source An acronym specifying the source of the information
    #' @param page The page from which the information was taken
    #' @param remark A remark
    add_soil_degradation = function(soils, DT50_mod, DT50_mod_ref,
      type = NA, country = NA,
      pH_orig = NA, pH_medium = NA, pH_H2O = NA,
      perc_OC = NA,
      temperature = NA, moisture = NA,
      category = "lab", formulation = NA,
      model = NA, chi2 = NA,
      remark = "", source, page = NA)
    {
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

    #' @field soil_ff Dataframe of formation fractions
    soil_ff = NULL,

    # Add one or more formation fractions for degradation in soil
    #' @param target The identifier(s) of the transformation product
    #' @param soils The soil name(s) in which the transformation was observed
    #' @param ff The formation fraction(s)
    add_soil_ff = function(target, soils, ff = 1,
      remark = "", source, page = NA)
    {
      new_soil_ff = data.frame(
        target = target,
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

    #' @field soil_sorption Dataframe of soil sorption data
    soil_sorption = NULL,

    #' Add soil sorption data
    #' @param Kf The sorption constant in L/kg, either linear (then `N` is 1)
    #' or according to Freundlich
    #' @param Kfoc The constant from above, normalised to soil organic carbon
    #' @param N The Freundlich exponent
    #' @param perc_clay The percentage of clay in the soil
    #' @param CEC The cation exchange capacity
    add_soil_sorption = function(soils, Kf, Kfoc, N,
      type = NA, pH_orig = NA, pH_medium = NA,
      pH_H2O = NA,
      perc_OC = NA, perc_clay = NA, CEC = NA,
      remark = "", source, page = NA)
    {
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

    #' Write a PDF image of the structure
    #' @param file The file to write to
    #' @param dir The directory to write the file to
    #' @param template A template expressed as SMILES to use in RDKit
    pdf = function(file = paste0(self$identifier, ".pdf"),
        dir = "structures/pdf", template = NULL) {
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

    #' Write a PNG image of the structure
    #' @param antialias Passed to [png][grDevices::png]
    png = function(file = paste0(self$identifier, ".png"),
      dir = "structures/png", antialias = 'gray')
    {
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

    #' Write an EMF image of the structure using [emf][devEMF::emf]
    #' @param file The file to write to
    emf = function(file = paste0(self$identifier, ".emf"),
      dir = "structures/emf")
    {
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
#' @examples
#' caffeine <- chent$new("caffeine")
#' print(caffeine)
#' if (!is.null(caffeine$Picture)) {
#'   plot(caffeine)
#' }
plot.chent = function(x, ...) {
  if (is.null(x$Picture)) stop("No Picture object in chent, was RDKit available during creation?")
  grid.picture(x$Picture)
}

#' @title An R6 class for pesticidal active ingredients and associated data
#'
#' @description The class is initialised with an identifier which is generally
#' an ISO common name.  Additional chemical information is retrieved from the
#' internet if available.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @examples
#' # On Travis, we get a certificate validation error,
#' # likely because the system (xenial) is so old,
#' # therefore don't run this example on Travis
#' if (Sys.getenv("TRAVIS") == "") {
#'
#' atr <- pai$new("atrazine")
#' print(atr)
#' if (!is.null(atr$Picture)) {
#'   plot(atr)
#' }
#'
#' }

pai <- R6Class("pai",
  inherit = chent,
  public = list(

    #' @field iso ISO common name of the active ingredient according to ISO 1750
    iso = NULL,

    #' @field bcpc Information retrieved from the BCPC compendium available online
    #' at <pesticidecompendium.bcpc.org>
    bcpc = NULL,

    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @description This class is derived from [chent]. It makes it easy
    #' to create a [chent] from the ISO common name of a pesticide active
    #' ingredient, and additionally stores the ISO name as well as
    #' the complete result of querying the BCPC compendium using
    #' [bcpc_query][webchem::bcpc_query].
    #'
    #' @param iso The ISO common name to be used in the query of the
    #' BCPC compendium
    #'
    #' @param identifier Alternative identifier used for querying pubchem
    initialize = function(iso, identifier = iso,
      smiles = NULL, inchikey = NULL, bcpc = TRUE,
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
          if (length(bcpc_ik) == 1 && !is.na(bcpc_ik)) {
            if (is.null(self$inchikey)) {
              self$inchikey = substr(self$bcpc$inchikey, 1, 27)
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
        smiles = smiles, inchikey = self$inchikey,
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

#' @title R6 class for a plant protection product with at least one active ingredient
#'
#' @description Contains basic information about the active ingredients in the
#' product
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object.

ppp <- R6Class("ppp",
  public = list(

    #' @field name The name of the product
    name = NULL,

    #' @field ais A list of active ingredients
    ais = list(),

    #' @field concentrations The concentration of the ais
    concentrations = NULL,

    #' @field concentration_units Defaults to g/L
    concentration_units = NULL,

    #' @field density The density of the product
    density = NULL,

    #' @field density_units Defaults to g/L
    density_units = "g/L",

    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @field ... Identifiers of the active ingredients
    #' @field concentrations Concentrations of the active ingredients
    #' @field concentration_units Defaults to g/L
    #' @field density The density
    #' @field density_units Defaults to g/L
    initialize = function(name, ..., concentrations, concentration_units = "g/L",
      density = 1000, density_units = "g/L")
    {
      self$name <- name
      self$ais <- list(...)
      self$concentrations <- concentrations
      self$density <- density
      self$density_units <- density_units
      names(self$concentrations) <- names(self$ais)
      self$concentration_units <- concentration_units
    },

    #' Printing method
    print = function() {
      cat("<pp> named", self$name, "\n")
    }
  )
)

# vim: set ts=2 sw=2 expandtab:
