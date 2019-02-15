# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "fireSense_NWT_DataPrep",
  description = "Prepare climate and vegetation data needed to run the fireSense modules for BCR6 and BCR6 contained in the Northwest Territories.", #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.4", fireSense_NWT_DataPrep = "1.0.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_NWT_DataPrep.Rmd"),
  reqdPkgs = list("dplyr", "raster", "rlang", "sf", "tibble"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(name = "res", class = "numeric", default = 10000,
                    desc = "at which resolution should we aggregate the data? By
                            default, 10km."),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start 
                            time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = 1, 
                    desc = "optional. Interval between two runs of this module,
                            expressed in years. By default, every year."),
    defineParameter(name = ".useCache", class = "logical", default = FALSE, 
                    desc = "Should this entire module be run with caching 
                            activated? This is generally intended for data-type
                            modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(
      objectName = "cloudFolderID",
      objectClass = "character",
      sourceURL = NA_character_,
      desc = "GDrive folder ID for cloud caching."
    ),
    expectsInput(
      objectName = "MDC_BCR6_NWT_250m",
      objectClass = "RasterStack",
      sourceURL = NA_character_,
      desc = "Monthly Drought Code (April to September) within BCR6 as contained in the Northwest Territories."
    ),
    expectsInput(
      objectName = "LCC05_BCR6_NWT",
      objectClass = "RasterLayer",
      sourceURL = "https://drive.google.com/open?id=1WhL-DxrByCbzAj8A7eRx3Y1FVujtGmtN",
      desc = "Land Cover Map of Canada 2005 (LCC05) within BCR6 as contained in the Northwest Territories."
    ),
    expectsInput(
      objectName = "NFDB_PO_BCR6_NWT",
      objectClass = "sf",
      sourceURL = "https://drive.google.com/open?id=15Fl6XCsNTZtA2G3py0Ma5ZTaESWwn622",
      desc = "National Fire DataBase polygon data (NFDB_PO) within BCR6 as contained in the Northwest Territories."
    ),
    expectsInput(
      objectName = "NFDB_PT_BCR6_NWT",
      objectClass = "sf",
      sourceURL = "https://drive.google.com/open?id=1HU2lGMYmMoyXkDVjYLGhvAiC0ZkY-XMg",
      desc = "National Fire DataBase point data (NFDB_PT) within BCR6 as contained in the Northwest Territories."
    )
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(
      objectName = "dataFireSense_FireFrequency", 
      objectClass = "data.frame", 
      desc = "Contains MDC, land-cover, fire data necessary to train fireSense_FireFrequency for BCR6 as contained in the Northwest Territories."
    )
  )
))

## event types
#   - type `init` is required for initialization

doEvent.fireSense_NWT_DataPrep = function(sim, eventTime, eventType) 
{
  switch(
    eventType,
    init = { sim <- Init(sim) },
    run = { sim <- Run(sim) },
    warning(
      paste(
        "Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
        "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""
      )
    )
  )
  
  invisible(sim)
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) 
{
  
  wetLCC <- Cache(
    reproducible::prepInputs,
    url = "https://drive.google.com/open?id=1YVTcIexNk-obATw2ahrgxA6uvIlr-6xm",
    targetFile = "wetlandsNWT250m.tif",
    rasterToMatch = sim[["LCC05_BCR6_NWT"]],
    maskWithRTM = TRUE,
    filename2 = NULL
  )
  
  # wetLCC code for Water 1
  # wetLCC code for Wetlands 2
  # wetLCC code for Uplands 3
  
  sim[["LCC05_BCR6_NWT"]][wetLCC == 1] <- 37 # LCC05 code for Water bodies
  sim[["LCC05_BCR6_NWT"]][wetLCC == 2] <- 19 # LCC05 code for Wetlands

  #
  # Reclassify LCC05 at initialisation, so we don't have to do it every year
  #
  mod[["reclass_code"]] <- b <- c(
    CN_HD =	1,
    CN_MD = 2,
    CN_LD = 3,
    CROPS = 4,
    HWOOD = 5,
    DISTB = 6,
    MX_CN = 7,
    MX_YG = 8,
    MX_HW = 9,
    NONVA = 10,
    OP_CN = 11,
    SHRUB = 12,
    WTLND = 13
  )
  
  rcl <- matrix(
      #    from,   to,      becomes
    c(        -1,  0.01, b[["SHRUB"]], # After visual inspection and discussion with GDT, likely herbs/shrubs
            0.99,  1.01, b[["CN_HD"]],
            1.99,  2.01, b[["HWOOD"]],
            2.99,  3.01, b[["MX_CN"]],
            3.99,  4.01, b[["MX_YG"]],
            4.99,  5.01, b[["MX_HW"]],
            5.99, 10.01, b[["CN_HD"]],
           10.99, 12.01, b[["HWOOD"]],
           12.99, 13.01, b[["MX_CN"]],
           13.99, 14.01, b[["MX_HW"]],
           14.99, 15.01, b[["DISTB"]],
           15.99, 16.01, b[["SHRUB"]],
           16.99, 17.01, b[["NONVA"]],
           17.99, 18.01, b[["SHRUB"]],
           18.99, 19.01, b[["WTLND"]], # LCC05 wetlands
           19.99, 20.01, b[["OP_CN"]],
           20.99, 24.01, b[["SHRUB"]],
           24.99, 25.01, b[["NONVA"]],
           25.99, 29.01, b[["CROPS"]],
           29.99, 31.01, b[["NONVA"]],
           31.99, 32.01, b[["WTLND"]], # Wetlands (here lichen-spruce bog)
           32.99, 33.01,            0, # do not burn
           33.99, 35.01, b[["DISTB"]],
           35.99, 39.01,            0  # do not burn
    ),
    ncol = 3,
    byrow = TRUE
  )
  
  mod[["LCC05_BCR6_NWT_rcl"]] <- cloudCache(
    cloudFolderID = sim[["cloudFolderID"]],
    reclassify, 
    x = sim[["LCC05_BCR6_NWT"]], 
    rcl = rcl
  )
  
  mod[["RTM"]] <- Cache(
    aggregate,
    mod[["LCC05_BCR6_NWT_rcl"]],
    fact = P(sim)$res / xres(mod[["LCC05_BCR6_NWT_rcl"]]),
    fun = function(x, ...) if (anyNA(x)) NA else 1
  )
  
  sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, "fireSense_NWT_DataPrep", "run")
  invisible(sim)
}

PrepThisYearMDC <- function(sim)
{
  currentYear <- time(sim, "year")
  
  mod[["MDC"]] <- Cache(
    stack,
    lapply(
      unstack(sim[["MDC_BCR6_NWT_250m"]]),
      postProcess,
      rasterToMatch = mod$RTM,
      maskWithRTM = TRUE,
      method = "bilinear",
      useCache = FALSE,
      filename2 = paste0("MDC_BCR6_NWT_250m", currentYear, ".tif"),
      overwrite = TRUE
    )
  )
  
  invisible(sim)
}

PrepThisYearLCC <- function(sim)
{
  year <- time(sim, "year")
  
  #
  # LCC05 with incremental disturbances
  #
  ## Calculate proportion of recently disturbed areas for each pixel of LCC05
  #
  prop_disturbed <- cloudCache(
    cloudFolderID = sim[["cloudFolderID"]],
    rasterize,
    x = SpatialPolygonsDataFrame(
      as(
        st_union(
          filter(sim[["NFDB_PO_BCR6_NWT"]], YEAR > (year - 15) & YEAR <= year)
        ),
        "Spatial"
      ), 
      data = data.frame(ID = 1),
      match.ID = FALSE
    ),
    y = mod[["LCC05_BCR6_NWT_rcl"]], 
    getCover = TRUE
  )

  #
  ## Update LCC05
  #
  mod[["LCC05_BCR6_NWT_rcl"]] <- cloudCache(
    cloudFolderID = sim[["cloudFolderID"]],
    `[<-`,
    x = mod[["LCC05_BCR6_NWT_rcl"]], 
    i = prop_disturbed[] >= .5, 
    value = mod[["reclass_code"]][["DISTB"]] # Code for disturbed areas
  )
  
  n_lcc <- 13
  
  mod$pp_lcc <- 
    lapply(
      1:n_lcc,
      function(cl_i)
      {
        calc_prop_lcc <- function(x, cl = cl_i, na.rm = TRUE)
        {
          if (anyNA(x)) return(NA)
          sum(x == cl, na.rm = na.rm) / (P(sim)$res ** 2)
        }
        
        col_name <- paste0("cl", cl_i)
        
        tibble( 
          !!col_name := aggregate(
            mod[["LCC05_BCR6_NWT_rcl"]], 
            fact = P(sim)$res / xres(mod[["LCC05_BCR6_NWT_rcl"]]), 
            fun = calc_prop_lcc
          )[]
        )
      }
    ) %>% bind_cols %>% filter_at(2, all_vars(!is.na(.)))
  
  invisible(sim)
}

PrepThisYearFire <- function(sim)
{
  currentYear <- time(sim, "year")
  
  NFDB_PT_BCR6_NWT <- NFDB_PT_BCR6_NWT %>%
    # Filter fire data for the current year
    dplyr::filter(YEAR == currentYear) %>%
    
    # Drop columns containing info we don't need
    dplyr::select(LATITUDE, LONGITUDE, YEAR, SIZE_HA, CAUSE) %>%
    
    # Keep only lightning fires
    dplyr::filter(CAUSE == "L")
  
  RTM_VT <- st_as_sf(rasterToPolygons(mod[["RTM"]]))
  RTM_VT[["PX_ID"]] <- which(!is.na(mod[["RTM"]][]))
  
  mod[["fires"]] <- st_set_geometry(
    mutate(
      st_join(RTM_VT, NFDB_PT_BCR6_NWT),
      YEAR = currentYear
    ), 
    NULL
  )
  
  invisible(sim)
}

Run <- function(sim) 
{
  # sim <- PrepThisYearMDC(sim)
  sim <- PrepThisYearLCC(sim)
  sim <- PrepThisYearFire(sim)

  px_id <- distinct(mod[["fires"]], PX_ID)[["PX_ID"]]
  
  # Prepare input data for the fireSense_FireFrequency module
  sim[["dataFireSense_FireFrequency"]] <- bind_cols(
    mod[["fires"]] %>%
      group_by(PX_ID, YEAR) %>%
      summarise(N_FIRES = n()),
    rename(
      as_tibble(mod[["MDC"]][px_id]),
      MDC04 = 1,
      MDC05 = 2,
      MDC06 = 3,
      MDC07 = 4,
      MDC08 = 5,
      MDC09 = 6
    ),
    mod[["pp_lcc"]]
  )
  
  if (!is.na(P(sim)$.runInterval)) # Assumes time only moves forward
    sim <- scheduleEvent(sim, time(sim) + P(sim)$.runInterval, "fireSense_NWT_DataPrep", "run")
  
  invisible(sim)
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
