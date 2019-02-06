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
  reqdPkgs = list("raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start 
                            time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between two runs of this module,
                            expressed in years."),
    defineParameter(name = ".useCache", class = "logical", default = FALSE, 
                    desc = "Should this entire module be run with caching 
                            activated? This is generally intended for data-type
                            modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(
      objectName = "MDC_BCR6_NWT_250m",
      objectClass = "RasterLayer",
      sourceURL = NA_character_,
      desc = "Monthly Drought Code within BCR6 as contained in the Northwest Territories."
    ),
    expectsInput(
      objectName = "LCC_BCR6_NWT",
      objectClass = "RasterLayer",
      sourceURL = "https://drive.google.com/open?id=1WhL-DxrByCbzAj8A7eRx3Y1FVujtGmtN",
      desc = "Land Cover Map of Canada 2005 (LCC05) within BCR6 as contained in the Northwest Territories."
    ),
    expectsInput(
      objectName = "NFDB_PO_BCR6_NWT",
      objectClass = "RasterLayer",
      sourceURL = "https://drive.google.com/open?id=15Fl6XCsNTZtA2G3py0Ma5ZTaESWwn622",
      desc = "National Fire DataBase polygon data (NFDB_PO) within BCR6 as contained in the Northwest Territories."
    )
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
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
  
  sim <- scheduleEvent(sim, P(sim)$.runInitialTime, "fireSense_NWT_DataPrep", "Run")
  
  invisible(sim)
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) 
{
  #
  # Reclassify LCC05 at initialisation, so we don't have to do it every year
  #
  rcl <- matrix(
      #    from,   to,   becomes
    c(        -1,  0.01,      14, # After visual inspection, likely herbs/shrubs
            0.99,  1.01,       1,
            1.99,  2.01,       5,
            2.99,  3.01,       7,
            3.99,  4.01,       8,
            4.99,  5.01,       9,
            5.99, 10.01,       1,
           10.99, 12.01,       5,
           12.99, 13.01,       7,
           13.99, 14.01,       9,
           14.99, 15.01,       6,
           15.99, 16.01,      14,
           16.99, 17.01,      10,
           17.99, 18.01,      14,
           18.99, 19.01,      15, # Wetlands, waiting for Tati's update
           19.99, 20.01,      11,
           20.99, 24.01,      14,
           24.99, 25.01,      10,
           25.99, 29.01,       4,
           29.99, 31.01,      10,
           31.99, 32.01,      15, # Wetlands (here lichen-spruce bog), waiting for Tati's update
           32.99, 33.01,       0, # do not burn
           33.99, 35.01,       6,
           35.99, 39.01,       0  # do not burn
    ),
    ncol = 3,
    byrow = TRUE
  )
  
  LCC05_BCR6_NWT_rcl <- cloudCache(
    reclassify, 
    x = LCC05_BCR6_NWT, 
    rcl = rcl, 
    cloudFolderID = "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
  )
  
  sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, "fireSense_NWT_DataPrep", "Run")
  invisible(sim)
}


PrepThisYearLCC <- function(sim)
{
  Cache(
    rasterize,
    x = as(
      st_union(
        filter(NFDB_PO_BCR6_NWT, YEAR > (year - 15) & YEAR <= year)
      ),
      "Spatial"
    ),
    y = LCC05_BCR6_NWT, 
    getCover = TRUE
  )
  
  
  climateNA_outputs <- read.csv.ffdf(
    file = "C:/climateNA_inputs_1995-2015AMT.csv", 
    header = TRUE, 
    colClasses = c("integer", "integer", "factor", rep("numeric", 171))
  )
  
  climateNA_outputs <- climateNA_outputs[
    ,
    vars_select(
      colnames(climateNA_outputs), 
      Year, ID1, Latitude, Longitude, 
      num_range("Tmax", 4:9, 2), num_range("PPT",  4:9, 2)
    )
    ]
  
  
  invisible(sim)
}


Run <- function(sim) 
{
  
  sim <- PrepThisYearLCC(sim)
  
  if (!is.na(P(sim)$.runInterval)) # Assumes time only moves forward
    sim <- scheduleEvent(sim, currentTime + P(sim)$.runInterval, moduleName, "run")
  
  invisible(sim)
}



### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(sim$object)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
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

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
