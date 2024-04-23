message('\nLoading dalycare package...')
library(RPostgres)
library(tidyverse)
library(docstring)

if (exists("user") & exists("password")){
    
    current_wd = getwd()
    setwd("/ngc/projects2/dalyca_r/clean_r/")

    # paths for all directories
    constants_path = "dalycare/constants/"
    helpers_path = "dalycare/helpers/"
    loaders_path = "dalycare/loaders/"
    definitions_path = "dalycare/definitions/"
    cleaners_path = "dalycare/cleaners/"
    plotters_path = "dalycare/plotters/"

    # all files in all directories
    constants = list.files(constants_path)
    loaders = list.files(loaders_path)
    helpers = list.files(helpers_path)
    definitions = list.files(definitions_path)
    cleaners = list.files(cleaners_path)
    plotters = list.files(plotters_path)

    # source all functions
    lapply(constants, function(constant) source(paste0(constants_path, constant)))
    lapply(loaders, function(loader) source(paste0(loaders_path, loader)))
    lapply(helpers, function(helper) source(paste0(helpers_path, helper)))
    lapply(definitions, function(definition) source(paste0(definitions_path, definition)))
    lapply(cleaners, function(cleaner) source(paste0(cleaners_path, cleaner)))
    lapply(plotters, function(plotter) source(paste0(plotters_path, plotter)))

    setwd(current_wd)

    }
else{
    print("The DALYCARE package relies on access to the database. Please define username and password as variables in your environment.")
}
