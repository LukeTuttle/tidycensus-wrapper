## Author: Luke Tuttle
## Created: Tue Jun 23 11:53:21 2020
## Updated:
## Purpose of script: to provide a self contained function for downloading Census ACS data
##
##
## -----------------------
## Notes: BEWARE!!!!!!!! 2009 VARIABLE LABELS TYPICALLY DIFFER FROM THE OTHER YEARS
## This function relies on the tidycensus package. All arguments
##  other than 'which.races' and 'use.parallel' simply feed into tidycensus::get_acs()
##  If you feel like you need more information on the arguments (particularly 'geography')
##  then look here https://github.com/walkerke/tidycensus or simply search 'tidycensus' online.
##
## -----------------------

# README!!!: To get acs data you will need to run the first two sections of this script:
# 'required packages and API key' and 'define function'. These two sections can theoretically be copied/pasted to
# any script and it should work. They are pretty much self contained and dont need access to Google Drive.
# After these two sections there are simply examples and explanations.



# required  packages and API Key ------------------------------------------


# check to see if the required packages are installed and if not install them
list.of.packages <- c('tidyverse', 'tidycensus', 'furrr', 'janitor')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load the required packages
lapply(list.of.packages, library, character.only = TRUE)


# This checks to see if the user already has a census API key
# and if not then it will install one and reload the R environment
if(is.na(Sys.getenv('CENSUS_API_KEY'))){
  #IMPORTANT! replace the hash at the end with the digits of my childhood
  # home's house number summed together minus 3. 
  census_api_key("19f8500f78c8baa62f59b7e6974647d454ef55f#", install = TRUE)
  readRenviron("~/.Renviron")
}


# define function ---------------------------------------------------------


get_acs_combo <- function(table = NULL, year = 2018, geography, state = NULL, county = NULL, survey = "acs5", which.races = NULL, use.parallel = T) {
  # acs1 doesn't have data for 2009
  if(survey == "acs1" & any(year %in% 2009)) stop("acs1 cannot be used with 2009")
  # ----------------------
  # Make sure required packages are loaded
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("furrr", quietly = TRUE)) {
    stop("Package \"furrr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("tidycensus", quietly = TRUE)) {
    stop("Package \"tidycensus\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("janitor", quietly = TRUE)) {
    stop("Package \"janitor\" needed for this function to work. Please install it.",
         call. = FALSE)
  }


  #  not needed in this version of the script ----------------------
  #  acsvars will be in the user's global environment
  # # This function takes a file-path in any format and returns the system-appropriate path.
  # compatible_path <- function(path_raw, dir_drive="") {
  #
  #   # Not useful in the current script (get_acs_combo.R), but necessary if using this function in a general context
  #   require(tidyverse)
  #
  #   # The system-appropriate path to the Drive directory is determined.
  #   # If a value for dir_drive was passed, then it is used instead.
  #   if (dir_drive == "") {
  #     if (Sys.info()["sysname"] == "Windows") {
  #       dir_drive <- "G:/My Drive/"
  #     } else {
  #       dir_drive <- "~/Google Drive/"
  #     }
  #   }
  #
  #
  #   # Names of the directories that are subsequent to the Drive (i.e. x, y, z, in "dir_drive/x/y/z/") are extracted individually.
  #   dir_subs <- path_raw %>%
  #     str_extract(pattern = "(?<=Drive(\\/|\\\\))(.+)") %>% ## Removing the old path to the Drive
  #     strsplit("\\/|\\\\") ## Splitting up each of the subdirectory names
  #
  #   dir_subs <- dir_subs[[1]]
  #
  #
  #   # The Drive and subsequent directory names are combined into a single string.
  #   path_result <- dir_drive
  #
  #   for (idx in seq_along(dir_subs)) {
  #     path_result <- paste0(path_result, dir_subs[idx], "/")
  #   }
  #
  #   return(path_result)
  # }


  # -----------------------
  # Multi threading
  #   run in parellel unless specified by the user not to do so. set plan status back to what it was previous upon function end
  if (use.parallel){
    oplan <- future::plan("multisession")
    on.exit(future::plan(oplan), add = TRUE)
  }
  # ----------------------
  # Capture and update arguments as needed
  arguments <- as.list(environment(), all.names = T)

  #   allows user to specify "US" to get data for all 50 states and D.C.
  if(any(arguments$state %in% c("US", "us"))){
    arguments$state <- unique(tidycensus::fips_codes$state)[c(1:51)]
  }

  years <- arguments$year
  if("2009" %in% as.character(years)) {
    message("WARNING: 2009 has wonky level label (levlabs) which may cause joining issues.
            Will fix labs in the future")
  }
  # ----------------------
  #TO DO: Figure out how to handle errors
  # ----------------------
  # Use tidycensus to ping census API
  # For non race iteration tables
  if(is.null(arguments$which.races)){
    # remove which.races so it doesn't filter down to si_acs()
    arguments <- arguments[which(names(arguments) != "which.races")]
    names(years) <- as.character(years)
    my_df<- furrr::future_map_dfr(years, .progress = T, .id = "year", .f = function(x){
      arguments$year <- x
      do.call(tidycensus::get_acs, args = arguments)
    })
    my_df$year <- as.integer(my_df$year)
  } else {
    # For race iteration tables
    # get a table with by-race breakdown
    races <- c(
      "White alone" = "A",
      "Black or African American Alone" = "B",
      "American Indian and Alaska Native Alone" = "C",
      "Asian Alone" = "D",
      "Native Hawaiian and Other Pacific Islander Alone" = "E",
      "Some Other Race Alone" = "F",
      "Two or More Races" = "G",
      "White Alone, Not Hispanic or Latino" = "H",
      "Hispanic or Latino" = "I"
    )
    races_input <- stringr::str_to_upper(arguments$which.races)
    arguments <- arguments[which(names(arguments) != "which.races")]

    if (races_input[1] == "ALL") {
      races <- races
    } else {
      races <- races[which(races %in% races_input)]
    }

    # this could be cleaned up a bit but its meant to preserve the names of races for use in row_binding in the future_map_dfr
    yr_l <- purrr::cross_df(list("races" = races, "years" = years))
    race_tibble <- tibble::tibble(race_code = races, race_name = names(races))
    race_set_names <- suppressMessages(tibble::tibble(race_code = yr_l$races) %>% left_join(race_tibble) %>% pull(race_name))

    race_named_vec <- yr_l$races %>% purrr::set_names(race_set_names)

    my_df <-  furrr::future_map2_dfr(.x = race_named_vec,
                                  .y = yr_l$years,
                                  .progress = T,
                                  .id = c("race"),
                                  .f = function(x,y){
                                    arguments$table <- paste0(arguments$table, x)
                                    arguments$year <- y
                                    x <- do.call(tidycensus::get_acs, args = arguments)
                                    x$year <- as.integer(y)
                                    x
                                  })

  }
  # -----------------------

  # Join the variable strings to the downloaded table and
  # give the column describing the geography a sensible name
  # if the user file structure doesn't fit with the path below
  # give them a sensible error message



  # TO DO: Figure out error message handling if acsvars cant be read in.

  # acsvars <- read_rds(paste0(compatible_path("~/Google Drive/SI/DataScience/data/maps_project/cleaned_data/census/"), "acsvars_", arguments$survey, ".rds"))

# get acsvar labels to join to -------------------------------------------------

  if(exists('my_df') && !exists('acsvars')) {
    message("\n\nThe Census data has succesfully downloaded\nNow we need to get meaningful labels.\nThis could take a while...")
    get_acs_vars <- function(years, survey) {

      furrr::future_map_dfr(years, .progress = T,
                            .f = function(x, y = survey){
                              load_variables(year = x, dataset = y, cache = T) %>%
                                mutate(level = str_count(label, pattern = "!!")) %>%
                                rowwise() %>%
                                mutate(levlab = str_split(label, pattern = "!!") %>% unlist() %>% .[level + 1]) %>%
                                ungroup() %>%
                                mutate(concept = str_to_title(concept),
                                       year = !! x) %>%
                                rename(variable = name)
                            })
    }

    # acsvars ends up containing all census variables. It's used to join
    # with actual data and give you more informative table levels and descriptors for when you actually pull the data in using si_acs()
    acsvars_bound <- bind_rows("acs1" = get_acs_vars(2010:2018, "acs1"),
                               "acs5" = get_acs_vars(2009:2018, "acs5"),
                               .id = "survey")


    # add columns which indicate which table a variable belongs to and whether it has racial iterations and if it has a non racial version as well. ToDo: Needs a column which tells variable by variable whether it is in the non race version of the table or not because the non racial versions have slightly different variable levels than the racial versions.
    x <- acsvars_bound %>% mutate(table_num_long = substr(variable, 1, 7))
    y <- acsvars_bound %>%
      mutate(table_num_long = substr(variable, 1, 7)) %>%
      distinct(table_num_long) %>%
      mutate(table_num = substr(table_num_long, 1, 6)) %>%
      add_count(table_num) %>%
      mutate(has_race_versions = ifelse(n >= 9 , T, F),
             has_non_race_versions = ifelse(n != 9, T, F))
    # special assignment operator assigns to global envr. so acsvars only has to be downloaded once
    acsvars <<- left_join(x,y) %>%
      filter(str_detect(variable, "PR", negate = T)) %>%
      select(-c(table_num_long, n)) %>%
      rowwise() %>%
      mutate(is_race_table_var = any(str_detect(substr(variable, 2, nchar(variable)), LETTERS[1:9]))) %>%
      ungroup() %>%
      select(-c("has_race_versions", "has_non_race_versions", "is_race_table_var"))

    # acsvars_acs5 <- filter(acsvars, survey == "acs5")
    # acsvars_acs1 <- filter(acsvars, survey == "acs1")
  }
  acsvars <- acsvars %>% filter(year %in% !! arguments$year, survey == arguments$survey)

  df2 <- janitor::clean_names(my_df)
  df3 <- left_join(df2, acsvars, by = c("variable", "year"))
  df4 <- select(df3, -moe, -any_of(c('has_race_versions', 'has_non_race_versions', 'is_race_table_var'))) %>% # remove cols
    select(matches("[^(end_year|survey)]"), survey, year)
  if (arguments$geography == "county"){
    df5 <- tidyr::separate(df4, name, into = c("county", "state"), sep = ", ")
  } else {
    df5 <- rename(df4,!! arguments$geography := name)
  }
  df5
}





# Arguments:
#   get_acs_combo(table = "", year = , geography = "", state = , county = , survey = , which.races = "", use.parallel = T)
#
# 1) `table` Takes either B or C ACS table codes in quotes. censusreporter.org is a great resource for looking up the correct
#            table number for the information you need. You can also look at the following link for tables commonly used
#            by the Center.
#   https://docs.google.com/spreadsheets/d/----REMOVED
#
# 2) `year` Takes a single or a vector of years
# Examples:
# 2018 - a particular year
# 2014:2018  - a period (also could be a vector's name)
#
#
# 3) `geography` identifies on which level data is needed (put in quotes).
# Options: below are the most common options, however, more options do exist
# "state"  - data by state
# "county" - data by county
# "tract" - data by census tract
#
# 4) `state` identifies which state(s) to pull data for. Can take state abbreviation or fips code
# Examples:
# "us" or "US" - all states
# "UT" - Utah state only
# c("NY","UT") - New York and Utah states only
#
# 5) `county` pull data for a specific county. We rarely use this argument. Takes a fips code or county name.
#             if you use a county name then you must also specify the state in the state arguement.
#
# 6) `survey` identifies which particular ACS survey to use.
# Options:
#   "acs5" - 5-year estimates
#   "acs1" - 1-year estimates
#
#
# 7) `which.races` specifies which race iteration tables you'd like to download (if available).
#
# Options:
# NULL - standard table (i.e. gen. pop, no racial iteration tables)
# "all" - All race tables A-I
# "A" - White alone
# "B" - Black or African American Alone
# "C" - American Indian and Alaska Native Alone
# "D" - Asian Alone
# "E" - Native Hawaiian and Other Pacific Islander Alone
# "F" - Some Other Race Alone
# "G" - Two or More Races
# "H" - White Alone, Not Hispanic or Latino
# "I" - Hispanic or Latino
#
# 6) `use.parallel` allows function to run faster via parrelell processing
# TRUE - parallelize the process
# FALSE - do not parallelize the process (function execution takes much longer)


# example code for obtaining acs data -------------------------------------
#B01003 table - population for 2009-2018 period on tract level.
#`which.races = NULL`  means no race iteration tables
#`state = US` to get data for all states
#`geography = tract` for data on tract level
# B01003 <- get_acs_combo(table = "B01003",year = 2009:2018, state = "US", geography = "tract", which.races = NULL, survey = 'acs5', use.parallel = T)
#
# #B01001 table -  population by age, sex, and race on tract level for 2010-2018
# #`which.races = all` to download all race iteration tables
# B01001 <- get_acs_combo(table = "B01001", year = 2010:2018, state = "US", which.races = 'all', geography = "tract", survey = 'acs5', use.parallel = T)
#
# #B01001 table -  population by age, sex, and race on tract level for 2013-2018
# #`which.races = NULL to download the the standard table only (i.e. the table for general population as opposed to a race-specific table)
# B01001 <- get_acs_combo(table = "B01001", year = 2013:2018, state = "US", which.races = NULL, geography = "tract", survey = 'acs5', use.parallel = T)


# output columns description ------------------------------------------------------
#"geoid"  - geoid of specific area
#"tract" or `state` or `county` - name of the tract, state, or county
#"race" - enthnicity/race group
#"estimate" - population/value
#"label" - description of the "estimate" column's value
#"variable" - code of variable's label
#"concept" - extra description of data
#"table_num" - name (code) of the census table used
#"survey" - type of survey data used (acs5 or acs1)
#"year" - year
#"levlab" - description of a group or subgroup of "level" column
#"level" - data level. The biggest group will be on level 1, each next subgroup will be 1 level higher.
#Example:  <Total population> - level 1,
#<Total Population Male> or <Total Population Female> - level 2,
#<Total Population Female 25 to 29 years old> - level 3
