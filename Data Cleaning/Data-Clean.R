# Load package libraries
library(knitr)      # for `kable()`
library(tidyverse)  # attaches the packages: ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, & forcats
library(haven)      # reads .sav files
library(lubridate)  # formats dates
library(reshape)    # re-formats data
library(zoo)        # interpolate missing values
library(cansim)     # access to Statistics Canada's "CANSIM" database

# Check Unicode characters are intact: Montréal

# Cansim Data -------------------------------------------------------------------------------------------------------
avail.data <- list_cansim_cubes()       # Uncomment to look into all available data tables from CANSIM
avail.data <- avail.data %>%
    dplyr::select("cansim_table_number", "cubeTitleEn", "cansimId", "cubeStartDate", "cubeEndDate",
                  "releaseTime", "archived", "subjectCode", "surveyCode", "frequencyCode", "corrections",
                  "dimensionNameEn", "surveyEn", "subjectEn")

## Housing Supply -------------------------------------------------------------------------------------------------------

# CMHC, housing starts, under construction and completions in selected census metropolitan areas, monthly
get_cansim_table_overview("34-10-0154", language = "english", refresh = FALSE)
supply <- get_cansim("34-10-0154")    # Spans 1951-01-01 to 2022-02-01,   Source: CMHC

supply <- supply %>% 
    dplyr::mutate(
        Date = ymd(Date),
        GEO = fct_recode(GEO,
                         "Calgary" = "Calgary, Alberta", 
                         "Edmonton" = "Edmonton, Alberta",
                         "Sudbury" = "Greater Sudbury, Ontario", 
                         "Guelph" = "Guelph, Ontario", 
                         "Hamilton" = "Hamilton, Ontario",
                         "Kelowna" = "Kelowna, British Columbia", 
                         "Kitchener" = "Kitchener-Cambridge-Waterloo, Ontario", 
                         "London" = "London, Ontario",
                         "Montreal" = "Montréal, Quebec",
                         "Oshawa" = "Oshawa, Ontario", 
                         "Ottawa" = "Ottawa-Gatineau, Ontario/Quebec",
                         "Niagara" = "St. Catharines-Niagara, Ontario",
                         "Toronto" = "Toronto, Ontario",
                         "Vancouver" = "Vancouver, British Columbia",
                         "Victoria" = "Victoria, British Columbia", 
                         "Windsor" = "Windsor, Ontario")) %>% 
    dplyr::filter(Date >= "2013-01-01" & Date <= "2021-12-01",
                  GEO %in% c("Calgary", "Edmonton", "Sudbury", "Guelph", "Hamilton", "Kelowna", 
                             "Kitchener", "London", "Montreal", "Oshawa", "Ottawa", "Niagara", 
                             "Toronto", "Vancouver", "Victoria", "Windsor")) %>% 
    dplyr::rename(Construction = `Housing estimates`,
                  Estate_Type = `Type of unit`) %>% 
    dplyr::mutate(Construction = as.factor(Construction),
                  Estate_Type = as.factor(Estate_Type),
                  Construction = fct_recode(Construction,
                                            "Starts" = "Housing starts",
                                            "UC" = "Housing under construction",
                                            "Compl" = "Housing completions"),
                  Estate_Type = fct_recode(Estate_Type,
                                           "total" = "Total units",
                                           "single" = "Single-detached units",
                                           "semi" = "Semi-detached units",
                                           "row" = "Row units")) %>%
    dplyr::filter(!(Estate_Type %in% c("Apartment and other unit types"))) %>%
    dplyr::select(Date, GEO, val_norm, Construction, Estate_Type) %>% 
    droplevels()

supply <- supply %>% 
    dplyr::mutate(Housing_type = paste0(Construction, "_", Estate_Type),
                  Housing_type = as.factor(Housing_type)) %>% 
    dplyr::select(-Construction, -Estate_Type) %>% 
    tidyr::spread(Housing_type, val_norm, fill = NA) %>% 
    dplyr::mutate(Compl_total = paste0(Compl_row + Compl_semi + Compl_single),     # must compute for correct total (-apartments)
                  Starts_total = paste0(Starts_row + Starts_semi + Starts_single),
                  UC_total = paste0(UC_row + UC_semi + UC_single),
                  Compl_total = as.numeric(Compl_total),
                  Starts_total = as.numeric(Starts_total),
                  UC_total = as.numeric(UC_total)) %>% 
    dplyr::select(Date, GEO, Compl_total, Compl_single, Compl_semi, Compl_row, Starts_total, Starts_single, 
                  Starts_semi, Starts_row, UC_total, UC_single, UC_semi, UC_row)


## Mortgage Rates Data -------------------------------------------------------------------------------------------------------

# CMHC, conventional mortgage lending rate, 5-year term: 34-10-0145 (CANSIM ID: 027-0015)
get_cansim_table_overview("34-10-0145", language = "english", refresh = FALSE)
mort.rates <- get_cansim("34-10-0145")    # Spans 1951-01-01 to 2022-02-01,   Source: CMHC,     Index: 2016-12=100

mort.rates <- mort.rates %>% 
    dplyr::mutate(Date = ymd(Date)) %>% 
    dplyr::filter(Date >= "2013-01-01" & Date <= "2021-12-01") %>% 
    dplyr::rename(Mort_rate = val_norm) %>% 
    dplyr::select(Date, Mort_rate)


# Funds advanced, outstanding balances, and interest rates for new and existing lending, Bank of Canada
get_cansim_table_overview("10-10-0006", language = "english", refresh = FALSE)
lending <- get_cansim("10-10-0006")    # Spans 2013-01-01 to 2022-01-01
lending <- lending %>%
    dplyr::mutate(Date = ymd(Date),
                  Components = as.factor(Components),
                  `Unit of measure` = fct_recode(`Unit of measure`, 
                                   "Dollars (in millions)" = "Dollars",
                                   "Interest rate (percentage)" = "Interest rate")) %>%
    dplyr::filter(Date >= "2013-01-01" & Date <= "2021-12-01") %>%
    dplyr::select(Date, GEO, VALUE, val_norm, Components, `Unit of measure`)

lending <- lending %>%
    dplyr::filter(Components %in% c("Total, funds advanced, residential mortgages, insured",
                                    "Variable rate, funds advanced, residential mortgages, insured",
                                    "Fixed rate, funds advanced, residential mortgages, insured, less than 1  year",
                                    "Fixed rate, funds advanced, residential mortgages, insured, from 1  to less than 3 years",
                                    "Fixed rate, funds advanced, residential mortgages, insured, from 3 to less than 5 years",
                                    "Fixed rate, funds advanced, residential mortgages, insured, 5 years and more",
                                    
                                    "Total, funds advanced, residential mortgages, uninsured",
                                    "Variable rate, funds advanced, residential mortgages, uninsured",
                                    "Fixed rate, funds advanced, residential mortgages, uninsured, less than 1 year",
                                    "Fixed rate, funds advanced, residential mortgages, uninsured, from 1 to less than 3 years",
                                    "Fixed rate, funds advanced, residential mortgages, uninsured, from 3 to less than 5 years",
                                    "Fixed rate, funds advanced, residential mortgages, uninsured, 5 years and more",
                                    
                                    "Total, funds advanced for non-mortgage loans, consumer credit"
                                    )) %>% 
    dplyr::mutate(Date = ymd(Date),
                  Components = fct_recode(Components, 
                                          "Credit_rate" = "Total, funds advanced for non-mortgage loans, consumer credit",
                                          
                                          "InMort_total" = "Total, funds advanced, residential mortgages, insured",
                                          "InMort_var" = "Variable rate, funds advanced, residential mortgages, insured",
                                          "InMort_fix0_1" = "Fixed rate, funds advanced, residential mortgages, insured, less than 1  year",
                                          "InMort_fix1_3" = "Fixed rate, funds advanced, residential mortgages, insured, from 1  to less than 3 years",
                                          "InMort_fix3_5" = "Fixed rate, funds advanced, residential mortgages, insured, from 3 to less than 5 years",
                                          "InMort_fix5" = "Fixed rate, funds advanced, residential mortgages, insured, 5 years and more",
                                          
                                          "UnMort_total" = "Total, funds advanced, residential mortgages, uninsured",
                                          "UnMort_var" = "Variable rate, funds advanced, residential mortgages, uninsured",
                                          "UnMort_fix0_1" = "Fixed rate, funds advanced, residential mortgages, uninsured, less than 1 year",
                                          "UnMort_fix1_3" = "Fixed rate, funds advanced, residential mortgages, uninsured, from 1 to less than 3 years",
                                          "UnMort_fix3_5" = "Fixed rate, funds advanced, residential mortgages, uninsured, from 3 to less than 5 years",
                                          "UnMort_fix5" = "Fixed rate, funds advanced, residential mortgages, uninsured, 5 years and more")) %>%
    droplevels()

# Various national mortgage & consumer credit interest rates
Lend.int <- lending %>% 
    dplyr::filter(`Unit of measure` == "Interest rate (percentage)") %>% 
    dplyr::select(Date, val_norm, Components) %>%
    droplevels() %>% 
    tidyr::spread(Components, val_norm, fill = NA) %>% 
    dplyr::mutate(InMort_fix1_over = (InMort_fix1_3 + InMort_fix3_5 + InMort_fix5)/3,
                  UnMort_fix1_over = (UnMort_fix1_3 + UnMort_fix3_5 + UnMort_fix5)/3,) %>% 
    dplyr::select(Date, Credit_rate, InMort_total, InMort_var, InMort_fix0_1, InMort_fix1_over,
                  UnMort_total, UnMort_var, UnMort_fix0_1, UnMort_fix1_over)

# Total national Mortgage & consumer credit debt amounts
# Lend.fig <- lending %>% 
#     dplyr::filter(`Unit of measure` == "Dollars (in millions)") %>% 
#     dplyr::mutate(val_norm = sprintf("%.0f", val_norm)) %>%      # set "%.0f" s.t. there are no decimals
#     dplyr::select(Date, val_norm, Components) %>%
#     droplevels() %>% 
#     tidyr::spread(Components, val_norm, fill = NA) # %>% 
#     dplyr::mutate() %>% 
#     dplyr::select(Date, InMort_total, InMort_var, InMort_fix0_1, InMort_fix1_3, InMort_fix3_5, InMort_fix5, 
#                   UnMort_total, UnMort_var, UnMort_fix0_1, UnMort_fix1_3, UnMort_fix3_5, UnMort_fix5, Credit_rate)


## Population Data -------------------------------------------------------------------------------------------------------

# Labour force characteristics, three-month moving average, seasonally adjusted
get_cansim_table_overview("14-10-0380", language = "english", refresh = FALSE)
population <- get_cansim("14-10-0380")      # Spans 2006-03-01 to 2022-02-01,   Source: LFS,     Index: 2016-12=100
population <- population %>% 
    dplyr::mutate(Date = ymd(Date),
                  GEO = as.factor(GEO)) %>% 
    dplyr::filter(Date >= "2013-01-01" & Date <= "2021-12-01",
                  `Data type` == "Seasonally adjusted",
                  Statistics == "Estimate") %>% 
    dplyr::rename(LF_vars = `Labour force characteristics`) %>% 
    dplyr::select(Date, GEO, val_norm, LF_vars)

population <- population %>% 
    dplyr::mutate(              # Rename factors
        GEO = fct_recode(GEO,
                         "Calgary" = "Calgary, Alberta", 
                         "Edmonton" = "Edmonton, Alberta",
                         "Sudbury" = "Greater Sudbury, Ontario", 
                         "Guelph" = "Guelph, Ontario", 
                         "Hamilton" = "Hamilton, Ontario",
                         "Kelowna" = "Kelowna, British Columbia", 
                         "Kitchener" = "Kitchener-Cambridge-Waterloo, Ontario", 
                         "London" = "London, Ontario",
                         "Montreal" = "Montréal, Quebec",
                         "Oshawa" = "Oshawa, Ontario", 
                         "Ottawa" = "Ottawa-Gatineau, Ontario/Quebec",
                         "Niagara" = "St. Catharines-Niagara, Ontario",
                         "Toronto" = "Toronto, Ontario",
                         "Vancouver" = "Vancouver, British Columbia",
                         "Victoria" = "Victoria, British Columbia", 
                         "Windsor" = "Windsor, Ontario")) %>% 
    dplyr::filter(GEO %in% c("Alberta", "British Columbia", "Calgary", "Edmonton", "Sudbury", "Guelph", "Hamilton", "Kelowna", 
                             "Kitchener", "London", "Montreal", "Ontario", "Oshawa", "Ottawa", "Niagara", "Quebec",
                             "Toronto", "Vancouver", "Victoria", "Windsor")) %>% 
    droplevels() %>% 
    tidyr::spread(LF_vars, val_norm, fill = NA) %>%    # reshape factor column s.t. each factor has its own column
    dplyr::rename(Pop = Population,
                  LF = `Labour force`,
                  Empl = Employment,
                  Unempl = Unemployment,
                  URATE = `Unemployment rate`,
                  LF_RATE = `Participation rate`,
                  ERATE = `Employment rate`)


## Housing Prices Index -------------------------------------------------------------------------------------------------------

# New housing price index, monthly: 18-10-0205-01 (formerly: CANSIM 327-0056)
get_cansim_table_overview("18-10-0205", language = "english", refresh = FALSE)
NHPI <- get_cansim("18-10-0205")    # Spans 1981-01-01 to 2021-12-01,   Index chained to: 2016-12=100

NHPI <- NHPI %>% 
    dplyr::mutate(Date = ymd(Date),
                  GEO = as.factor(GEO)) %>% 
    dplyr::filter(Date >= "2013-01-01" & Date <= "2021-12-01",
                  `New housing price indexes` == "House only",
                  GEO %in% c("Alberta",                          # Regions to include
                             "British Columbia", 
                             "Calgary, Alberta", 
                             "Edmonton, Alberta", 
                             "Greater Sudbury, Ontario",
                             "Guelph, Ontario",
                             "Hamilton, Ontario", 
                             "Kelowna, British Columbia",
                             "Kitchener-Cambridge-Waterloo, Ontario", 
                             "London, Ontario",
                             "Montréal, Quebec",
                             "Ontario",
                             "Oshawa, Ontario",
                             "Ottawa-Gatineau, Ontario part, Ontario/Quebec",
                             "Quebec", 
                             "St. Catharines-Niagara, Ontario", 
                             "Toronto, Ontario",
                             "Vancouver, British Columbia", 
                             "Victoria, British Columbia", 
                             "Windsor, Ontario")) %>% 
    dplyr::rename(HPI = val_norm) %>% 
    dplyr::select(Date, GEO, HPI) %>% 
    dplyr::mutate(GEO = fct_recode(GEO,
                                   "Calgary" = "Calgary, Alberta", 
                                   "Edmonton" = "Edmonton, Alberta",
                                   "Sudbury" = "Greater Sudbury, Ontario",
                                   "Guelph" = "Guelph, Ontario",
                                   "Hamilton" = "Hamilton, Ontario",
                                   "Kelowna" = "Kelowna, British Columbia",
                                   "Kitchener" = "Kitchener-Cambridge-Waterloo, Ontario",
                                   "London" = "London, Ontario",
                                   "Montreal" = "Montréal, Quebec",
                                   "Oshawa" = "Oshawa, Ontario",
                                   "Ottawa" = "Ottawa-Gatineau, Ontario part, Ontario/Quebec",
                                   "Niagara" = "St. Catharines-Niagara, Ontario",
                                   "Toronto" = "Toronto, Ontario",
                                   "Vancouver" = "Vancouver, British Columbia",
                                   "Victoria" = "Victoria, British Columbia",
                                   "Windsor" = "Windsor, Ontario")) %>% 
    droplevels()


## Income Data -------------------------------------------------------------------------------------------------------

# Distribution of market, total and after-tax income by economic family type, Canada, provinces and selected census metropolitan areas (CMAs)
get_cansim_table_overview("11-10-0237", language = "english", refresh = FALSE)
Inc.dist <- get_cansim("11-10-0237")  # from 1976-01-01 to 2020-01-01,   chained to: 2020 constant dollars
Inc.dist <- Inc.dist %>% 
    dplyr::mutate(Date = ymd(Date),
                  GEO = as.factor(GEO),
                  `Income concept` = as.factor(`Income concept`),
                  `Economic family type` = as.factor(`Economic family type`),
                  Statistics = as.factor(Statistics)) %>% 
    dplyr::filter(Date >= "2013-01-01" & Date <= "2021-12-01",
                  `Income concept` == "Total income",
                  `Economic family type` == "Economic families and persons not in an economic family",
                  GEO %in% c("Alberta", 
                             "British Columbia", 
                             "Calgary, Alberta", 
                             "Edmonton, Alberta", 
                             "Montréal, Quebec",
                             "Ontario",
                             "Ottawa-Gatineau, Ontario/Quebec",
                             "Quebec", 
                             "Toronto, Ontario",
                             "Vancouver, British Columbia")) %>% 
    dplyr::rename(Family_type = `Economic family type`) %>% 
    dplyr::mutate(GEO = fct_recode(GEO,
                                   "Calgary" = "Calgary, Alberta", 
                                   "Edmonton" = "Edmonton, Alberta",
                                   "Montreal" = "Montréal, Quebec",
                                   "Ottawa" = "Ottawa-Gatineau, Ontario/Quebec",
                                   "Toronto" = "Toronto, Ontario",
                                   "Vancouver" = "Vancouver, British Columbia")) %>% 
    dplyr::select(Date, GEO, VALUE, val_norm, SCALAR_FACTOR, UOM, Statistics) %>% 
    dplyr::mutate(Statistics = fct_recode(Statistics,
                                          "Inc10k_less" = "Percentage under $10,000 (including zeros and losses)", 
                                          "Inc10_19k" = "$10,000 to $19,999", 
                                          "Inc20_29k" = "$20,000 to $29,999",
                                          "Inc30_39k" = "$30,000 to $39,999",
                                          "Inc40_49k" = "$40,000 to $49,999", 
                                          "Inc50_59k" = "$50,000 to $59,999",
                                          "Inc60_69k" = "$60,000 to $69,999", 
                                          "Inc70_79k" = "$70,000 to $79,999",
                                          "Inc80_89k" = "$80,000 to $89,999",
                                          "Inc90_99k" = "$90,000 to $99,999",
                                          "Inc100_149k" = "$100,000 to 149,999",
                                          "Inc100k_over" = "$100,000 and over", 
                                          "Inc150k_over" = "$150,000 and over")) %>% 
    dplyr::filter(Statistics %in% c("Inc10k_less", "Inc10_19k", "Inc20_29k", "Inc30_39k", "Inc40_49k",
                                    "Inc50_59k", "Inc60_69k", "Inc70_79k", "Inc80_89k", "Inc90_99k",
                                    "Inc100_149k", "Inc100k_over", "Inc150k_over")) %>%
    dplyr::select(Date, GEO, val_norm, Statistics) %>% 
    droplevels() %>% 
    tidyr::spread(Statistics, val_norm, fill = NA)

Inc.dist <- Inc.dist %>%   # Constructing larger income brackets
    dplyr::mutate(Inc10_39k = Inc10_19k + Inc20_29k + Inc30_39k,
                  Inc40_59k = Inc40_49k + Inc50_59k,
                  Inc60_79k = Inc60_69k + Inc70_79k,
                  Inc80_99k = Inc80_89k + Inc90_99k) %>%
    dplyr::select(Date, GEO, Inc10k_less, Inc10_39k, Inc40_59k, Inc60_79k, Inc80_99k, Inc100_149k, Inc100k_over, Inc150k_over)


## Census Data (demographics & income) -------------------------------------------------------------------------------------------------------

# Census families by family type and family composition including before and after-tax median income of the family
get_cansim_table_overview("11-10-0017", language = "english", refresh = FALSE)
Census <- get_cansim("11-10-0017")
Census <- Census %>% 
    dplyr::mutate(Date = ymd(Date),
                  GEO = as.factor(GEO),
                  `Census family type` = as.factor(`Census family type`),
                  `Family type composition` = as.factor(`Family type composition`)) %>% 
    dplyr::filter(Date >= "2013-01-01" & Date <= "2021-12-01",
                  GEO %in% c("Alberta",
                             "British Columbia",
                             "Ontario",
                             "Quebec",
                             "Calgary, Alberta",
                             "Edmonton, Alberta",
                             "Guelph, Ontario",
                             "Hamilton, Ontario",
                             "Kelowna, British Columbia", 
                             "Kitchener-Cambridge-Waterloo, Ontario", 
                             "London, Ontario",
                             "Montréal, Quebec",
                             "St. Catharines-Niagara, Ontario",
                             "Oshawa, Ontario",
                             "Ottawa-Gatineau, Ontario part",
                             "Greater Sudbury, Ontario", 
                             "Toronto, Ontario",
                             "Vancouver, British Columbia",
                             "Victoria, British Columbia", 
                             "Windsor, Ontario")) %>% 
    dplyr::rename(Census_type = `Census family type`,
                  Family_type = `Family type composition`) %>% 
    dplyr::mutate(GEO = fct_recode(GEO,
                                   "Calgary" = "Calgary, Alberta",
                                   "Edmonton" = "Edmonton, Alberta",
                                   "Guelph" = "Guelph, Ontario",
                                   "Hamilton" = "Hamilton, Ontario",
                                   "Kelowna" = "Kelowna, British Columbia", 
                                   "Kitchener" = "Kitchener-Cambridge-Waterloo, Ontario", 
                                   "London" = "London, Ontario",
                                   "Montreal" = "Montréal, Quebec",
                                   "Niagara" = "St. Catharines-Niagara, Ontario",
                                   "Oshawa" = "Oshawa, Ontario",
                                   "Ottawa" = "Ottawa-Gatineau, Ontario part",
                                   "Sudbury" = "Greater Sudbury, Ontario", 
                                   "Toronto" = "Toronto, Ontario",
                                   "Vancouver" = "Vancouver, British Columbia",
                                   "Victoria" = "Victoria, British Columbia", 
                                   "Windsor" = "Windsor, Ontario")) %>%
    dplyr::select(Date, GEO, VALUE, val_norm, SCALAR_FACTOR, UOM, Census_type, Family_type, Statistics) %>% 
    droplevels()


# Median after-tax income of couples & all family types by region & year
Inc.med <- Census %>% 
    dplyr::filter(Statistics == "Median after-tax family income",
                  Census_type == "All family units",
                  Family_type == "Family types with or without children") %>% 
    dplyr::rename(Inc_med = val_norm) %>%       # using all family median income as the base median income stat
    dplyr::select(Date, GEO, Inc_med) %>% 
    droplevels()

# Number of economic family types by region & year
pop.fam <- Census %>% 
    dplyr::filter(Statistics == "Number of families",
                  # number of families refers to pairs (ie. a couple consists of 2 people but would be counted as 1 family)
                  Census_type %in% c("All census families",
                                     "Couple families",
                                     "Lone-parent families",
                                     "Persons not in census families"),
                  Family_type == "Family types with or without children") %>% 
    dplyr::mutate(Census_type = fct_recode(Census_type,
                                           "num_fam" = "All census families",
                                           "num_cpl" = "Couple families",
                                           "num_lone" = "Lone-parent families",
                                           "num_single" = "Persons not in census families")) %>% 
    dplyr::select(Date, GEO, val_norm, Census_type) %>% 
    droplevels() %>% 
    tidyr::spread(Census_type, val_norm, fill = NA)


# Number of families w/ kids by region & year
pop.cpl <- Census %>% 
    dplyr::filter(Statistics == "Number of families",
                  Census_type == "Couple families",
                  Family_type %in% c("Family types without children",
                                     "Family types with 1 child",
                                     "Family types with 2 children",
                                     "Family types with 3 or more children")) %>% 
    dplyr::mutate(Family_type = fct_recode(Family_type,
                                           "cpl_nokid" = "Family types without children",
                                           "cpl_child" = "Family types with 1 child",
                                           "cpl_kid2" = "Family types with 2 children",
                                           "cpl_kid3" = "Family types with 3 or more children")) %>% 
    dplyr::select(Date, GEO, val_norm, Family_type) %>% 
    droplevels() %>% 
    tidyr::spread(Family_type, val_norm, fill = NA) %>% 
    dplyr::mutate(cpl_kids = cpl_kid2 + cpl_kid3) %>% 
    dplyr::select(-cpl_kid2, -cpl_kid3)

# Number of families w/ kids by region & year
pop.kid <- Census %>% 
    dplyr::filter(Statistics == "Number of families",
                  Census_type %in% c("All census families"),   # OR: Census_type %in% c("All census families", "Couple families")
                  Family_type %in% c("Family types without children",
                                     "Family types with 1 child",
                                     "Family types with 2 children",
                                     "Family types with 3 or more children")) %>% 
    dplyr::mutate(Family_type = fct_recode(Family_type,
                                           "pop_nk" = "Family types without children",
                                           "pop_1kid" = "Family types with 1 child",
                                           "pop_2kid" = "Family types with 2 children",
                                           "pop_3kid" = "Family types with 3 or more children")) %>% 
    dplyr::select(Date, GEO, val_norm, Family_type) %>% 
    droplevels() %>% 
    tidyr::spread(Family_type, val_norm, fill = NA) %>% 
    dplyr::mutate(pop_kids = pop_2kid + pop_3kid) %>% 
    dplyr::select(Date, GEO, pop_nk, pop_1kid, pop_kids)


# Age distribution by region & year
pop.age <- Census %>%
    dplyr::filter(Statistics %in% c("Number of persons aged 0 to 17 years",
                                    "Number of persons aged 18 to 64 years",
                                    "Number of persons aged 65 years and over"),
                  Census_type == "All family units",
                  Family_type == "Family types with or without children") %>%
    dplyr::mutate(Statistics = fct_recode(Statistics,
                                          "Age0_17" = "Number of persons aged 0 to 17 years",
                                          "Age18_64" = "Number of persons aged 18 to 64 years",
                                          "Age65_over" = "Number of persons aged 65 years and over")) %>%
    dplyr::select(Date, GEO, val_norm, Statistics) %>%
    droplevels() %>%
    tidyr::spread(Statistics, val_norm, fill = NA)

# Median after-tax income of couples with kids by region & year
Inc.cpl <- Census %>%
    dplyr::filter(Statistics == "Median after-tax family income",
                  Census_type == "Couple families",
                  Family_type %in% c("Family types with or without children",
                                     "Family types without children",
                                     "Family types with 1 child",
                                     "Family types with 2 children",
                                     "Family types with 3 or more children")) %>%
    dplyr::mutate(Family_type = fct_recode(Family_type,
                                           "Inc_allcpl" = "Family types with or without children",
                                           "Inc_cpl_nk" = "Family types without children",
                                           "Inc_cpl_1k" = "Family types with 1 child",
                                           "Inc_cpl_w2k" = "Family types with 2 children",
                                           "Inc_cpl_w3k" = "Family types with 3 or more children")) %>% 
    dplyr::select(Date, GEO, val_norm, Family_type) %>%
    droplevels() %>%
    tidyr::spread(Family_type, val_norm, fill = NA) %>% 
    dplyr::mutate(Inc_cpl_kids = (Inc_cpl_w2k + Inc_cpl_w3k)/2) %>% # combining & averaging the income of couples w/ 2 or more kids
    dplyr::select(Date, GEO, Inc_allcpl, Inc_cpl_nk, Inc_cpl_1k, Inc_cpl_kids)




# Interpolating CANSIM Data -------------------------------------------------------------------------------------------------------
# Interpolate data to s.t. it becomes monthly (instead of annual):
# Data to interpolate: Inc.cpl, Inc.med, pop.age, pop.cpl, pop.fam, pop.kid

df.groups <- dplyr::select(NHPI, -HPI)   # Date & region dataframe

# Interpolate "Inc.cpl"
df_Inc.cpl <- merge(df.groups, Inc.cpl, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
cat(names(df_Inc.cpl), sep=', ')
df_Inc.cpl <- df_Inc.cpl %>% 
    group_by(GEO, .add = TRUE) %>% group_split() %>% 
    map(. %>%
            dplyr::mutate(Inc_allcpl = round(na.spline(object = Inc_allcpl, na.rm = FALSE), 0),
                          Inc_cpl_nk = round(na.spline(object = Inc_cpl_nk, na.rm = FALSE), 0),
                          Inc_cpl_1k = round(na.spline(object = Inc_cpl_1k, na.rm = FALSE), 0),
                          Inc_cpl_kids = round(na.spline(object = Inc_cpl_kids, na.rm = FALSE), 0))
    ) %>% 
    data.table::rbindlist()     # merge the list of regions


# Interpolate "Inc.med"
df_Inc.med <- merge(df.groups, Inc.med, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
cat(names(df_Inc.med), sep=', ')
df_Inc.med <- df_Inc.med %>% 
    group_by(GEO, .add = TRUE) %>% group_split() %>% 
    map(. %>%
            dplyr::mutate(Inc_med = round(na.spline(object = Inc_med, na.rm = FALSE), 0))) %>% 
    data.table::rbindlist()     # merge the list of regions


# Interpolate "pop.age"
df_pop.age <- merge(df.groups, pop.age, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
cat(names(df_pop.age), sep=', ')
df_pop.age <- df_pop.age %>% 
    group_by(GEO, .add = TRUE) %>% group_split() %>% 
    map(. %>%
            dplyr::mutate(Age0_17 = round(na.spline(object = Age0_17, na.rm = FALSE), 0),
                          Age18_64 = round(na.spline(object = Age18_64, na.rm = FALSE), 0),
                          Age65_over = round(na.spline(object = Age65_over, na.rm = FALSE), 0))
        ) %>% 
    data.table::rbindlist()     # merge the list of regions


# Interpolate "pop.cpl"
df_pop.cpl <- merge(df.groups, pop.cpl, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
cat(names(df_pop.cpl), sep=', ')
df_pop.cpl <- df_pop.cpl %>% 
    group_by(GEO, .add = TRUE) %>% group_split() %>% 
    map(. %>%
            dplyr::mutate(cpl_nokid = round(na.spline(object = cpl_nokid, na.rm = FALSE), 0),
                          cpl_child = round(na.spline(object = cpl_child, na.rm = FALSE), 0),
                          cpl_kids = round(na.spline(object = cpl_kids, na.rm = FALSE), 0))
    ) %>% 
    data.table::rbindlist()     # merge the list of regions


# Interpolate "pop.fam"
df_pop.fam <- merge(df.groups, pop.fam, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
cat(names(df_pop.fam), sep=', ')
df_pop.fam <- df_pop.fam %>% 
    group_by(GEO, .add = TRUE) %>% group_split() %>% 
    map(. %>%
            dplyr::mutate(num_fam = round(na.spline(object = num_fam, na.rm = FALSE), 0),
                          num_cpl = round(na.spline(object = num_cpl, na.rm = FALSE), 0),
                          num_lone = round(na.spline(object = num_lone, na.rm = FALSE), 0),
                          num_single = round(na.spline(object = num_single, na.rm = FALSE), 0))
    ) %>% 
    data.table::rbindlist()     # merge the list of regions

# Interpolate "pop.kid"
df_pop.kid <- merge(df.groups, pop.kid, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
cat(names(df_pop.kid), sep=', ')
df_pop.kid <- df_pop.kid %>% 
    group_by(GEO, .add = TRUE) %>% group_split() %>% 
    map(. %>%
            dplyr::mutate(pop_nk = round(na.spline(object = pop_nk, na.rm = FALSE), 0),
                          pop_1kid = round(na.spline(object = pop_1kid, na.rm = FALSE), 0),
                          pop_kids = round(na.spline(object = pop_kids, na.rm = FALSE), 0))
    ) %>% 
    data.table::rbindlist()     # merge the list of regions


# New date & region dataframe
df.groups <- dplyr::select(NHPI, -HPI) %>% 
    dplyr::filter(GEO %in% c("Alberta", "British Columbia", "Calgary", "Edmonton", "Montreal", 
                             "Ontario", "Ottawa", "Quebec", "Toronto", "Vancouver")) %>% 
    droplevels()

# Interpolate "Inc.dist"
df_Inc.dist <- merge(df.groups, Inc.dist, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
cat(names(df_Inc.dist), sep=', ')
df_Inc.dist <- df_Inc.dist %>% 
    group_by(GEO, .add = TRUE) %>% group_split() %>% 
    map(. %>%
            dplyr::mutate(Inc10k_less = round(na.approx(object = Inc10k_less, na.rm = FALSE), 3),
                          Inc10_39k = round(na.approx(object = Inc10_39k, na.rm = FALSE), 3),
                          Inc40_59k = round(na.approx(object = Inc40_59k, na.rm = FALSE), 3),
                          Inc60_79k = round(na.approx(object = Inc60_79k, na.rm = FALSE), 3),
                          Inc80_99k = round(na.approx(object = Inc80_99k, na.rm = FALSE), 3),
                          Inc100_149k = round(na.approx(object = Inc100_149k, na.rm = FALSE), 3),
                          Inc100k_over = round(na.approx(object = Inc100k_over, na.rm = FALSE), 3),
                          Inc150k_over = round(na.approx(object = Inc150k_over, na.rm = FALSE), 3))
    ) %>% 
    data.table::rbindlist()     # merge the list of regions


# Renaming dataframes:
Inc.med <- df_Inc.med
pop.age <- df_pop.age
pop.cpl <- df_pop.cpl
pop.fam <- df_pop.fam
Inc.cpl <- df_Inc.cpl
pop.kid <- df_pop.kid
Inc.dist <- df_Inc.dist

rm(df.groups, df_Inc.cpl, df_Inc.med, df_pop.age, df_pop.cpl, df_pop.fam, df_pop.kid, df_Inc.dist)



# Merging NHPI & CANSIM Data -------------------------------------------------------------------------------------------------------

# All Cansim data: Census, Inc.cpl, Inc.dist, Inc.med, Lend.int, lending, mort.rates, NHPI, pop.age, pop.cpl, pop.fam, pop.kid, population, supply
rm(Census, lending)     # Remove all "top-tree" CANSIM data

nlevels(NHPI$GEO); nlevels(population$GEO); nlevels(Inc.cpl$GEO); nlevels(Inc.med$GEO); nlevels(pop.age$GEO); 
nlevels(pop.cpl$GEO); nlevels(pop.fam$GEO); nlevels(pop.kid$GEO); nlevels(supply$GEO); nlevels(Inc.dist$GEO)
# Lend.int, mort.rates      # 1 national level

# Order of merge: NHPI, population, supply, mort.rates, Lend.int, Inc.med, pop.age, pop.fam, pop.kid, pop.cpl, Inc.cpl, Inc.dist

# Merging dataframes
df.full = merge(NHPI, population, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
df.full = merge(df.full, supply, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
df.full = merge(df.full, mort.rates, by=c("Date"), all.x = TRUE, all.y = FALSE)
df.full = merge(df.full, Lend.int, by=c("Date"), all.x = TRUE, all.y = FALSE)
df.full = merge(df.full, Inc.med, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
df.full = merge(df.full, pop.age, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
df.full = merge(df.full, pop.fam, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
df.full = merge(df.full, pop.kid, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
df.full = merge(df.full, pop.cpl, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
df.full = merge(df.full, Inc.cpl, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)
df.full = merge(df.full, Inc.dist, by=c("GEO","Date"), all.x = TRUE, all.y = FALSE)

write.csv(df.full, "~\\Projects\\spec-tax-project\\Data\\FULL-DATA.csv", row.names = FALSE)

names(df.full)
levels(df.full$GEO)

# Remove small datasets
rm(Inc.cpl, Inc.dist, Inc.med, Lend.int, mort.rates, NHPI, pop.age, pop.cpl, pop.fam, pop.kid, population, supply)


