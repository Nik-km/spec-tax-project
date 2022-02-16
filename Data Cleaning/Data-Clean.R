
setwd("~/Projects/spec-tax-project/Data Cleaning")

library(knitr)      # # for `kable()`
library(tidyverse)  # attaches the packages: ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, & forcats
library(readxl)
library(XML)
library(xml2)
library(haven)      # reads .sav files
library(lubridate)  # formats dates


library(lmtest)
library(sandwich)   # fn. for coeftest
library(plm)        # R function that can estimate linear panel regression models
library(stargazer)
library(dynlm)      # Time-series regression



##################################
###     Labour Force Survey     ##
##################################

# Create a large list of all the datasets
setwd("~/Projects/spec-tax-project/Data Cleaning/LFS_clean_data")
LFS.2021.df <- list() # creates an empty list
listsav <- dir(pattern = "*.sav") # creates the list of all the csv files in the directory
for (k in 1:length(listsav)){
    LFS.2021.df[[k]] <- read_sav(listsav[k]) # str(LFS.df[[1]])
}

LFS.2021.df <- data.table::rbindlist(LFS.2021.df)   # merge the list of tibbles into 1 data frame

# Prepping the full year dataset
PROV_ex <- c(10, 11, 12, 13, 46, 47)  # excluded provinces
CMA_ex <- c(1,6)  # excluded CMAs
LFS.2021.df <- LFS.2021.df %>%   # de-merge into list of tibbles again
    group_by(SURVMNTH, .add = TRUE) %>% group_split() %>% 
    map(. %>% 
            dplyr::filter(!(PROV %in% PROV_ex)) %>% 
            dplyr::filter(!(CMA %in% CMA_ex)) %>% 
            mutate(DAY = 1) %>% 
            mutate(DATE = make_date(SURVYEAR, SURVMNTH, DAY)) %>% 
            dplyr::select("REC_NUM","DATE","SURVMNTH","LFSSTAT","PROV","CMA","AGE_12","SEX","MARSTAT",  # Keep the following columns
                          "EDUC","FTPTLAST", "COWMAIN","IMMIG","NAICS_21","NOC_10",
                          "HRLYEARN","UNION","PERMTEMP","ESTSIZE","FIRMSIZE","DURUNEMP",
                          "UNEMFTPT","DURJLESS","SCHOOLN","EFAMTYPE","AGYOWNK"
            )
    )

LFS.2021.df <- data.table::rbindlist(LFS.2021.df)   # final merge the list of tibbles

colnames(LFS.2021.df)

# saving dataset as a .csv file
write.csv(LFS.2021.df, "C:\\Users\\M\\Projects\\spec-tax-project\\LFS Data\\LFS-2021.csv", row.names = FALSE)






##################################
###          CANSIM             ##
##################################
library(cansim)

?get_cansim_table_list
avail.data <- list_cansim_cubes()


# Retrieve "New housing price index, monthly": 18-10-0205-01 (formerly: CANSIM 327-0056)
get_cansim_table_overview("18-10-0205", language = "english", refresh = FALSE)

NHPI <- get_cansim("18-10-0205")    # Spans 1981-01-01 to 2021-12-01,   Index: 2016-12=100

# Regions to include
CMA_ID <- sort(c(550, 537, 541, 532, 539, 535, 915, 933, 935,                   # treatments
                 825, 835, 462, 421, 580, 555, 559, 35505, 24505,59,35,24))     # controls & BC, ON, QB

NHPI <- NHPI %>% 
    dplyr::rename(Estate_Type = "New housing price indexes", 
                  Type_Hierarchy = "Hierarchy for New housing price indexes",
                  GEO_Hierarchy = "Hierarchy for GEO") %>% 
    dplyr::select(Date, GEO, GeoUID, "VECTOR", Estate_Type, VALUE, STATUS, val_norm, 
                  Type_Hierarchy, GEO_Hierarchy) %>% 
    dplyr::mutate(Date = ymd(Date)) %>% 
    dplyr::filter(Date >= "2013-01-01" & Date <= "2021-12-01") %>% 
    dplyr::filter(GeoUID %in% CMA_ID) %>% 
    dplyr::mutate(GEO = as.factor(GEO), GeoUID = as.factor(GeoUID))


levels(NHPI$GEO)
levels(NHPI$GeoUID)

### GeoUID Markers ###
# Guelph: 550     Hamilton: 537     Kitchener-Cambridge-Waterloo: 541       Oshawa: 532          St. Catharines-Niagara: 539       Toronto: 535
# Kelowna: 915      Vancouver: 933      Victoria: 935

### Controls:
# Calgary: 825        Edmonton: 835        Montreal: 462       Quebec City: 421
# Greater Sudbury: 580        London: 555       Windsor: 559        Ottawa-Gatineau (ON part): 35505        Ottawa-Gatineau (QB part): 24505


# NOTE: Sudbury & London are a strong contenders as control regions for comparing the NRST



##################################
###     Housing Prices Index   ###
##################################



TO.HP.df <- NHPI %>% 
    dplyr::filter(VECTOR == "v111955500")



# Making the province and territory names consistent
can.df <- can.df %>%
    dplyr::mutate(
        Province_Territory = gsub(", Canada", "", Province_Territory) %>% factor(),
        Province_Territory = gsub(",Canada", "", Province_Territory) %>% factor()
    )








##################################
###           CANSIM            ##
##################################


# 36-10-0126 -- Property income of households, Canada

# 11-10-0057 -- Assets and debts by after-tax income quintile, Canada, provinces and selected census metropolitan areas, Survey of Financial Security 11100057

# 11-10-0239 -- Income of individuals by age group, sex and income source, Canada, provinces and selected census metropolitan areas 206-0052

# 11-10-0238 -- Distribution of market, total and after-tax income of individuals, Canada, provinces and selected census metropolitan areas 206-0051

# 11-10-0237 -- Distribution of market, total and after-tax income by economic family type, Canada, provinces and selected census metropolitan areas (CMAs) 206-0012

# 11-10-0190 -- Market income, government transfers, total income, income tax and after-tax income by economic family type 206-0011

# 11-10-0191 -- Income statistics by economic family type and income source 206-0021

# 11-10-0004 -- Selected characteristics of tax filers and dependants, income and demographics (final T1 Family File) 111-0004









