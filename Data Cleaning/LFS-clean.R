# Labour Force Survey ------------------------------------------------------------------------------------------------------- 
## LFS micro-data conversion -------------------------------------------------------------------------------------------------------

# Create a large list of all the datasets
setwd("~/Projects/spec-tax-project/Data Cleaning/LFS_clean_data")
LFS.df <- list() # creates an empty list
listsav <- dir(pattern = "*.sav") # creates the list of all the csv files in the directory
for (k in 1:length(listsav)){
    LFS.df[[k]] <- read_sav(listsav[k]) # str(LFS.df[[1]])
}

LFS.df <- data.table::rbindlist(LFS.df, fill=TRUE)   # merge the list of tibbles into 1 data frame


# Prepping the full year dataset
PROV_ex <- c(10, 11, 12, 13, 46, 47)  # excluded provinces: Newfoundland, PEI, Nova Scotia, New Brunswick, Manitoba, & Saskatchewan
CMA_ex <- c(1,6)  # excluded CMAs
LFS.total.df <- LFS.df %>%   # de-merge into list of tibbles again
    group_by(SURVYEAR, SURVMNTH, .add = TRUE) %>% group_split() %>% 
    map(. %>% 
            dplyr::filter(!(PROV %in% PROV_ex)) %>% 
            # dplyr::filter(!(CMA %in% CMA_ex)) %>% 
            mutate(DAY = 1) %>% 
            mutate(DATE = make_date(SURVYEAR, SURVMNTH, DAY)) %>% 
            dplyr::select("REC_NUM","DATE", "SURVYEAR", "SURVMNTH","LFSSTAT","PROV","CMA","AGE_12","SEX","MARSTAT",  # Keep the following columns
                          "EDUC","FTPTLAST", "COWMAIN","IMMIG","NAICS_21","NOC_10",
                          "HRLYEARN","UNION","PERMTEMP","ESTSIZE","FIRMSIZE","DURUNEMP",
                          "UNEMFTPT","DURJLESS","SCHOOLN","EFAMTYPE","AGYOWNK"
            )
    )

LFS.total.df <- data.table::rbindlist(LFS.total.df)   # final merge the list of tibbles

colnames(LFS.total.df)

# saving dataset as a .csv file
write.csv(LFS.total.df, "~\\Projects\\spec-tax-project\\LFS Data\\LFS-2017-2019.csv", row.names = FALSE)
rm(LFS.total.df, LFS.df)

# Prepping the full year datasets (for 2013-2016)
PROV_ex <- c(10, 11, 12, 13, 46, 47)  # excluded provinces: Newfoundland, PEI, Nova Scotia, New Brunswick, Manitoba, & Saskatchewan

LFS.total.df <- LFS.df %>%   # de-merge into list of tibbles again
    group_by(SURVYEAR, SURVMNTH, .add = TRUE) %>% group_split() %>% 
    map(. %>% 
            dplyr::filter(!(PROV %in% PROV_ex)) %>% 
            # dplyr::filter(!(CMA %in% CMA_ex)) %>% 
            mutate(DAY = 1) %>% 
            rename(EDUC = EDUC90) %>% 
            rename(AGYOWNK = AGYOWNKN) %>% 
            mutate(DATE = make_date(SURVYEAR, SURVMNTH, DAY)) %>% 
            dplyr::select("REC_NUM","DATE", "SURVYEAR", "SURVMNTH","LFSSTAT","PROV","CMA","AGE_12","SEX","MARSTAT",  # Keep the following columns
                          "EFAMTYPE", "AGYOWNK", "EDUC","FTPTLAST", "COWMAIN",
                          "HRLYEARN","UNION","PERMTEMP","ESTSIZE",
                          "FIRMSIZE","DURUNEMP", "UNEMFTPT","DURJLESS","SCHOOLN"
            )
    )

LFS.total.df <- data.table::rbindlist(LFS.total.df)   # final merge the list of tibbles

# saving dataset as a .csv file
write.csv(LFS.total.df, "~\\Projects\\spec-tax-project\\LFS Data\\LFS-2015-2014.csv", row.names = FALSE)

# EFAMSIZE = Number of individuals in economic family
# Missing "IMMIG", "NAICS_21", "NOC_10", from 2013-2016 

LFS.2021.df <- read.csv("~/Projects/spec-tax-project/LFS Data/LFS-2021.csv")

LFS.2020.df <- LFS.2020.df %>%   # de-merge into list of tibbles again
    group_by(SURVYEAR, SURVMNTH, .add = TRUE) %>% group_split()


rm(LFS.total.df, LFS.df, k, listsav, PROV_ex)




## LFS Subsetting -------------------------------------------------------------------------------------------------------
setwd("~/Projects/spec-tax-project/LFS Data")
LFS.2013 <- read.csv("LFS-2013.csv")

names(LFS.2013)
str(LFS.2013)

LFS.2013 <- LFS.2013 %>%    # correct formatting from the .csv import
    dplyr::mutate(Date = ymd(DATE)) %>% 
    dplyr::select("Date", "PROV", "CMA", "LFSSTAT", "AGE_12", "SEX",
                  "MARSTAT", "EDUC", "COWMAIN","HRLYEARN","EFAMTYPE","AGYOWNK") %>% 
    dplyr::rename(AGE = AGE_12) %>% 
    dplyr::mutate(PROV = as.factor(PROV),
                  CMA = as.factor(CMA),
                  LFSSTAT = as.factor(LFSSTAT),
                  AGE = as.factor(AGE),
                  SEX = as.factor(SEX),
                  MARSTAT = as.factor(MARSTAT),
                  EDUC = as.factor(EDUC),
                  COWMAIN = as.factor(COWMAIN),
                  EFAMTYPE = as.factor(EFAMTYPE),
                  AGYOWNK = as.factor(AGYOWNK))

levels(LFS.2013$PROV)
levels(LFS.2013$CMA)

LFS.2013 <- LFS.2013 %>%
    dplyr::mutate(
        PROV = fct_recode(PROV, 
                          "Quebec" = "24", 
                          "Ontario" = "35", 
                          "Alberta" = "48",
                          "British Columbia" = "59"),
        CMA = fct_recode(CMA, 
                         "Montreal" = "1",
                         "Toronto" = "2",
                         "Vancouver" = "3",
                         "Other CMA" = "4"))

# Re-code the factors
LFS.2013 <- LFS.2013 %>%
    dplyr::mutate(
        LFSSTAT = fct_recode(LFSSTAT, 
                             "Employed_working" = "1",          # Employed, at work
                             "Employed_absent" = "2",           # Employed, absent from work
                             "Unemployed" = "3",                # Unemployed, temporary layoff
                             "Unemployed" = "4",                # Unemployed
                             "Unemployed" = "5",                # Unemployed
                             "Not_in_LF" = "6"),                # Not in labour force
        AGE = fct_recode(AGE, 
                         "16_to_24_years" = "1",             # 15 to 19 years
                         "16_to_24_years" = "2",             # 20 to 24 years
                         "25_to_34_years" = "3",             # 25 to 29 years
                         "25_to_34_years" = "4",             # 30 to 34 years
                         "35_to_44_years" = "5",             # 35 to 39 years
                         "35_to_44_years" = "6",             # 40 to 44 years
                         "45_to_54_years" = "7",             # 45 to 49 years
                         "45_to_54_years" = "8",             # 50 to 54 years
                         "55_to_64_years" = "9",             # 55 to 59 years
                         "55_to_64_years" = "10",            # 60 to 64 years
                         "over_65_years" = "11",             # 65 to 69 years
                         "over_65_years" = "12"),            # 70 and over
        SEX = fct_recode(SEX,
                         "Male" = "1",
                         "Female" = "2"),
        MARSTAT = fct_recode(MARSTAT, 
                             "Married" = "1",                 # Married
                             "Common_Law" = "2",              # Living in common-law
                             "Widowed" = "3",                 # Widowed
                             "Separated" = "4",               # Separated
                             "Divorced" = "5",                # Divorced
                             "Single" = "6"),                 # Single, never married
        EDUC = fct_recode(EDUC, 
                          "Minimal_Educ" = "0",                 # 0 to 8 years
                          "Some_High_School" = "1",             # Some high school
                          "High_School" = "2",                  # High school graduate
                          "Some_Postsecondary" = "3",           # Some post-secondary
                          "Postsecondary" = "4",                # Post-secondary certificate or diploma
                          "Bachelors" = "5",                    # Bachelor's degree
                          "Above_Bachelors" = "6"),             # Above bachelor's degree
        COWMAIN = fct_recode(COWMAIN,
                             "Employee" = "1",                     # Public sector employees
                             "Employee" = "2",                     # Private sector employees
                             "Self_Employed" = "3",                # Self-employed incorporated, with paid help
                             "Self_Employed" = "4",                # Self-employed incorporated, no paid help
                             "Self_Employed" = "5",                # Self-employed unincorporated, with paid help
                             "Self_Employed" = "6",                # Self-employed unincorporated, no paid 
                             "Self_Employed" = "7"),               # Unpaid family worker
        AGYOWNK = fct_recode(AGYOWNK,
                             "YKID_newborn_preK" = "1",      # Youngest child less than 3 years
                             "YKID_newborn_preK" = "2",      # Youngest child is between 3-5 years
                             "YKID_schoolage" = "3",         # Youngest child 6 to 12 years
                             "YKID_Teen" = "4",              # Youngest child 13 to 15 years
                             "YKID_Teen" = "5",              # Youngest child 16 to 17 years
                             "YKID_adult" = "6"),            # Youngest child 18 to 24 years
        EFAMTYPE = fct_recode(EFAMTYPE, 
                              "Unattached" = "1",                        # Person not in an economic family
                              "Couple/Dual/no_kid" = "2",                # Dual-earner couple, no children or none under 25
                              "Couple/Dual/young_kid" = "3",             # Dual-earner couple, youngest child 0 to 17
                              "Couple/Dual/old_kid" = "4",               # Dual-earner couple, youngest child 18 to 24
                              "Couple/Single_earn/no_kid" = "5",         # Single-earner couple, male employed, no children or none under 25
                              "Couple/Single_earn/young_kid" = "6",      # Single-earner couple, male employed, youngest child 0 to 17
                              "Couple/Single_earn/old_kid" = "7",        # Single-earner couple, male employed, youngest child 18 to 24
                              "Couple/Single_earn/no_kid" = "8",         # Single-earner couple, female employed, no children or none under 25
                              "Couple/Single_earn/young_kid" = "9",      # Single-earner couple, female employed, youngest child 0 to 17
                              "Couple/Single_earn/old_kid" = "10",       # Single-earner couple, female employed, youngest child 18 to 24
                              "Couple/Non_earn/no_kid" = "11",           # Non-earner couple, no children or none under 25
                              "Couple/Non_earn/young_kid" = "12",        # Non-earner couple, youngest child 0 to 17
                              "Couple/Non_earn/old_kid" = "13",          # Non-earner couple, youngest child 18 to 24
                              "Lone/Empl/young_kid" = "14",              # Lone-parent family, parent employed, youngest child 0 to 17
                              "Lone/Empl/old_kid" = "15",                # Lone-parent family, parent employed, youngest child 18 to 24
                              "Lone/Unempl/young_kid" = "16",            # Lone-parent family, parent not employed, youngest child 0 to 17
                              "Lone/Unempl/old_kid" = "17",              # Lone-parent family, parent not employed, youngest child 18 to 24
                              "Other_Fam" = "18"),                       # Other families
    ) %>%
    # Merge CMA & PROV columns
    dplyr::mutate(GEO = paste0(CMA, ", ", PROV),
                  GEO = as.factor(GEO),
                  GEO = fct_recode(GEO, 
                                   "Montreal" = "Montreal, Quebec",
                                   "Alberta" = "Other CMA, Alberta",
                                   "British Columbia" = "Other CMA, British Columbia",
                                   "Ontario" = "Other CMA, Ontario",
                                   "Quebec" = "Other CMA, Quebec",
                                   "Toronto" = "Toronto, Ontario",
                                   "Vancouver" = "Vancouver, British Columbia")) %>% 
    dplyr::select(Date, GEO, LFSSTAT, AGE, SEX, MARSTAT, EDUC, COWMAIN, HRLYEARN, EFAMTYPE, AGYOWNK)

write.csv(LFS.2013, "~\\Projects\\spec-tax-project\\LFS-Merge\\Full-LFS-2013.csv", row.names = FALSE)

str(LFS.2013)
levels(LFS.2013$GEO)
names(LFS.2013)

# Aggregating the dataset (percentage aggregation)
agg.fn <- function(dat, x) {
    df <- dat %>% 
        group_by(GEO, Date, {{x}}) %>%
        dplyr::summarise(count = n()) %>%
        dplyr::mutate(freq = round(count/sum(count), digits = 5))
    assign("df", df, envir = .GlobalEnv)
}

# v <- c("LFSSTAT", "AGE_12", "SEX", "MARSTAT", "EDUC", "COWMAIN", "IMMIG", "EFAMTYPE", "AGYOWNK")

agg.fn(LFS.2013, LFSSTAT)
df1 <- reshape::cast(df, GEO + Date ~ LFSSTAT, value = "freq")

agg.fn(LFS.2013, AGE)
df2 <- reshape::cast(df, GEO + Date ~ AGE, value = "freq")

agg.fn(LFS.2013, SEX)
df3 <- reshape::cast(df, GEO + Date ~ SEX, value = "freq")

agg.fn(LFS.2013, MARSTAT)
df4 <- reshape::cast(df, GEO + Date ~ MARSTAT, value = "freq")

agg.fn(LFS.2013, EDUC)
df5 <- reshape::cast(df, GEO + Date ~ EDUC, value = "freq")

agg.fn(LFS.2013, COWMAIN)
df6 <- df %>% 
    reshape::cast(GEO + Date ~ COWMAIN, value = "freq") %>% 
    dplyr::select(-`NA`)  # dropping the NA column associated w/ individuals who're neither employed nor self-employed

agg.fn(LFS.2013, EFAMTYPE)
df7 <- reshape::cast(df, GEO + Date ~ EFAMTYPE, value = "freq")

agg.fn(LFS.2013, AGYOWNK)
df8 <- df %>% 
    reshape::cast(GEO + Date ~ AGYOWNK, value = "freq") %>% 
    dplyr::select(-`NA`)  # dropping the NA column

df9 <- aggregate(cbind(HRLYEARN) ~ GEO + Date, data = LFS.2013, FUN = mean)  # Find mean hourly earnings, grouped by GEO and Date

df.list <- c(list(df1, df2, df3, df4, df5, df6, df7, df8, df9))

# Merge list of aggregated dataframes
LFS_2013 <- Reduce(
    function(x, y, ...) merge(x, y, ..., by=c("GEO", "Date")), 
    df.list
)

rm(df, df1, df2, df3, df4, df5, df6, df7, df8, df9, df.list)

write.csv(LFS_2013, "~\\Projects\\spec-tax-project\\LFS-Merge\\LFS-2013.csv", row.names = FALSE)

rm(LFS.2013, LFS_2013)




## LFS Merge All Years -------------------------------------------------------------------------------------------------------
setwd("~/Projects/spec-tax-project/LFS-Merge")
LFS.2021 <- read.csv("LFS-2021.csv")
LFS.2020 <- read.csv("LFS-2020.csv")
LFS.2017_19 <- read.csv("LFS-2017-19.csv")
LFS.2016 <- read.csv("LFS-2016.csv")
LFS.2014_15 <- read.csv("LFS-2014-15.csv")
LFS.2013 <- read.csv("LFS-2013.csv")

# correct formatting from the .csv import
LFS.2021 <- LFS.2021 %>% 
    dplyr::mutate(Date = ymd(Date),
                  GEO = as.factor(GEO))
LFS.2020 <- LFS.2020 %>% 
    dplyr::mutate(Date = ymd(Date),
                  GEO = as.factor(GEO))
LFS.2017_19 <- LFS.2017_19 %>% 
    dplyr::mutate(Date = ymd(Date),
                  GEO = as.factor(GEO))
LFS.2016 <- LFS.2016 %>% 
    dplyr::mutate(Date = ymd(Date),
                  GEO = as.factor(GEO),
                  New_immigrant = NA,
                  Old_immigrant = NA,
                  Non_immigrant = NA)
LFS.2014_15 <- LFS.2014_15 %>% 
    dplyr::mutate(Date = ymd(Date),
                  GEO = as.factor(GEO),
                  New_immigrant = NA,
                  Old_immigrant = NA,
                  Non_immigrant = NA)
LFS.2013 <- LFS.2013 %>% 
    dplyr::mutate(Date = ymd(Date),
                  GEO = as.factor(GEO),
                  New_immigrant = NA,
                  Old_immigrant = NA,
                  Non_immigrant = NA)


# Merge data LFS sets 
LSF.data <- base::rbind(LFS.2021, LFS.2020, LFS.2017_19, LFS.2016, LFS.2014_15, LFS.2013)   # final cleaned dataset

write.csv(LSF.data, "~\\Projects\\spec-tax-project\\LFS-Merge\\LFS-DATA.csv", row.names = FALSE)
