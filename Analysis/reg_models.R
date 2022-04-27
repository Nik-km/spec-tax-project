library(knitr)          # for `kable()`
library(tidyverse)      # attaches the packages: ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, & forcats
library(lubridate)      # formats dates
library(foreign)
library(Hmisc)
library(car)            # to measure the variance-inflation factor of models
library(plm)            # for fixed-effects regression
library(lmtest)         # for coeftest()
library(stargazer)      # regression table output
library(kableExtra)

# Data Prep -------------------------------------------------------------------------------------------------------
# Merged dataset includes all months from 2013-01 to 2021-12
full.data <- read.csv("~/Projects/spec-tax-project/Data/FULL-DATA.csv")
full.data <- full.data %>%              # Correct formatting from the .csv import
    dplyr::mutate(Date = ymd(Date),
                  GEO = as.factor(GEO))

# Preparing the data for regression
df <- full.data %>% 
    dplyr::filter(Date >= "2015-01-01" & Date <= "2018-12-01",
                  !(GEO %in% c("Alberta", "British Columbia", "Ontario", "Quebec"))) %>%    # excluding all collected provincial data
    droplevels() %>% 
    dplyr::mutate(
        # Dummy variables to indicate the dates when the policies were enacted
        NRST = ifelse(Date >= "2017-04-21", 1, 0),
        APT1 = ifelse(Date >= "2016-08-02" & Date <= "2018-02-19", 1, 0),
        APT2 = ifelse(Date >= "2018-02-20", 1, 0),
        
        # Create a dummy variable to identify groups exposed to the treatment
        tr_NRST = ifelse(GEO %in% c("Guelph", "Hamilton", "Kitchener", "London", "Niagara", "Oshawa", "Toronto"), 1, 0),
        tr_APT1 = ifelse(GEO %in% c("Vancouver"), 1, 0),
        tr_APT2 = ifelse(GEO %in% c("Vancouver", "Victoria", "Kelowna"), 1, 0),
        
        # Create an interaction between time and treated
        TRxNRST = NRST * tr_NRST,
        TRxAPT1 = APT1 * tr_APT1,
        TRxAPT2 = APT2 * tr_APT2) %>% 
    dplyr::select(Date, GEO, HPI, NRST, APT1, APT2, tr_NRST, tr_APT1, tr_APT2, TRxNRST, TRxAPT1, TRxAPT2, 
                  Pop, URATE, LF_RATE, 
                  # ERATE, LF, Empl, Unempl,                                # excluded after the fact, based on VIF
                  Compl_total, Compl_single, Compl_semi, Compl_row, 
                  Starts_total, Starts_single, Starts_semi, Starts_row, 
                  UC_total, UC_single, UC_semi, UC_row, 
                  Mort_rate, 
                  # Credit_rate, InMort_total, InMort_var, UnMort_total, UnMort_var,      # excluded after the fact, based on VIF
                  Inc_med, Age0_17, Age18_64, Age65_over, 
                  num_fam, pop_kids, 
                  # pop_nk, pop_1kid, cpl_nokid, cpl_child, cpl_kids,    # excluded after the fact, based on VIF
                  # num_cpl, Inc_allcpl, Inc_cpl_1k,                      # excluded after the fact, based on VIF
                  Inc_cpl_kids)

# Log transformations on the data
df.final <- df %>%
    dplyr::mutate(lnHPI = log(HPI),
                  lnPop = log(Pop),
                  lnUR = log(URATE),
                  lnLFPR = log(LF_RATE),
                  
                  lnMort = log(Mort_rate), 
                  # lnCredit_rate = log(Credit_rate), 
                  # lnInMort = log(InMort_total), 
                  # lnInMVar = log(InMort_var), 
                  # lnUnMort = log(UnMort_total), 
                  # lnUnMVar = log(UnMort_var), 
                  
                  lnCompl_total = log(Compl_total), 
                  lnCompl_single = log(Compl_single), 
                  lnCompl_semi = log(Compl_semi), 
                  lnCompl_row = log(Compl_row), 
                  lnStarts_total = log(Starts_total), 
                  lnStarts_single = log(Starts_single), 
                  lnStarts_semi = log(Starts_semi), 
                  lnStarts_row = log(Starts_row), 
                  lnUC_total = log(UC_total), 
                  lnUC_single = log(UC_single), 
                  lnUC_semi = log(UC_semi), 
                  lnUC_row = log(UC_row),
                  
                  lnInc_med = log(Inc_med),
                  lnAge0_17 = log(Age0_17),
                  lnAge18_64 = log(Age18_64),
                  lnAge65_over = log(Age65_over),
                  
                  lnFam = log(num_fam),
                  # lnCpl = log(num_cpl),
                  # lnKidless = log(pop_nk),
                  # lnKid1 = log(pop_1kid),
                  lnKids = log(pop_kids),
                  # lnCpl_child = log(cpl_child),
                  # lnCpl_kids = log(cpl_kids),
                  lnInc_Cwkids = log(Inc_cpl_kids)
                  ) %>% 
    dplyr::na_if(-Inf)




# DID Estimation -------------------------------------------------------------------------------------------------------
### DID regression w/o controls -------------------------------------------------------------------------------------------------------
did.ON <- lm(HPI ~ tr_NRST + NRST + NRST*tr_NRST, data = df)
summary(lm(HPI ~ tr_NRST + NRST + tr_NRST*NRST, data = df))

lm(HPI ~ factor(tr_NRST)*factor(NRST), data = df)
summary(df$tr_NRST[df$NRST==1])

# BC: incorporate both interaction terms for APT1 & FB2
did.BC <- lm(HPI ~ tr_APT1 + tr_APT2 + APT1 + APT2 + tr_APT1*APT1 + tr_APT2*APT2, data = df)
summary(did.BC)

# All Speculation Taxes
did.base = lm(HPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2, data = df)
summary(did.base)

{plot(HPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2, data = df)
abline(did.base)}

rm(did.BC, did.ON)



### DID regression w/ controls --------------------------------------------------------------------------------------------

# All possible controls:
# lnPop + lnUR + lnLFPR + lnMort 
# + lnCompl_total + lnCompl_single + lnCompl_semi + lnCompl_row 
# + lnStarts_total + lnStarts_single + lnStarts_semi + lnStarts_row 
# + lnUC_total + lnUC_single + lnUC_semi + lnUC_row 
# + lnAge0_17 + lnAge18_64 + lnAge65_over + lnFam + lnKids 
# + lnInc_med + lnInc_Cwkids

# Base model
did.base = lm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2, data = df.final)
summary(did.base)

# Base controls
did.ctrls = lm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2 
             + lnPop + lnMort + lnInc_med, data = df.final)
summary(did.ctrls)

did.ctrls2 = lm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2 
               + lnPop + lnMort + lnInc_med
               + lnUR + lnLFPR, data = df.final)
summary(did.ctrls2)

# Housing supply controls
did.hc = lm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2 
            + lnStarts_total + lnCompl_total  #+ lnUC_total     # lnUC_total was found to be highly correlated w/ other vars
            + lnMort, data = df.final)
summary(did.hc)

did.hc2 = lm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2 
             + lnCompl_single + lnCompl_semi + lnCompl_row + 
                 # lnUC_single + lnUC_semi + lnUC_row +         # all UC vars were found to be highly correlated
                 lnStarts_single + lnStarts_semi + lnStarts_row,
             data = df.final)
summary(did.hc2)

# Demographic controls
did.demo = lm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2 
              + lnMort + lnInc_med + lnStarts_total + lnCompl_total + lnFam,
              data = df.final)
summary(did.demo)

# Income controls
did.inc = lm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2
             + lnMort + lnFam + lnUR + lnLFPR + lnInc_Cwkids,
             data = df.final)
summary(did.inc)

# Full model w/ every type of control
did.all = lm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2
             + lnPop + lnMort + lnUR + lnLFPR + lnInc_med
             + lnStarts_total + lnCompl_total
             + lnUR + lnLFPR,
             data = df.final)
summary(did.all)

did.all2 = lm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2
             + lnPop + lnMort + lnInc_med + lnUR + lnLFPR
             + lnCompl_single + lnCompl_semi + lnCompl_row + lnStarts_single + lnStarts_semi + lnStarts_row,
             data = df.final)
summary(did.all2)


# VIF tests
vif(did.base)
vif(did.all)
vif(did.all2)

vif(did.ctrls)
vif(did.ctrls2)
vif(did.hc)
vif(did.hc2)
vif(did.demo)
vif(did.inc)


# FE DID Estimation -------------------------------------------------------------------------------------------------------
summary(did.base)


# Base fixed-effects model
fe.base <- plm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2, 
               data = df.final,
               index = c("GEO", "Date"), 
               model = "within")
coeftest(fe.base, vcov. = vcovHC, type = "HC1") # summary using robust std. errors

# Base controls
fe.ctrls <- plm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2 
                + lnPop + lnMort + lnInc_med,
               data = df.final,
               index = c("GEO", "Date"), 
               model = "within")
coeftest(fe.ctrls, vcov. = vcovHC, type = "HC1")

fe.ctrls2 <- plm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2 
                + lnPop + lnMort + lnInc_med
                + lnUR + lnLFPR,
                data = df.final,
                index = c("GEO", "Date"), 
                model = "within")
coeftest(fe.ctrls2, vcov. = vcovHC, type = "HC1")


# Housing supply controls
fe.hc <- plm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2 
             + lnStarts_total + lnCompl_total + lnMort,
             data = df.final,
             index = c("GEO", "Date"), 
             model = "within")
coeftest(fe.hc, vcov. = vcovHC, type = "HC1")

fe.hc2 <- plm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2 
              + lnCompl_single + lnCompl_semi + lnCompl_row + lnStarts_single + lnStarts_semi + lnStarts_row,
             data = df.final,
             index = c("GEO", "Date"), 
             model = "within")
coeftest(fe.hc2, vcov. = vcovHC, type = "HC1")

# Demographic controls
fe.demo <- plm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2 
             + lnMort + lnInc_med + lnStarts_total + lnCompl_total + lnFam,
             data = df.final,
             index = c("GEO", "Date"), 
             model = "within")
coeftest(fe.demo, vcov. = vcovHC, type = "HC1")

# Income controls
fe.inc <- plm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2
              + lnMort + lnFam + lnUR + lnLFPR + lnInc_Cwkids,
               data = df.final,
               index = c("GEO", "Date"), 
               model = "within")
coeftest(fe.inc, vcov. = vcovHC, type = "HC1")

# Full model w/ every type of control
fe.all <- plm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2
              + lnPop + lnMort + lnUR + lnLFPR + lnInc_med
              + lnStarts_total + lnCompl_total
              + lnUR + lnLFPR,
              data = df.final,
              index = c("GEO", "Date"), 
              model = "within")
coeftest(fe.all, vcov. = vcovHC, type = "HC1")

fe.all2 <- plm(lnHPI ~ tr_NRST*NRST + tr_APT1*APT1 + tr_APT2*APT2
               + lnPop + lnMort + lnInc_med + lnUR + lnLFPR
               + lnCompl_single + lnCompl_semi + lnCompl_row + lnStarts_single + lnStarts_semi + lnStarts_row,
              data = df.final,
              index = c("GEO", "Date"), 
              model = "within")
coeftest(fe.all2, vcov. = vcovHC, type = "HC1")


# Regression Tables ------------------------------------------------------------------------------------

# Base DID Model w/o controls
stargazer(did.base, 
          type = "latex",
          header = FALSE,    # gets ride of citation
          dep.var.labels = c("log(HPI)"),
          font.size = "normalsize",
          # out = "~\\Projects\\spec-tax-project\\Analysis\\did_base_reg.html",
          title = "Base difference-in-differences (DID) regression model",
          order = c("Constant",
                    "tr_NRST:NRST", 
                    "tr_APT1:APT1", 
                    "tr_APT2:APT2",
                    "tr_NRST",
                    "NRST",
                    "tr_APT1",
                    "APT1",
                    "tr_APT2",
                    "APT2"),
          covariate.labels = c("Constant",
                               "NRST interaction",
                               "ABT1 interaction",
                               "ABT2 interaction",
                               
                               "NRST regional dummy",
                               "NRST post-treatment dummy",
                               "ABT1 regional dummy",
                               "ABT1 post-treatment dummy",
                               "ABT2 regional dummy",
                               "ABT2 post-treatment dummy"),
          notes.append = FALSE,
          column.separate = c(1, 1))



# Final DID model specifications: did.base, did.ctrls, did.ctrls2, did.hc, did.hc2, did.demo, did.inc, did.all, did.all2

# Full DID Model w/ all model specs, removed: did.hc
stargazer(did.base, did.ctrls, did.ctrls2, did.hc2, did.demo, did.inc, did.all, did.all2,
          title = "DID regression models with controls and log(HPI) as the dependent variable",
          header = FALSE,    # gets rid of citation
          type = "latex",
          # out = "~\\Projects\\spec-tax-project\\Analysis\\did_full_reg.html",
          dep.var.labels.include = FALSE, 
          font.size = "footnotesize",
          column.sep.width = "-10pt",
          style = "aer",
          keep=c("Constant", 
                 "tr_NRST:NRST", 
                 "tr_APT1:APT1", 
                 "tr_APT2:APT2",
                 "lnPop", 
                 "lnFam", 
                 "lnUR", 
                 "lnLFPR", 
                 "lnMort", 
                 "lnInc_med", 
                 "lnInc_Cwkids",
                 "lnStarts_total", 
                 "lnStarts_single", 
                 "lnStarts_semi", 
                 "lnStarts_row", 
                 "lnCompl_total", 
                 "lnCompl_single", 
                 "lnCompl_semi", 
                 "lnCompl_row"),
          order = c("Constant", 
                    "tr_NRST:NRST", 
                    "tr_APT1:APT1", 
                    "tr_APT2:APT2",
                    
                    "lnPop", 
                    "lnFam", 
                    "lnUR", 
                    "lnLFPR", 
                    "lnMort", 
                    "lnInc_med", 
                    "lnInc_Cwkids",
                    
                    "lnStarts_total", 
                    "lnStarts_single", 
                    "lnStarts_semi", 
                    "lnStarts_row", 
                    
                    "lnCompl_total", 
                    "lnCompl_single", 
                    "lnCompl_semi", 
                    "lnCompl_row"),
          covariate.labels = c("Constant",
                               "NRST interaction",
                               "ABT1 interaction",
                               "ABT2 interaction",
                               
                               "Population",
                               "Number of Families",
                               "Unemployment rate",
                               "Labour force participation rate",
                               "Mortgage rate",
                               "Median income",
                               "Income of couples with kids",
                               
                               "Total construction starts",
                               "Starts on single-detached units",
                               "Starts on semi-detached units",
                               "Starts on row units",
                               "Total construction completions",
                               "Completed single-detached units",
                               "Completed semi-detached units",
                               "Completed row units"),
          notes = c("all variables are log transformations, ∗ p<0.1; ∗∗ p<0.05; ∗∗∗ p<0.01"),
          notes.append = FALSE,
          column.separate = c(1, 1),
          omit.stat = c("f", "ser","rsq"))


# Full FE DID Model
stargazer(fe.base, fe.ctrls, fe.ctrls2, fe.hc2, fe.demo, fe.inc, fe.all, fe.all2,
          title = "Fixed effects difference-in-differences regression models with log(HPI) as the dependent variable",
          header = FALSE,    # gets rid of citation
          type = "latex",
          # out = "~\\Projects\\spec-tax-project\\Analysis\\fe_reg.html",
          dep.var.labels.include = FALSE, 
          font.size = "footnotesize",
          column.sep.width = "-10pt",
          style = "aer",
          keep=c("tr_NRST:NRST", 
                 "tr_APT1:APT1", 
                 "tr_APT2:APT2",
                 "lnPop", 
                 "lnFam", 
                 "lnUR", 
                 "lnLFPR", 
                 "lnMort", 
                 "lnInc_med", 
                 "lnInc_Cwkids",
                 "lnStarts_total", 
                 "lnStarts_single", 
                 "lnStarts_semi", 
                 "lnStarts_row", 
                 "lnCompl_total", 
                 "lnCompl_single", 
                 "lnCompl_semi", 
                 "lnCompl_row"),
          order = c("tr_NRST:NRST", 
                    "tr_APT1:APT1", 
                    "tr_APT2:APT2",
                    
                    "lnPop", 
                    "lnFam", 
                    "lnUR", 
                    "lnLFPR", 
                    "lnMort", 
                    "lnInc_med", 
                    "lnInc_Cwkids",
                    
                    "lnStarts_total", 
                    "lnStarts_single", 
                    "lnStarts_semi", 
                    "lnStarts_row", 
                    
                    "lnCompl_total", 
                    "lnCompl_single", 
                    "lnCompl_semi", 
                    "lnCompl_row"),
          covariate.labels = c("NRST interaction",
                               "ABT1 interaction",
                               "ABT2 interaction",
                               
                               "Population",
                               "Number of Families",
                               "Unemployment rate",
                               "Labour force participation rate",
                               "Mortgage rate",
                               "Median income",
                               "Income of couples with kids",
                               
                               "Total construction starts",
                               "Starts on single-detached units",
                               "Starts on semi-detached units",
                               "Starts on row units",
                               "Total construction completions",
                               "Completed single-detached units",
                               "Completed semi-detached units",
                               "Completed row units"),
          notes = c("all variables are log transformations, ∗ p<0.1; ∗∗ p<0.05; ∗∗∗ p<0.01"),
          notes.append = FALSE,
          column.separate = c(1, 1),
          omit.stat = c("f", "ser","rsq"))


