library(knitr)          # for `kable()`
library(tidyverse)      # attaches the packages: ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, & forcats
library(lubridate)      # formats dates
library(reshape)        # re-formats data
library(gghighlight)
library(gganimate)
library(gifski)
library(foreign)
library(corrplot)       # for correlation matrix & plot
library(Hmisc)
library(car)            # to measure the variance-inflation factor of models
library(plm)            # for fixed-effects regression
library(lmtest)         # for coeftest()
library(stargazer)      # regression table output
library(kableExtra)
library(magrittr)
library(readr)
library(ISLR2)
library(leaps)          # for variable selection process

# Data Prep -------------------------------------------------------------------------------------------------------
# Merged dataset includes all months from 2013-01 to 2021-12
full.data <- read.csv("~/Projects/spec-tax-project/FULL-DATA.csv")
full.data <- full.data %>%              # Correct formatting from the .csv import
    dplyr::mutate(Date = ymd(Date),
                  GEO = as.factor(GEO))

# Preparing the data for DID regression
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
                  num_fam, pop_wkids, 
                  # pop_noKid, pop_kid1, cpl_nokid, cpl_child, cpl_kids,    # excluded after the fact, based on VIF
                  # num_cpl,Inc_allcpl, Inc_cpl_child,                      # excluded after the fact, based on VIF
                  Inc_cpl_kids)

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
                  # lnKidless = log(pop_noKid),
                  # lnKid1 = log(pop_kid1),
                  lnKids = log(pop_wkids),
                  # lnCpl_child = log(cpl_child),
                  # lnCpl_kids = log(cpl_kids),
                  lnInc_Cwkids = log(Inc_cpl_kids)
                  ) %>% 
    dplyr::na_if(-Inf)


# Visualizations -------------------------------------------------------------------------------------------------------

# All regions
full.data %>% dplyr::filter(!(GEO %in% c("Alberta", "Ontario", "Quebec", "British Columbia")), Date >= "2013-01-01" & Date <= "2021-12-01") %>% 
    ggplot() +
    geom_line(aes(x = Date, y = HPI, color = GEO)) +
    # Add line for introduction of the NRST
    geom_vline(xintercept = as.numeric(as.Date("2017-04-21")), colour = "red", linetype = 5) +
    annotate("text", x = as.Date("2017-03-01"), y = 160, size = 4.5, angle = 0, colour = "red", label = paste("NRST")) +
    # Add line for the introduction of the 15% APT
    geom_vline(xintercept = as.numeric(as.Date("2016-08-02")), colour = "dodgerblue4", linetype = 5) +
    annotate("text", x = as.Date("2016-06-15"), y = 160, size = 4.5, label = paste("APT1")) +
    # Add line for the introduction of the 20% APT
    geom_vline(xintercept = as.numeric(as.Date("2018-02-20")), colour = "dodgerblue", linetype = 5) +
    annotate("text", x = as.Date("2018-04-20"), y = 160, size = 4.5, label = paste("APT2")) +
    scale_x_date(date_breaks = "4 month", date_labels = "%Y-%b") +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
              # legend.position = c(0, 1), 
              # legend.justification = c(0, 1), 
              # legend.background = theme_rect(colour = NA, fill = "white"),
          axis.text.x = element_text(angle=45, hjust=1)) +
    labs(title = "House Price Index of Major Canadian Cities (2013-2021), Chained to 2016-12-01", 
         y = "House Price Index (2016-12=100)", 
         x = "Date")

# Major Ontario cities
df %>% dplyr::filter(GEO %in% c("Hamilton", "London", "Ottawa", "Toronto")) %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = HPI, color = GEO), size = 1.5) +
    gghighlight(max(HPI), label_params = list(size = 6)) + 
    # Add line for introduction of the NRST
    geom_vline(xintercept = as.numeric(as.Date("2017-04-21")), colour = "red", linetype = 5, size = 1) +
    annotate("text", x = as.Date("2017-03-01"), y = 120, size = 6, angle = 0, colour = "red", label = paste("NRST")) +
    scale_x_date(date_breaks = "3 month", date_labels = "%Y-%b") +
    theme(title = element_text(size = 19),
          axis.title = element_text(size = 15),
          axis.text.x = element_text(angle=45, hjust=1, size = 13),
          axis.text.y = element_text(size = 13)) +
    labs(y = "House Price Index (2016-12=100)", 
         # title = "House Price Index of Ontario Cities (2015-2020), Chained to 2016-12-01", 
         x = "Date")

# Just BC cities
df %>% dplyr::filter(GEO %in% c("Vancouver", "Kelowna", "Victoria")) %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = HPI, color = GEO), size = 1.5) +
    gghighlight(max(HPI), label_params = list(size = 6)) + 
    # Add line for the introduction of the 15% APT
    geom_vline(xintercept = as.numeric(as.Date("2016-08-02")), colour = "dodgerblue4", linetype = 5, size = 1) +
    annotate("text", x = as.Date("2016-06-10"), y = 115, size = 6, label = paste("APT1")) +
    # Add line for the introduction of the 20% APT
    geom_vline(xintercept = as.numeric(as.Date("2018-02-20")), colour = "black", linetype = 5, size = 1) +
    annotate("text", x = as.Date("2018-04-20"), y = 115, size = 6, label = paste("APT2")) + 
    scale_x_date(date_breaks = "3 month", date_labels = "%Y-%b") +
    theme(title = element_text(size = 19),
          axis.title = element_text(size = 15),
          axis.text.x = element_text(angle=45, hjust=1, size = 13),
          axis.text.y = element_text(size = 13)) +
    labs(y = "House Price Index (2016-12=100)", 
         # title = "House Price Index of BC Cities (2015-2020), Chained to 2016-12-01",
         x = "Date")

# Major Canadian cities
full.data %>% 
    dplyr::filter(GEO %in% c("Vancouver", "Toronto", "Calgary", "Montreal", "Ottawa"), 
                  Date >= "2015-01-01" & Date <= "2019-12-01") %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = HPI, color = GEO), size = 1.5) +
    gghighlight(max(HPI), label_params = list(size = 6)) + 
    # Add line for introduction of the NRST
    geom_vline(xintercept = as.numeric(as.Date("2017-04-21")), colour = "red", linetype = 5, size = 1) +
    annotate("text", x = as.Date("2017-02-20"), y = 120, size = 6, angle = 0, colour = "red", label = paste("NRST")) +
    # Add line for the introduction of the 15% APT
    geom_vline(xintercept = as.numeric(as.Date("2016-08-02")), colour = "dodgerblue4", linetype = 5, size = 1) +
    annotate("text", x = as.Date("2016-06-10"), y = 120, size = 6, label = paste("APT1")) +
    # Add line for the introduction of the 20% APT
    geom_vline(xintercept = as.numeric(as.Date("2018-02-20")), colour = "black", linetype = 5, size = 1) +
    annotate("text", x = as.Date("2018-04-20"), y = 120, size = 6, label = paste("APT2")) +
    
    scale_x_date(date_breaks = "3 month", date_labels = "%Y-%b") +
    theme(title = element_text(size = 19),
          axis.title = element_text(size = 15),
          axis.text.x = element_text(angle=45, hjust=1, size = 13),
          axis.text.y = element_text(size = 13)) +
    labs(y = "House Price Index (2016-12=100)", 
         # title = "House Price Index of Major Canadian Cities (2015-2020), Chained to 2016-12-01"),
         x = "Date")


# Summary Statistics & Correlation -------------------------------------------------------------------------------------------------------

# # # # # # # # # # # # # # #
#   Correlation Analysis    #
# # # # # # # # # # # # # # #

# Correlation matrix & plot
df.cor = round(cor(as.matrix(df[13:ncol(df)]), method = "pearson"), digits = 3)
corrplot(df.cor, type="lower", order="hclust", tl.col="black", tl.srt=45)

symnum(df.cor)  # highly correlated

df.cor[upper.tri(df.cor)] <- "-"
kbl(df.cor, booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position"))

# Computing the p-value of correlations
df.cor = round(cor(as.matrix(df[13:ncol(df)]), method = "pearson"), digits = 3)
cor.mtest <- function(mat, ...) {   # ... : further arguments to pass to the native R cor.test function
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
}
p.mat <- cor.mtest(df.cor)  # Matrix of the p-value of the correlation
# head(p.mat[, 1:5])

# Specialized the insignificant value according to the significant level
corrplot(df.cor, type="upper", order="hclust", tl.col="black", tl.srt=45, p.mat = p.mat, sig.level = 0.01)

# Square correlation plot w/ numbers
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(df.cor, method="color", col=col(200),  
         type="upper", #order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
    # based on the initial correlation plot, there are many groups of covariates that are highly correlated to one another
    # must exclude in order to avoid introducing multicollinearity in the regression models


# # # # # # # # # # # # # #
#   Summary Statistics    #
# # # # # # # # # # # # # #
df.stats <- df %>% 
    dplyr::mutate(Mort_rate = Mort_rate * 100,
                  URATE = URATE * 100,
                  LF_RATE = LF_RATE * 100) %>% 
    dplyr::select(Date, GEO, HPI, 
                  Pop, URATE, LF_RATE, Mort_rate,
                  Compl_total, Compl_single, Compl_semi, Compl_row, 
                  Starts_total, Starts_single, Starts_semi, Starts_row, 
                  UC_total, UC_single, UC_semi, UC_row, 
                  Inc_med, Inc_cpl_kids, 
                  Age0_17, Age18_64, Age65_over, 
                  num_fam, pop_wkids
                  )

# Summary statistics table
stargazer(df.stats, type = "latex", digits = 1, font.size = "scriptsize") # "tiny", "scriptsize", "footnotesize", "small", "normalsize"


# Summary statistics for the HPI
sum_HPI <- df.stats %>% 
    group_by(GEO) %>% 
    summarise(N = n(),
              mean = round(mean(HPI, na.rm = T), 2),
              sd = round(sd(HPI, na.rm = T), 2),
              min = round(min(HPI, na.rm = T), 2),
              q1 = round(fivenum(HPI, na.rm = TRUE)[2], 2),   # gives quantile statistics
              median = round(median(HPI, na.rm = T), 2),
              q3 = round(fivenum(HPI, na.rm = TRUE)[4], 2),
              max = round(max(HPI, na.rm = T), 2))

# kbl(sum_HPI, booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position"))

rm(col, cor.mtest, df.cor, df.stats, p.mat, sum_HPI)


# Variable Selection -------------------------------------------------------------------------------------------------------

length(names(df[13:ncol(df)]))  # w/ 21 possible predictors, this means there are 2^21 = 2,097,152 possible models

df.sel <- df %>% 
    select(Date, GEO, HPI, NRST, APT1, APT2, tr_NRST, tr_APT1, tr_APT2, TRxNRST, TRxAPT1, TRxAPT2, 
           Pop, LF, Unempl, 
           Compl_total, Starts_total, UC_total,
           Compl_single, Compl_semi, Compl_row, 
           Starts_single, Starts_semi, Starts_row, 
           UC_single, UC_semi, UC_row, 
           Mort_rate, InMort_total, 
           Inc_med, Age0_17, Age18_64, Age65_over, 
           num_fam, pop_wkids, 
           Inc_cpl_kids) %>% 
    na.omit()

# hist(df.sel$HPI)    # histogram follows a normal distribution shape, therefore don't need to take the log of HPI
cat(names(df.sel[13:ncol(df.sel)]), sep=' + ')  # all predictors

regfit.full <- regsubsets(HPI ~ TRxNRST + TRxAPT1 + TRxAPT2 + 
                              Pop + LF + Unempl + Compl_total + Starts_total + UC_total + Compl_single + 
                              Compl_semi + Compl_row + Starts_single + Starts_semi + Starts_row + 
                              UC_single + UC_semi + UC_row + Mort_rate + InMort_total + Inc_med + 
                              Age0_17 + Age18_64 + Age65_over + num_fam + pop_wkids + Inc_cpl_kids, 
                          data = df.sel, nvmax = 9)    # set nvmax = total amount of predictors
# regfit.full

info <- summary(regfit.full)
# info
var_sel <- cbind(
    info$which, 
    round(cbind(adjr2 = info$adjr2, 
                cp = info$cp, 
                bic = info$bic, 
                rss = info$rss), 3))
# var_sel

kbl(var_sel, booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position"))




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


# Regression & Statistics Tables ------------------------------------------------------------------------------------

# Summary Statistics
stargazer(df.stats, 
          type = "latex", 
          header = FALSE,    # gets rid of citation
          title = "Summary statistics for the subsetted data",
          digits = 1, 
          font.size = "normalsize",
          covariate.labels = c("House price index (HPI)",
                               "Population",
                               'Unemployment rate*',
                               "Labour force participation rate*",
                               "Mortgage rate*",
                               
                               "Total housing completions",
                               "Completed single-detached units",
                               "Completed semi-detached units",
                               "Completed row units",
                               
                               "Total housing starts",
                               "Starts on single-detached units",
                               "Starts on semi-detached units",
                               "Starts on row units",
                               
                               "Total under construction (u.c.)",
                               "Single-detached units (u.c.)",
                               "Semi-detached units (u.c.)",
                               "Row units (u.c.)",
                               
                               "Median income",
                               "Income of couples with kids",
                               "Population aged under 17",
                               "Population aged 18-64",
                               "Population aged 65 and over",
                               "Total number of Families",
                               "Total families with children"),
          notes.align = "l",
          notes = c("Note: variables with an asterisk(*) are percentage figures"),
          notes.append = TRUE)

# Summary statistics for the HPI
kbl(sum_HPI, booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position"))


# Base DID Model w/o controls
stargazer(did.base, 
          type = "latex",
          header = FALSE,    # gets ride of citation
          dep.var.labels = c("log(HPI)"),
          font.size = "normalsize",
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
          out = "~\\Projects\\spec-tax-project\\Analysis\\fit_lm.html",
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
          out = "~\\Projects\\spec-tax-project\\Analysis\\fit_lm.html",
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


