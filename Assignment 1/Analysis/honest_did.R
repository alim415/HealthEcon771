# Install some packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stargazer, withr, fixest, modelsummary, did, HonestDiD)

library(devtools)
install_github("bcallaway11/BMisc", dependencies = TRUE)
install_github("bcallaway11/did", dependencies = TRUE)
install_github("asheshrambachan/HonestDiD", dependencies = TRUE)
#--------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------
# Libraries
# Load libraries
library(ggplot2)
library(here)
library(foreign)
library(tidyverse)
library(dplyr)
library(did)
library(HonestDiD)
## -----------------------------------------------------------------------------

#' @title honest_did
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021)
#' @param es an event study
honest_did <- function(es, ...) {
  UseMethod("honest_did", es)
}


#' @title honest_did.AGGTEobj
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021) when
#'  the event study is estimating using the `did` package
#'
#' @param e event time to compute the sensitivity analysis for.
#'  The default value is `e=0` corresponding to the "on impact"
#'  effect of participating in the treatment.
#' @param type Options are "smoothness" (which conducts a
#'  sensitivity analysis allowing for violations of linear trends
#'  in pre-treatment periods) or "relative_magnitude" (which
#'  conducts a sensitivity analysis based on the relative magnitudes
#'  of deviations from parallel trends in pre-treatment periods).
#' @inheritParams HonestDiD::createSensitivityResults
#' @inheritParams HonestDid::createSensitivityResults_relativeMagnitudes
honest_did.AGGTEobj <- function(es,
                                e=0,
                                type=c("smoothness", "relative_magnitude"),
                                method=NULL,
                                bound="deviation from parallel trends",
                                Mvec=NULL,
                                Mbarvec=NULL,
                                monotonicityDirection=NULL,
                                biasDirection=NULL,
                                alpha=0.05,
                                parallel=FALSE,
                                gridPoints=10^3,
                                grid.ub=NA,
                                grid.lb=NA,
                                ...) {
  
  
  type <- type[1]
  
  # make sure that user is passing in an event study
  if (es$type != "dynamic") {
    stop("need to pass in an event study")
  }
  
  # check if used universal base period and warn otherwise
  if (es$DIDparams$base_period != "universal") {
    warning("it is recommended to use a universal base period for honest_did")
  }
  
  # recover influence function for event study estimates
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  
  # recover variance-covariance matrix
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / (n*n) 
  
  
  nperiods <- nrow(V)
  npre <- sum(1*(es$egt < 0))
  npost <- nperiods - npre
  
  baseVec1 <- basisVector(index=(e+1),size=npost)
  
  orig_ci <- constructOriginalCS(betahat = es$att.egt,
                                 sigma = V, numPrePeriods = npre,
                                 numPostPeriods = npost,
                                 l_vec = baseVec1)
  
  if (type=="relative_magnitude") {
    if (is.null(method)) method <- "C-LF"
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat = es$att.egt, sigma = V, 
                                                             numPrePeriods = npre, 
                                                             numPostPeriods = npost,
                                                             bound=bound,
                                                             method=method,
                                                             l_vec = baseVec1,
                                                             Mbarvec = Mbarvec,
                                                             monotonicityDirection=monotonicityDirection,
                                                             biasDirection=biasDirection,
                                                             alpha=alpha,
                                                             gridPoints=100,
                                                             grid.lb=-1,
                                                             grid.ub=1,
                                                             parallel=parallel)
    
  } else if (type=="smoothness") {
    robust_ci <- createSensitivityResults(betahat = es$att.egt,
                                          sigma = V, 
                                          numPrePeriods = npre, 
                                          numPostPeriods = npost,
                                          method=method,
                                          l_vec = baseVec1,
                                          monotonicityDirection=monotonicityDirection,
                                          biasDirection=biasDirection,
                                          alpha=alpha,
                                          parallel=parallel,
                                          Mvec = Mvec)
  }
  
  list(robust_ci=robust_ci, orig_ci=orig_ci, type=type)
}
## need to define the CS estimate, so i am using Ka Yan's code to just run the analogous program in R  to create the estimate in order to run number 8 
# Import data -------------------------------------------------------------
setwd('~/Documents/Econ771/Assignment 1')

#out_data.path <- "/"

dataset_list = c("HCRIS_Data", "medicaid_expansion", "pos-data-combined")

for (i in dataset_list){
  assign( paste0(i, "_df"),
          read.table(paste0( i ,".txt"),
                     header = TRUE, fill = TRUE, 
                     colClasses=c("character"),
                     check.names = FALSE,
                     sep = '\t', quote = "")
  )
}

# Cleaning data -------------------------------------------------------------

##1. Tidy up individual dataframe
## Medicaid_expansion
medicaid_expansion_df = 
  medicaid_expansion_df %>%
  mutate(year = format(as.Date(medicaid_expansion_df$date_adopted), format="%Y")) %>%
  rename(year_expand = year, expanded_ever = expand_ever) %>% 
  select(state, expanded_ever, year_expand)

## POS
`pos-data-combined_df` = 
  `pos-data-combined_df` %>%
  rename(provider_number = provider) %>%
  filter(category == "Hospital") %>%
  select(provider_number, state, own_type, year)%>%
  mutate(private = ifelse( own_type == "Non-profit Private" | own_type == "Profit" , 1, 0),
         non_profit_private = ifelse( own_type == "Non-profit Private" , 1, 0))

## HCRIS
HCRIS_Data_df = 
  HCRIS_Data_df %>%
  filter(year>=2003 & year <=2019) %>%
  select(provider_number, uncomp_care, tot_pat_rev, year)

##2. Merging into one dataset with only relevant variables (also drop rows with na uncomp_care)
combined_df = HCRIS_Data_df %>%
  
  left_join(`pos-data-combined_df`,
            by = c("provider_number", "year")) %>%
  
  left_join(
    medicaid_expansion_df,
    by = "state") %>%
  
  drop_na(uncomp_care, tot_pat_rev, expanded_ever) %>%
  
  transform(uncomp_care = as.numeric(uncomp_care), 
            tot_pat_rev = as.numeric(tot_pat_rev),
            year = as.numeric(year), 
            year_expand = as.numeric(year_expand),
            expanded_ever = as.integer(as.logical(expanded_ever))) %>%
  
  transform(uncomp_care = uncomp_care/1000000, 
            tot_pat_rev = tot_pat_rev/1000000) %>%
  
  mutate(expanded_t = ifelse(year < year_expand | is.na(year_expand), 0, 1))

#CS Calculation 
CS.data = combined_df %>%
  mutate(year_expand=ifelse(is.na(year_expand),0,year_expand)) %>%
  group_by(provider_number) %>%
  mutate(provider_number=cur_group_id()) %>% ungroup()

CS = att_gt(yname="uncomp_care", tname="year", idname="provider_number",
            gname="year_expand",
            data=CS.data, panel=TRUE, est_method="dr",
            allow_unbalanced_panel=TRUE)
CS_event <- aggte(CS, type="dynamic")

#Sensitivity plot of the estimated CS ATT

M_grid = c(500, 1000, 1500, 2000)

hd_cs_smooth_never <- honest_did(CS_event,
                                 type="smoothness",
                                 Mvec=M_grid)

#Drop 0 as that is not really allowed
hd_cs_smooth_never$robust_ci <- hd_cs_smooth_never$robust_ci[-1,]

# make sensitivity analysis plots
cs_HDiD_smooth <- createSensitivityPlot(hd_cs_smooth_never$robust_ci,
                                        hd_cs_smooth_never$orig_ci)

# Save workspace to pass to Rmd ------------------------------------------------
save.image (file = "~/Documents/Econ771/Assignment 1/emp_ex1.RData")

