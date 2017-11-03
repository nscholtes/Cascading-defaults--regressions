########################################################################
########################################################################
# R Code for running network level regressions based on cascading
# default simulations run in MATLAB

# Current script: liquidity effects OFF

# Copyright Nicolas K. Scholtes, 2017
########################################################################
########################################################################

library(stargazer)
library(devtools)
library(knitr)
library(kableExtra)
library(sandwich)
library(dplyr)
library(car)

########################################################################
# Input datasets created in MATLAB
########################################################################
rm(list=ls())
cat("\014") 
setwd("/Users/nscholte/Desktop/Research/Ch.3 - Systemic risk/R Codes")
tableoutput <- file.path("/Users", "nscholte", "Desktop","Research", "Ch.3 - Systemic risk", "Drafts", "Tables", "LROff/")
file.exists(tableoutput)

# Random shock datasets
## 1 shocked bank
dataset_RS_1_LRoff  <- read.table("data_19-Oct-2017_RS_1_LRoff.csv",header = TRUE,sep = ",")
## 5 shocked banks
dataset_RS_5_LRoff  <- read.table("data_19-Oct-2017_RS_5_LRoff.csv",header = TRUE,sep = ",")

# Targeted shock datasets
dataset_TS_1_LRoff  <- read.table("data_19-Oct-2017_TS_1_LRoff.csv",header = TRUE,sep = ",")
## 5 shocked banks
dataset_TS_5_LRoff  <- read.table("data_19-Oct-2017_TS_5_LRoff.csv",header = TRUE,sep = ",")

# Change working directory for output of LaTeX tables
setwd("/Users/nscholte/Desktop/Research/Ch.3 - Systemic risk/Drafts/Tables/LROff") # Data filepath

########################################################################
# Create new variables
########################################################################

#-----------------------------------------------------------------------
# Diffusion variable = cascade size/total initial shock
#-----------------------------------------------------------------------
## Random shocks
dataset_RS_1_LRoff$Diffusion  <- ((dataset_RS_1_LRoff$TotCapitalLoss/dataset_RS_1_LRoff$Shock_size)*(1000-1)+0.5)/1000
dataset_RS_5_LRoff$Diffusion  <- ((dataset_RS_5_LRoff$TotCapitalLoss/dataset_RS_5_LRoff$Shock_size)*(1000-1)+0.5)/1000

## Targeted shocks
dataset_TS_1_LRoff$Diffusion  <- ((dataset_TS_1_LRoff$TotCapitalLoss/dataset_TS_1_LRoff$Shock_size)*(1000-1)+0.5)/1000
dataset_TS_5_LRoff$Diffusion  <- ((dataset_TS_5_LRoff$TotCapitalLoss/dataset_TS_5_LRoff$Shock_size)*(1000-1)+0.5)/1000

#-----------------------------------------------------------------------
# Normalised shock size = shock size/total initial capital
#-----------------------------------------------------------------------

##Random shocks
dataset_RS_1_LRoff$Norm_Shock_size  <- dataset_RS_1_LRoff$Shock_size/dataset_RS_1_LRoff$Init_Capital
dataset_RS_5_LRoff$Norm_Shock_size  <- dataset_RS_5_LRoff$Shock_size/dataset_RS_5_LRoff$Init_Capital

## Targeted shocks
dataset_TS_1_LRoff$Norm_Shock_size <- dataset_TS_1_LRoff$Shock_size/dataset_TS_1_LRoff$Init_Capital
dataset_TS_5_LRoff$Norm_Shock_size <- dataset_TS_5_LRoff$Shock_size/dataset_TS_5_LRoff$Init_Capital

#-----------------------------------------------------------------------
# Round number of failed banks for Poisson regression
#-----------------------------------------------------------------------

## Random shocks
dataset_RS_1_LRoff$NumFailedBanks_Round <- round(dataset_RS_1_LRoff$NumFailedBanks)
dataset_RS_5_LRoff$NumFailedBanks_Round <- round(dataset_RS_5_LRoff$NumFailedBanks)

## Targeted shocks
dataset_TS_1_LRoff$NumFailedBanks_Round <- round(dataset_TS_1_LRoff$NumFailedBanks)
dataset_TS_5_LRoff$NumFailedBanks_Round <- round(dataset_TS_5_LRoff$NumFailedBanks)

#--------------------------------------------------------------------------------------------------
# Adjust normalised dependent variables (in [0,1] interval) to (0,1) interval for beta regressions
#-------------------------------------------------------------------------------------------------

# Number of failed banks/Total number of banks
dataset_RS_1_LRoff$Norm_FailedBanks <- (dataset_RS_1_LRoff$Norm_FailedBanks*(1000-1)+0.5)/1000
dataset_RS_5_LRoff$Norm_FailedBanks <- (dataset_RS_5_LRoff$Norm_FailedBanks*(1000-1)+0.5)/1000

dataset_TS_1_LRoff$Norm_FailedBanks <- (dataset_TS_1_LRoff$Norm_FailedBanks*(1000-1))/1000
dataset_TS_5_LRoff$Norm_FailedBanks <- (dataset_TS_5_LRoff$Norm_FailedBanks*(1000-1))/1000

# Capital loss/Total initial capital
dataset_RS_1_LRoff$Norm_CapitalLoss <- (dataset_RS_1_LRoff$Norm_CapitalLoss*(1000-1)+0.5)/1000
dataset_RS_5_LRoff$Norm_CapitalLoss <- (dataset_RS_5_LRoff$Norm_CapitalLoss*(1000-1))/1000

dataset_TS_1_LRoff$Norm_CapitalLoss <- (dataset_TS_1_LRoff$Norm_CapitalLoss*(1000-1)+0.5)/1000
dataset_TS_5_LRoff$Norm_CapitalLoss <- (dataset_TS_5_LRoff$Norm_CapitalLoss*(1000-1)+0.5)/1000

# Deposit loss/Total initial deposits
dataset_RS_1_LRoff$Norm_DepositLoss <- (dataset_RS_1_LRoff$Norm_DepositLoss*(1000-1)+0.5)/1000
dataset_RS_5_LRoff$Norm_DepositLoss <- (dataset_RS_5_LRoff$Norm_DepositLoss*(1000-1)+0.5)/1000

dataset_TS_1_LRoff$Norm_DepositLoss <- (dataset_TS_1_LRoff$Norm_DepositLoss*(1000-1)+0.5)/1000
dataset_TS_5_LRoff$Norm_DepositLoss <- (dataset_TS_5_LRoff$Norm_DepositLoss*(1000-1)+0.5)/1000

########################################################################
# Reorganise tables
########################################################################

dataset_RS_1_LRoff <- dataset_RS_1_LRoff[c(1,39,2,seq(3,7,length=5),37,seq(8,36,length=29),38)]
dataset_RS_5_LRoff <- dataset_RS_5_LRoff[c(1,39,2,seq(3,7,length=5),37,seq(8,36,length=29),38)]

dataset_TS_1_LRoff <- dataset_TS_1_LRoff[c(1,39,2,seq(3,7,length=5),37,seq(8,36,length=29),38)]
dataset_TS_5_LRoff <- dataset_TS_5_LRoff[c(1,39,2,seq(3,7,length=5),37,seq(8,36,length=29),38)]

########################################################################
# Running various regressions
########################################################################

# Explanatory variables in regression tables take the form of:
### 1st Row: Network variables
### 2nd Row: Balance sheet variables (initial and aggregate)
### 3rd Row: Shock variables (centrality of shocked nodes and size of shock)

#-----------------------------------------------------------------------
# Dependent variable: Shock diffusion
#-----------------------------------------------------------------------

# Random shock
## 1 bank
RS_1_Diffusion.reg <- betareg(Diffusion
                        ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                        + Norm_Init_Capital + Norm_Init_IB_exp
                        + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB
                        + Init_capital_SB + Init_IBB_SB + Init_IBL_SB,
                         data = dataset_RS_1_LRoff)


## 5 banks
RS_5_Diffusion.reg <- betareg(Diffusion
                        ~ Density + Diameter +  Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                        + Norm_Init_Capital + Norm_Init_IB_exp
                        + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB
                        + Init_capital_SB + Init_IBB_SB + Init_IBL_SB,
                        data = dataset_RS_5_LRoff)

# Targeted shock
## 1 bank
TS_1_Diffusion.reg <- betareg(Diffusion
                        ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                        + Norm_Init_Capital + Norm_Init_IB_exp
                        + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB
                        + Init_capital_SB + Init_IBB_SB + Init_IBL_SB,
                        data = dataset_TS_1_LRoff)
## 5 banks
TS_5_Diffusion.reg <- betareg(Diffusion
                        ~ Density + Diameter +  Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                        + Norm_Init_Capital + Norm_Init_IB_exp
                        + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB
                        + Init_capital_SB + Init_IBB_SB + Init_IBL_SB,
                        data = dataset_TS_5_LRoff)

RS_1_Diffusion.vif <- vif(RS_1_Diffusion.reg)
RS_5_Diffusion.vif <- vif(RS_5_Diffusion.reg)
TS_1_Diffusion.vif <- vif(TS_1_Diffusion.reg)
TS_5_Diffusion.vif <- vif(TS_5_Diffusion.reg)
# # Computing robust standard errors
# RS_1_Diffusion.cov <- vcovHC(RS_1_Diffusion.reg, type = "HC") 
# RS_1_Diffusion.rse <- sqrt(diag(RS_1_Diffusion.cov))
# 
# RS_5_Diffusion.cov <- vcovHC(RS_5_Diffusion.reg, type = "HC")
# RS_5_Diffusion.rse <- sqrt(diag(RS_5_Diffusion.cov))
# 
# TS_1_Diffusion.cov <- vcovHC(TS_1_Diffusion.reg, type = "HC")
# TS_1_Diffusion.rse <- sqrt(diag(TS_1_Diffusion.cov))
# 
# TS_5_Diffusion.cov <- vcovHC(TS_5_Diffusion.reg, type = "HC")
# TS_5_Diffusion.rse <- sqrt(diag(TS_5_Diffusion.cov))

#-----------------------------------------------------------------------
# Dependent variable: Number of failed banks
#-----------------------------------------------------------------------

# Random shock
## 1 bank
RS_1_NumFail.reg <- betareg(Norm_FailedBanks
                     ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                     + Norm_Init_Capital + Norm_Init_IB_exp
                     + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                     + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Norm_Shock_size,
                     data = dataset_RS_1_LRoff)
## 5 banks
RS_5_NumFail.reg <- betareg(Norm_FailedBanks
                     ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                     + Norm_Init_Capital + Norm_Init_IB_exp
                     + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                     + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Norm_Shock_size,
                     data = dataset_RS_5_LRoff)
# Targeted shock
## 1 bank
TS_1_NumFail.reg <- betareg(Norm_FailedBanks
                     ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                     + Norm_Init_Capital + Norm_Init_IB_exp
                     + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                     + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Norm_Shock_size,
                     data = dataset_TS_1_LRoff)
## 5 banks
TS_5_NumFail.reg <- betareg(Norm_FailedBanks
                     ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                     + Norm_Init_Capital + Norm_Init_IB_exp
                     + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                     + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Norm_Shock_size,
                     data = dataset_TS_5_LRoff)

# Computing Variance Inflation Factors
RS_1_NumFail.vif <- vif(RS_1_NumFail.reg)
RS_5_NumFail.vif <- vif(RS_5_NumFail.reg)
TS_1_NumFail.vif <- vif(TS_1_NumFail.reg)
TS_5_NumFail.vif <- vif(TS_5_NumFail.reg)

# # Computing robust standard errors
# RS_1_NumFail.cov <- vcovHC(RS_1_NumFail.reg, type = "HC") 
# RS_1_NumFail.rse <- sqrt(diag(RS_1_NumFail.cov))
# 
# RS_5_NumFail.cov <- vcovHC(RS_5_NumFail.reg, type = "HC")
# RS_5_NumFail.rse <- sqrt(diag(RS_5_NumFail.cov))
# 
# TS_1_NumFail.cov <- vcovHC(TS_1_NumFail.reg, type = "HC")
# TS_1_NumFail.rse <- sqrt(diag(TS_1_NumFail.cov))
# 
# TS_5_NumFail.cov <- vcovHC(TS_5_NumFail.reg, type = "HC")
# TS_5_NumFail.rse <- sqrt(diag(TS_5_NumFail.cov))

#-----------------------------------------------------------------------
# Dependent variable: Cascade size
#-----------------------------------------------------------------------

# Random shock
## 1 bank
RS_1_CapLoss.reg <- betareg(Norm_CapitalLoss
                      ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                      + Norm_Init_Capital + Norm_Init_IB_exp 
                      + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                      + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Norm_Shock_size,
                      data = dataset_RS_1_LRoff)

## 5 banks
RS_5_CapLoss.reg <- betareg(Norm_CapitalLoss
                      ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                      + Norm_Init_Capital + Norm_Init_IB_exp 
                      + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                      + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Norm_Shock_size,
                      data = dataset_RS_5_LRoff)
# Targeted shock
## 1 bank
TS_1_CapLoss.reg <- betareg(Norm_CapitalLoss
                      ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                      + Norm_Init_Capital + Norm_Init_IB_exp 
                      + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                      + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Norm_Shock_size,
                      data = dataset_TS_1_LRoff)

## 5 banks
TS_5_CapLoss.reg <- betareg(Norm_CapitalLoss
                      ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                      + Norm_Init_Capital + Norm_Init_IB_exp 
                      + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                      + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Norm_Shock_size,
                      data = dataset_TS_5_LRoff)

# # Computing robust standard errors
# RS_1_CapLoss.cov <- vcovHC(RS_1_CapLoss.reg, type = "HC") 
# RS_1_CapLoss.rse <- sqrt(diag(RS_1_CapLoss.cov))
# 
# RS_5_CapLoss.cov <- vcovHC(RS_5_CapLoss.reg, type = "HC")
# RS_5_CapLoss.rse <- sqrt(diag(RS_5_CapLoss.cov))
# 
# TS_1_CapLoss.cov <- vcovHC(TS_1_CapLoss.reg, type = "HC")
# TS_1_CapLoss.rse <- sqrt(diag(TS_1_CapLoss.cov))
# 
# TS_5_CapLoss.cov <- vcovHC(TS_5_CapLoss.reg, type = "HC")
# TS_5_CapLoss.rse <- sqrt(diag(TS_5_CapLoss.cov))

# Computing Variance Inflation Factors
RS_1_CapLoss.vif <- vif(RS_1_CapLoss.reg)
RS_5_CapLoss.vif <- vif(RS_5_CapLoss.reg)
TS_1_CapLoss.vif <- vif(TS_1_CapLoss.reg)
TS_5_CapLoss.vif <- vif(TS_5_CapLoss.reg)


#-----------------------------------------------------------------------
# Dependent variable: Total deposit loss
#-----------------------------------------------------------------------

# Random shock
## 1 bank
RS_1_DepLoss.reg  <- betareg(Norm_DepositLoss
                       ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Norm_Init_Capital + Norm_Init_IB_exp 
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                       + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Norm_Shock_size,
                       data = dataset_RS_1_LRoff)

## 5 banks
RS_5_DepLoss.reg  <- betareg(Norm_DepositLoss
                       ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Norm_Init_Capital + Norm_Init_IB_exp 
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                       + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Norm_Shock_size,
                       data = dataset_RS_5_LRoff)


# Targeted shock
## 1 bank
TS_1_DepLoss.reg  <- betareg(Norm_DepositLoss
                       ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Norm_Init_Capital + Norm_Init_IB_exp 
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                       + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Norm_Shock_size,
                       data = dataset_TS_1_LRoff)

## 5 banks
TS_5_DepLoss.reg  <- betareg(Norm_DepositLoss
                       ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Norm_Init_Capital + Norm_Init_IB_exp 
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                       + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Norm_Shock_size,
                       data = dataset_TS_5_LRoff)

# # Computing robust standard errors
# RS_1_DepLoss.cov <- vcovHC(RS_1_DepLoss.reg, type = "HC") 
# RS_1_DepLoss.rse <- sqrt(diag(RS_1_DepLoss.cov))
# 
# RS_5_DepLoss.cov <- vcovHC(RS_5_DepLoss.reg, type = "HC")
# RS_5_DepLoss.rse <- sqrt(diag(RS_5_DepLoss.cov))
# 
# TS_1_DepLoss.cov <- vcovHC(TS_1_DepLoss.reg, type = "HC")
# TS_1_DepLoss.rse <- sqrt(diag(TS_1_DepLoss.cov))
# 
# TS_5_DepLoss.cov <- vcovHC(TS_5_DepLoss.reg, type = "HC")
# TS_5_DepLoss.rse <- sqrt(diag(TS_5_DepLoss.cov))

# Computing Variance Inflation Factors
RS_1_DepLoss.vif  <- vif(RS_1_DepLoss.reg)
RS_5_DepLoss.vif  <- vif(RS_5_DepLoss.reg)
TS_1_DepLoss.vif  <- vif(TS_1_DepLoss.reg)
TS_5_DepLoss.vif  <- vif(TS_5_DepLoss.reg)

########################################################################
# Outputting regression tables
########################################################################

CovLabels <- c("$DSTY$","$DIAM$","$APL$","$ACC$","$RPTY$","$ASTY^{oi}$","$ASTY^{io}$","$ASTY^{oo}$","$ASTY^{ii}$",    # Network
              "$\\hat{K}^{0}$","$\\hat{L}^{0}$",                                                              # Initial normalised balance sheet
              "$INDEG^{S}$","$OUTDEG^{S}$","$INCLOSE^{S}$","$OUTCLOSE^{S}$","$BTWN^{S}$","$PAGERANK^{S}$",
              "$k^{S}$","$b^{S}$","$l^{S}$","$\\hat{S}$")  # Shock properties 

#-----------------------------------------------------------------------
# DV = Diffusion
#-----------------------------------------------------------------------

## Regression tables
`textit{1 bank}`  = RS_1_Diffusion.reg; `textit{5 banks}` = RS_5_Diffusion.reg
`Textit{1 bank}`  = TS_1_Diffusion.reg; `Textit{5 banks}` = TS_5_Diffusion.reg

stargazer(`textit{1 bank}`,`textit{5 banks}`,`Textit{1 bank}`,`Textit{5 banks}`, object.names = TRUE,
          title = "Regression results - Shock diffusion (liquidity effects off)",
          align = TRUE, report = "vc*",
          column.labels = c("\\textbf{Random shock}","\\textbf{Targeted shock}"), column.separate = c(2, 2),
          dep.var.labels.include = FALSE, dep.var.caption = "",
          covariate.labels = CovLabels, model.numbers = FALSE,
          keep.stat=c("rsq"), no.space=TRUE,font.size = "footnotesize",table.placement ="H",label = "results_LRoff_Diffusion",
          omit.table.layout = "n",out=paste(tableoutput,"RegResults_LRoff_Diffusion.tex", sep=""))

## VIFs
sink("VIFtable_LRoff_Diffusion.tex")
  kable(cbind(RS_1_Diffusion.vif,RS_5_Diffusion.vif,TS_1_Diffusion.vif,TS_5_Diffusion.vif), booktabs = T, digits = 3,linesep = "",col.names = c("textit{1 bank} ","textit{5 banks}","textit{1 bank}","textit{5 banks}"),
  caption ="Variance Inflation Factors for $DIFFUSION$ regression (liquidity effects off)") %>%
  kable_styling(font_size = 7,latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" ", "textbf{Random shock}" = 2, "textbf{Targeted shock}" = 2))
sink()
#-----------------------------------------------------------------------
# DV = Number of failed banks
#-----------------------------------------------------------------------

`textit{1 bank}`  = RS_1_NumFail.reg; `textit{5 banks}` = RS_5_NumFail.reg
`Textit{1 bank}`  = TS_1_NumFail.reg; `Textit{5 banks}` = TS_5_NumFail.reg

stargazer(`textit{1 bank}`,`textit{5 banks}`,`Textit{1 bank}`,`Textit{5 banks}`,object.names = TRUE,
          title = "Regression results - Number of failed banks (liquidity effects off)",
          align = TRUE, report = "vc*",
          column.labels = c("\\textbf{Random shock}","\\textbf{Targeted shock}"), column.separate = c(2, 2),
          dep.var.labels.include = FALSE, dep.var.caption = "",
          covariate.labels = CovLabels, model.numbers = FALSE,
          keep.stat=c("rsq"), no.space=TRUE,font.size = "footnotesize",table.placement ="H",label = "results_LRoff_Numfail_Norm",
          omit.table.layout = "n", out=paste(tableoutput,"RegResults_LRoff_Numfail_Norm.tex",sep=""))

## VIFs
sink("VIFtable_LRoff_Numfail_Norm.tex")
  kable(cbind(RS_1_NumFail.vif,RS_5_NumFail.vif,TS_1_NumFail.vif,TS_5_NumFail.vif), booktabs = T, digits = 3,linesep = "",col.names = c("textit{1 bank} ","textit{5 banks}","textit{1 bank}","textit{5 banks}"),
  caption ="Variance Inflation Factors for $NUMFAIL$ regression (liquidity effects off)") %>%
  kable_styling(font_size = 7,latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" ", "textbf{Random shock}" = 2, "textbf{Targeted shock}" = 2))
sink()

#-----------------------------------------------------------------------
# DV = Total capital loss
#-----------------------------------------------------------------------

`textit{1 bank}`  = RS_1_CapLoss.reg; `textit{5 banks}` = RS_5_CapLoss.reg
`Textit{1 bank}`  = TS_1_CapLoss.reg; `Textit{5 banks}` = TS_5_CapLoss.reg

stargazer(`textit{1 bank}`,`textit{5 banks}`,`Textit{1 bank}`,`Textit{5 banks}`,object.names = TRUE,
          title = "Regression results - Cascade size (liquidity effects off)",
          align = TRUE, report = "vc*",
          column.labels = c("\\textbf{Random shock}","\\textbf{Targeted shock}"), column.separate = c(2, 2),
          dep.var.labels.include = FALSE, dep.var.caption = "",
          covariate.labels = CovLabels, model.numbers = FALSE,
          keep.stat=c("rsq"), no.space=TRUE,font.size = "footnotesize",table.placement ="H",label = "results_LRoff_Cascsize_Norm",
          omit.table.layout = "n", out=paste(tableoutput,"RegResults_LRoff_Cascsize_Norm.tex",sep=""))

## VIFs
sink("VIFtable_LRoff_Cascsize_Norm.tex")
  kable(cbind(RS_1_CapLoss.vif,RS_5_CapLoss.vif,TS_1_CapLoss.vif,TS_5_CapLoss.vif), booktabs = T, digits = 3,linesep = "",col.names = c("textit{1 bank} ","textit{5 banks}","textit{1 bank}","textit{5 banks}"),
  caption ="Variance Inflation Factors for $CASCSIZE$ regression (liquidity effects off)") %>%
  kable_styling(font_size = 7,latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" ", "textbf{Random shock}" = 2, "textbf{Targeted shock}" = 2))
sink()

#-----------------------------------------------------------------------
# DV = Total deposit loss
#-----------------------------------------------------------------------

`textit{1 bank}`  = RS_1_DepLoss.reg; `textit{5 banks}` = RS_5_DepLoss.reg
`Textit{1 bank}`  = TS_1_DepLoss.reg; `Textit{5 banks}` = TS_5_DepLoss.reg

stargazer(`textit{1 bank}`,`textit{5 banks}`,`Textit{1 bank}`,`Textit{5 banks}`,object.names = TRUE,
          title = "Regression results - Deposit loss (liquidity effects off)",
          align = TRUE, report = "vc*",
          column.labels = c("\\textbf{Random shock}","\\textbf{Targeted shock}"), column.separate = c(2, 2),
          dep.var.labels.include = FALSE, dep.var.caption = "",
          covariate.labels = CovLabels, model.numbers = FALSE,
          keep.stat=c("rsq"), no.space=TRUE,font.size = "footnotesize",table.placement ="H",label = "results_LRoff_Deploss_Norm",
          omit.table.layout = "n", out=paste(tableoutput,"RegResults_LRoff_Deploss_Norm.tex",sep=""))

## VIFs
sink("VIFtable_LRoff_Deploss_Norm.tex")
  kable(cbind(RS_1_DepLoss.vif,RS_5_DepLoss.vif,TS_1_DepLoss.vif,TS_5_DepLoss.vif), booktabs = T, digits = 3,linesep = "",col.names = c("textit{1 bank} ","textit{5 banks}","textit{1 bank}","textit{5 banks}"),
  caption ="Variance Inflation Factors for $DEPOSITLOSS$ regression (liquidity effects off)") %>%
  kable_styling(font_size = 7,latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" ", "textbf{Random shock}" = 2, "textbf{Targeted shock}" = 2))
sink()

