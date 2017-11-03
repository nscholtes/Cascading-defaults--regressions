########################################################################
########################################################################
# R Code for running network level regressions based on cascading
# default simulations run in MATLAB

# Current script: liquidity effects ON

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
tableoutput <- file.path("/Users", "nscholte", "Desktop","Research", "Ch.3 - Systemic risk", "Drafts", "Tables", "LROn/")
file.exists(tableoutput)

# Random shock datasets
## 1 shocked bank
dataset_RS_1_LRon  <- read.table("data_19-Oct-2017_RS_1_LRon.csv",header = TRUE,sep = ",")
## 5 shocked banks
dataset_RS_5_LRon  <- read.table("data_19-Oct-2017_RS_5_LRon.csv",header = TRUE,sep = ",")

# Targeted shock datasets
dataset_TS_1_LRon  <- read.table("data_19-Oct-2017_TS_1_LRon.csv",header = TRUE,sep = ",")
## 5 shocked banks
dataset_TS_5_LRon  <- read.table("data_19-Oct-2017_TS_5_LRon.csv",header = TRUE,sep = ",")

# Change working directory for output of LaTeX tables
setwd("/Users/nscholte/Desktop/Research/Ch.3 - Systemic risk/Drafts/Tables/LROn") # Data filepath

########################################################################
# Create new variables
########################################################################

#-----------------------------------------------------------------------
# Diffusion variable = cascade size/total initial shock
#-----------------------------------------------------------------------
## Random shocks
dataset_RS_1_LRon$Diffusion  <- dataset_RS_1_LRon$TotCapitalLoss/dataset_RS_1_LRon$Shock_size
dataset_RS_5_LRon$Diffusion  <- dataset_RS_5_LRon$TotCapitalLoss/dataset_RS_5_LRon$Shock_size

## Targeted shocks
dataset_TS_1_LRon$Diffusion  <- dataset_TS_1_LRon$TotCapitalLoss/dataset_TS_1_LRon$Shock_size
dataset_TS_5_LRon$Diffusion  <- dataset_TS_5_LRon$TotCapitalLoss/dataset_TS_5_LRon$Shock_size

#-----------------------------------------------------------------------
# Normalised shock size = shock size/total initial capital
#-----------------------------------------------------------------------

##Random shocks
dataset_RS_1_LRon$Norm_Shock_size  <- dataset_RS_1_LRon$Shock_size/dataset_RS_1_LRon$Init_Capital
dataset_RS_5_LRon$Norm_Shock_size  <- dataset_RS_5_LRon$Shock_size/dataset_RS_5_LRon$Init_Capital

## Targeted shocks
dataset_TS_1_LRon$Norm_Shock_size <- dataset_TS_1_LRon$Shock_size/dataset_TS_1_LRon$Init_Capital
dataset_TS_5_LRon$Norm_Shock_size <- dataset_TS_5_LRon$Shock_size/dataset_TS_5_LRon$Init_Capital

#-----------------------------------------------------------------------
# Round number of failed banks for Poisson regression
#-----------------------------------------------------------------------

## Random shocks
dataset_RS_1_LRon$NumFailedBanks_Round <- round(dataset_RS_1_LRon$NumFailedBanks)
dataset_RS_5_LRon$NumFailedBanks_Round <- round(dataset_RS_5_LRon$NumFailedBanks)

## Targeted shocks
dataset_TS_1_LRon$NumFailedBanks_Round <- round(dataset_TS_1_LRon$NumFailedBanks)
dataset_TS_5_LRon$NumFailedBanks_Round <- round(dataset_TS_5_LRon$NumFailedBanks)

########################################################################
# Reorganise tables
########################################################################

dataset_RS_1_LRon <- dataset_RS_1_LRon[c(1,39,2,seq(3,7,length=5),37,seq(8,36,length=29),38)]
dataset_RS_5_LRon <- dataset_RS_5_LRon[c(1,39,2,seq(3,7,length=5),37,seq(8,36,length=29),38)]

dataset_TS_1_LRon <- dataset_TS_1_LRon[c(1,39,2,seq(3,7,length=5),37,seq(8,36,length=29),38)]
dataset_TS_5_LRon <- dataset_TS_5_LRon[c(1,39,2,seq(3,7,length=5),37,seq(8,36,length=29),38)]

########################################################################
# Descriptive statistics
########################################################################  

cols_cascade <- c(1,4,6,8,9,10) #  Cascade output (4: Number of failed banks, Cascade size, deposit loss, simulation time, diffusion)
cols_shock   <- c(23:28)   # Shocked banks: various (local) centrality measures
cols_centmeas   <- c(32:38)   # Shocked banks: various (local) centrality measures

# Cascade dynamics across different shock specifications

# Random shock
## 1 bank
stargazer(dataset_RS_1_LRon[,cols_cascade],nobs = FALSE,
          title="Summary statistics - Model dynamics (Random shock, liquidity effects on)",
          font.size = "scriptsize",table.placement ="H",no.space=TRUE,label="Summstats_casc_RS_LRon",
          covariate.labels=c("$NUMFAIL$","$CASCSIZE$","$DEPOSITLOSS$","$SIMTIME$","$DIFFUSION$","$\\Delta ASSETPRICE$"),
          out=paste(tableoutput,"Summstats_casc_RS_1_LRon.tex",sep=""))
# 5 banks
stargazer(dataset_RS_5_LRon[,cols_cascade],nobs = FALSE,
          font.size = "scriptsize",table.placement ="H",no.space=TRUE,label="Summstats_casc_RS_LRon",
          covariate.labels=c("$NUMFAIL$","$CASCSIZE$","$DEPOSITLOSS$","$SIMTIME$","$DIFFUSION$","$\\Delta ASSETPRICE$"),
          out=paste(tableoutput,"Summstats_casc_RS_5_LRon.tex",sep=""))

# N.B. Combine tables in LaTeX using Master File: Summstats_casc_RS_LRon.tex

# Targeted shock
## 1 bank
stargazer(dataset_TS_1_LRon[,cols_cascade],nobs = FALSE,
          title="Summary statistics - Model dynamics (Targeted shock, liquidity effects on)",
          font.size = "scriptsize",table.placement ="H",no.space=TRUE,label="Summstats_casc_TS_LRon",
          covariate.labels=c("$NUMFAIL$","$CASCSIZE$","$DEPOSITLOSS$","$SIMTIME$","$DIFFUSION$","$\\Delta ASSETPRICE$"),
          out=paste(tableoutput,"Summstats_casc_TS_1_LRon.tex",sep=""))
## 5 banks
stargazer(dataset_TS_5_LRon[,cols_cascade],nobs = FALSE,
          font.size = "scriptsize",table.placement ="H",no.space=TRUE,label="Summstats_casc_TS_LRon",
          covariate.labels=c("$NUMFAIL$","$CASCSIZE$","$DEPOSITLOSS$","$SIMTIME$","$DIFFUSION$","$\\Delta ASSETPRICE$"),
          out=paste(tableoutput,"Summstats_casc_TS_5_LRon.tex",sep=""))

# N.B. Combine tables in LaTeX using Master File: Summstats_casc_TS_LRon.tex

########################################################################
# Correlation matrices 
########################################################################   

# Network centrality measures
## Random shocks
correlationmatrix.RS_1 <- cor(dataset_RS_1_LRon[,cols_centmeas])
round(correlationmatrix.RS_1, 2)
stargazer(correlationmatrix.RS_1, title="Correlation Matrix - centrality measures (Liquidity effects on)",
          font.size = "footnotesize",table.placement ="H",no.space=TRUE,label="corrmat_RS_1_LRon",
          out=paste(tableoutput,"corrmat_RS_1_LRon.tex",sep=""))

correlationmatrix.RS_5 <- cor(dataset_RS_5_LRon[,cols_centmeas])
round(correlationmatrix.RS_5 , 2)
stargazer(correlationmatrix.RS_5 , title="Correlation Matrix - centrality measures (Liquidity effects on)",
          font.size = "footnotesize",table.placement ="H",no.space=TRUE,label="corrmat_RS_5_LRon",
          out=paste(tableoutput,"corrmat_RS_5_LRon.tex",sep=""))

## Targeted shocks
correlationmatrix.TS_1 <- cor(dataset_TS_1_LRon[,cols_centmeas])
round(correlationmatrix.TS_1 , 2)
stargazer(correlationmatrix.TS_1 , title="Correlation Matrix - centrality measures (Liquidity effects on)",
          font.size = "footnotesize",table.placement ="H",no.space=TRUE,label="corrmat_TS_1_LRon",
          out=paste(tableoutput,"corrmat_TS_1_LRon.tex",sep=""))


correlationmatrix.TS_5 <- cor(dataset_TS_5_LRon[,cols_centmeas])
round(correlationmatrix.TS_5, 2)
stargazer(correlationmatrix.TS_5, title="Correlation Matrix - centrality measures (Liquidity effects on)",
          font.size = "footnotesize",table.placement ="H",no.space=TRUE,label="corrmat_TS_5_LRon",
          out=paste(tableoutput,"corrmat_TS_5_LRon.tex",sep=""))

########################################################################
# Running various regressions
########################################################################

# Explanatory variables in regression tables take the form of:
### 1st Row: Network variables
### 2nd Row: Balance sheet variables (initial and aggregate)
### 3rd Row: Shock variables (centrality of shocked nodes and size of shock)

#-----------------------------------------------------------------------
# Dependent variable: Number of failed banks
#-----------------------------------------------------------------------

# Random shock
## 1 bank
RS_1_NumFail.reg <- lm(NumFailedBanks
                       ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Init_Capital + Init_IB_exp + Init_Assets
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                       + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                       data = dataset_RS_1_LRon)


## 5 banks
RS_5_NumFail.reg <- lm(NumFailedBanks
                       ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Init_Capital + Init_IB_exp + Init_Assets
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                       + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                       data = dataset_RS_5_LRon)
# Targeted shock
## 1 bank
TS_1_NumFail.reg <- lm(NumFailedBanks
                       ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Init_Capital + Init_IB_exp + Init_Assets
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                       + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                       data = dataset_TS_1_LRon)
## 5 banks
TS_5_NumFail.reg <- lm(NumFailedBanks
                       ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Init_Capital + Init_IB_exp + Init_Assets
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                       + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                       data = dataset_TS_5_LRon)

# Computing robust standard errors
RS_1_NumFail.cov <- vcovHC(RS_1_NumFail.reg, type = "HC") 
RS_1_NumFail.rse <- sqrt(diag(RS_1_NumFail.cov))

RS_5_NumFail.cov <- vcovHC(RS_5_NumFail.reg, type = "HC")
RS_5_NumFail.rse <- sqrt(diag(RS_5_NumFail.cov))

TS_1_NumFail.cov <- vcovHC(TS_1_NumFail.reg, type = "HC")
TS_1_NumFail.rse <- sqrt(diag(TS_1_NumFail.cov))

TS_5_NumFail.cov <- vcovHC(TS_5_NumFail.reg, type = "HC")
TS_5_NumFail.rse <- sqrt(diag(TS_5_NumFail.cov))

# Computing Variance Inflation Factors
RS_1_NumFail.vif <- vif(RS_1_NumFail.reg)
RS_5_NumFail.vif <- vif(RS_5_NumFail.reg)
TS_1_NumFail.vif <- vif(TS_1_NumFail.reg)
TS_5_NumFail.vif <- vif(TS_5_NumFail.reg)

#-----------------------------------------------------------------------
# Dependent variable: Number of failed banks (rounded + Poisson regression for count data)
#-----------------------------------------------------------------------

# Random shock
## 1 bank
RS_1_NumFail_Poiss.reg <- glm(NumFailedBanks_Round
                              ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                              + Init_Capital + Init_IB_exp + Init_Assets
                              + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                              + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                              data = dataset_RS_1_LRon,family = poisson)

## 5 banks
RS_5_NumFail_Poiss.reg <- glm(NumFailedBanks_Round
                              ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                              + Init_Capital + Init_IB_exp + Init_Assets
                              + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                              + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                              data = dataset_RS_5_LRon,family = poisson)

# Targeted shock
## 1 bank
TS_1_NumFail_Poiss.reg <- glm(NumFailedBanks_Round
                              ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                              + Init_Capital + Init_IB_exp + Init_Assets
                              + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                              + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                              data = dataset_TS_1_LRon,family = poisson)

## 5 banks
TS_5_NumFail_Poiss.reg <- glm(NumFailedBanks_Round
                              ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                              + Init_Capital + Init_IB_exp + Init_Assets
                              + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                              + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                              data = dataset_TS_5_LRon,family = poisson)

# Computing robust standard errors
RS_1_NumFail_Poiss.cov <- vcovHC(RS_1_NumFail_Poiss.reg, type = "HC") 
RS_1_NumFail_Poiss.rse <- sqrt(diag(RS_1_NumFail_Poiss.cov))

RS_5_NumFail_Poiss.cov <- vcovHC(RS_5_NumFail_Poiss.reg, type = "HC")
RS_5_NumFail_Poiss.rse <- sqrt(diag(RS_5_NumFail_Poiss.cov))

TS_1_NumFail_Poiss.cov <- vcovHC(TS_1_NumFail_Poiss.reg, type = "HC")
TS_1_NumFail_Poiss.rse <- sqrt(diag(TS_1_NumFail_Poiss.cov))

TS_5_NumFail_Poiss.cov <- vcovHC(TS_5_NumFail_Poiss.reg, type = "HC")
TS_5_NumFail_Poiss.rse <- sqrt(diag(TS_5_NumFail_Poiss.cov))

# Computing Variance Inflation Factors
RS_1_NumFail_Poiss.vif <- vif(RS_1_NumFail_Poiss.reg)
RS_5_NumFail_Poiss.vif <- vif(RS_5_NumFail_Poiss.reg)
TS_1_NumFail_Poiss.vif <- vif(TS_1_NumFail_Poiss.reg)
TS_5_NumFail_Poiss.vif <- vif(TS_5_NumFail_Poiss.reg)

#-----------------------------------------------------------------------
# Dependent variable: Cascade size
#-----------------------------------------------------------------------

# Random shock
## 1 bank
RS_1_CapLoss.reg <- lm(TotCapitalLoss
                       ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Init_Capital + Init_IB_exp + Init_Assets
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                       + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                       data = dataset_RS_1_LRon)

## 5 banks
RS_5_CapLoss.reg <- lm(TotCapitalLoss
                       ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Init_Capital + Init_IB_exp + Init_Assets
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                       + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                       data = dataset_RS_5_LRon)
# Targeted shock
## 1 bank
TS_1_CapLoss.reg <- lm(TotCapitalLoss
                       ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Init_Capital + Init_IB_exp + Init_Assets
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                       + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                       data = dataset_TS_1_LRon)

## 5 banks
TS_5_CapLoss.reg <- lm(TotCapitalLoss
                       ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Init_Capital + Init_IB_exp + Init_Assets
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                       + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                       data = dataset_TS_5_LRon)

# Computing robust standard errors
RS_1_CapLoss.cov <- vcovHC(RS_1_CapLoss.reg, type = "HC") 
RS_1_CapLoss.rse <- sqrt(diag(RS_1_CapLoss.cov))

RS_5_CapLoss.cov <- vcovHC(RS_5_CapLoss.reg, type = "HC")
RS_5_CapLoss.rse <- sqrt(diag(RS_5_CapLoss.cov))

TS_1_CapLoss.cov <- vcovHC(TS_1_CapLoss.reg, type = "HC")
TS_1_CapLoss.rse <- sqrt(diag(TS_1_CapLoss.cov))

TS_5_CapLoss.cov <- vcovHC(TS_5_CapLoss.reg, type = "HC")
TS_5_CapLoss.rse <- sqrt(diag(TS_5_CapLoss.cov))

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
RS_1_DepLoss.reg  <- lm(TotDepositLoss
                        ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                        + Init_Capital + Init_IB_exp + Init_Assets 
                        + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                        + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                        data = dataset_RS_1_LRon)

## 5 banks
RS_5_DepLoss.reg  <- lm(TotDepositLoss
                        ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                        + Init_Capital + Init_IB_exp + Init_Assets 
                        + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                        + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                        data = dataset_RS_5_LRon)

# Targeted shock
## 1 bank
TS_1_DepLoss.reg  <- lm(TotDepositLoss
                        ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                        + Init_Capital + Init_IB_exp + Init_Assets 
                        + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                        + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                        data = dataset_TS_1_LRon)

## 5 banks
TS_5_DepLoss.reg  <- lm(TotDepositLoss
                        ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                        + Init_Capital + Init_IB_exp + Init_Assets 
                        + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                        + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                        data = dataset_TS_5_LRon)

# Computing robust standard errors
RS_1_DepLoss.cov <- vcovHC(RS_1_DepLoss.reg, type = "HC") 
RS_1_DepLoss.rse <- sqrt(diag(RS_1_DepLoss.cov))

RS_5_DepLoss.cov <- vcovHC(RS_5_DepLoss.reg, type = "HC")
RS_5_DepLoss.rse <- sqrt(diag(RS_5_DepLoss.cov))

TS_1_DepLoss.cov <- vcovHC(TS_1_DepLoss.reg, type = "HC")
TS_1_DepLoss.rse <- sqrt(diag(TS_1_DepLoss.cov))

TS_5_DepLoss.cov <- vcovHC(TS_5_DepLoss.reg, type = "HC")
TS_5_DepLoss.rse <- sqrt(diag(TS_5_DepLoss.cov))

# Computing Variance Inflation Factors
RS_1_DepLoss.vif  <- vif(RS_1_DepLoss.reg)
RS_5_DepLoss.vif  <- vif(RS_5_DepLoss.reg)
TS_1_DepLoss.vif  <- vif(TS_1_DepLoss.reg)
TS_5_DepLoss.vif  <- vif(TS_5_DepLoss.reg)

#-----------------------------------------------------------------------
# Dependent variable: Change in asset price
#-----------------------------------------------------------------------

# Random shock
## 1 bank
RS_1_dAP.reg <- betareg(d_assetprice_abs
                  ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                  + Init_Capital + Init_IB_exp + Init_Assets
                  + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB
                  + Init_capital_SB + Init_IBB_SB + Init_IBL_SB  + Shock_size,
                  data = dataset_RS_1_LRon)
## 5 banks
RS_5_dAP.reg <- betareg(d_assetprice_abs
                  ~ Density + Diameter +  Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                  + Init_Capital + Init_IB_exp + Init_Assets
                  + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB
                  + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                  data = dataset_RS_5_LRon)

# Targeted shock
## 1 bank
TS_1_dAP.reg <- betareg(d_assetprice_abs
                  ~ Density + Diameter + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                  + Init_Capital + Init_IB_exp + Init_Assets
                  + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB
                  + Init_capital_SB + Init_IBB_SB + Init_IBL_SB  + Shock_size,
                  data = dataset_TS_1_LRon)
## 5 banks
TS_5_dAP.reg <- lm(d_assetprice_abs
                  ~ Density + Diameter +  Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                  + Init_Capital + Init_IB_exp + Init_Assets
                  + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB 
                  + Init_capital_SB + Init_IBB_SB + Init_IBL_SB + Shock_size,
                  data = dataset_TS_5_LRon)

# Computing Variance Inflation Factors
RS_1_dAP.vif  <- vif(RS_1_dAP.reg)
RS_5_dAP.vif  <- vif(RS_5_dAP.reg)
TS_1_dAP.vif  <- vif(TS_1_dAP.reg)
TS_5_dAP.vif  <- vif(TS_5_dAP.reg)

########################################################################
# Outputting regression tables
########################################################################

CovLabels =  c("$DSTY$","$DIAM$","$APL$","$ACC$","$RPTY$","$ASTY^{oi}$","$ASTY^{io}$","$ASTY^{oo}$","$ASTY^{ii}$",           # Network
               "$K^{0}$","$L^{0}$","$A^{0}$",                                              # Initial aggregate balance sheet
               "$INDEG^{S}$","$OUTDEG^{S}$","$INCLOSE^{S}$","$OUTCLOSE^{S}$","$BTWN^{S}$","$PAGERANK^{S}$",
               "$k^{S}$","$b^{S}$","$l^{S}$","$S$")  # Shock properties 

#-----------------------------------------------------------------------
# DV = Number of failed banks
#-----------------------------------------------------------------------

## Regression tables
`textit{1 bank}`  = RS_1_NumFail.reg; `textit{5 banks}` = RS_5_NumFail.reg
`Textit{1 bank}`  = TS_1_NumFail.reg; `Textit{5 banks}` = TS_5_NumFail.reg

stargazer(`textit{1 bank}`,`textit{5 banks}`,`Textit{1 bank}`,`Textit{5 banks}`,object.names = TRUE,
          title = "Regression results - Number of failed banks (liquidity effects on)",
          align = TRUE, report = "vc*", se = list(RS_1_NumFail.rse,RS_5_NumFail.rse,TS_1_NumFail.rse,TS_5_NumFail.rse),
          column.labels = c("\\textbf{Random shock}","\\textbf{Targeted shock}"), column.separate = c(2, 2),
          dep.var.labels.include = FALSE, dep.var.caption = "",
          covariate.labels = CovLabels, model.numbers = FALSE,
          keep.stat=c("rsq"), no.space=TRUE,font.size = "footnotesize",table.placement ="H",label = "results_LRon_Numfail",
          notes = "\\parbox[t]{10cm}{${}^{\\star}p<10\\%$;${}^{\\star\\star}p<5\\%$;${}^{\\star\\star\\star}p<1\\%$.Significance based on robust standard errors.}", notes.append = FALSE, notes.align = "l",
          out=paste(tableoutput,"RegResults_LRon_Numfail.tex",sep=""))

## VIFs
sink("VIFtable_LRon_Numfail.tex")
  kable(cbind(RS_1_NumFail.vif,RS_5_NumFail.vif,TS_1_NumFail.vif,TS_5_NumFail.vif), booktabs = T, digits = 3,linesep = "",col.names = c("textit{1 bank} ","textit{5 banks}","textit{1 bank}","textit{5 banks}"),
  caption ="Variance Inflation Factors for $NUMFAIL$ regression (liquidity effects on)") %>%
  kable_styling(font_size = 7,latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" ", "textbf{Random shock}" = 2, "textbf{Targeted shock}" = 2))
sink()

#-----------------------------------------------------------------------
# Dependent variable: Number of failed banks (rounded + Poisson regression for count data)
#-----------------------------------------------------------------------

## Regression tables
`textit{1 bank}`  = RS_1_NumFail_Poiss.reg; `textit{5 banks}` = RS_5_NumFail_Poiss.reg
`Textit{1 bank}`  = TS_1_NumFail_Poiss.reg; `Textit{5 banks}` = TS_5_NumFail_Poiss.reg

stargazer(`textit{1 bank}`,`textit{5 banks}`,`Textit{1 bank}`,`Textit{5 banks}`,object.names = TRUE,
          title = "Regression results - Number of failed banks (liquidity effects on)",
          align = TRUE, report = "vc*",se = list(RS_1_NumFail_Poiss.rse,RS_5_NumFail_Poiss.rse,TS_1_NumFail_Poiss.rse,TS_5_NumFail_Poiss.rse),
          column.labels = c("\\textbf{Random shock}","\\textbf{Targeted shock}"), column.separate = c(2, 2),
          dep.var.labels.include = FALSE, dep.var.caption = "",
          covariate.labels = CovLabels, model.numbers = FALSE,
          keep.stat=c("rsq"), no.space=TRUE,font.size = "footnotesize",table.placement ="H",label = "results_LRon_Numfail_Poiss",
          notes = "\\parbox[t]{10cm}{${}^{\\star}p<10\\%$;${}^{\\star\\star}p<5\\%$;${}^{\\star\\star\\star}p<1\\%$. Significance based on robust standard errors. Dependent variable $NUMFAIL$ rounded to recreate a count variable on which a Poisson regression is run.}", notes.append = FALSE, notes.align = "l",
          out=paste(tableoutput,"RegResults_LRon_Numfail_Poiss.tex",sep=""))

## VIFs
sink("VIFtable_LRon_NumFail_Poiss.tex")
  kable(cbind(RS_1_NumFail_Poiss.vif,RS_5_NumFail_Poiss.vif,TS_1_NumFail_Poiss.vif,TS_5_NumFail_Poiss.vif), booktabs = T,linesep = "",digits = 3,col.names = c("textit{1 bank} ","textit{5 banks}","textit{1 bank}","textit{5 banks}"),
  caption ="Variance Inflation Factors for $NUMFAIL$ regression (liquidity effects on)") %>%
  kable_styling(font_size = 7,latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" ", "textbf{Random shock}" = 2, "textbf{Targeted shock}" = 2))
sink()

#-----------------------------------------------------------------------
# DV = Total capital loss
#-----------------------------------------------------------------------

## Regression tables
`textit{1 bank}`  = RS_1_CapLoss.reg; `textit{5 banks}` = RS_5_CapLoss.reg
`Textit{1 bank}`  = TS_1_CapLoss.reg; `Textit{5 banks}` = TS_5_CapLoss.reg

stargazer(`textit{1 bank}`,`textit{5 banks}`,`Textit{1 bank}`,`Textit{5 banks}`,object.names = TRUE,
          title = "Regression results - Cascade size (liquidity effects on)",
          align = TRUE, report = "vc*", se=list(RS_1_CapLoss.rse,RS_5_CapLoss.rse,TS_1_CapLoss.rse,TS_5_CapLoss.rse),
          column.labels = c("\\textbf{Random shock}","\\textbf{Targeted shock}"), column.separate = c(2, 2),
          dep.var.labels.include = FALSE, dep.var.caption = "",
          covariate.labels = CovLabels, model.numbers = FALSE,
          omit.stat=c("LL","ser","n","rsq"), no.space=TRUE,font.size = "footnotesize",table.placement ="H",label = "results_LRon_Cascsize",
          notes = "\\parbox[t]{10cm}{${}^{\\star}p<10\\%$;${}^{\\star\\star}p<5\\%$;${}^{\\star\\star\\star}p<1\\%$. Significance based on robust standard errors.}", notes.append = FALSE, notes.align = "l",
          out=paste(tableoutput,"RegResults_LRon_Cascsize.tex",sep=""))

## VIFs
sink("VIFtable_LRon_Cascsize.tex")
  kable(cbind(RS_1_CapLoss.vif,RS_5_CapLoss.vif,TS_1_CapLoss.vif,TS_5_CapLoss.vif), booktabs = T, digits = 3,linesep = "",col.names = c("textit{1 bank} ","textit{5 banks}","textit{1 bank}","textit{5 banks}"),
  caption ="Variance Inflation Factors for $CASCSIZE$ regression (liquidity effects on)") %>%
  kable_styling(font_size = 7,latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" ", "textbf{Random shock}" = 2, "textbf{Targeted shock}" = 2))
sink()

#-----------------------------------------------------------------------
# DV = Total deposit loss
#-----------------------------------------------------------------------

## Regression tables
`textit{1 bank}`  = RS_1_DepLoss.reg; `textit{5 banks}` = RS_5_DepLoss.reg
`Textit{1 bank}`  = TS_1_DepLoss.reg; `Textit{5 banks}` = TS_5_DepLoss.reg

stargazer(`textit{1 bank}`,`textit{5 banks}`,`Textit{1 bank}`,`Textit{5 banks}`,object.names = TRUE,
          title = "Regression results - Deposit loss (liquidity effects on)",
          align = TRUE, report = "vc*", se=list(RS_1_DepLoss.rse,RS_5_DepLoss.rse,TS_1_DepLoss.rse,TS_5_DepLoss.rse),
          column.labels = c("\\textbf{Random shock}","\\textbf{Targeted shock}"), column.separate = c(2, 2),
          dep.var.labels.include = FALSE, dep.var.caption = "",
          covariate.labels = CovLabels, model.numbers = FALSE,
          keep.stat=c("rsq"), no.space=TRUE,font.size = "footnotesize",table.placement ="H",label = "results_LRon_Deploss",
          notes = "\\parbox[t]{10cm}{${}^{\\star}p<10\\%$;${}^{\\star\\star}p<5\\%$;${}^{\\star\\star\\star}p<1\\%$. Significance based on robust standard errors.}", notes.append = FALSE, notes.align = "l",
          out=paste(tableoutput,"RegResults_LRon_Deploss.tex",sep=""))

## VIFs
sink("VIFtable_LRon_Deploss.tex")
kable(cbind(RS_1_DepLoss.vif,RS_5_DepLoss.vif,TS_1_DepLoss.vif,TS_5_DepLoss.vif), booktabs = T, digits = 3,linesep = "",col.names = c("textit{1 bank} ","textit{5 banks}","textit{1 bank}","textit{5 banks}"),
      caption ="Variance Inflation Factors for $DEPOSITLOSS$ regression (liquidity effects on)") %>%
  kable_styling(font_size = 7,latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" ", "textbf{Random shock}" = 2, "textbf{Targeted shock}" = 2))
sink()

#-----------------------------------------------------------------------
# DV = Change in asset price
#-----------------------------------------------------------------------

## Regression tables
`textit{1 bank}`  = RS_1_dAP.reg; `textit{5 banks}` = RS_5_dAP.reg
`Textit{1 bank}`  = TS_1_dAP.reg; `Textit{5 banks}` = TS_5_dAP.reg

stargazer(`textit{1 bank}`,`textit{5 banks}`,`Textit{1 bank}`,`Textit{5 banks}`,object.names = TRUE,
          title = "Regression results - Change in asset price (liquidity effects on)",
          align = TRUE, report = "vc*",
          column.labels = c("\\textbf{Random shock}","\\textbf{Targeted shock}"), column.separate = c(2, 2),
          dep.var.labels.include = FALSE, dep.var.caption = "",
          covariate.labels = CovLabels, model.numbers = FALSE,
          keep.stat=c("rsq"), no.space=TRUE,font.size = "footnotesize",table.placement ="H",label = "results_LRon_dAP",
          notes = "\\parbox[t]{10cm}{${}^{\\star}p<10\\%$;${}^{\\star\\star}p<5\\%$;${}^{\\star\\star\\star}p<1\\%$. Significance based on robust standard errors.}", notes.append = FALSE, notes.align = "l",
          out=paste(tableoutput,"RegResults_LRon_dAP.tex",sep=""))

## VIFs
sink("VIFtable_LRon_dAP.tex")
kable(cbind(RS_1_dAP.vif,RS_5_dAP.vif,TS_1_dAP.vif,TS_5_dAP.vif), booktabs = T, digits = 3,linesep = "",col.names = c("textit{1 bank} ","textit{5 banks}","textit{1 bank}","textit{5 banks}"),
      caption ="Variance Inflation Factors for $Delta ASSETPRICE$ regression (liquidity effects on)") %>%
  kable_styling(font_size = 7,latex_options = c("striped", "hold_position")) %>%
  add_header_above(c(" ", "textbf{Random shock}" = 2, "textbf{Targeted shock}" = 2))
sink()
