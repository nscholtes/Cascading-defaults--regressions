########################################################################
########################################################################
# R Code for running network level regressions based on cascading
# default simulations run in MATLAB

# Current script: Targeted shocks, liquidity effects ON

# Copyright Nicolas K. Scholtes, 2017
########################################################################
########################################################################

library(stargazer)
library(sandwich)
library(dplyr)

########################################################################
# Input datasets created in MATLAB
########################################################################
setwd("/Users/nscholte/Desktop/Research/Ch.3 - Systemic risk/R Codes")
tableoutput <- file.path("/Users", "nscholte", "Desktop","Research", "Ch.3 - Systemic risk", "Drafts", "Tables", "Targeted", "LRon/")
file.exists(tableoutput)

# Targeted shock datasets
## 1 shocked bank
dataset_TS_1_LRon  <- read.table("data_19-Sep-2017_TS_1_LRon.csv",header = TRUE,sep = ",")
## 5 shocked banks
dataset_TS_5_LRon  <- read.table("data_19-Sep-2017_TS_5_LRon.csv",header = TRUE,sep = ",")
## 10 shocked banks
dataset_TS_10_LRon  <- read.table("data_19-Sep-2017_TS_10_LRon.csv",header = TRUE,sep = ",")

########################################################################
# Create new variables
########################################################################  

# Diffusion variable = cascade size/total initial shock
dataset_TS_1_LRon$Diffusion  <- dataset_TS_1_LRon$TotCapitalLoss/dataset_TS_1_LRon$Shock_size
dataset_TS_5_LRon$Diffusion  <- dataset_TS_5_LRon$TotCapitalLoss/dataset_TS_5_LRon$Shock_size
dataset_TS_10_LRon$Diffusion <- dataset_TS_10_LRon$TotCapitalLoss/dataset_TS_10_LRon$Shock_size

# Reorder dataset
dataset_TS_1_LRon <- dplyr::select(dataset_TS_1_LRon, Diffusion, everything())
dataset_TS_5_LRon <- dplyr::select(dataset_TS_5_LRon, Diffusion, everything())
dataset_TS_10_LRon <- dplyr::select(dataset_TS_10_LRon, Diffusion, everything())

########################################################################
# Descriptive statistics
########################################################################  

cols_cascade <- c(2,4,6) #  Cascade output (Number of failed banks, Cascade size, simulation time, asset price change)
cols_shock   <- c(22:27)   # Shocked banks: various (local) centrality measures

# Cascade dynamics across different shock specifications

# 1 bank
stargazer(dataset_TS_1_LRon[,cols_cascade],nobs = FALSE,
          title="Summary statistics - Model dynamics (Targeted shock, liquidity effects on)",
          font.size = "footnotesize",table.placement ="h!",no.space=TRUE,label="Summstats_casc_TS_1_LRon",
          covariate.labels=c("NumFail","CascSize","SimTime"),
          out=paste(tableoutput,"Summstats_casc_TS_1_LRon.tex",sep=""))

# 5 banks
stargazer(dataset_TS_5_LRon[,cols_cascade],nobs = FALSE,
          font.size = "footnotesize",table.placement ="h!",no.space=TRUE,
          covariate.labels=c("NumFail","CascSize","SimTime"),
          out=paste(tableoutput,"Summstats_casc_TS_5_LRon.tex",sep=""))

# 10 banks
stargazer(dataset_TS_10_LRon[,cols_cascade],nobs = FALSE,
          font.size = "footnotesize",table.placement ="h!",no.space=TRUE,
          covariate.labels=c("NumFail","CascSize","SimTime"),
          out=paste(tableoutput,"Summstats_casc_TS_10_LRon.tex",sep=""))

# N.B. Combine tables in LaTeX using Master File: Summstats_casc_TS_LRon.tex

########################################################################
# Correlation matrices - 5 banks
########################################################################   

## Cascade output
correlationmatrix.cascade <- cor(dataset_TS_5_LRon[,cols_cascade])
round(correlationmatrix.cascade, 2)
stargazer(correlationmatrix.cascade, title="Correlation Matrix - Cascade properties (Targeted shock to 5 banks, liquidity effects on)",
          font.size = "footnotesize",table.placement ="h!",no.space=TRUE,label="corrmat_casc_TS_5_LRon",
          out=paste(tableoutput,"Corrmat_casc_TS_5_LRon.tex",sep=""))

## Shocked bank centralities
correlationmatrix.shock <- cor(dataset_TS_5_LRon[,cols_shock])
round(correlationmatrix.shock, 2)
stargazer(correlationmatrix.shock, title="Correlation Matrix - Shocked bank centralities (Targeted shock to 5 banks, liquidity effects on)",
          font.size = "footnotesize",table.placement ="h!",no.space=TRUE,label="corrmat_casc_TS_5_LRon",
          out=paste(tableoutput,"Corrmat_shock_TS_5_LRon.tex",sep=""))  

########################################################################
# Running various regressions
########################################################################

# Explanatory variables in regression tables take the form of:
### 1st Row: Network variables
### 2nd Row: Balance sheet variables (initial and aggregate)
### 3rd Row: Shock variables (centrality of shocked nodes and size of shock)

## 1.1. 1 bank
### 1.1.1. DV = Total capital loss
TS_1_DV_CapLoss <- lm(-TotCapitalLoss
                      ~ Density + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                      + Init_Capital + Init_IB_exp + Init_Assets
                      + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB + Shock_size,
                      data = dataset_TS_1_LRon)

### 1.1.2. DV = Number of failed banks
TS_1_DV_NumFail <- lm(NumFailedBanks 
                      ~ Density + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                      + Init_Capital + Init_IB_exp + Init_Assets
                      + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB + Shock_size,
                      data = dataset_TS_1_LRon)

### 1.1.3. DV = Shock diffusion
TS_1_DV_Diffusion <- lm(-Diffusion
                        ~ Density + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                        + Norm_Init_Capital + Norm_Init_IB_exp
                        + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB,
                        data = dataset_TS_1_LRon)

### 1.1.4. DV = Change in asset price
TS_1_DV_dAssetPrice <- lm(-d_assetprice_abs
                          ~ Density + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                          + Norm_Init_Capital + Norm_Init_IB_exp
                          + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB,
                          data = dataset_TS_1_LRon)

#-----------------------------------------------------------------------
## 1.2. 5 banks
### 1.2.1. DV = Total capital loss
TS_5_DV_CapLoss <- lm(-TotCapitalLoss
                      ~ Density + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                      + Init_Capital + Init_IB_exp + Init_Assets
                      + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB + Shock_size,
                      data = dataset_TS_5_LRon)

### 1.2.2. DV = Number of failed banks
TS_5_DV_NumFail <- lm(NumFailedBanks 
                      ~ Density + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                      + Init_Capital + Init_IB_exp + Init_Assets
                      + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB + Shock_size,
                      data = dataset_TS_5_LRon)

### 1.2.3. DV = Shock diffusion
TS_5_DV_Diffusion <- lm(-Diffusion
                        ~ Density + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                        + Norm_Init_Capital + Norm_Init_IB_exp
                        + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB,
                        data = dataset_TS_5_LRon)

### 1.2.4. DV = Change in asset price
TS_5_DV_dAssetPrice <- lm(-d_assetprice_abs
                          ~ Density + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                          + Norm_Init_Capital + Norm_Init_IB_exp
                          + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB,
                          data = dataset_TS_5_LRon)
#-----------------------------------------------------------------------
## 1.3 10 banks
### 1.3.1. DV = Total capital loss
TS_10_DV_CapLoss <- lm(-TotCapitalLoss
                       ~ Density + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Init_Capital + Init_IB_exp + Init_Assets
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB + Shock_size,
                       data = dataset_TS_10_LRon)

### 1.3.2. DV = Number of failed banks
TS_10_DV_NumFail <- lm(NumFailedBanks 
                       ~ Density + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                       + Init_Capital + Init_IB_exp + Init_Assets
                       + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB + Shock_size,
                       data = dataset_TS_10_LRon)

### 1.1.3. DV = Shock diffusion
TS_10_DV_Diffusion <- lm(-Diffusion
                         ~ Density + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                         + Norm_Init_Capital + Norm_Init_IB_exp
                         + Indeg_cent_SB + Outdeg_cent_SB + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB,
                         data = dataset_TS_10_LRon)

### 1.1.4. DV = Change in asset price
TS_10_DV_dAssetPrice <- lm(-d_assetprice_abs
                           ~ Density + Average_Path_Length + Average_Clustering + Reciprocity + OutIn_Assortativity + InOut_Assortativity + OutOut_Assortativity + InIn_Assortativity
                           + Norm_Init_Capital + Norm_Init_IB_exp
                           + Indeg_cent_SB + Outdeg_cent_SB + Inclose_cent_SB + Outclose_cent_SB  + Btwn_cent_SB + Pagerank_cent_SB,
                           data = dataset_TS_10_LRon)

#-----------------------------------------------------------------------

CovLabels.Abs =  c("$DSTY$","$APL$","$ACC$","$RPTY$","$ASTY^{oi}$","$ASTY^{io}$","$ASTY^{oo}$","$ASTY^{ii}$",               # Network
                   "$K^{0}$","$L^{0}$","$A^{0}$",                                               # Initial aggregate balance sheet
                   "$INDEG^{S}$","$OUTDEG^{S}$","$OUTCLOSE^{S}$","$INCLOSE^{S}$","$BTWN^{S}$","$PAGERANK^{S}$","$SHOCKSIZE$")  # Shock properties 

CovLabels.Ratios = c("$DSTY$","$APL$","$ACC$","$RPTY$","$ASTY^{oi}$","$ASTY^{io}$","$ASTY^{oo}$","$ASTY^{ii}$",    # Network
                     "$\\hat{K}^{0}$","$\\hat{L}^{0}$",                                                              # Initial normalised balance sheet
                     "$INDEG^{S}$","$OUTDEG^{S}$","$OUTCLOSE^{S}$","$INCLOSE^{S}$","$BTWN^{S}$","$PAGERANK^{S}$")  # Shock properties 

## 1.4. Output to LaTeX
### 1.4.1. DV = Total capital loss
stargazer(TS_1_DV_CapLoss,TS_5_DV_CapLoss,TS_10_DV_CapLoss,
          title = "Regression results - Targeted shock (liquidity effects on)",
          align = TRUE,report = "vc*",
          column.labels = c("1 bank", "5 banks","10 banks"),dep.var.labels   = "$CASCSIZE$",
          covariate.labels = CovLabels.Abs,
          omit.stat=c("LL","ser"), no.space=TRUE,font.size = "footnotesize",table.placement ="h!",label = "results_TS_LRon_Cascsize",
          out=paste(tableoutput,"RegResults_TS_LRon_Cascsize.tex",sep=""))

### 1.4.2. DV = Number of failed banks
stargazer(TS_1_DV_NumFail,TS_5_DV_NumFail,TS_10_DV_NumFail,
          title = "Regression results - Targeted shock (liquidity effects on)",
          align = TRUE, report = "vc*",
          column.labels = c("1 bank", "5 banks","10 banks"),dep.var.labels   = "$NUMFAIL$",
          covariate.labels = CovLabels.Abs,
          omit.stat=c("LL","ser"), no.space=TRUE,font.size = "footnotesize",table.placement ="h!",label = "results_TS_LRon_Numfail",
          out=paste(tableoutput,"RegResults_TS_LRon_Numfail.tex",sep=""))

### 1.4.3. DV = Shock diffusion
stargazer(TS_1_DV_Diffusion,TS_5_DV_Diffusion,TS_10_DV_Diffusion,
          title = "Regression results - Targeted shock (liquidity effects on)",
          align = TRUE, report = "vc*",
          column.labels = c("1 bank", "5 banks","10 banks"),dep.var.labels   = "$DIFFUSION$",
          covariate.labels = CovLabels.Ratios,
          omit.stat=c("LL","ser"), no.space=TRUE,font.size = "footnotesize",table.placement ="h!",label = "results_TS_LRon_Diffusion",
          out=paste(tableoutput,"RegResults_TS_LRon_Diffusion.tex",sep=""))

### 1.4.4. DV = Change in asset price
stargazer(TS_1_DV_dAssetPrice,TS_5_DV_dAssetPrice,TS_10_DV_dAssetPrice,
          title = "Regression results - Targeted shock (liquidity effects on)",
          align = TRUE, report = "vc*",
          column.labels = c("1 bank", "5 banks","10 banks"), dep.var.labels   = "$\\Delta ASSETPRICE$",
          covariate.labels = CovLabels.Ratios,
          omit.stat=c("LL","ser"), no.space=TRUE,font.size = "footnotesize",table.placement ="h!",label = "results_TS_LRon_dAssetPrice",
          out=paste(tableoutput,"RegResults_TS_LRon_dAssetPrice.tex",sep=""))

