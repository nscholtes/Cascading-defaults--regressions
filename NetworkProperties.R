########################################################################
# 
########################################################################

library(stargazer)
library(sandwich)
library(PerformanceAnalytics)

setwd("/Users/nscholte/Desktop/Research/Ch.3 - Systemic risk/R Codes")
tableoutput <- file.path("/Users", "nscholte", "Desktop","Research", "Ch.3 - Systemic risk", "Drafts", "Tables/")
file.exists(tableoutput)

dataset_TS_1 <- read.table("data_09-Oct-2017_TS_1_LRoff.csv",header = TRUE,sep = ",")

########################################################################
# Network measures 
########################################################################

cols_network <- c(15:23)   # Global network measures

# Summary statistics - network measures
stargazer(dataset_TS_1[,cols_network],nobs = FALSE, title="Summary statistics - Global network measures",
          font.size = "footnotesize",table.placement ="H",no.space=TRUE,label="Summstats_network",
          covariate.labels = c("$DSTY$","$DIAM$","$RPTY$","$ASTY^{oi}$","$ASTY^{io}$","$ASTY^{oo}$","$ASTY^{ii}$","$APL$","$ACC$"),
          out=paste(tableoutput,"Summstats_network.tex",sep=""))

# Correlation matrix
## Global network measures
correlationmatrix.network <- cor(dataset_TS_1[,cols_network])
round(correlationmatrix.network, 2)
stargazer(correlationmatrix.network, title="Correlation Matrix - Global network measures",
          font.size = "footnotesize",table.placement ="H",no.space=TRUE,label="corrmat_network",
          out=paste(tableoutput,"corrmat_network.tex",sep=""))

# covariate.labels = c("$DSTY$","$RPTY$","$ASTY^{oi}$","$ASTY^{io}$","$ASTY^{oo}$","$ASTY^{ii}$","$APL$","$ACC$"),


# Correlation Plot
chart.Correlation(dataset_TS_1[,cols_network], histogram=TRUE, pch=19)
