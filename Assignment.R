install.packages("mice")
library(mice)

# data <- read.csv("Life_Expectancy_Data1.csv")
# attach(data)
# 
# head(data)
# dim(data)
# md.pattern(data)
# 
# 
# which(is.na(data))
# sum(is.na(data))
# 
# sapply(data, function(x) which(is.na(x)))
# numberOfNA = sapply(data, function(x) sum(is.na(x)))
# colNames = colnames(data)
# numberOfRow = dim(data)[01]
# ratio = numberOfNA/numberOfRow
# 
# filterdData = data
# for (i in 1:length(ratio)) {
#   if (ratio[i] > 0.5){
#     print(ratio[i])
#     filterdData = filterdData[,-i]
#   }
# }
# 
# 
# imputations <- mice(filterdData, method = "cart", m = 10, seed = 2019)
# 
# imputations$imp
# stripplot(imputations$data$SE.PRM.CUAT.ZS, method = 'jitter', pch = 1)
# 
# xyplot(imputations, FR.INR.RINR ~ SH.HIV.INCD | .imp, pch = 20, cex = 1.4)

install.packages("tidyverse")
install.packages("GGally")
install.packages("imputeTS")
install.packages("dplyr")

library(tidyverse)
library(GGally)
library(imputeTS)
library(dplyr)

# Accessing the Data set using read.csv()
exp_t=read.csv("Life_Expectancy_Data1.csv")
# Preview of the imported data set
view(exp_t)
print(dim(exp_t))
# Performing the Explanatory Analysis
is.null(exp_t)
is.na(exp_t)
colSums(is.na(exp_t))
df = summary(exp_t$SP.DYN.LE00.IN)
print(df)
par(2,2)
hist(exp_t$SP.DYN.LE00.IN)
hist(exp_t$EG.ELC.ACCS.ZS)
hist(exp_t$NY.ADJ.NNTY.KD.ZG)
hist(exp_t$SP.POP.GROW)
# Imputation of the NA values with Mean
exp_clean <- na_mean(exp_t)
# The data column 25 has only NA values and hence, this data column has been removed by using the below code
exp_new = subset(exp_clean, select = -c(EG.FEC.RNEW.ZS) )
# Checking again for any missing value
anyNA(exp_new)
summary(exp_new$SP.DYN.LE00.IN)
tapply(exp_new$SP.DYN.LE00.IN, exp_new$Continent, summary)
hist(exp_new$SP.DYN.LE00.IN)

md.pattern(exp_new)
dim(exp_new)

# Forward feature selection
model1<-lm(SP.DYN.LE00.IN~1,data=exp_new)
step1<-step(model1,scope=~EG.ELC.ACCS.ZS + NY.ADJ.NNTY.KD.ZG + NY.ADJ.NNTY.PC.KD.ZG + SH.HIV.INCD.14 + 
              SE.PRM.UNER + SE.PRM.CUAT.ZS + SE.TER.CUAT.BA.ZS + SP.DYN.IMRT.IN + SE.PRM.CMPT.ZS + 
              SE.ADT.LITR.ZS + FR.INR.RINR + SP.POP.GROW + EN.POP.DNST + SP.POP.TOTL +
              SH.XPD.CHEX.GD.ZS + SL.UEM.TOTL.NE.ZS + NY.GDP.MKTP.KD.ZG  + 
              SP.DYN.CBRT.IN  + SH.HIV.INCD + SH.H2O.SMDW.ZS + 
              SI.POV.LMIC + SE.COM.DURS,
            method='forward')
summary(step1)

# Backward feature selection
model2<-lm(SP.DYN.LE00.IN~.,data=exp_new)
step2<-step(model2,method="backward")
summary(step2)

# Correlation 
install.packages("corrplot")
library(corrplot)
install.packages("faraway")
library("faraway")
vif(step1)

X<-exp_new[,-4] 
exp_new.corr<-cor(cor(exp_new[, unlist(lapply(exp_new, is.numeric))]))
exp_new.corr
corrplot(exp_new.corr, lower.col = "black", number.cex = .7)

# Picking Smaller Model
model3<-lm(SP.DYN.LE00.IN~EG.ELC.ACCS.ZS +
             SE.PRM.CUAT.ZS +  SP.DYN.IMRT.IN +  
               EN.POP.DNST + 
             SH.XPD.CHEX.GD.ZS +  
             SP.DYN.CBRT.IN  + SH.HIV.INCD + SH.H2O.SMDW.ZS ,data=exp_new)
summary(model3)

anova(step1, model3)
