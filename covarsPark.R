library(ggpubr)
dall<-data.frame(All1min)
#### ACIsee  ######
ggscatter(dall, x = "ACIsee", y = "NDSIsee", size = 0.5,
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ACIsee",
          ylab = "NDSI")


cAm<-cor.test(dall$ACIsee, dall$SPLmean,
              method = "spearman", exact=FALSE)
cAm #no corr
cA50<-cor.test(dall$ACIsee, dall$SPL50,
               method = "spearman", exact=FALSE)
cA50 #no cor 
cAMx<-cor.test(dall$ACIsee, dall$SPLMax,
               method = "spearman", exact=FALSE)
cAMx #very weak neg

cAMn<-cor.test(dall$ACIsee, dall$SPLMin,
               method = "spearman", exact=FALSE)
cAMn #no cro

#with other indexes
cAN<-cor.test(dall$ACIsee, dall$NDSIsee,
              method = "spearman", exact=FALSE)
cAN #weak
cAT<-cor.test(dall$ACIsee, dall$TFSDsee,
              method = "spearman", exact=FALSE)
cAT #weak
cAA<-cor.test(dall$ACIsee, dall$Amp,
              method = "spearman", exact=FALSE)
cAA #weak

#### ADI ####
ggscatter(dall, x = "ADI", y = "NDSIsee", size = 0.5,
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ADI",
          ylab = "NDSI")


cDm<-cor.test(dall$ADI, dall$SPLmean,
            method = "spearman", exact=FALSE)
cDm #weak
cD50<-cor.test(dall$ADI, dall$SPL50,
               method = "spearman", exact=FALSE)
cD50 #moderate
cDMx<-cor.test(dall$ADI, dall$SPLMax,
              method = "spearman", exact=FALSE)
cDMx #moderate negative

cDMn<-cor.test(dall$ADI, dall$SPLMin,
               method = "spearman", exact=FALSE)
cDMn #no cor

#with other indexes
cDN<-cor.test(dall$ADI, dall$NDSIsee,
               method = "spearman", exact=FALSE)
cDN #moderate
cDT<-cor.test(dall$ADI, dall$TFSDsee,
              method = "spearman", exact=FALSE)
cDT #moderate
cDA<-cor.test(dall$ADI, dall$Amp,
              method = "spearman", exact=FALSE)
cDA #moderate

##### AEI ####
ggscatter(dall, x = "AEI", y = "NDSIsee", size = 0.5,
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "AEI",
          ylab = "NDSI")


cEm<-cor.test(dall$AEI, dall$SPLmean,
              method = "spearman", exact=FALSE)
cEm #weak
cE50<-cor.test(dall$AEI, dall$SPL50,
               method = "spearman", exact=FALSE)
cE50 #weak
cEMx<-cor.test(dall$AEI, dall$SPLMax,
               method = "spearman", exact=FALSE)
cEMx #moderate 

cEMn<-cor.test(dall$AEI, dall$SPLMin,
               method = "spearman", exact=FALSE)
cEMn #very weak

#with other indexes
cEN<-cor.test(dall$AEI, dall$NDSIsee,
              method = "spearman", exact=FALSE)
cEN #moderate negative
cET<-cor.test(dall$AEI, dall$TFSDsee,
              method = "spearman", exact=FALSE)
cET #strong negative
cEA<-cor.test(dall$AEI, dall$Amp,
              method = "spearman", exact=FALSE)
cEA #moderate negative

#### BIO #####
ggscatter(dall, x = "BIO", y = "NDSIsee", size = 0.5,
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "BIO",
          ylab = "NDSI")


cBm<-cor.test(dall$BIO, dall$SPLmean,
              method = "spearman", exact=FALSE)
cBm #VERY weak NEG
cB50<-cor.test(dall$BIO, dall$SPL50,
               method = "spearman", exact=FALSE)
cB50 #weak
cBMx<-cor.test(dall$BIO, dall$SPLMax,
               method = "spearman", exact=FALSE)
cBMx #weak negative

cBMn<-cor.test(dall$BIO, dall$SPLMin,
               method = "spearman", exact=FALSE)
cBMn #no cor

#with other indexes
cBN<-cor.test(dall$BIO, dall$NDSIsee, size = 0.5,
              method = "spearman", exact=FALSE)
cBN #moderate
cBT<-cor.test(dall$BIO, dall$TFSDsee,
              method = "spearman", exact=FALSE)
cBT #moderate
cBA<-cor.test(dall$BIO, dall$Amp,
              method = "spearman", exact=FALSE)
cBA #weak

#### H ####
ggscatter(dall, x = "He", y = "NDSIsee", size = 0.5,
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "He",
          ylab = "NDSI")


cHm<-cor.test(dall$He, dall$SPLmean,
              method = "spearman", exact=FALSE)
cHm #weak
cH50<-cor.test(dall$He, dall$SPL50,
               method = "spearman", exact=FALSE)
cH50 #weak
cHMx<-cor.test(dall$He, dall$SPLMax,
               method = "spearman", exact=FALSE)
cHMx #strong negative

cHMn<-cor.test(dall$He, dall$SPLMin,
               method = "spearman", exact=FALSE)
cHMn #no cor

#with other indexes
cHN<-cor.test(dall$He, dall$NDSIsee,
              method = "spearman", exact=FALSE)
cHN #strong
cHT<-cor.test(dall$He, dall$TFSDsee,
              method = "spearman", exact=FALSE)
cHT #strong
cHA<-cor.test(dall$He, dall$Amp,
              method = "spearman", exact=FALSE)
cHA #Strong

#### MAE ####

ggscatter(dall, x = "MAE", y = "NDSIsee", size = 0.5,
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MAE",
          ylab = "NDSI")


cMm<-cor.test(dall$MAE, dall$SPLmean,
              method = "spearman", exact=FALSE)
cMm #strong
cM50<-cor.test(dall$MAE, dall$SPL50,
               method = "spearman", exact=FALSE)
cM50 #strong
cMMx<-cor.test(dall$MAE, dall$SPLMax,
               method = "spearman", exact=FALSE)
cMMx #strong

cMMn<-cor.test(dall$MAE, dall$SPLMin,
               method = "spearman", exact=FALSE)
cMMn #no cor

#with other indexes
cMN<-cor.test(dall$MAE, dall$NDSIsee,
              method = "spearman", exact=FALSE)
cMN #weak neg
cMT<-cor.test(dall$MAE, dall$TFSDsee,
              method = "spearman", exact=FALSE)
cMT #moderate neg
cMA<-cor.test(dall$MAE, dall$Amp,
              method = "spearman", exact=FALSE)
cMA #weak neg

#### AMP ####
ggscatter(dall, x = "Amp", y = "NDSIsee", size = 0.5,
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Amp",
          ylab = "NDSI")


CAmm<-cor.test(dall$Amp, dall$SPLmean,
              method = "spearman", exact=FALSE)
CAmm #weak
CAm50<-cor.test(dall$Amp, dall$SPL50,
               method = "spearman", exact=FALSE)
CAm50 #weak
CAmMx<-cor.test(dall$Amp, dall$SPLMax,
               method = "spearman", exact=FALSE)
CAmMx #weak negative

CAmMn<-cor.test(dall$Amp, dall$SPLMin,
               method = "spearman", exact=FALSE)
CAmMn #weak


####### other correlations ########

cor.test(dall$hnum, dall$SPLMax, #moderate
         method = "spearman", exact=FALSE)
cor.test(dall$hnum, dall$SPLMin, #neg weak
         method = "spearman", exact=FALSE)
cor.test(dall$hnum, dall$SPL50, #no cor
         method = "spearman", exact=FALSE)
cor.test(dall$hnum, dall$SPLmean, #no cor 
         method = "spearman", exact=FALSE)


cor.test(dall$MAE, dall$He, #neg moderate
         method = "spearman", exact=FALSE)
cor.test(dall$ADI, dall$AEI, #very strong neg
         method = "spearman", exact=FALSE)
cor.test(dall$ADI, dall$He, #very strong 
         method = "spearman", exact=FALSE)
cor.test(dall$ACIsee, dall$BIO, #weak
         method = "spearman", exact=FALSE)
