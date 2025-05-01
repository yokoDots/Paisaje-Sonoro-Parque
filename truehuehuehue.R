
#' #dom.freq60()->dom.freq60min #The vector "Fdom.freq" call your result
#' #dom.freq60min #Call the vector, and see your results.        wav<-readWave(fil[1])

library(tidyverse)
library(tidyr)
library(tuneR)
library(seewave)
library(soundecology)
library(dplyr)
library(reshape)
library(lubridate)
library(data.table)
library(ggplot2)

  files <- list.files(pattern = "wav$", ignore.case = F)
# for(j in 1:length(files)){
# 
#     for(i in 1:5){
#       wav <- readWave(files[j], units = "seconds", from = 12*(i-1), to= 12*i )}}
 
 
  #soundindex1 <- function(){
  data(tico)
  M(readWave(files[1]), msmooth=c(500,50), plot=TRUE)
  
    df <- data.frame()
    
    for(j in 1:length(files)){
      for(i in 1:10){
        wav <- readWave(files[j], units = "minutes", 
                        from = i-1, to= i )

        x <-soundscapespec(wav, plot=FALSE) 
        powmean<-mean(x[,2])

        #spect<-seewave::spec(wav, plot=FALSE)
        envwav<-seewave::env(wav, plot=FALSE)
        #peaks<-fpeaks(spect, plot=F)
        
        AEI <- acoustic_evenness(wav, max_freq = 16000)
        AEI.L <- AEI$aei_left

        ADI <- acoustic_diversity(wav, max_freq = 16000)
        ADI.L <- ADI$adi_left

        ACIsee <-ACI(wav, flim=c(1,16)) #Acoustic Complexity Index #{seewave}
        
        BIO <- bioacoustic_index(wav,min_freq = 1200, max_freq = 16000)
        BIO.L <-BIO$left_area
        
        NDSIsee<-NDSI(x, max = TRUE)
        TFSDsee<-TFSD(wav)
        He<-H(wav) 
        Ht<-th(envwav) 
        
        MAE<-M(wav) #Median of amplitude envelope #{seewave}
        
        #doms<-dfreq(wav, bandpass = c(800, 12000)) #returns time and freq
        
        #NP<-length(peaks)/2
        # hf<-str_sub(files[j], 7,11)
        # 
        # hf=parse_date_time(hf,"HM")
        # tt = hf+60*(i-1)
        # tt=format(tt, format="%H:%M")
        #tt=hm(tt)
        
       # tnum=as.numeric(tt, "hours")

        z <- list(
                  AEI = AEI.L,
                  ADI= ADI.L,
                  ACIsee=ACIsee,
                  BIO =BIO.L,
                  NDSIsee=NDSIsee,
                  He=He,
                  Ht=Ht,
                  MAE=MAE,
                  #NP=NP,
                  TFSDsee=TFSDsee,
                  #Technophony=Technophony, 
                  #BIOAC=Bioc, TB=TB
                 # Hora=tt,
                  Amp=powmean
                  )

         df <- rbind(df, data.frame(z, 
              row.names = make.names((files[j])) ) )
      rm(wav)
      
        }
    }


        # return(df)
   df
   str(df)
   dat=data.frame()
   
   df1=data.frame()
   df1=df
   df1$Hora = hm(df1$Hora)
   #df1$hnum = aiFactors$hnum[1:240]
   df1$Hora=as.numeric(df1$Hora, "hours")
   df1
   Col <- colorRampPalette(c("green", "magenta", "gray4"))
   colors=c(colfunc(5))
   colors
   dat  <-data.frame( Col(15)[as.numeric(cut(df1$Amp,breaks = 10))])
   #for (i in 1:ncol(df1))
   for (i in 1:9) {

   plot(df1$hnum, (df1[,i] ),
        #ylim=c(-1.0, 1.0),
        xlab="Horas", ylab= names(df)[i],
        col=(dat$Col), cex=1)
   }
   

   
   write.csv(aiFactors, "FactorsAI.csv" )   
