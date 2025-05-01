
colfunc <- colorRampPalette(c("powderblue", "deeppink"))
colors=c(colfunc(5))
cn=0
#soundindex1 <- function(){
  minutos<-seq(1:10)
  
  df <- data.frame()
  list_AR=list()
  list_NP=data.frame(NP=as.numeric())
  list_ACI=data.frame(ACI_1=as.numeric(), ACI_2=as.numeric())
  names(list_ACI)[2]="Horas"
  dirs<-list.dirs()
  #files <- list.files(pattern = "wav$", ignore.case = T )
  rm(ACI)
  dirs
  setwd("/home/yc/R/Projects/Horas-parque_500/")
  getwd()
  for (i in 2:length(dirs)) {
    d=str_sub(dirs[i], 2)
    d=paste("/home/yc/R/Projects/Horas-parque_500",d, sep="")
    setwd(d)
    files <- list.files(pattern = "wav$", ignore.case = T )
    for(file in 1:length(files)){
      for(i in 1:(length(minutos))){
        wav <- readWave(files[file], from = minutos[i]-1, to = minutos[i],
                        units = "minutes")
        ACI_1 <-ACI(wav, flim=c(1, 11), wl=2048, ovlp = 50)
        ACI_2 <-ACI(wav, flim=c(1, 11), wl=2048)

                list_ACI<-list_ACI%>%add_row(ACI_1,ACI_2)

        }
    }
  }
  aciwindow<-list_ACI
  t.test(list_ACI[1], list_ACI[2])
  
  list_ACI<-list_ACI%>%add_column(aiFactors$Horas)
  
  NP
  list_NP
  for(file in 1:length(files)){

    for(i in 1:(length(minutos))){
      
      wav <- readWave(files[file], from = minutos[i]-1, to = minutos[i],
                      units = "minutes")
      
     # ACI_1 <-ACI(wav, flim=c(1, 11))
     # list_NP[i]<-cbind(NP)
      
     
     # x <-soundscapespec(wav, plot=TRUE) 	## function call to compute soundscape power in R-seewave(no plots)
    #  v <- as.vector(x[,2]) 				## soundscape power F 1-20 kHz
      
      # f=11000
      # 
      # Bioph <- colSums(x)-v[1]				## Biophony
      # Technophony <- v[1]						## Technophony
      # 
      # Bioc <-x[11,2] 			## 10-11 kHz
      # 
      # TB<-(Technophony/Bioc)
      # 
      # # AEI <- acoustic_evenness(wav,max_freq = 10000)#Acoustic Evenness Index  {soundecology}
      # # AEI.L <- AEI$aei_left
      # # AEI.R <- AEI$aei_right
      # # 
      # # ADI <- acoustic_diversity(wav, max_freq = 10000) #Acoustic Diversity Index {soundecology}
      # # ADI.L <- ADI$adi_left
      # # ADI.R <- ADI$adi_right
      # # 
      # # ACIsee <-ACI(wav) #Acoustic Complexity Index #{seewave}
      # # 
      # # ACI <- acoustic_complexity(wav, min_freq=2000,max_freq = 11000)#Acoustic Complexity Index #{soundecology}
      # # ACI.L <- ACI$AciTotAll_left
      # # ACI.R <- ACI$AciTotAll_right
      # # 
      # # BIO <- bioacoustic_index(wav,min_freq = 2000, max_freq = 8000)#Bioacoustic Index
      # # BIO.L <-BIO$left_area
      # # BIO.R <-BIO$right_area
      # # 
      # # NDSI <- ndsi(wav) #Normalized Difference Soundscape Index {soundecology}
      # # NDSI.L<-(NDSI$ndsi_left)
      # # NDSI.R<-(NDSI$ndsi_right)
      # # 
      # # TE<-H(wav) #(Total entropy), total entropy of a time wave. #{seewave}
      # 
      # envorni<-env(wav, plot=TRUE)
      # Ht<-th(envorni) # (Temporal entropy), entropy of a temporal envelope. Calculate the temporal Entropy (Ht; Sueur et al. 2008b) by calling the "env" function from the "seewave" package.
      # 
      # # 
      # # speca<-spec(wav,f=f, plot=FALSE) #{seewave}
      # # Hf<-sh(speca) #calculate the frequency Entropy (Hf; Sueur et al. 2008b) by calling the "sh" function from the "seewave" package
      # # 
      # # MAE<-M(wav) #Median of amplitude envelope #{seewave}
      # # 
      # # spec <- meanspec(wav, plot=FALSE) #{seewave}
      # # peaks<-fpeaks(spec, plot=FALSE)
      # # NP<-length(peaks)/2
      # 
      # finf <- data.table(file.info(dir(getwd()), extra_cols = F))
      # finft<- data.table(finf$mtime[file=file])
      # 
      # z <- list(AEI.L = AEI.L,
      #           AEI.R = AEI.R,
      #           ADI.L=ADI.L,
      #           ADI.R=ADI.R,
      #           ACIsee=ACIsee,
      #           ACI.L=ACI.L,
      #           ACI.R=ACI.R,
      #           BIO.L=BIO.L,
      #           BIO.R=BIO.R,
      #           NDSI.L=NDSI.L,
      #           NDSI.R=NDSI.R,
      #           TE=TE,
      #           Ht=Ht,
      #           Hf=Hf,
      #           MAE=MAE,
      #           NP=NP,
      #           Technophony=Technophony, BIOAC=Bioc, TB=TB,
      #           DateTime=finft)
      # 
      # df <- rbind(df, data.frame(z, row.names = make.names(rep(files[file], length(z[[1]])), unique = TRUE)))
    }
    
    
hf<-str_sub(files[file], 7,11)
hf
hf=parse_date_time(pres$Hora,"HM")
tt = seq(hf, hf+540, 60)
tt=format(tt, format="%H:%M")
tt=hm(tt)
tt
tnum=as.numeric(tt, "hours")
ta=t(as.numeric(list_ADI))
df=data.frame()
df=data.frame(Hora = tnum,
                iADI = t(ta))

#ggplot(data=df,aes(x=Hora, y=iADI)) + 
 # geom_point()
if (file==1){
plot(ai10$hnum, ai10$NDSIsee,
          #xlim=c(16,19),
          #ylim=c(0,2.5),
         xlab="Horas", ylab="Indice de Diversidad Acustica",
          col=(colors), cex=0.9)
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col = "#f7f7f7") # Color
  
  # Add a new plot
  par(new = TRUE)
}
else{
  plot(df$Hora, df$iADI,
       xlim=c(16,19),
       ylim=c(0,2.5),
       ann=F,
       col=(colors[c]), cex=1, axes=F)
}

    par(new=T )
    
  }
  # names(df)[20]<-paste("Date-Time")
  #return(df)
#}

  
  
  
  aiFactors <- aiFactors %>% relocate(Amp , .before = Hora)
  write.csv(ai10, "ai10.csv")
  
  ai10<-data.frame()
  ai10=colMeans(c)
  
  for (i in 0:82) {
    a=(10*i)+1
    b=(10*i)+10
    c=aiFactors[a:b,2:12]
    d=colMeans(c)
    ai10<-data.frame(ai10, d)
  }
  
  hai<-aiFactors$Puntos[(seq(1,830,10))]
  ai10<-ai10[,-1]
  ai10<-t(ai10)
  hai
  ai10<-data.frame(ai10, hai)
 names(ai10)[14]="Puntos"
  
   names(aiFactors)[4]="ACI"
  aiFactors=aidd
  aidd=aiFactors
  aiFactors$ACIsee<-list_ACI$ACI_1
  aiFactors<-aiFactors %>% add_column(list_NP,.before=11)
  
  plot(x=df$hnum, y=list_ACI$ACI_1)
  plot(x=df$hnum, y=list_ACI$ACI_2)
  plot(x=df$hnum, y=df$ACI)
  
  