
df<- data.frame()

files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T )
#minutos<-seq(0.1:0.60)# 1-minute interval

for(file in 1:length(files)){
  
  #for(i in 1:(length(minutos))){
  par(mfrow=c(3,1))
    wav <- readWave(files[file])
    freq<-wav@samp.rate
    spectrum0<-(meanspec(wav,f=freq,norm=F,plot=T, main=files[file]))
    fp=fpeaks(spectrum0, nmax=10)
    domfre=dfreq(wav, threshold = 80)
    spct=spectro(wav)
    sndspec=soundscapespec(wav)
  
    spectrum0=data.table(spectrum0)
    spectrum0[,SPL_Un:=20*log10(y/(2*10e-5))]
    SPLm<- data.table(mean(spectrum0$SPL_Un))
    SPLsd<- data.table(sd(spectrum0$SPL_Un))
    SPLQ25<- data.table(quantile(spectrum0$SPL_Un, .25))
    SPLMe<- data.table(median(spectrum0$SPL_Un))
    SPLQ75<- data.table(quantile(spectrum0$SPL_Un, .75))
    SPLMax<- data.table(max(spectrum0$SPL_Un))
    SPLMin<- data.table(min(spectrum0$SPL_Un))
    ttest<- t.test(spectrum0$SP)
    lwrci <- ttest$conf.int[1]
    uprci <- ttest$conf.int[2]
    finf <- data.table(file.info(dir(getwd()), extra_cols = F))
    finft<- data.table(finf$mtime[file=file])
    
    z <- list(SPLm=SPLm, SPLsd=SPLsd,SPLQ25=SPLQ25, SPLMe=SPLMe, SPLQ75=SPLQ75,
              SPLMax=SPLMax, SPLMin=SPLMin, lwrci=lwrci,uprci=uprci,finft=finft)
    
    df <- rbind(df[], data.frame(z, row.names = make.names(rep(files[file], length(z[[1]])), unique = TRUE)))
    
  #}
}

