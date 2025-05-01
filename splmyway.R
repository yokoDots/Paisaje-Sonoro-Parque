colfunc <- colorRampPalette(c("purple", "cyan", "yellow", "purple"))
colors=c(colfunc(25))
par(cex=0.7)

#JUST A SIMPLE TRY
data(orni)
data(tico)
a<-spec(tico,f=22050,wl=512,at=0.2,plot=FALSE)
soundscapespec(tico, plot=TRUE, col="darkgreen")
simp=data.frame(spec(orni, f = 22050,
                PSD=T, col=(colors[2])))
simpi=data.table(spec(orni))
ffpp <- fpeaks(simpi, f = freq, plot = F,  col=(colors[3]))

axis(side=1, at=c(0:10))
par(new=TRUE)
meanspec(orni,f=22050, FUN=var, col=(colors[8]))


#set working dir 
setwd()
#make a data frame
cn=0
df <- data.frame()
msdf <- data.frame(freq=numeric(), amp=numeric(), fname=character())

#get files based on wav
files1 <- list.files(path = getwd(), pattern = "wav$", ignore.case = T)
#divide into 1 minute fragments
minutos <- seq(0:9)

for (file in 1:length(files1)) {
  cn=cn+1
  for (i in 1:(length(minutos))) {
    #get the wav info
    wav <- readWave(files1[file], from = minutos[i] - 
                      1, to = minutos[i], units = "minutes")
    freq <- wav@samp.rate
    #mean relative amplitude of the freq distribution
            #spectrum0 <- data.table(meanspec(wav, f = freq, norm = F, 
            #plot = F, main = files[file]) , col=(colors[cn]))
    SPL_Un <- NULL
    x <- NULL
    ms <- meanspec(wav, f = freq, plot = T, FUN = max, col=(colors[cn]))
   axis(side=1, at=c(0:15))
   par(new=TRUE)
    spl = data.table(ms)

    #normalizes or pseudo callibrates, this factor might or should change
    #spl[, `:=`(SPL_Un, 2 * 1(y/(0.5)))]
    spl[, `:=`(SPL_Un, 2*y*0.5)]
    
    #get the different statistics for the sound pressure
    SPLm <- data.table(median(spl$SPL_Un))
    SPLsd <- data.table(sd(spl$SPL_Un))
    SPLQ25 <- data.table(quantile(spl$SPL_Un, 0.25))
    SPLMe <- data.table(median(spl$SPL_Un))
    SPLQ75 <- data.table(quantile(spl$SPL_Un, 0.75))
    SPLMax <- data.table(max(spl$SPL_Un))
    SPLMin <- data.table(min(spl$SPL_Un))
    
    ttest <- t.test(spl$SP)
    lwrci <- ttest$conf.int[1]
    uprci <- ttest$conf.int[2]
    
    #get the info of the files
    finf <- data.table(file.info(dir(getwd()), extra_cols = F))
    #get the time
    finft <- data.table(finf$mtime[file = file])
    #make a list with different info
    z <- list(SPLm = SPLm, SPLsd = SPLsd, SPLQ25 = SPLQ25, 
              SPLMe = SPLMe, SPLQ75 = SPLQ75, SPLMax = SPLMax, 
              SPLMin = SPLMin, lwrci = lwrci, uprci = uprci, 
              finft = finft)
    #bind by rows to data frame
    df <- rbind(df[], data.frame
          (z, row.names = make.names(rep(files1[file],
              length(z[[1]])), unique = TRUE)       ))
    
    msdf <- rbind( msdf[], data.frame
            (ms, fname=rep(files1[file], length(ms[,1])) ))
  }
}
dev.off()
#give names to columns
names(df)<-  c("SPLmean", "SPLsd", "SPLQ25", "SPL50",
"SPLQ75", "SPLMax", "SPLMin", "lwr.ci(95%)", "upr.ci(95%)"
,"Date-Time")

#get the file name for each row, or info on row 1
filenames=rownames(df)
#install stringr
#extract hours from the blob vector, watch out for the plsvrmrny
hours<-str_sub(filenames, 7,7)
#hours<-replace(hours, hours=="1.", 13 )
#extract point from the blob vector
Puntos<-str_sub(aitotal$X, 1,2)
#make hours an integer and organize into a tibble
hours1<-strtoi(hours)
hours2=as_tibble(hours1)
names(hours2)[1] <- "Hour"

#add column Minutes generating random minutes
a=floor(runif(length(files), min=0, max=49))
   #make 10 minute intervals
aa=rep(a,each=10)
aa=c(aa+seq(0,9))

hours2<-hours2 %>%add_column(Minutes = aa)
#aitotal<-aitotal %>%add_column(Puntos)


library(lubridate)
times<-hours2 %>% 
  mutate(t = str_c(Hour,Minutes,sep = ":"))

times<-hours2 %>% 
  mutate(t = str_c(Hour,Minutes,sep = ":")) %>% 
  mutate(t = t %>% hm())

times<-times%>%add_column(tFrmt = times$t )
times<-times%>%add_column(SPLmean = df$SPLmean)
times<-times%>%add_column(Puntos = puntos)
times$t<- as.numeric(times$t, "hours")

#save the table
write.csv(df, "splMax4pm.csv", row.names=T)

#plot!

#Density plots are used to study the distribution of one or a few variables. 
ggplot(aitotal, aes(x=BIO))+
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  facet_wrap(~Horas, scale="fixed")
times=aitotal
ggplot(times, aes(x=BIO, fill=Puntos)) +
  geom_density(alpha=0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#e9ecef"))

ggplot(times, aes(x=BIO, fill=Horas)) +
  geom_density(alpha=0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#e9ecef"))

#A histogram is an accurate graphical representation of 
#the distribution of a numeric variable. It takes as input numeric variables only.

#data %>%
  #filter( a variable<300 ) %>%
  ggplot(times, aes(x=SPLmean)) +
  stat_bin(breaks=seq(70,100,5), fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("SPLmean values")
  
  ggplot(times, aes(x=BIO)) +
    geom_histogram()
  
  ggplot(times, aes(x=BIO)) +
    geom_histogram()+
    facet_wrap(~Horas)
  
  ggplot(times, aes(x=BIO, fill=Horas)) +
    geom_histogram( color="#e9ecef", alpha=0.6) +
    scale_fill_manual(values=c("#69b3a2", "#404080","#000000"))
  
#A scatterplot is made to study the relationship between 2 variables.
  ggplot(times, aes(x=Hora, y=BIO)) + 
    geom_point() 
  
  ggplot(times, aes(x=Puntos, y=BIO)) + 
    geom_point() 
  
  ggplot(times,aes(x=Hora, y=BIO)) +
    geom_point(color="#69b3a2", alpha=0.8) +
    facet_wrap(~Puntos, scale="fixed")
  
  ggplot(times, aes(x=Hora, y=BIO, color=Puntos)) +
    scale_fill_manual(values=c("#69b3a2", "#404080","#29b3a2"))+
    geom_point()+
    xlab("4pm - 6pm")
    
#A Ridgeline plot (sometimes called Joyplot) shows the distribution of a numeric
#value for several groups. Distribution can be represented using histograms
#or density plots, all aligned to the same horizontal scale and presented with 
#a slight overlap.

  library(ggridges)
  
#will not use, makes sense with more variables
  
# A boxplot gives a nice summary of one or more numeric variables.
  ggplot(aitotal, aes(x=Horas, y=BIO)) +
    geom_boxplot()+
    geom_jitter(color="blue", size=0.7, alpha=0.5)
  
  ggplot(times, aes(y=SPLmean)) +
    geom_boxplot()

  ggplot(times, aes(x=Hour, y=SPLmean)) +
  geom_boxplot()+
  geom_jitter(color="blue", size=0.7, alpha=0.5)
  
  ggplot(times, aes(x=Horas, y=BIO, fill=Puntos)) +
    geom_boxplot()  
  
hr1 <- times  %>%
  group_by(Hour, Puntos)

    
hr2 <- times  %>%
  group_by(Hour, Puntos) %>%
  summarise(n = sum(SPLmean)) %>%
  mutate(totalHour = sum(n))

hr3 <- times  %>%
  group_by(Puntos, Hour) %>%
  summarise(n = sum(SPLmean)) %>%
  mutate(percentage = n / sum(n))

ggplot(hr3, aes(x=Hour, y=percentage, fill=Puntos)) + 
  geom_area(alpha=0.6 , size=0.5, colour="black")

ggplot(hr2, aes(x=Hour, y=percentage, fill=Puntos)) + 
  geom_area(alpha=0.6 , size=0.5, colour="black")

ggplot(hr1, aes(x=Hour, y=SPLmean, fill=Puntos)) + 
  geom_density(alpha=0.6)

ggplot(times, aes(x=Puntos, y=Hour, fill= SPLmean)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue")

times$Puntos <- factor(times$Puntos , 
                        levels=c("p1", "p2", "p3") )
times$Hour <- factor(times$Hour, 
                       levels=c("4", "5", "6") )
