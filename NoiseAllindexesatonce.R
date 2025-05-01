# library(tuneR)
# library(seewave)
# library(soundecology)
 library(viridis)

files <- list.files(pattern = "wav$", ignore.case = F)

noises <- data.frame()

for(j in 1:length(files)){
  
    wav <- readWave(files[j],units = "seconds" )
    
    x<-soundscapespec(wav, plot=F)
    soundscapespec(wav, plot=T)
    AEI <- acoustic_evenness(wav, max_freq = 9999)

    ADI <- acoustic_diversity(wav, max_freq = 9999)

    ACIsee <-ACI(wav, flim=c(0.6,11)) #Acoustic Complexity Index #{seewave}
    
    BIO <- bioacoustic_index(wav,min_freq = 2000, max_freq = 8000)

    NDSIsee<-NDSI(x, max = TRUE)
    TFSDsee<-TFSD(wav)
    He<-H(wav) 

round(0.0056765, 3)
    z <- list(
      AEI = round(AEI$aei_left,2),
      ADI= round(ADI$adi_left,2),
      H=round(He,2),
      ACI=round(ACIsee,2),
      BI = round(BIO$left_area,2),
      NDSI=round(NDSIsee,2),
      TFSD=round(TFSDsee,2))
    
    noises <- rbind(noises, data.frame(z, 
    row.names = make.names(rep(files[j], length(z[[1]])),
    unique = TRUE)) )
    rm(wav)
    
}
noises <- tibble::rownames_to_column(noises, "Sound")
noises$Sound[1]="Rosado"
noises$Sound[2]="1 kHz"
noises$Sound[3]="Tonos"
noises$Sound[4]="Blanco"

#n<-noises
#noises<-n
Max<-c("Max")
Min<-c("Min")

for (i in names(noises)[2:8]) {
  Max<-data.frame(Max, max(df[[i]]))
  Min<-data.frame(Min, min(df[[i]]))}
names(m)<-names(noises)
names(m)[1]<-"Nivel"

m<-Min
m<-rbind(m, Max)

noises$Sound<-factor(noises$Sound,
        levels = c("Blanco","Rosado", "1 kHz",
                  "Tonos"))
ggplot(data=noises, aes(Sound, AEI))+
  geom_point()
  
ggplot(data=noises, aes(ADI, color=Sound))+
  geom_point()

ggplot(data=noises, aes(Sound, AEI, color=Sound))+
  geom_point(size=2)+
  #scale_color_viridis(discrete = T)+
  #ylim(range(All1min[[AEI]]))+
  theme_classic()

ggplot()+
  geom_point(data=noises, aes( Sound,AEI,color=Sound),size=3.2)+
  scale_color_viridis(discrete = T, option = "H")+
  geom_hline (data=m, aes(yintercept=AEI),linetype = "dashed", size=1.6)+
  theme_classic()+
  theme(axis.text.x=element_blank())

ggplot()+
  geom_point(data=noises, aes(Sound, H, color=Sound),size=3.2)+
  scale_color_viridis(discrete = T, option = "H")+
  geom_hline (data=m, aes(yintercept=H),linetype = "dashed", size=1.6)+
  theme_classic()+
  theme(axis.text.x=element_blank())

ggplot()+
  geom_point(data=noises, aes(Sound, ACI, color=Sound),size=3.2)+
  scale_color_viridis(discrete = T, option = "H")+
  geom_hline (data=m, aes(yintercept=ACI),linetype = "dashed", size=1.6)+
  theme_classic()+
  theme(axis.text.x=element_blank())
ggplot()+
  geom_point(data=noises, aes(Sound, BI, color=Sound),size=3.2)+
  scale_color_viridis(discrete = T, option = "H")+
  geom_hline (data=m, aes(yintercept=BI),linetype = "dashed", size=1.6)+
  theme_classic()+
  theme(axis.text.x=element_blank())
ggplot()+
  geom_point(data=noises, aes(Sound, NDSI, color=Sound),size=3.2)+
  scale_color_viridis(discrete = T, option = "H")+
  geom_hline (data=m, aes(yintercept=NDSI),linetype = "dashed", size=1.6)+
  theme_classic()+
  theme(axis.text.x=element_blank())
ggplot()+
  geom_point(data=noises, aes(Sound, TFSD, color=Sound),size=3.2)+
  scale_color_viridis(discrete = T, option = "H")+
  geom_hline (data=m, aes(yintercept=TFSD),linetype = "dashed", size=1.6)+
  theme_classic()+
  theme(axis.text.x=element_blank())
round(All1min$hnum, 0)
str(All1min)

ggplot(data=All1min, aes(Hora, SPLMax, color=He))+
  geom_point(alpha=0.7, size=0.9)+
  scale_color_viridis(direction = -1, option = "A")+
  theme_classic() + theme(legend.key.size = unit(0.3, "cm"))

ggplot(data=All1min, aes(Hora, SPLMax, color=ACIsee))+
  geom_point(alpha=0.7, size=0.9)+
  scale_color_viridis(direction = -1, option = "A")+
  theme_classic()+ theme(legend.key.size = unit(0.3, "cm"))

ggplot(data=All1min, aes(Hora, SPLMax, color=NDSIsee))+
  geom_point(alpha=0.7, size=0.9)+
  scale_color_viridis(direction = -1, option = "A")+
  theme_classic()+ theme(legend.key.size = unit(0.3, "cm"))

ggplot(data=df, aes(hr, ACI, color=NDSI))+
  geom_point(alpha=0.7, size=0.9)+
  scale_color_viridis(direction = -1, option = "A")+
  theme_classic()+ theme(legend.key.size = unit(0.3, "cm"))

ggplot(data=df, aes(hr, ACI, color=AEI))+
  geom_point(alpha=0.7, size=0.9)+
  scale_color_viridis(direction = -1, option = "A")+
  facet_wrap("Puntos")+
  theme_classic()+ theme(legend.key.size = unit(0.3, "cm"))

ggplot(data=df, aes(hr, BI, color=SPLMax))+
  geom_point(alpha=0.7, size=0.9)+
  scale_color_viridis(direction = -1, option = "A")+
  facet_wrap("Puntos")+
  theme_classic()+ theme(legend.key.size = unit(0.3, "cm"))

ggplot(data=df, aes(hr, NDSI, color=SPLMax))+
  geom_point(alpha=0.7, size=0.9)+
  scale_color_viridis(direction = -1, option = "A")+
  facet_wrap("Puntos")+
  theme_classic()+ theme(legend.key.size = unit(0.2, "cm"))

tempdir()
