
#####   Scatter Plot with color intensity  ####
        #read db fix db
        #assign color
        #loop through variables to graph
names(df1)[4]="ACI"
names(df1)[6]="NDSI"
names(df1)[9]="TFSD"
#names(df1)[11]="RelAmp"
FactorsAI<-FactorsAI%>%add_column(pres$SPLmean, 
                    .before = "Amp") 
names(dfAll)[11]="SPLmean"


library(viridis)
Col <-viridis


#FactorsAI<-dfAll
df10min<-ai10
dat  <-data.frame( Col(10)[as.numeric(cut(dfAll$SPLmean,breaks = 15))])

#for (i in 1:ncol(df1)) 
#general variable for plotting
df<-dfAll
  for (i in 2:12) {
    # par(mar = c(3.9, 3.9, 2,1), cex.axis=0.86, cex.lab=0.8)
    # plot(df$hnum, (df[[i-1]] ),
    #      xlab="Hora", ylab= names(df2)[i-1],
    #      col=(dat$Col), cex=0.7, pch=16, axes=FALSE)
    # par(new=T)
  plot(dfAll$hnum, (dfAll[[i]] ),
       xlab="Hora", ylab= names(dfAll)[i],
       col=(dat$Col), cex=0.4, 
       main=paste(names(dfAll)[i], "en Funcion del Tiempo", sep=" "),
       cex.main=0.9)
    #par(new=F)
  }


#### plotnoises #####
#read db, plot()
library(ggplot2)
aiNoises <- read.csv(file = 'noises/noisesai.csv')
aiNoises<-aiNoises[,-8]
rangesai<-apply(aiNoises[2:10],2, range) 
aiNoises$X<-as.factor(aiNoises$X)
names(aiNoises)[4]="ACI"
names(aiNoises)[6]="NDSI"
names(aiNoises)[9]="TFSD"
names(aiNoises)[10]="RelAmp"

aiNoises$X <- factor(aiNoises$X, levels = c('puretone1k.wav', 'tones.wav', 'pink.wav', 'white.wav'))

for (i in 10:10) {
  print(
    ggplot(aiNoises, aes(x=1, y=aiNoises[,i], color=X)) +
      geom_point(size=2.5)+
      scale_color_viridis(option = "plasma", discrete=TRUE) +
      ylab(paste("Rango:", names(aiNoises)[i],
             round(rangesai[1,i-1], 2),"-", round(rangesai[2,i-1], 2), sep = "  ")) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), legend.title = element_text(size = 11),
            legend.position="left", axis.title=element_text(size = 12))+
      xlab("") +labs(colour = "Sonido")
    )
}

wav<-readWave("noises/tones.wav")
spectro(wav, fastdisp = T, palette = viridis, scale = T, 
         collevels=seq(-50,0,1), cexlab =0.9, cexaxis = 0.8,
        main = "Tonos 1 - 8 kHz", 
         widths = c(0.39,0.09))
### I hate you noises

#### SPLsss for godsake! ###
dfSPL<-tibby1
library(hrbrthemes)
plHrSPL<-ggplot(dfSPL, aes(x=SPL50, fill=Horas)) +
  geom_density(alpha=0.5) +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#e9ecef"))+
  xlab("SPL: Presión del Sonido (dB)")+
  ggtitle("Densidad de la Distribución de SPL")

old<- theme_set(theme_bw()+theme(plot.title = element_text(size = 12)))
plHrSPL

plDHr<-ggplot(dfSPL, aes(x=Dia, y=SPL50, color=Horas, name=X)) +
  geom_point(alpha = 0.6)+
  scale_color_viridis(option = "plasma", discrete=T, direction = -1) +
  xlab("Día - Noviembre, 2020")+
  ylab("SPL (dB)")+
  labs(color="Horas")+
  ggtitle("Presión del Sónido Por Dia")
fig <- ggplotly(plDHr)
plDHr
fig
plHrPt<-ggplot(dfSPL, aes(x=hnum, y=SPL50, color=Puntos, name=X)) +
  geom_point(alpha = 0.6)+
  scale_color_viridis(option = "plasma", discrete=T, direction = -1) +
  xlab("Horas de Grabación: 4am-6am, 11am-1pm, 4pm-6pm")+
  ylab("SPL (dB)")+
  labs(color="Puntos")+
  ggtitle("Presión del Sónido Por Horas")
fig2 <- ggplotly(plHrPt)
fig2
plHrPt

#### ridges #
library(ggridges)
# Plot
ggplot(tibby1, aes(x = ACI, y = Horas, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
  scale_fill_viridis(option = "A", discrete = F) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.05, "lines"),
    strip.text.x = element_text(size = 8)
  )
