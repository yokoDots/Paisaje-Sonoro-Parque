library(viridis)
##### Read the files! ###########
all10<-read.csv("All10min.csv", header = T)
all1<-read.csv("All1min.csv", header = T)
#check and adjust
head(all10)
head(all1)

#tibby1=data.table(all1[-1])
tibby1=as_tibble(All1min)

names(tibby1)[c(8,10,11,13)]
names(tibby1)[c(8,10,11,13)]<-c("ACI","NDSI", "H", "TFSD")
names(tibby1)[9]="BI"

tibby1$Hh<-substr(tibby1$Hora, 1,2 )
tibby1$Horas<-as.factor(tibby1$Horas)
tibby1$Horas <- factor(tibby1$Horas,levels=c("4-6","11-13","16-18"))
tibby1$Puntos<-as.factor(tibby1$Puntos)
tibby1$Dia<-as.factor(tibby1$Dia)

str(tibby1)

##### Exploratory Data Analysis with Graphs  ########

#heat
ggplot(tibby1, aes(y=round(BI, 0), x=round(hnum,0), fill= NDSI)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE, direction = -1, option = "B") 
  
ggplot(tibby1, aes(x=Hh, y=round(BIO,0), fill= NDSI)) + 
  geom_tile() +
    scale_fill_gradient(low="pink", high="blue") 

ggplot(tibby1, aes(y=round(ACI, 0), x=round(hnum,0), fill= NDSI)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE, direction = -1, option = "B") 

#Density with one variable and frequency
ggplot(tibby1, aes(AEI, fill= Horas))+
  geom_density(alpha=0.4)+
  scale_fill_viridis(discrete=T, direction = -1) +
  geom_rug(alpha=0.4)+ #optional
  theme_bw()
old<- theme_set(theme_bw()+theme(plot.title = element_text(size = 10)))

d1<-ggplot(tibby1, aes(AEI, fill= Horas))+
  geom_density(alpha=0.5) +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#e9ecef"))+
  ggtitle("Indice de Paridad Acústica")+
  theme(legend.position = "none", legend.key.size  = unit(0.5, "cm") )

d2<-ggplot(tibby1, aes(ADI, fill= Horas))+
  geom_density(alpha=0.5) +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#e9ecef"))+
  ggtitle("Indice de Diversidad Acústica")+
  theme(legend.position = "none")
d3<-ggplot(tibby1, aes(H, fill= Horas))+
  geom_density(alpha=0.5) +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#e9ecef"))+
  ggtitle("Indice de Entropía Acústica")+
  theme(legend.position = "none")
d4<-ggplot(tibby1, aes(MAE, fill= Horas))+
  geom_density(alpha=0.5) +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#e9ecef"))+
  ggtitle("Indice Media Amplitud")+
  theme(legend.position = "none")
d5<-ggplot(tibby1, aes(ACI, fill= Horas))+
  geom_density(alpha=0.5) +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#e9ecef"))+
  ggtitle("Indice de Complejidad Acústica")+
  theme(legend.position = "none")
d6<-ggplot(tibby1, aes(BIO, fill= Horas))+
  geom_density(alpha=0.5) +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#e9ecef"))+
  ggtitle("Indice de Biodiversidad")+
  theme(legend.position = "none")
d7<-ggplot(tibby1, aes(NDSI, fill= Horas))+
  geom_density(alpha=0.5) +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#e9ecef"))+
  ggtitle("Indice Normalizado Paisaje Sonoro")+
  theme(legend.position = "none")
d8<-ggplot(tibby1, aes(TFSD, fill= Horas))+
  geom_density(alpha=0.5) +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#e9ecef"))+
  ggtitle("Derivada del Tiempo y Frecuencia")+
  theme(legend.position = "none")
d1 
d2 
d3 
d4 
d5 
d6 
d7 
d8

grid.arrange(d1,d2,d3,d4,d5,d6,d7,d8, ncol=4)

#several
tibby1 %>% 
  select(AEI, ADI, ACI, H, MAE, BIO, TFSD, NDSI, NP, Horas) %>% 
  explore_all(target = Horas)

#scatter with NFSD

    ## a variable for the ggplot part
gRA<-ggplot(tibby1, aes(x=Horas, y=BI, color=RelAmp, name=X))+
  scale_color_viridis(option = "plasma", discrete=F) +
  geom_point(size=0.5, alpha=0.5)
gRA
    
gBI<-ggplot(tibby1, aes(x=BI, y=ACI, color=Horas, name=X))+
  scale_color_viridis(option = "plasma", discrete=T) +
  geom_point(size=0.5, alpha=0.5)
gBI

gACI<-ggplot(tibby1, aes(x=hnum, y=H, color= ACI, name=X))+
  scale_color_viridis(discrete=F) 


ACIly<-ggplotly(gACI+geom_point(size=0.4, alpha=0.7))
ACIly

BIly<-ggplotly(gBI)
BIly

ggplotly(ggplot(tibby1, aes(x=hnum, y=TFSD, color= BI, name=X))+
           geom_point(size=0.4, alpha=0.8)+
           scale_color_viridis( discrete=F, direction = -1) )

ggplotly(ggplot(tibby1, aes(x=H, y=MAE, color=ADI, name=X))+
  scale_color_viridis(option = "plasma", discrete=F) +
  geom_point(size=0.5, alpha=0.7))

ggplotly(ggplot(tibby1, aes(x=ACI, y=BI, color=NDSI, name=X))+
           scale_color_viridis(option = "plasma", discrete=F) +
           geom_point(size=0.5, alpha=0.8))

ggplotly(ggplot(tibby1, aes(x=ACI, y=BI, color=TFSD, name=X))+
           scale_color_viridis(option = "plasma", discrete=F) +
           geom_point(size=0.5, alpha=0.7))

#### plot by factor ####
gRA1<-ggplot(tibby1, aes(x=Horas, y=ACI, color=RelAmp, name=X))+
  scale_color_viridis(option = "plasma", discrete=F) +
  geom_point(size=0.6, alpha=0.5)+ theme(legend.position="none", text=element_text(size=8))

gRA2<-ggplot(tibby1, aes(x=Horas, y=BI, color=RelAmp, name=X))+
  scale_color_viridis(option = "plasma", discrete=F) +
  geom_point(size=0.6, alpha=0.5)+ theme(legend.position="none", text=element_text(size=8))

gRA3<-ggplot(tibby1, aes(x=Horas, y=AEI, color=RelAmp, name=X))+
  scale_color_viridis(option = "plasma", discrete=F) +
  geom_point(size=0.6, alpha=0.5)+ theme(legend.position="none", text=element_text(size=8))

gRA4<-ggplot(tibby1, aes(x=Horas, y=ADI, color=RelAmp, name=X))+
  scale_color_viridis(option = "plasma", discrete=F) +
  geom_point(size=0.6, alpha=0.5)+ theme(legend.position="none", text=element_text(size=8))

gRA5<-ggplot(tibby1, aes(x=Horas, y=H, color=RelAmp, name=X))+
  scale_color_viridis(option = "plasma", discrete=F) +
  geom_point(size=0.6, alpha=0.5)+ theme(legend.position="none", text=element_text(size=8))

gRA6<-ggplot(tibby1, aes(x=Horas, y=NDSI, color=RelAmp, name=X))+
  scale_color_viridis(option = "plasma", discrete=F) +
  geom_point(size=0.6, alpha=0.5)+ theme(legend.position="none", text=element_text(size=8))

gRA7<-ggplot(tibby1, aes(x=Horas, y=TFSD, color=RelAmp, name=X))+
  scale_color_viridis(option = "plasma", discrete=F) +
  geom_point(size=0.6, alpha=0.5)+ theme(legend.position="none", text=element_text(size=8))

gRA8<-ggplot(tibby1, aes(x=Horas, y=MAE, color=RelAmp, name=X))+
  scale_color_viridis(option = "plasma", discrete=F) +
  geom_point(size=0.6, alpha=0.5)+ 
  theme(text=element_text(size=8), legend.position="none")

gRA9<-ggplot(tibby1, aes(x=0, y=0, color=RelAmp))+
  scale_color_viridis(option = "plasma", discrete=F) +
  geom_point(size=0.6, alpha=0.5)+ 
  theme(text=element_text(size=8), legend.key.size = unit(0.4,"cm"))

gRA9 <- get_legend(gRA9)
gRA9<-as_ggplot(gRA9)
gRA9
ggarrange(gRA1, gRA2, gRA3, gRA4, gRA5, gRA6, gRA7, gRA8,gRA9, 
        common.legend = F)
grid.arrange(gRA1, gRA2, gRA3, gRA4, gRA5, gRA6, gRA7, gRA8)
