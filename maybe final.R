##### data, libraries ######
library(tidyverse)
library(rstatix)
#import All1min or db to use
df=All1min

###### remove outliers #####
outies<-data.frame()
for (i in 2:15) {
  o<-df %>%               
    group_by(Horas) %>%
    identify_outliers(i)
  outies<-rbind(outies, o)
}
#remove extrems and duplicates from outies
outiesNonDup<-outies[outies$is.extreme == "TRUE", ]
outiesNonDup <-outiesNonDup%>%distinct(X, .keep_all = T)  
noOuties<- df[!df$X %in% outiesNonDup$X,]
df=noOuties
#df2=noOuties
#remove1<-df #save with 1 removal
table(df$Horas)
##########db and changes##############

## changes ##
df$Horas<-factor(df$Horas, levels=c("4-6","11-13","16-18"))
df$Puntos=factor(df$Puntos)

#square transform of sound levels
##df[, 2:5] <- (df[,2:5])*(df[,2:5])
### add only Hour number
hr<-data.frame()
df$hr<-hr<-str_sub(df$hnum, 1,2)

df$hr<-ifelse(str_sub(df$hr, 2,2) == ".", str_sub(df$hr, 1,1), str_sub(df$hr, 1,2))
df$hr<-factor(df$hr, levels=c("4", "5", "6", "11", "12","13","16", "17", "18"))

## db with only vars ##

# shortdf<-data.frame(df$Horas, df$Puntos, df$NDSIsee, df$SPLMax, df$Amp, df$hnum, df$Hora, df$hr)
# names(shortdf)<-c("Horas", "Puntos", "NDSI", "SPLMax", "Rel-Amp", "hnum", "Hora", "hr")
df=df[-c(1,2,4,5,12,14,20)]
names(df)<-c("SPLMax", "AEI", "ADI", "ACI", "BI","NDSI","H","TFSD","Rel.Amp", "Hora", "Horas", "Puntos","hnum", "hr")
zSPL<-scale(df$SPLMax)
df<-data.frame(df,zSPL)
head(df)
library(reshape2)
meltdf<-meltdf %>%
  group_by(variable) %>%
  mutate(
    Max = max(value, na.rm = T),
  ) 
meltdf1<-meltdf[meltdf$variable != "NDSI", ]

######

##### data exploration plots ####
plot(ADI~hr, df)
####### selected model for all #####
library(nlme)
SPLMax10=(df$SPLMax)/10
MW<-gls(TFSD~Horas*SPLMax10, 
        weights=varIdent(form=~1|Horas),
        method="ML", data=df,
        correlation=corAR1(form=~1|Puntos/Horas))

MW2<-gls(TFSD~Horas, 
        weights=varIdent(form=~1|Horas),
        method="ML", data=df,
        correlation=corAR1(form=~1|Puntos/Horas))

MW1<-gls(ADI~Horas*SPLMax10, ### worse
        weights=varIdent(form=~1|Puntos),
        method="ML", data=df,
        correlation=corAR1(form=~1|Puntos/Horas))
summary(MW1)
# MW2<-gls(ADI~Horas*SPLMax10, 
#          weights=varIdent(form=~1|Horas*Puntos),
#          method="ML", data=df,
#          correlation=corAR1(form=~1|Puntos/Horas))

MW3<-gls(ADI~Horas*Puntos, #this seems to be best in xy plots
         method="REML", data=df,
         correlation=corAR1(form=~1|Puntos))
##### check assumptions ####

MA<-MW3
AIC(MW,MW2)
anova(MW, MW2)
plot(resid(MA, type="normalized", main = AIC(MA)))
#qqnorm(MA)
EA<- resid(MA, type="normalized")
FA<- fitted(MA)
op<- par(mfrow=c(2, 2), mar=c(4, 4, 3, 2))
MyYlab<- "Residuals"
plot(x=FA, y=EA, xlab="Fitted values", ylab=MyYlab)
boxplot(EA~Horas, data=df,
        ylab=MyYlab)
plot(EA~(SPLMax10), data=df,
     ylab=MyYlab)
boxplot(EA~Puntos, data=df,
        ylab=MyYlab)
par(op)
xyplot(resid(MW, type="normalized")∼Horas|Puntos,
       data=df, ylab="Residuales",
       xlab="Hora",
       panel=function(x,y){
         panel.grid(h=-1, v=2)
         panel.points(x, y, col=1)
         panel.loess(x, y, span=0.7, col=1,lwd=2)})
summary(MW3)

##### Interactions plot #####
library(emmeans)
emmeans(MW, "Horas", mode = "df.error")
emmip(MW,  SPLMax10~Horas, 
      mode = "df.error")
(emmeans(MW, pairwise ~ SPLMax10 | Horas, mode = "df.error"))
#make with fitted and not fitted
interaction.plot(x.factor =df$ Horas, #x-axis variable
                 trace.factor = df$Puntos, #variable for lines
                 response = FA, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "ADI",
                 xlab = "Horas",
                 col = c("pink", "blue", "black"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Puntos")

results <- get_contrasts(fit, "Emotion_Condition")
print(results$contrasts)

# ggplot(df, aes(x=SPLMax10, y=ADI, group = interaction(Horas, Puntos))) + 
#   geom_smooth(method="lm", alpha = 0., aes(lty=Puntos, color=Horas))

r <- get_contrasts(MW3)
print(results$contrasts)

#### plotting model ####

performance(MW1.glmer)

plot(FA~df$Horas)
library(viridis)

ggplot(meltdf, aes(hnum, value, , color=Puntos)) +
  geom_smooth(alpha=0.2, size=0.5)+
  geom_point(size=0.4, alpha=0.5) +
  scale_color_viridis(option = "plasma", discrete=T, direction = 1)+
  facet_wrap(~variable, scales = "free_y")+
  ylab("Valor del Indice")+
  xlab("Hora")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
ggplot(meltdf, aes(x=value, fill=Horas)) +
  geom_density(alpha=0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#e9ecef"))+
  xlab(names(df1)[i])+
  facet_wrap(~variable, scales = "free")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



ggplot(meltdf1, aes(x=(value/Max), y = variable, fill = Horas)) +
  geom_density_ridges(alpha=0.5, scale = 1.2) +
  scale_fill_viridis(name = "Horas", option = "C", discrete=T) +
  theme_ridges() + 
  xlab("Densidad relativa")
  theme(legend.position = "none")


library(lme4)
MW.glmer <- glmer(
  ADI ~ Horas *SPLMax10+ (1|Puntos:Horas), 
  data = df, 
)
summary(MW.glmer)
AIC(MW.glmer)

library(ggeffects)
ggpredict(MW.glmer, c( "SPLMax10[all]", "Horas" )) %>% plot()
ggpredict(MW3, c( "SPLMax10", "Horas" )) %>% plot()

plot((SPLMax)~hnum, data=df)
summary(MA)


library(lattice)
xyplot(predict(MW3)∼Horas|Puntos,
       data=df1, superpose=T,
       ylab="Residuales",
       xlab="Hora",
       panel=function(x,y){
         panel.grid(h=-1, v=2)
         panel.points(x, y, col=1)
         panel.loess(x, y, span=0.5, col=1,lwd=2)})

M.lm=gls(ADI~Horas*(SPLMax10), data=df)
xyplot(resid(M.lm, type="normalized")∼Horas|Puntos,
       data=df, ylab="Residuals",
       xlab="Hora",
       panel=function(x,y){
         panel.grid(h=-1, v=2)
         panel.points(x, y, col=1)
         panel.loess(x, y, span=0.5, col=1,lwd=2)})

report(
  
)