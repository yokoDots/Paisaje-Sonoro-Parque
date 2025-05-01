varlist <- c("AEI", "ADI", "H")

fMW<-function(v){
    gls(as.formula(paste0(v,'~',"Horas*zSPL")), 
    weights=varIdent(form=~1|Horas),
    method="REML", data=df,
    correlation=corAR1(form=~1|Puntos/Horas))}

fMW("AEI")

MWs<-lapply(varlist, fMW)
names(MWs)<-varlist
MWs
lapply(MWs, summary)
MWs$AEI
table(df$Horas)
summary(MW2b)

MWd<-gls(ADI~Horas*zSPL, 
        weights=varIdent(form=~1|Horas),
        method="REML", data=df,
        correlation=corAR1(form=~1|Puntos/Horas))
MWe<-gls(AEI~Horas*zSPL, 
        weights=varIdent(form=~1|Horas),
        method="REML", data=df,
        correlation=corAR1(form=~1|Puntos/Horas))
MWh<-gls(H~Horas*zSPL, 
        weights=varIdent(form=~1|Horas),
        method="REML", data=df,
        correlation=corAR1(form=~1|Puntos/Horas))

MW3n<-gls(NDSI~Horas*zSPL, #this seems to be best in xy plots
         weights=varIdent(form=~1|Horas*Puntos),
         method="REML", data=df,
         correlation=corAR1(form=~1|Puntos))
MW3t<-gls(TFSD~Horas*zSPL, #this seems to be best in xy plots
         weights=varIdent(form=~1|Horas*Puntos),
         method="REML", data=df,
         correlation=corAR1(form=~1|Puntos))
MW2b<-gls(BI~Horas, 
         weights=varIdent(form=~1|Horas),
         method="REML", data=df,
         correlation=corAR1(form=~1|Puntos/Horas))
MW2c<-gls(ACI~Horas, 
          weights=varIdent(form=~1|Horas),
          method="REML", data=df,
          correlation=corAR1(form=~1|Puntos/Horas))



library(ggeffects)
prp <- ggpredict(MWe, c("Horas", "Puntos"))
pr <- ggpredict(MWe, c("Horas"))
#prf <- ggeffect(MWe, c("Horas")) gives same plot

#plot(pr)

ggplot(prp, aes(x, predicted, color=group)) +
  geom_point(size=2.5, 
             position=position_dodge(w=0.3)) +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                width=0.4, size=0.7,
                position=position_dodge(w=0.3))+
  scale_color_viridis(discrete = T)+
  theme_classic() +
  xlab("Horas")+
  ylab("AEI")
  
ggplot(pr, aes(x, predicted, color=x)) +
  geom_point(size=2.5, 
             position=position_dodge(w=0.3)) +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                width=0.4, size=0.7,
                position=position_dodge(w=0.3))+
  scale_color_viridis(discrete = T)+
  theme_classic() +
  xlab("Horas")+
  ylab("AEI")

prSpl <- ggpredict(MW3n, terms = c("zSPL","Horas"))


ggplot(prSpl, aes(x, predicted,
          color=group, )) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low,
            ymax = conf.high), alpha = .06,
            linetype = 0)+
  scale_color_viridis(discrete = T)+
  theme_classic() +
  xlab("z SPL-Max ")+
  ylab("NDSI")


plot(prSpl)

sjPlot::plot_model(MWh, type = "pred", terms = c("zSPL","Horas"))


MA<-MWe

EA<- resid(MA, type="normalized")
FA<- fitted(MA)
op<- par(mfrow=c(2, 2), mar=c(4, 4, 3, 2))
MyYlab<- "Residuals"
plot(x=FA, y=EA, xlab="Fitted values", ylab=MyYlab)
boxplot(EA~Horas, data=df,
        main="Horas", ylab=MyYlab)
plot(EA~SPLMax, data=df,
     main="SPLMax", ylab=MyYlab)
plot(EA~Puntos, data=df,
     main="Puntos", ylab=MyYlab)
par(op)



#correlation per horas in puntos
corhp<-(1.946)^2/((1.946)^2+(6.269)^2)
corhp
## due to high correlation with SPLMax in summary
# it should be centralized p155

#### Model Validation ####
Final<-MW
E<- resid(Final, type="normalized")
Fi<- fitted(Final)
op<- par(mfrow=c(2, 2), mar=c(4, 4, 3, 2))
MyYlab<- "Residuals"
plot(x=Fi, y=E, xlab="Fitted values", ylab=MyYlab)
boxplot(E~Horas, data=df,
        main="Horas", ylab=MyYlab)
plot(E~zSPL, data=df,
     main="SPLMax", ylab=MyYlab)
plot(E~Horas*zSPL, data=df,
     main="interaction", ylab=MyYlab)
boxplot(E~Puntos, data=df,
        main="Puntos", ylab=MyYlab)
par(op)

#everything is better but not necessarilly convincing

library(lattice)
xyplot(E~Puntos|Horas,
       data=df, ylab="Residuals",
       
       panel=function(x,y){
         panel.grid(h=-1, v=2)
         panel.points(x, y, col=1)
         panel.loess(x, y, span=0.5, col=1,lwd=2)})
