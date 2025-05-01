# 1. Start with a linear regression model 
# that contains as many explanatory variables 
# and their interactions as possible.
base<-lm(ACI~Horas*SPLMax*Rel.Amp*SPLMed*Puntos, data=df1)

# 2. Investigate whether the homogeneity assumptions are valid
# by plotting the standardised residuals versus fitted
# values and by plotting the standardised residuals versus
# each individual explanatory variable. 
MA=gls11
EA<- resid(MA)
FA<- fitted(MA)
MyYlab<- "Residuals"
plot(x=FA, y=EA, xlab="Fitted values", ylab=MyYlab)
boxplot(EA~Horas, data=df1,
        main="Horas", ylab=MyYlab)
plot(EA~SPLMax, data=df1,
     main="SPLMax", ylab=MyYlab)
plot(EA~SPLMax, data=df1,
     main="SPLMax", ylab=MyYlab)
plot(EA~Rel.Amp, data=df1,
     main="Rel.Amp", ylab=MyYlab)
plot(EA~Puntos, data=df1,
     main="Puntos", ylab=MyYlab)
vif(MA)
# 3. For formal model comparison, repeat step 1 using 
# the gls function from the nlme package. Do not 
# specify any special variance structure yet and
# ensure that REML estimation is used

gls1<-gls(ACI~Horas*Rel.Amp *SPLMax*Puntos,
          method="ML", data=df1)
gls11<-gls(ACI~Horas*Rel.Amp *SPLMax,
          method="ML", data=df1)
anova(gls1,gls11)
# 4. Depending on the graphical model validation in step 1,
# choose an appropriate variance structure. It helps to
# plot residuals versus fitted values and use different
# colours and symbols

gls2<-gls(ACI~Horas*SPLMax*Rel.Amp,
          method="ML", data=df1,
          weights=varIdent(form=???1|Horas) )
MA=gls2
EA<- resid(MA, type = "normalized")
FA<- fitted(MA, type = "normalized")
plot(x=FA, y=EA, xlab="Fitted values", ylab=MyYlab, 
     main="form=~smax.h")
AIC(MA,gls1)
anova(MA, gls1)
boxplot(EA~Horas, data=df1,
        main="Horas", ylab=MyYlab)
plot(EA~SPLMax, data=df1,
     main="SPLMax", ylab=MyYlab)
# plot(EA~SPLMax, data=df1,
#      main="SPLMax", ylab=MyYlab)
plot(EA~Rel.Amp, data=df1,
     main="Rel.Amp", ylab=MyYlab)
plot(EA~Puntos, data=df1,
     main="Puntos", ylab=MyYlab)

identify(FA, EA)
hist(EA, nclass=15)

xyplot(EA~SPLMax|Horas,
       data=df1, ylab="Residuals",
       
       panel=function(x,y){
         panel.grid(h=-1, v=2)
         panel.points(x, y, col=1)
         panel.loess(x, y, span=0.5, col=1,lwd=2)})
coplot(EA???SPLMax|Horas, data=df1,
       ylab="Normalised residuals")

# Fit a new gls model with the selected variance covariance 
# structure selectedin step 3. Ensure that REML estimation 
# is used, which is done with gls(...,method ="REML"), and 
# that you use the same selection of explanatoryvariables. This
# is now called the fixed part of the model, and the residuals
# are called the random part. We will first try to find the
# optimal random structureusing as many explanatory variables 
# in the fixed part as possible.
gls3<-gls(ACI~Horas+SPLMax+Rel.Amp+
            Horas:SPLMax + Horas:Rel.Amp+SPLMax:Rel.Amp+
            Horas:SPLMax:Rel.Amp,
          method="ML", data=df1,
          weights=varIdent(form=???1|Horas) )
vif(gls3)
anova(gls4)
anova(gls6,gls4)
gls4<-gls(ACI~Horas+SPLMax+Rel.Amp+
            Horas:SPLMax + Horas:Rel.Amp+SPLMax:Rel.Amp,
          method="ML", data=df1,
          weights=varIdent(form=???1|Horas) )

gls5<-gls(ACI~Horas+SPLMax+Rel.Amp+
            Horas:SPLMax + Horas:Rel.Amp,
          method="ML", data=df1,
          weights=varIdent(form=???1|Horas) )
SPLMax10=(df1$SPLMax)/10
gls6<-gls(ACI~Horas*SPLMax,
          method="ML", data=df1,
          weights=varIdent(form=???1|Horas) )
summary(gls5)
max(df1$ACI)
boxplot(predict(MW22c)???Horas,
        data=df1)
boxplot(resid(MWd)???Horas*Puntos,
        data=df1)
AIC(MW22c, MW22cc)
hour(df$Hora)
plot(ACI~hour(Hora), data=df)
plot(ACI~Puntos, data=df)

acf(residuals(base))

acf(residuals(MW22cc, type="normalized"))
MW22cc<-gls(ACI~hour(Hora), ##order of puntos horas doesn't matter
            weights=varIdent(form=~1|hour(Hora)),
           method="REML", data=df1,
           correlation=corAR1(form=~1|hour(Hora)/Puntos))
anova(MW3n, MW3n1)
summary(MW2c)

MW3n<-gls(NDSI~Horas*zSPL, #this seems to be best in xy plots
          weights=varIdent(form=~1|Horas*Puntos),
          method="REML", data=df,
          correlation=corAR1(form=~1|Puntos))
MW3n1<-gls(NDSI~Horas*zSPL, #this seems to be best in xy plots
          weights=varIdent(form=~1|Horas),
          method="REML", data=df,
          correlation=corAR1(form=~1|Puntos/Horas))

MA=MW2c
EA<- resid(MA, type = "normalized")
FA<- fitted(MA, type = "normalized")
plot(x=FA, y=EA, xlab="Fitted values", ylab=MyYlab)

boxplot(EA~Horas, data=df1,
        main="Horas", ylab=MyYlab)
plot(EA~SPLMax, data=df1,
     main="SPLMax", ylab=MyYlab)
# plot(EA~SPLMax, data=df1,
#      main="SPLMax", ylab=MyYlab)
plot(EA~Rel.Amp, data=df1,
     main="Rel.Amp", ylab=MyYlab)
plot(EA~Puntos, data=df1,
     main="Puntos", ylab=MyYlab)

identify(FA, EA)
hist(resid(lm(ADI~Horas, data=df)), nclass=15)

xyplot(EA~Puntos|Horas,
       data=df1, ylab="Residuals",
       
       panel=function(x,y){
         panel.grid(h=-1, v=2)
         panel.points(x, y, col=1)
         panel.loess(x, y, span=0.5, col=1,lwd=2)})
coplot(EA???Puntos|Horas, data=df1,
       ylab="Normalised residuals")

zSPL=scale(df$SPLMax)
