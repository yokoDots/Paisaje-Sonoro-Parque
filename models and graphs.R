#Transform and regular regression
logSPL <- log(pres$SPL50)
hist(logSPL)

lmLogSPL<-lm(logSPL~pres$hnum)
plot(logSPL~pres$hnum)
abline(lmLogSPL)
####GLM s  ######
glmSPL<-glm(SPL50~hnum, family="Gamma", data=pres)
glmLogSPL<-glm(logSPL~pres$hnum, family="Gamma")

lmSPL<-lm(SPL50~hnum, data=pres)
abline(lmSPL)
#glm(formula = vs ~ wt, family = binomial, data = mtcars)
plot(glmSPL)
glmSPL
summary(glmSPL)
summary(lmSPL)

#range(mtcars$wt)
range(pres$hnum)

rhnum <- seq(4.00, 19, 0.01)

#Now we use the predict() function to create the model for all of the values of xweight.
#yweight <- predict(model_weight, list(wt = xweight),type="response")
ySPL <- predict(glmSPL, list( hnum = rhnum), type="response")
yLogSPL <- predict(glmLogSPL, list( hnum = rhnum), type="response")

#plot(mtcars$wt, mtcars$vs, pch = 16, xlab = "WEIGHT (g)", ylab = "VS")
plot(all10$hnum, all10$SPL50, xlab = "Horas", ylab = "SPL (dB)")
lines(rhnum, ySPL)
lines(pres$hnum, yLogSPL)


#### VIF multicollinearity ####
VIF(M)
M <- lm(ACIsee~.,data=aiFactors[2:12])
Mch <- lm(ACIsee~hnum,data=aiFactors)
Mdh <- lm(ADI~hnum,data=aiFactors)
Meh <- lm(AEI~hnum,data=aiFactors)
Mhh <- lm(He~hnum,data=aiFactors)
Mmh <- lm(MAE~hnum,data=aiFactors)
Mth <- lm(Ht~hnum,data=aiFactors)
Mnh <- lm(NDSIsee~hnum,data=aiFactors)
Mnph <- lm(NP~hnum,data=aiFactors)
Mamph <- lm(Amp~hnum,data=aiFactors)
Mtsh <- lm(TFSDsee~hnum,data=aiFactors)
Mbh <- lm(BIO~hnum,data=aiFactors)

coef(Mtsh)
summary(Mmh)$r.squared
mods <- list(Mbh, Mch, Mamph, Mhh)

plot(BIO ~ hnum,
     data = aiFactors
     )

abline(Mbh,
       col="blue",
       lwd=2)
prd <- predict(lm(BIO ~ poly(hnum, 2), data=ai10))
aidd$prd<-prd

p1 <- ggplot(ai10, aes(x = hnum, y=BIO)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=0))

print(p1)

## check the model
p1 +
  geom_line(aes(y = prd), color="red")

## extrapolate based on model
pred <- data.frame(hnum=7:11)
pred$BIO <- predict(lm(BIO ~ poly(hnum, 2), data=ai10),newdata=pred)

p1 +
  geom_line(color="red", data=pred)

model.l = loess(BIO ~ hnum,
                data = aiFactors,
                span = 0.75,        ### higher numbers for smoother fits
                degree=2,           ### use polynomials of order 2
                family="gaussian")  ### the default, use least squares to fit

summary(model.l)
plot(model.l)

plotPredy(data  = aiFactors,
         x     = hnum,
         y     = BIO,
         model = model.l,
         xlab  = "Calories per day",
         ylab  = "Sodium intake per day")

library(quantreg)

model.q = rq(BIO ~ hnum,
             data = aiFactors,
             tau = 0.5)

summary(model.q)


### Values under Coefficients are used to determine the fit line.
### bd appears to be a confidence interval for the coefficients

model.null = rq(BIO ~ 1,
                data = aiFactors,
                tau = 0.5)

anova(model.q, model.null)

  ### p-value for model overall
  
  library(rcompanion)
nagelkerke(model.q)


#Plot with statistics
#spearman correlation????
  
plot(BIO ~ hnum,
     data = aiFactors
     )
#this was just model
abline(model.q,
       col="blue",
       lwd=2)
library(rcompanion)
Pvalue = anova(model.q, model.null)[[1]][1,4]
Intercept = as.numeric(summary(model.q)$coefficients[1,1])
Slope     = as.numeric(summary(model.q)$coefficients[2,1])
R2     = nagelkerke(model.q)[[2]][3,1]
#t3 and t4 just model
t1     = paste0("p-value: ", signif(Pvalue, digits=3))
t2     = paste0("R-squared: ", signif(R2, digits=3))
t3     = paste0("Intercept: ", signif(coefficients(model.q)[1], digits=3))
t4     = paste0("Slope: ", signif(coefficients(model.q)[2], digits=3))

text(1160, 2600, labels = t1, pos=4)
text(1160, 2500, labels = t2, pos=4)
text(1160, 2400, labels = t3, pos=4)
text(1160, 2300, labels = t4, pos=4)


require(mgcv)
model.g = gam(BIO ~ s(hnum),
              data = aiFactors,
              family=gaussian())

summary(model.g)

model.null = gam(BIO ~ 1,
                 data = aiFactors,
                 family=gaussian())

anova(model.g,
      model.null)

require(lmtest)

lrtest(model.g,
       model.null)

#  Plot with statistics

library(rcompanion)

plotPredy(data  = aiFactors,
          x     = hnum,
          y     = BIO,
          model = model.g,
          xlab  = "Calories per day",
          ylab  = "Sodium intake per day")

Pvalue    = 2.25e-14
R2        = 0.718

t1     = paste0("p-value: ", signif(Pvalue, digits=3))
t2     = paste0("R-squared (adj.): ", signif(R2, digits=3))

text(1160, 2600, labels = t1, pos=4)
text(1160, 2500, labels = t2, pos=4)

median(aiFactors$ACIsee)
median(ai10$ACIsee)
t.test(ai10$NDSIsee,aiFactors$NDSIsee)
#all variables are statistically equal for ai10 and aiFactors
