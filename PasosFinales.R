#### Adjustments ####
tibby1<-as_tibble(All1min)
head(tibby1)
names(tibby1)[9]="BI"
names(tibby1)[8]="ACI"
names(tibby1)[10]="NDSI"
names(tibby1)[11]="H"
names(tibby1)[13]="TFSD"
names(tibby1)[15]="RelAmp"
tibby1<-tibby1 %>% relocate(NDSI, .after = RelAmp)
varlist <- names(tibby1)[6:14]
tibby1$Puntos<-factor(tibby1$Puntos)
tibby1$Horas<-factor(tibby1$Horas, levels = c("4-6", "11-13", "16-18"))

#### Scatter plots and histograms  ####
Col <-viridis
dat  <-data.frame( Col(10)[as.numeric(cut(tibby1$SP50m,breaks = 15))])

par(mfrow=c(3,5),  
    mar = c(2.5, 1.8, 1.5, 1.5),
    mgp=c(1,0.1,0))

for (i in 2:15) {
  if (names(tibby1)[i] %in% colnames(rangesai)){
    n=names(tibby1)[i]
    yl=c(rangesai[,n])
    if(n=="ACI" || n=="BIO"){yl=NULL}
  }
  else {yl=NULL}
  plot(tibby1$hnum, (tibby1[[i]] ),
       xlab="Hora: 4:00 am - 6:00 pm", ylab= names(tibby1)[i],
       ylim = yl,
       col=(dat$Col), cex=0.5, cex.axis=0.5,
       main=paste(names(tibby1)[i], "vs. Horas", sep=" "), 
       cex.main=0.7, tck = 0.05, cex.lab=0.7,
       xaxp = c(4, 18, 14))
}

for(n in 2:15) {
  hist(tibby1[[n]] ,
       xlab= names(tibby1)[n],
       col=(dat$Col), cex=0.5, cex.axis=0.5, cex.label = 0.7, cex.main=0.8,
       main=paste(names(tibby1)[n], "Distribucion", sep=" "))
}


#### ggplot regression lines ####
library(viridis)
library(purrr) 
xlab <- "Hora"

ggplot(tibby1) + 
  aes(x = hnum, y = AEI, Facet=Puntos, color=Puntos) + 
  stat_smooth( method = "lm", se = T, size=0.1) +
  # Put the points on top of lines
  geom_point(size = 0.5) +
  labs(x = xlab) +
  scale_color_viridis(discrete = T)+
  theme_classic()

reglines = function(d, ai) {
  ggplot(d) + 
    aes(x = hnum, y = .data[[ai]], Facet=Puntos, color=Puntos) + 
    stat_smooth(method = "lm", se = T, size=0.3, alpha=0.4) +
    geom_point(size = 0.05) +
    labs(x = xlab) +
    scale_color_viridis(discrete = T)+
    theme_classic()+
    theme(text=element_text(size=8),   
          legend.position = "none")
}

reglines(tibby1, "AEI")

#map(varlist, ~reglines(tibby1, .x) )

line_plots
grid.arrange(reglines(tibby1, "ACI"), reglines(tibby1, "BI"),
             reglines(tibby1, "ADI"), reglines(tibby1, "AEI"), 
             reglines(tibby1, "H")
             )



#### Simple GLM and Tukey ####
#NDSI has neg values
mGLM<- glm(NDSI~Horas, data=tibby1)
summary(mGLM)
varlist
glms <- lapply(varlist, function(x) {
  glm(substitute(i ~ Horas, list(i = as.name(x))),
      data=tibby1, family=Gamma(link = "inverse")) })
lapply(glms, summary)

library(lsmeans)
lsmeans(mGLM, pairwise~Horas, adjust="tukey") 

lapply(c(5:13), function(x){
  lsmeans(glms[[x]], pairwise~Horas, adjust="tukey")   })

#### glmm and montecarlo? or someway to compare ####
library(lme4)

mm <- lmer(NDSI ~ Horas + (1|Puntos), data=tibby1)
summary(mm)
summary(mm, cor=F) 

m0 <- lmer(NDSI ~ 1 + (1|Puntos), data=tibby1)
anova(mm,m0,mmr, mGLM)
str(tibby1)
library(lmerTest)
mmr <- lmer(ACI ~ hnum + (1|Puntos), REML = F, data=tibby1)
mm
anova(mm)

mmlr=lapply(c(6:15), function(x){ 
  lmer(tibby1[[x]] ~ tibby1$Horas + (1|tibby1$Puntos))  })

mml=lapply(c(6:15), function(x){ 
  lmer(tibby1[[x]] ~ tibby1$Horas + (1|tibby1$Puntos), REML = F)  })

mml1=lapply(c(6:15), function(x){ 
  lmer(tibby1[[x]] ~ 1 + (1|tibby1$Puntos))  })

lapply(c(1:9), function(x){ anova(mml[[x]], mmlr[[x]], mml1[[x]], glms1[[1]], refit=F ) } )

glms1=lapply(c(6:14), function(x){ 
  glm(tibby1[[x]] ~ tibby1$Horas, family=Gamma(link = "inverse"))
})

anova(mmlr[[1]], glms1[[1]], type = 1, test = "LRT", refit=F)





