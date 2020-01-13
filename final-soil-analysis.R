
########################################
# Script for generating results for manuscript:
#        Land use changes influence soil bulk density and pH but not soil carbon stocks in an Amazonian Oxisol
#
# Rodrigo Pinheiro Bastos, Inge Stupak, Robyn Margaret Stuart, Joao Baptista Silva Ferraz, Karsten Raulund-Rasmussen
# Date last modified: Jan 13, 2020
########################################

########################################
# Load libraries
########################################

library (nlme)
library (emmeans)
library (multcomp)
library (lme4)
library (xlsx)
library (gof)
library (ggplot2)
library (readxl)
library (reshape)

########################################
# Set global variables and read in data
########################################
data <- read_excel("Amazon processed data.xlsx")
data$Land.use = factor(data$`Land use`)
data = data[data$Land.use != "Brazil nut",] # Remove Brazil nut
data$Land.use = factor(data$Land.use)

data$Depth = factor(data$Depth)
data$Site = factor(data$Site)
data$C = data$C10
data$N = data$N100
data$CN = data$C100/data$N100
data$logitC = log((data$C/100)/(1-data$C/100))
data$logitN = log((data$N/100)/(1-data$N/100))
data$logitPh = log(((data$pH-3)/100))/(1-((data$pH-3)/100))
data$BD = data$`Bulk Density`
data$logitBD = log((data$`Bulk Density`/100)/(1-data$`Bulk Density`/100))
data$logitESM = log((data$ESMSoil/1000)/(1-data$ESMSoil/1000))
data$logitNESM = log((data$NESM/100)/(1-data$NESM/100))
data$Ref.Cumul.Mineral.Stock = data$`Ref Cumul Mineral Stock`
data$Ref.Cumul.C.Stock = data$`Ref Cumul C Stock`
data$Ref.Cumul.N.Stock = data$`Ref Cumul N Stock`
data$Cumul.Min.Stock = data$`Cumul Mineral Stock`
data$Cumul.C.Stock = data$`Cumul C stock`
data$Cumul.N.Stock = data$`Cumul N stock`
data$East.distance = data$`East distance`
data$North.distance = data$`North distance`


########################################
# Create summary statistic tables
########################################
depvars = with(data=data,data.frame(C, N, CN, `Bulk Density`, pH, ESMSoil, NESM, Ref.Cumul.Mineral.Stock, Land.use, Depth2))
moltenvars = melt(depvars, id=c("Land.use","Depth2"))
st.err <- function(x) { sd(x)/sqrt(length(x)) }
means <- aggregate(moltenvars$value, list(moltenvars$Depth2, moltenvars$Land.use, moltenvars$variable), mean)
stdevs <- aggregate(moltenvars$value, list(moltenvars$Depth2, moltenvars$Land.use, moltenvars$variable), sd)
sterrs <- aggregate(moltenvars$value, list(moltenvars$Depth2, moltenvars$Land.use, moltenvars$variable), st.err)
sumstats <- cbind(means,stdevs[,4],sterrs[,4])[,c(3,2,1,4,5,6)]
colnames(sumstats) <- c("Soil parameter", "Land use", "Depth", "Mean", "St dev", "St err")
#write.xlsx(x = sumstats, file = "Results/Summary statistics.xlsx", sheetName = "Summary", row.names = FALSE)


# Run t tests comparing soil C and BC in the top 2 layers
level = "0-5" 
t.test(C~Land.use, data=subset(data[data$Depth == level,], Land.use %in% c("Pasture", "Primary forest")))
t.test(C~Land.use, data=subset(data[data$Depth == level,], Land.use %in% c("Pasture", "Secondary forest")))
t.test(C~Land.use, data=subset(data[data$Depth == level,], Land.use %in% c("Secondary forest", "Primary forest")))
t.test(BD~Land.use, data=subset(data[data$Depth == level,], Land.use %in% c("Pasture", "Primary forest")))
t.test(BD~Land.use, data=subset(data[data$Depth == level,], Land.use %in% c("Pasture", "Secondary forest")))
t.test(BD~Land.use, data=subset(data[data$Depth == level,], Land.use %in% c("Secondary forest", "Primary forest")))

level = "5-10"
t.test(C~Land.use, data=subset(data[data$Depth == level,], Land.use %in% c("Pasture", "Primary forest")))
t.test(C~Land.use, data=subset(data[data$Depth == level,], Land.use %in% c("Pasture", "Secondary forest")))
t.test(C~Land.use, data=subset(data[data$Depth == level,], Land.use %in% c("Secondary forest", "Primary forest")))
t.test(BD~Land.use, data=subset(data[data$Depth == level,], Land.use %in% c("Pasture", "Primary forest")))
t.test(BD~Land.use, data=subset(data[data$Depth == level,], Land.use %in% c("Pasture", "Secondary forest")))
t.test(BD~Land.use, data=subset(data[data$Depth == level,], Land.use %in% c("Secondary forest", "Primary forest")))

# Data visualization of soil C
(g<- ggplot(aes(y = C, x = Land.use, col=Site), data = data[data$Depth2 %in% c(5,10,20,30),])+ #c("0-5","5-10","10-20","20-30"),])+ 
    geom_boxplot() + 
    geom_point(aes(y = C, x = Land.use), shape=16)+
    theme_bw() + 
    ylab("Soil C") +
    xlab("Land use") +
    facet_wrap(~Depth2, scales = "free") +
    theme(axis.text    = element_text(face = "bold"),
          legend.title = element_blank())
  )
fn <- "Figs/C_by_depth.png"
ggsave(fn, g, device="png", dpi=300)

(g<- ggplot(aes(y = BD, x = Land.use), data = data[data$Depth2 %in% c(5,10,20,30),])+ #c("0-5","5-10","10-20","20-30"),])+ 
    geom_boxplot() + 
    geom_point(aes(y = BD, x = Land.use), shape=16)+
    theme_bw() + 
    ylab("BD") +
    xlab("Land use") +
    facet_wrap(~Depth2, scales = "free") +
    #coord_cartesian(ylim = c(0, 5)) +
    theme(axis.text    = element_text(face = "bold"),
          legend.title = element_blank())
)
fn <- "Figs/BD_by_depth.png"
ggsave(fn, g, device="png", dpi=300)


########################################
# Make models for main soil parameters
########################################

######## Model for C
ss = setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("contrast","estimate","SE","df","t.ratio","p.value"))
mC0 <- lme(logitC~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="0-5"), na.action=na.exclude, method="ML")
drop1(mC0, test="Chisq")
emmC0 <- emmeans(mC0, "Land.use")
pairs(emmC0)
ss[1:3,] = data.frame(pairs(emmC0))

mC5 <- lme(logitC~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="5-10"), na.action=na.exclude, method="ML")
drop1(mC5, test="Chisq")
emmC5 <- emmeans(mC5, "Land.use")
pairs(emmC5)
ss[4:6,] = data.frame(pairs(emmC5))

mC10 <- lme(logitC~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="10-20"), na.action=na.exclude, method="ML")
drop1(mC10, test="Chisq")
emmC10 <- emmeans(mC10, "Land.use")
pairs(emmC10)
ss[7:9,] = data.frame(pairs(emmC10))

mC20 <- lme(logitC~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="20-30"), na.action=na.exclude, method="ML")
drop1(mC20, test="Chisq")
emmC20 <- emmeans(mC20, "Land.use")
pairs(emmC20)
ss[10:12,] = data.frame(pairs(emmC20))

mC30 <- lme(logitC~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="30-40"), na.action=na.exclude, method="ML")
drop1(mC30, test="Chisq")
emmC30 <- emmeans(mC30, "Land.use")
pairs(emmC30)
ss[13:15,] = data.frame(pairs(emmC30))

mC40 <- lme(logitC~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="40-80"), na.action=na.exclude, method="ML")
drop1(mC40, test="Chisq")
emmC40 <- emmeans(mC40, "Land.use")
pairs(emmC40)
ss[16:18,] = data.frame(pairs(emmC40))

mC80 <- lme(logitC~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="80-120"), na.action=na.exclude, method="ML")
drop1(mC80, test="Chisq")
emmC80 <- emmeans(mC80, "Land.use")
pairs(emmC80)
ss[19:21,] = data.frame(pairs(emmC80))

mC120 <- lme(logitC~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="120-160"), na.action=na.exclude, method="ML")
drop1(mC120, test="Chisq")
emmC120 <- emmeans(mC120, "Land.use")
pairs(emmC120)
ss[22:24,] = data.frame(pairs(emmC120))

mC160 <- lme(logitC~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="160-200"), na.action=na.exclude, method="ML")
drop1(mC160, test="Chisq")
emmC160 <- emmeans(mC160, "Land.use")
pairs(emmC160)
ss[25:27,] = data.frame(pairs(emmC160))

ss$depth = c(0,0,0,5,5,5,10,10,10,20,20,20,30,30,30,40,40,40,80,80,80,120,120,120,160,160,160)
ss$contrast = rep(c("PA/PF","PA/SF","PF/SF"),9)
ss$indicator = rep("Soil C",27)

soil_c_comparisons = ss[c("depth","contrast","p.value")]
write.xlsx(x = soil_c_comparisons, file = "Results/Soil C comparisons.xlsx", sheetName = "Summary", row.names = FALSE)

# Land use was not significant in explaining the variation in C (tested at individual depths)

######## Model for N
mN0 <- lme(logitN~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="0-5"), na.action=na.exclude, method="ML")
drop1(mN0, test="Chisq")
mN5 <- lme(logitN~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="5-10"), na.action=na.exclude, method="ML")
drop1(mN5, test="Chisq")
mN10 <- lme(logitN~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="10-20"), na.action=na.exclude, method="ML")
drop1(mN10, test="Chisq")
mN20 <- lme(logitN~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="20-30"), na.action=na.exclude, method="ML")
drop1(mN20, test="Chisq")
mN30 <- lme(logitN~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="30-40"), na.action=na.exclude, method="ML")
drop1(mN30, test="Chisq")
mN40 <- lme(logitN~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="40-80"), na.action=na.exclude, method="ML")
drop1(mN40, test="Chisq")
mN80 <- lme(logitN~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="80-120"), na.action=na.exclude, method="ML")
drop1(mN80, test="Chisq")
mN120 <- lme(logitN~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="120-160"), na.action=na.exclude, method="ML")
drop1(mN120, test="Chisq")
mN160 <- lme(logitN~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="160-200"), na.action=na.exclude, method="ML")
drop1(mN160, test="Chisq")
# Land use was not significant in explaining the variation in N (tested at individual depths)

######## Model for C/N
mCN0 <- lme(CN~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="0-5"), na.action=na.exclude, method="ML")
drop1(mCN0, test="Chisq")
mCN5 <- lme(CN~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="5-10"), na.action=na.exclude, method="ML")
drop1(mCN5, test="Chisq")
mCN10 <- lme(CN~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="10-20"), na.action=na.exclude, method="ML")
drop1(mCN10, test="Chisq")
# Land use was not significant in explaining the variation in C/N  (tested at individual depths)

######## Model for Bulk Density
mBD <- lme(logitBD~Land.use*Depth, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=data, na.action=na.exclude, method="ML")
mBD1 <- lme(logitBD~Depth, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=data, na.action=na.exclude, method="ML")
anova(mBD1,mBD)
drop1(mBD,test="Chisq")
# Land use was significant in explaining the variation in bulk density (Chisq(18) = 56.61, p<0.001)

######## Model for pH
mPh <- lme(logitPh~Land.use*Depth, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=data, na.action=na.exclude, method="ML")
mPh1 <- lme(logitPh~Depth, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=data, na.action=na.exclude, method="ML")
anova(mPh1,mPh)
drop1(mPh,test="Chisq")
# Land use was significant in explaining the variation in pH (Chisq(18) = 331.68, p<0.001)

######## Model for ESM
mESM <- lme(ESMSoil~Land.use*Depth, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=data, na.action=na.exclude, method="ML")
mESMn <- lme(ESMSoil~Depth, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=data, na.action=na.exclude, method="ML")

mESM0 <- lme(ESMSoil~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="0-5"), na.action=na.exclude, method="ML")
drop1(mESM0, test="Chisq")
mESM1 <- lme(ESMSoil~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="5-10"), na.action=na.exclude, method="ML")
drop1(mESM1, test="Chisq")
mESM2 <- lme(ESMSoil~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="10-20"), na.action=na.exclude, method="ML")
drop1(mESM2, test="Chisq")
mESM3 <- lme(ESMSoil~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="20-30"), na.action=na.exclude, method="ML")
drop1(mESM3, test="Chisq")
mESM4 <- lme(ESMSoil~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="30-40"), na.action=na.exclude, method="ML")
drop1(mESM4, test="Chisq")
mESM5 <- lme(ESMSoil~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="40-80"), na.action=na.exclude, method="ML")
drop1(mESM5, test="Chisq")
mESM6 <- lme(ESMSoil~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="80-120"), na.action=na.exclude, method="ML")
drop1(mESM6, test="Chisq")
mESM7 <- lme(ESMSoil~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="120-160"), na.action=na.exclude, method="ML")
drop1(mESM7, test="Chisq")
mESM8 <- lme(ESMSoil~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="160-200"), na.action=na.exclude, method="ML")
drop1(mESM8, test="Chisq")

######## Model for NESM
mNESM0 <- lme(NESM~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="0-5"), na.action=na.exclude, method="ML")
drop1(mNESM0, test="Chisq")
mNESM1 <- lme(NESM~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="5-10"), na.action=na.exclude, method="ML")
drop1(mNESM1, test="Chisq")
mNESM2 <- lme(NESM~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="10-20"), na.action=na.exclude, method="ML")
drop1(mNESM2, test="Chisq")
mNESM3 <- lme(NESM~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="20-30"), na.action=na.exclude, method="ML")
drop1(mNESM3, test="Chisq")
mNESM4 <- lme(NESM~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="30-40"), na.action=na.exclude, method="ML")
drop1(mNESM4, test="Chisq")
mNESM5 <- lme(NESM~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="40-80"), na.action=na.exclude, method="ML")
drop1(mNESM5, test="Chisq")
mNESM6 <- lme(NESM~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="80-120"), na.action=na.exclude, method="ML")
drop1(mNESM6, test="Chisq")
mNESM7 <- lme(NESM~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="120-160"), na.action=na.exclude, method="ML")
drop1(mNESM7, test="Chisq")
mNESM8 <- lme(NESM~Land.use, random=~1|Site, corr=corGaus(form=~East.distance+North.distance+Depth3|Site, nugget=TRUE), data=subset(data,Depth=="160-200"), na.action=na.exclude, method="ML")
drop1(mNESM8, test="Chisq")



########################################
# Do post-hoc tests on main soil parameters
########################################
depths = c("0-5","5-10","10-20","20-30","30-40","40-80","80-120","120-160","160-200")
depths2 = c(5,10,20,30,40,80,120,160,200)
ndepths = length(depths)
nlands = 3

# BD
for(d in 1:ndepths){
  aa <- cld(lsmeans(mBD,"Land.use",at=list(Depth=depths[d])),type="response")
  
  for (lu in levels(aa$Land.use)){
    for (thing in c("lsmean","lower.CL","upper.CL")){
      sumstats[sumstats$`Soil parameter`=="Bulk.Density"&sumstats$Depth==depths2[d]&sumstats$`Land use`==lu,thing]<- 100*(exp(aa[aa$Land.use==lu,thing])/(1+exp(aa[aa$Land.use==lu,thing])))
    }
    sumstats[sumstats$`Soil parameter`=="Bulk.Density"&sumstats$Depth==depths2[d]&sumstats$`Land use`==lu,"Group"]<- aa[aa$Land.use==lu,".group"]
  }
}

# pH
for(d in 1:ndepths){
  aa <- cld(lsmeans(mPh,"Land.use",at=list(Depth=depths[d])),type="response")
  
  for (lu in levels(aa$Land.use)){
    for (thing in c("lsmean","lower.CL","upper.CL")){
      sumstats[sumstats$`Soil parameter`=="pH"&sumstats$Depth==depths2[d]&sumstats$`Land use`==lu,thing]<- 100*(exp(aa[aa$Land.use==lu,thing])/(1+exp(aa[aa$Land.use==lu,thing]))) + 3
    }
    sumstats[sumstats$`Soil parameter`=="pH"&sumstats$Depth==depths2[d]&sumstats$`Land use`==lu,"Group"]<- aa[aa$Land.use==lu,".group"]
  }
}

write.xlsx(x = sumstats, file = "Results/Summary statistics.xlsx", sheetName = "Summary", row.names = FALSE)

confint(contrast(lsmeans(mBD,"Land.use",at=list(Depth="0-5")),"pairwise",type="response"))
confint(contrast(lsmeans(mPh,"Land.use",at=list(Depth="0-5")),"pairwise",type="response"))
confint(contrast(lsmeans(mPh,"Land.use",at=list(Depth="5-10")),"pairwise",type="response"))
confint(contrast(lsmeans(mPh,"Land.use",at=list(Depth="10-20")),"pairwise",type="response"))
confint(contrast(lsmeans(mPh,"Land.use",at=list(Depth="20-30")),"pairwise",type="response"))

lsmeans(mPh,"Land.use",at=list(Depth="0-5"),type="response")
