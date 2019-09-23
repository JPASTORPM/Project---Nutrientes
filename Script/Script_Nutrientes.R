#------------------------------------------------
# Script: Nutrientes
# Autor: Junior Pastor PÃ©rez-Molina
# Date: 09-23-2019
#------------------------------------------------



#------------------------------------------------
# Initial steps
#------------------------------------------------
rm(list = ls()) # Remove all objects
graphics.off()  # Remove all graphics
cat("\014")     # Remove script in console
#------------------------------------------------



#------------------------------------------------
# Loading database
#------------------------------------------------
data<-read.table("Data/datos.txt", sep="\t", dec=".", header = TRUE)
str(data)
data$Nutriente2<-as.character(data$Nutriente)
data$Nutriente2[data$Nutriente=="Control"]<-"1_C"
data$Nutriente2[data$Nutriente=="N"]<-"2_N"
data$Nutriente2[data$Nutriente=="P"]<-"3_P"
data$Nutriente2[data$Nutriente=="NP"]<-"4_NP"
data<-na.omit(data)
#------------------------------------------------



#------------------------------------------------
# Packages
#------------------------------------------------
if(!require(yarrr)){githubinstall("yarrr")}
if(!require(openxlsx)){githubinstall("openxlsx")}
if(!require(broom)){githubinstall("broom")}
if(!require(lsmeans)){githubinstall("lsmeans")}
if(!require(multcomp)){githubinstall("multcomp")}
if(!require(multcomp)){githubinstall("multcompView")}
#------------------------------------------------



#------------------------------------------------
# Analisis estadísticos
#------------------------------------------------
fun.table.anova<-function(Name,variable, factor1, factor2){
  data.anova<-data.frame()
  model = aov(variable ~ factor1*factor2)
  summary_model<-summary(model)
  library(broom)
  inf<-round(glance(model),3)
  P.value<-round(as.numeric(inf[5]),3)
  R2<-round(as.numeric(inf[1]),2)
  F<-round(as.numeric(inf[4]),1)
  coef<-data.frame((summary_model[[1]][5]))
  coef<-na.omit(coef)
  names(coef)<-c("Factor")
  Nutrientes<-round(coef[1,], 3)
  Luz<-round(coef[2,], 3)
  NutrientesXLuz<-round(coef[3,], 3)
  P<-data.frame(Nutrientes, Luz, NutrientesXLuz)
  
  library(lsmeans)
  lsm = lsmeans(model, pairwise ~ factor1*factor2, adjust="B")
  t1<-data.frame(cld(lsm[[1]], alpha=.05, Letters=letters))
  t2<-data.frame(t1[c(1)],t1[c(2)],t1[c(3)],t1[c(4)],t1[c(8)])
  mean2<-paste(round(t2$lsmean, 3), round(t2$SE, 3), sep=" ± ")
  mean1<-paste(t2$factor1, t2$factor2, sep="*")
  mean3<-paste(mean2,t(t2$".group"), sep=" ")
  mean<-data.frame(mean1,mean3)  
  mean<-mean[order(mean$mean1),]  
  mean<-data.frame(t(mean))
  names(mean)<-c("C*Alta","C*Baja","N*Alta" , "N*Baja","P*Alta","P*Baja", "NP*Alta" ,"NP*Baja")
  mean<-mean[-1,]
  a<-shapiro.test(model$residuals)
  P_value_Shapiro_Wilk<-a$p.value
  a<-ifelse(a$p.value>0.05, "Normalidad de residuos", "Residuos asimétricos")
  
  data.anova <- rbind(data.anova,data.frame(P_value_Shapiro_Wilk, a, Name, mean, P, F, R2, P.value))
  ###
  return(data.anova)
}
#------------------------------------------------
BT<-fun.table.anova(Name="BT",variable= data$BT, factor1= data$Nutriente2, factor2= data$Luz)
RHT<-fun.table.anova(Name="RHT",variable= data$RHT, factor1= data$Nutriente2, factor2= data$Luz)
RRT<-fun.table.anova(Name="RRT",variable= data$RRT, factor1= data$Nutriente2, factor2= data$Luz)
RRV<-fun.table.anova(Name="RRV",variable= data$RRV, factor1= data$Nutriente2, factor2= data$Luz)
#------------------------------------------------



#------------------------------------------------
# Cuadro 1
#------------------------------------------------
Cuadro_1<-merge(BT, RHT, all=TRUE)
Cuadro_1<-merge(Cuadro_1, RRT, all=TRUE)
Cuadro_1<-merge(Cuadro_1, RRV, all=TRUE)
#------------------------------------------------
write.xlsx(Cuadro_1, "Results/Cuadro 1.xlsx",
           sheetName="Cuadro 1",col.names=TRUE,
           row.names=FALSE, append=FALSE,
           showNA=TRUE, password=NULL)
#------------------------------------------------




#------------------------------------------------
# Figure
#------------------------------------------------
pdf(file = "Results/Fig. 1.pdf",
    width = 10, height = 10)
#------------------------------------------------
par(xpd = FALSE,mfrow=c(2,2),mgp = c(2.5,0.5,0), mar = c(6.5,4,1,1))
pirateplot(formula = BT ~ Luz + Nutriente2, data = data, 
           main = "", xlab = "", ylab = "Peso seco total (mg, ±SE)",
           ylim=c(0.0,1.6),point.pch=NA, 
           bar.f.col=c("white", "gray45"),avg.line.fun=mean,plot=TRUE,
           theme = 4, gl.col = NA,
           avg.line.col = "gray10",
           inf.f.col = "gray10",
           jitter.val = 0.09,
           avg.line.o = 1,bar.b.o=1,
           avg.line.lwd=3,
           inf.lwd=1,
           inf.method="se")
par(xpd = TRUE)
#------------------------------------------------
pirateplot(formula = RHT ~ Luz + Nutriente2, data = data, 
           main = "", xlab = "", ylab = "Relación peso seco hojas/total (±SE)",
           ylim=c(0.0,0.8),point.pch=NA, 
           bar.f.col=c("white", "gray45"),avg.line.fun=mean,plot=TRUE,
           theme = 4, gl.col = NA,
           avg.line.col = "gray10",
           inf.f.col = "gray10",
           jitter.val = 0.09,
           avg.line.o = 1,bar.b.o=1,
           avg.line.lwd=3,
           inf.lwd=1,
           inf.method="se")
par(xpd = TRUE)
#------------------------------------------------
pirateplot(formula = RRT ~ Luz + Nutriente2, data = data, 
           main = "", xlab = "", ylab = "Relación peso seco raíz/total (±SE)",
           ylim=c(0.0,0.8),point.pch=NA, 
           bar.f.col=c("white", "gray45"),avg.line.fun=mean,plot=TRUE,
           theme = 4, gl.col = NA,
           avg.line.col = "gray10",
           inf.f.col = "gray10",
           jitter.val = 0.09,
           avg.line.o = 1,bar.b.o=1,
           avg.line.lwd=3,
           inf.lwd=1,
           inf.method="se")
par(xpd = TRUE)
#------------------------------------------------
pirateplot(formula = RRV ~ Luz + Nutriente2, data = data, 
           main = "", xlab = "", ylab = "Relación peso seco raíz/Vástago (±SE)",
           ylim=c(0.0,0.6),point.pch=NA, 
           bar.f.col=c("white", "gray45"),avg.line.fun=mean,plot=TRUE,
           theme = 4, gl.col = NA,
           avg.line.col = "gray10",
           inf.f.col = "gray10",
           jitter.val = 0.09,
           avg.line.o = 1,bar.b.o=1,
           avg.line.lwd=3,
           inf.lwd=1,
           inf.method="se")
par(xpd = TRUE)
#------------------------------------------------
dev.off()
#------------------------------------------------


