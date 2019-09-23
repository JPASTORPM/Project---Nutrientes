#------------------------------------------------
# Script: HAs
# Autor: Junior Pastor Pérez-Molina
# Date: 09-20-2019
#------------------------------------------------


#------------------------------------------------
# Initial steps
#------------------------------------------------
rm(list = ls()) # Remove all objects
graphics.off()  # Remove all graphics
cat("\014")     # Remove script in console
#------------------------------------------------



#------------------------------------------------
dat<-matrix(c(0.9,0.8,0.7, 0.7,0.6, 0.5, 0.6,0.55,0.5, 0.2, 0.1,0.1),nrow = 3, ncol = 4)
library(plot3D) #exemple(plot3D)
nr <- r<- nrow(dat)
nc <- c<- ncol(dat)
a<-image2D(z = dat, x = 1:nr, y = 1:nc, lwd = 2, shade = 0.2, rasterImage = TRUE,main = "HAs", clab = "Nivel")
#------------------------------------------------

#------------------------------------------------






