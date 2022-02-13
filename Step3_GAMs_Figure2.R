# Ordered Factor Smooth Comparison
# Code from: https://fromthebottomoftheheap.net/2017/12/14/difference-splines-ii/
# 30 Sept 2021

# ========= PACKAGES ========== #
if (!require(mgcv)) install.packages('mgcv')
library(mgcv)

#=============================================================
# Be sure to run Step1_Munging.R first
bmb_gam = bmb %>%
  # filter(doy>128) %>%
  mutate(treatment = as_factor(treatment), 
         fishtreat = ordered(treatment, 
                             levels = c("nofish", "harvested", "ambient")))

#============================================================
#Set up the plotting window
windows(height = 6.5, width = 6.5)
par(mfrow = c(3,3), 
    omi = c(0.4,0.5,0.1,0.1), 
    mai = c(0.2,0.3,0.2,0.1))

#Colors for GAM plots
high1 = rgb(102,205,170, max = 255, alpha = 200)
high2 = "aquamarine3" 
low1 = rgb(65, 105, 225, max = 255, alpha = 180)
low2 = "royalblue"
no1 = rgb(153,153,153, max = 255, alpha = 180)
no2 = "gray60" 

#Chlorophyll GAM
chl_gam <- gam(chl ~ treatment + s(doy, k = 40) + s(doy, by = fishtreat), 
               data = bmb_gam, method = 'REML')
summary(chl_gam)
# gam.check(chl_gam)

#Plot of the chlorophyll reference (no fish) smooths
plot(chl_gam, select = 1, 
     seWithMean = TRUE, shift = coef(chl_gam)[1],
     se=TRUE, residuals=TRUE, all.terms=TRUE, shade=TRUE, rug=FALSE,
     shade.col=no1, cex = 0,
     xlab = "", ylab = "", cex.axis= 1.2)
text(150, 29, "edf = 25.587***", col = "gray40", cex = 1.2, font = 4)
mtext(side = 3, line = 0.5, "Reference Smooth")
mtext(side = 2, line = 3, "Chlorophyll a")

#plot of the difference smooths for the harvested and ambient densities
plot(chl_gam, select = 2,  
     seWithMean = TRUE, shift = coef(chl_gam)[1],
     se=T, residuals=T, all.terms=TRUE,shade=TRUE,rug=F,
     shade.col=low1, cex= 0, cex.axis= 1.2,
     main = "", xlab = "", ylab = "")
lines(c(124,124), c(-50,500), lty = 3) #fish addition line
lines(c(128,128), c(-50,500), lty = 3) #fish addition line

par(new = TRUE) #add new smooth to the same plot
plot(chl_gam, select = 3,  
     seWithMean = TRUE, shift = coef(chl_gam)[1],
     se=T, residuals=T, all.terms=TRUE,shade=TRUE,rug=F,
     shade.col=high1, cex = 0, 
     main = "", xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
mtext(side = 3, line = 0.5, "Difference Smooths")
text(150, 29, "edf = 6.406***", col = low2, cex = 1.2, font = 4) #summary() results
text(150, 24, "edf = 6.837***", col = high2, cex = 1.2, font = 4) #summary() results

#parametric effects plot
plot(c(1,2,3), c(0, -0.5147, 0.6135), 
     col = c(no2, low2, high2), pch = 15, cex = 2,
     xlim = c(0.5,3.5), ylim = c(-1,1), xaxt = "n", cex.axis = 1.2)
axis(1, at = c(1,2,3), labels = c("Ref","Harv","Amb"), cex.axis = 1.2)
#connect the reference point to the two other treatments
lines(c(1,2), c(0,-0.5147), lty = 2)
lines(c(1,3), c(0, 0.6135), lty = 2)
points(c(1,2,3), c(0, -0.5147, 0.6135), 
       col = c(no2, low2, high2), pch = 15, cex = 2)
#Plot the error terms to the parametric estimates
lines(c(2,2), c(-0.5147+0.2236, -0.5147-0.2236), col = low2, lwd = 2)
text(2, -0.5147+0.2236, "*", cex = 2)
lines(c(3,3), c(0.6135+0.2236, 0.6135-0.2236), col = high2, lwd = 2)
text(3, 0.6135+0.2236, "**", cex = 2)
mtext(side = 3, line = 0.5, "Parametric Effects")

#===================================================================
#Zooplankton Biomass GAM
zoop_gam <- gam(log10(total_zoop) ~ treatment + 
                 s(doy) + 
                 s(doy, by = fishtreat), 
               data = bmb_gam,method = 'REML')
summary(zoop_gam)
# gam.check(zoop_gam)

plot(zoop_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zoop_gam)[1],
     se=TRUE, residuals=TRUE, all.terms=TRUE, shade=TRUE, rug=FALSE,
     shade.col=no1, cex = 0, cex.axis = 1.2,
     xlab = "", ylab = "")
text(150, log10(7000), "edf = 1.000", col = "gray40", cex = 1.2)
mtext(side = 2, line = 3, "log(Zooplankton \nBiomass)")

plot(zoop_gam, select = 2,  
     seWithMean = TRUE, shift = coef(zoop_gam)[1],
     se=T, residuals=T, all.terms=TRUE,shade=TRUE,rug=F,
     shade.col=low1, cex= 0, cex.axis= 1.2,
     main = "", xlab = "", ylab = "")
lines(c(124,124), c(-5000,50000), lty = 3) #fish addition line
lines(c(128,128), c(-5000,50000), lty = 3)

par(new = TRUE)
plot(zoop_gam, select = 3,  
     seWithMean = TRUE, shift = coef(zoop_gam)[1],
     se=T, residuals=T, all.terms=TRUE,shade=TRUE,rug=F,
     shade.col=high1, cex = 0, 
     main = "", xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
text(150, log10(7500), "edf = 1.000*", col = low2, cex = 1.2, font = 4)
text(150, log10(4000), "edf = 4.448***", col = high2, cex = 1.2, font = 4)

plot(c(1,2,3), c(0, -0.13061, -0.16944), #ref, low, high treatments
     col = c(no2, low2, high2), pch = 15, cex = 2, cex.axis = 1.2,
     xlim = c(0.5,3.5), ylim = c(-0.3, 0.05), xaxt = "n")
axis(1, at = c(1,2,3), labels = c("Ref","Harv","Amb"), cex.axis = 1.2)
lines(c(1,2), c(0,-0.13061), lty = 2)
lines(c(1,3), c(0, -0.16944), lty = 2)
points(c(1,2,3), c(0, -0.13061, -0.16944), 
       col = c(no2, low2, high2), pch = 15, cex = 2)
lines(c(2,2), c(-0.13061+0.09994, -0.13061-0.09994), col = low2, lwd = 2)
lines(c(3,3), c(-0.16944+0.10051, -0.16944-0.10051), col = high2, lwd = 2)

#===============================================================
#Zooplankton Mean length GAM
length_gam <- gam(commLength ~ treatment + 
                  s(doy) + 
                  s(doy, by = fishtreat), 
                data = bmb_gam,method = 'REML')
summary(length_gam)
# gam.check(length_gam)

plot(length_gam, select = 1, 
     seWithMean = TRUE, shift = coef(length_gam)[1],
     se=TRUE, residuals=TRUE, all.terms=TRUE, shade=TRUE, rug=FALSE,
     shade.col=no1, cex = 0, cex.axis= 1.2,
     xlab = "", ylab = "")
text(150, 160, "edf = 5.004***", col = "gray40", cex = 1.2, font = 4)
mtext(side = 2, line = 3, "Abundance-Weighted \nMean Length")
mtext(side = 1, line = 3, "Day of Year")

plot(length_gam, select = 2,  
     seWithMean = TRUE, shift = coef(length_gam)[1],
     se=T, residuals=T, all.terms=TRUE,shade=TRUE,rug=F,
     shade.col=low1, cex= 0, cex.axis = 1.2,
     main = "", xlab = "", ylab = "")
lines(c(124,124), c(-50,500), lty = 3) #fish addition line
lines(c(128,128), c(-50,500), lty = 3)

par(new = TRUE)
plot(length_gam, select = 3,  
     seWithMean = TRUE, shift = coef(length_gam)[1],
     se=T, residuals=T, all.terms=TRUE,shade=TRUE,rug=F,
     shade.col=high1, cex = 0, 
     main = "", xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
mtext(side = 1, line = 3, "Day of Year")
text(150, 160, "edf = 2.557***", col = low2, cex = 1.2, font = 4)
text(150, 136, "edf = 2.784***", col = high2, cex = 1.2, font = 4)

plot(c(1,2,3), c(0, -8.594, -12.669), 
     col = c(no2, low2, high2), pch = 15, cex = 2, cex.axis = 1.2,
     xlim = c(0.5,3.5), ylim = c(-16,2), xaxt="n")
axis(1, at=c(1,2,3), labels = c("Ref", "Harv", "Amb"), cex.axis = 1.2)
lines(c(1,2), c(0,-8.594), lty = 2)
lines(c(1,3), c(0, -12.669), lty = 2)
points(c(1,2,3), c(0, -8.594, -12.669), 
       col = c(no2, low2, high2), pch = 15, cex = 2)
lines(c(2,2), c(-8.594+3.126, -8.594-3.126), col = low2, lwd = 2)
text(2, -8.594+3.126, "**", cex = 2)
lines(c(3,3), c(-12.669+3.109, -12.669-3.1099), col = high2, lwd = 2)
text(3, -12.669+3.109, "***", cex = 2)
mtext(side = 1, line = 3, "Treatment")
