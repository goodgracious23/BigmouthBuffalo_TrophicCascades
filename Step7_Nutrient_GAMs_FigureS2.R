# ========= PACKAGES ========== #
if (!require(mgcv)) install.packages('mgcv')
library(mgcv)

#=============================================================
# Be sure to run Step1_Munging.R first
nutrient_gam = bmb %>%
  filter(!(is.na(tp))) %>%
  mutate(treatment = as_factor(treatment), 
         fishtreat = ordered(treatment, 
                             levels = c("nofish", "harvested", "ambient")))


#Total Phosphorus GAM =====================
tp_gam <- gam(tp ~ treatment + s(doy, k = 12) + s(doy, by = fishtreat), 
               data = nutrient_gam, method = 'REML')
summary(tp_gam)
# gam.check(tp_gam)

#Total Nitrogen GAM =======================
tn_gam <- gam(tn ~ treatment + s(doy, k = 12) + s(doy, by = fishtreat), 
              data = nutrient_gam, method = 'REML')
summary(tn_gam)
# gam.check(tn_gam)

#============================================================
#Set up the plotting window
windows(height = 4.5, width = 6.5)
par(mfrow = c(2,3), 
    omi = c(0.4,0.5,0.1,0.1), 
    mai = c(0.2,0.3,0.2,0.1))

#Colors for GAM plots
high1 = rgb(102,205,170, max = 255, alpha = 200)
high2 = "aquamarine3" 
low1 = rgb(65, 105, 225, max = 255, alpha = 180)
low2 = "royalblue"
no1 = rgb(153,153,153, max = 255, alpha = 180)
no2 = "gray60" 


#Plot of the chlorophyll reference (no fish) smooths
plot(tp_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tp_gam)[1],
     se=TRUE, residuals=TRUE, all.terms=TRUE, shade=TRUE, rug=FALSE,
     shade.col=no1, cex = 0, 
     xlab = "", ylab = "", cex.axis= 1.2)
text(150, 60, "edf = 7.046***", col = "gray40", cex = 1.2, font = 4)
mtext(side = 3, line = 0.5, "Reference Smooth")
mtext(side = 2, line = 3, "Total Phosphorus")

#plot of the difference smooths for the harvested and ambient densities
plot(tp_gam, select = 2,  
     seWithMean = TRUE, shift = coef(chl_gam)[1],
     se=T, residuals=T, all.terms=TRUE,shade=TRUE,rug=F,
     shade.col = low1, cex = 0, cex.axis = 1.2, 
     main = "", xlab = "", ylab = "")
lines(c(124,124), c(-50,500), lty = 3) #fish addition line
lines(c(128,128), c(-50,500), lty = 3) #fish addition line

par(new = TRUE) #add new smooth to the same plot
plot(tp_gam, select = 3,  
     seWithMean = TRUE, shift = coef(chl_gam)[1],
     se=T, residuals=T, all.terms=TRUE,shade=TRUE,rug=F,
     shade.col=high1, cex = 0, 
     main = "", xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
mtext(side = 3, line = 0.5, "Difference Smooths")
text(150, 38, "edf = 1.396", col = low2, cex = 1.2, font = 1) #summary() results
text(150, 30, "edf = 1.001", col = high2, cex = 1.2, font = 1) #summary() results

#parametric effects plot
plot(c(1,2,3), c(0, -1.783, 2.171), 
     col = c(no2, low2, high2), pch = 15, cex = 2,
     xlim = c(0.5,3.5), ylim = c(-4,4), xaxt = "n", cex.axis = 1.2)
axis(1, at = c(1,2,3), labels = c("Ref","Harv","Amb"), cex.axis = 1.2)
#connect the reference point to the two other treatments
lines(c(1,2), c(0,-1.783), lty = 2)
lines(c(1,3), c(0, 2.171), lty = 2)
points(c(1,2,3), c(0, -1.783, 2.171), 
       col = c(no2, low2, high2), pch = 15, cex = 2)
#Plot the error terms to the parametric estimates
lines(c(2,2), c(-1.783+1.924, -1.783-1.924), col = low2, lwd = 2)
lines(c(3,3), c(2.171+1.924, 2.171-1.924), col = high2, lwd = 2)
mtext(side = 3, line = 0.5, "Parametric Effects")


#====================================================
plot(tn_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tn_gam)[1],
     se=TRUE, residuals=TRUE, all.terms=TRUE, shade=TRUE, rug=FALSE,
     shade.col=no1, cex = 0, cex.axis = 1.2,
     xlab = "", ylab = "")
text(150, 25, "edf = 5.520***", col = "gray40", cex = 1.2, font = 4)
mtext(side = 2, line = 3, "Total Nitrogen")

plot(tn_gam, select = 2,  
     seWithMean = TRUE, shift = coef(tn_gam)[1],
     se=T, residuals=T, all.terms=TRUE,shade=TRUE,rug=F,
     shade.col=low1, cex= 0, cex.axis= 1.2,
     main = "", xlab = "", ylab = "")
lines(c(124,124), c(-5000,50000), lty = 3) #fish addition line
lines(c(128,128), c(-5000,50000), lty = 3)

par(new = TRUE)
plot(tn_gam, select = 3,  
     seWithMean = TRUE, shift = coef(tn_gam)[1],
     se=T, residuals=T, all.terms=TRUE,shade=TRUE,rug=F,
     shade.col=high1, cex = 0, 
     main = "", xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
text(150, 26, "edf = 1.163", col = low2, cex = 1.2, font = 1)
text(150, 22, "edf = 1.001", col = high2, cex = 1.2, font = 1)

plot(c(1,2,3), c(0, -0.7477, -0.9708), #ref, low, high treatments
     col = c(no2, low2, high2), pch = 15, cex = 2, cex.axis = 1.2,
     xlim = c(0.5,3.5), ylim = c(-2, 2), xaxt = "n")
axis(1, at = c(1,2,3), labels = c("Ref","Harv","Amb"), cex.axis = 1.2)
lines(c(1,2), c(0,-0.7477), lty = 2)
lines(c(1,3), c(0, -0.9708), lty = 2)
points(c(1,2,3), c(0, -0.7477, -0.9708), 
       col = c(no2, low2, high2), pch = 15, cex = 2)
lines(c(2,2), c(-0.7477+ 0.8970, -0.7477- 0.8970), col = low2, lwd = 2)
lines(c(3,3), c(-0.9708+ 0.8970, -0.9708- 0.8970), col = high2, lwd = 2)
