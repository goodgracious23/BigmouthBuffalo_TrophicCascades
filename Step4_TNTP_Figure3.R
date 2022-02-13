# ========= TREATMENT COLOR CODE ========= #
high1 = "aquamarine3" # Pond B - ambient density
high2 = "aquamarine4" #Pond F - ambient density
low1 = "royalblue" #Pond C - harvested density
low2 = "royalblue3" #Pond E - harvested density
no1 = "gray60" #Pond A - no fish
no2 = "gray30" #Pond D - no fish

bmb_tp = bmb %>% 
  filter(!(is.na(tp)))

windows(height = 4.5, width = 3.5)
par(mfrow = c(2,1), omi = c(0.7,0.3,0.1,0.1), mai = c(0.1,0.5,0.1,0.1))

#Total Phosphorus
plot(bmb_tp[bmb_tp$pond=="B", "doy"], bmb_tp[bmb_tp$pond=="B", "tp"], 
     col = high1, lwd = 2, type="l", las = 2,
     xlim = c(115,168), ylim = c(0,100), xaxt = 'n')
axis(1, at = c(120,130,140,150,160,170), labels = c("","","","","",""))
mtext(side = 2, line = 2.5, expression(Total~P~"("*mu*g~L^-1*")"))

#Fish addition polygons
polygon(c(115,123,123,115), c(85,85,100,100), col="gray20") 
lines(c(123, 123), c(-10,100), lwd=2, lty=3, col="gray20")
text(119, 92, "Pre", col="white", font=2)
lines(c(129.5, 129.5), c(-10,100), lwd=2, lty=3, col="gray20")
polygon(c(129,168,168,129), c(85,85,100,100), col="gray20") 
text(150, 92, "Post Fish", col="white", font=2)

#Other ponds TP
points(bmb_tp[bmb_tp$pond=="F", "doy"], bmb_tp[bmb_tp$pond=="F", "tp"], 
       col=high1, lwd=2, type="l")
points(bmb_tp[bmb_tp$pond=="C", "doy"], bmb_tp[bmb_tp$pond=="C", "tp"], 
       col=low1, lwd=2, type="l")
points(bmb_tp[bmb_tp$pond=="E", "doy"], bmb_tp[bmb_tp$pond=="E", "tp"], 
       col=low1, lwd=2, type="l")
points(bmb_tp[bmb_tp$pond=="A", "doy"], bmb_tp[bmb_tp$pond=="A", "tp"], 
       col=no1, lwd=2, type="l")
points(bmb_tp[bmb_tp$pond=="D", "doy"], bmb_tp[bmb_tp$pond=="D", "tp"], 
       col=no1, lwd=2, type="l")

#Total Nitrogen
plot(bmb_tp[bmb_tp$pond=="B", "doy"], bmb_tp[bmb_tp$pond=="B", "tn"], 
     col = high1, lwd = 2, type="l", las = 2, xaxt="n",
     xlim = c(115,168), ylim = c(0,32))
axis(side = 1, at = c(120,130,140,150,160,170))
mtext(side = 2, line = 2.5, expression(Total~N~"("*mg~L^-1*")"))
mtext(side = 1, line = 2.5, "Day of Year")

#Fish addition polygons
polygon(c(115,123,123,115), c(27,27,32,32), col="gray20") 
lines(c(123, 123), c(-10,100), lwd=2, lty=3, col="gray20")
text(119, 29.5, "Pre", col="white", font=2)
lines(c(129.5, 129.5), c(-10,100), lwd=2, lty=3, col="gray20")
polygon(c(129,168,168,129), c(27,27,32,32), col="gray20") 
text(150, 29.5, "Post Fish", col="white", font=2)

#Other Ponds TN
points(bmb_tp[bmb_tp$pond=="F", "doy"], bmb_tp[bmb_tp$pond=="F", "tn"], 
       col=high1, pch=19, lwd=2, type="l")
points(bmb_tp[bmb_tp$pond=="C", "doy"], bmb_tp[bmb_tp$pond=="C", "tn"], 
       col=low1, pch=19, lwd=2, type="l")
points(bmb_tp[bmb_tp$pond=="E", "doy"], bmb_tp[bmb_tp$pond=="E", "tn"], 
       col=low1, pch=19, lwd=2, type="l")
points(bmb_tp[bmb_tp$pond=="A", "doy"], bmb_tp[bmb_tp$pond=="A", "tn"], 
       col=no1, pch=19, lwd=2, type="l")
points(bmb_tp[bmb_tp$pond=="D", "doy"], bmb_tp[bmb_tp$pond=="D", "tn"], 
       col=no1, pch=19, lwd=2, type="l")
