## Bigmouth Buffalo Trophic Cascade Project ##
## NOTE: Be sure to run STEP1: Data Munging first

# ========= TREATMENT COLOR CODE ========= #
high1 = "aquamarine3" # Pond B - ambient density
high2 = "aquamarine4" #Pond F - ambient density
low1 = "royalblue" #Pond C - harvested density
low2 = "royalblue3" #Pond E - harvested density
no1 = "gray60" #Pond A - no fish
no2 = "gray30" #Pond D - no fish

windows(width = 6.5, height = 9)
par(mfrow = c(3,1), omi = c(0.7,0.7,0.2,0.2), mai = c(0.1,0.2,0.1,0.1))
#==========================================================================
# FIGURE 1 - CHLOROPHYLL TIME SERIES
plot(bmb[bmb$pond=="B", "doy"], log10(bmb[bmb$pond=="B", "chl"]), 
     col=high1, pch=19, lwd=4, type="l", xaxt = 'n',
     ylim=c(log10(0.2), log10(100)), xlim=c(115,168), yaxt='n')
mtext(side=2, line=3.75, expression(Chlorophyll~a~"("*mu*g~L^-1*")"), cex=1.2)

axis(side=2, 
     at=c(log10(0.1), log10(0.2), log10(0.3), log10(0.4), log10(0.5), 
          log10(0.), log10(0.7), log10(0.8), log10(0.9),
          log10(1), log10(2), log10(3), log10(4), log10(5), 
          log10(6), log10(7), log10(8), log10(9), log10(10), 
          log10(20), log10(30), log10(40), log10(50), 
          log10(60), log10(70), log10(80), log10(90), log10(100)),
     labels = c("0.1", " ", " ", " ", " ", " ", " ", " ", " ",
                "1", " ", " ", " ", " ", " ", " ", " ", " ",
                "10", " ", " ", " ", " ", " ", " ", " ", " ", "100"), 
     las=2, cex.axis = 1.2)

polygon(c(115,123,123,115), c(log10(55), log10(55), log10(100), log10(100)), col="gray20") 
lines(c(123, 123), c(log10(0.2), log10(100)), lwd=2, lty=3, col="gray20")
text(119, log10(75), "Pre Fish", col="white", font=2, cex = 1.2)
lines(c(129.1, 129.1), c(log10(0.2), log10(100)), lwd=2, lty=3, col="gray20")
polygon(c(129,168,168,129), c(log10(55), log10(55), log10(100), log10(100)), col="gray20") 
text(150, log10(75), "Post Fish Addition", col="white", font=2, cex = 1.2)

points(bmb[bmb$pond=="F", "doy"], log10(bmb[bmb$pond=="F", "chl"]), 
       col=high1, pch=19, lwd=3, type="l")
points(bmb[bmb$pond=="C", "doy"], log10(bmb[bmb$pond=="C", "chl"]), 
       col=low2, pch=19, lwd=3, type="l")
points(bmb[bmb$pond=="E", "doy"], log10(bmb[bmb$pond=="E", "chl"]), 
       col=low2, pch=19, lwd=3, type="l")
points(bmb[bmb$pond=="A", "doy"], log10(bmb[bmb$pond=="A", "chl"]), 
       col=no1, pch=19, lwd=3, type="l")
points(bmb[bmb$pond=="D", "doy"], log10(bmb[bmb$pond=="D", "chl"]), 
       col=no1, pch=19, lwd=3, type="l")



# TOTAL ZOOPLANKTON TIME SERIES
bmb_zoop = bmb %>%
  select(doy:period, biomassCyclopoid:total_zoop) %>%
  filter(!(is.na(biomassCyclopoid) 
           | is.na(biomassNauplii) 
           | is.na(biomassRotifer) 
           | is.na(biomassCalanoid) 
           | is.na(biomassSmCladocera) 
           | is.na(biomassLgCladocera) 
           | is.na(total_zoop) ))

plot(bmb_zoop[bmb_zoop$pond=="F", "doy"], 
     log10(bmb_zoop[bmb_zoop$pond=="F", "total_zoop"]), 
     col = high1, pch = 19, lwd = 3, type = "l", xaxt = 'n',
     ylim = c(log10(5), log10(10000)), xlim = c(115,168), yaxt='n')
mtext(side=2, line=4.5, expression(Zooplankton), cex=1.2)
mtext(side=2, line=3, expression(Biomass~"("*mu*g~L^-1*")"), cex = 1.2)

axis(side=2, 
     at=c(log10(10), log10(20), log10(30), log10(40), log10(50), 
          log10(60), log10(70), log10(80), log10(90), log10(100), 
          log10(200), log10(300), log10(400), log10(500), 
          log10(600), log10(700), log10(800), log10(900), log10(1000), 
          log10(2000), log10(3000), log10(4000), log10(5000), 
          log10(6000), log10(7000), log10(8000), log10(9000), log10(10000)),
     labels = c("10", " ", " ", " ", " ", " ", " ", " ", " ", 
                "100", " ", " ", " ", " ", " ", " ", " ", " ", 
                "1000"," "," "," "," "," "," "," "," ","10000"), 
     las=2, cex.axis = 1.2)

polygon(c(115,123,123,115), c(log10(5000), log10(5000), log10(10000), log10(10000)), col="gray20") # Pre-Fish
lines(c(122.9, 122.9), c(log10(5), log10(5000)), lwd=2, lty=3, col="gray20")
text(119, log10(7500), "Pre Fish", col="white", font=2, cex = 1.2)
lines(c(129.1, 129.1), c(log10(5), log10(6000)), lwd=2, lty=3, col="gray20")
polygon(c(129,168,168,129), c(log10(5000), log10(5000), log10(10000), log10(10000)), col="gray20") # Second Fish
text(148, log10(7500), "Post Fish Addition", col="white", font=2, cex = 1.2)

points(bmb_zoop[bmb_zoop$pond=="B", "doy"], 
       log10(bmb_zoop[bmb_zoop$pond=="B", "total_zoop"]), 
       col=high1, pch=19, lwd=3, type="l")
points(bmb_zoop[bmb_zoop$pond=="C", "doy"], 
       log10(bmb_zoop[bmb_zoop$pond=="C", "total_zoop"]), 
       col=low2, pch=19, lwd=3, type="l")
points(bmb_zoop[bmb_zoop$pond=="E", "doy"], 
       log10(bmb_zoop[bmb_zoop$pond=="E", "total_zoop"]), 
       col=low2, pch=19, lwd=3, type="l")
points(bmb_zoop[bmb_zoop$pond=="A", "doy"], 
       log10(bmb_zoop[bmb_zoop$pond=="A", "total_zoop"]), 
       col=no1, pch=19, lwd=3, type="l")
points(bmb_zoop[bmb_zoop$pond=="D", "doy"], 
       log10(bmb_zoop[bmb_zoop$pond=="D", "total_zoop"]), 
       col=no1, pch=19, lwd=3, type="l")



##### Zooplankton Length
plot(comm_length[comm_length$pond=="B", "doy"],
     log10(comm_length[comm_length$pond=="B", "commLength"]),
     col = high1, ylim = c(log10(10),log10(200)), type = "l", lwd = 3,
     ylab = "", xlab = "",
     yaxt = "n", cex.axis = 1.2)
mtext(side=2, line=4.5, expression(Abundance~Weighted), cex=1.2)
mtext(side=2, line=3, expression(Mean~Length~"("*mu*m*")"), cex = 1.2)
mtext(side=1, line=3, "Day of Year", cex=1.2)
axis(side = 2, las = 2, cex.axis = 1, 
     at=c(log10(10), log10(20), log10(30), log10(40), log10(50), 
          log10(60), log10(70), log10(80), log10(90), 
          log10(100), log10(200)),
     labels = c("10", " ", " ", " ", "50", 
                " ", " ", " ", " ", "100", ""), cex.axis = 1.2)

# Time Series Treatment labels
polygon(c(115,123,123,115), c(log10(150), log10(150), log10(200), log10(200)), col="gray20") # Pre-Fish
lines(c(122.9, 122.9), c(log10(10), log10(200)), lwd=2, lty=3, col="gray20")
text(119, log10(175), "Pre Fish", col="white", font=2, cex = 1.2)
lines(c(129.1, 129.1), c(log10(10), log10(200)), lwd=2, lty=3, col="gray20")
polygon(c(129,168,168,129), c(log10(150), log10(150), log10(200), log10(200)), col="gray20") # Second Fish
text(148, log10(175), "Post Fish Addition", col="white", font=2, cex = 1.2)

points(comm_length[comm_length$pond=="F", "doy"],
       log10(comm_length[comm_length$pond=="F", "commLength"]),
       col = high1, type = "l", lwd = 3)
points(comm_length[comm_length$pond=="C", "doy"],
       log10(comm_length[comm_length$pond=="C", "commLength"]),
       col = low2, type = "l", lwd = 3)
points(comm_length[comm_length$pond=="E", "doy"],
       log10(comm_length[comm_length$pond=="E", "commLength"]),
       col = low2, type = "l", lwd = 3)
points(comm_length[comm_length$pond=="A", "doy"],
       log10(comm_length[comm_length$pond=="A", "commLength"]),
       col = no1, type = "l", lwd = 3)
points(comm_length[comm_length$pond=="D", "doy"],
       log10(comm_length[comm_length$pond=="D", "commLength"]),
       col = no1, type = "l", lwd = 3)
