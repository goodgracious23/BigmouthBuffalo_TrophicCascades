## Bigmouth Buffalo Trophic Cascade Project ###
# Code originally written by GM Wilkinson May 2020
# Updated: October 2021
# Run Step1_Munging.R first

zp_wide = as.data.frame(zp_wide)
#=====================================
#Copepod Colors
cala = rgb(133,196,201, max = 255) #biomassNauplii
cyclo = rgb(42,86,116, max = 255) #biomassCyclopoids
naup = rgb(79,144,166, max = 255) #biomassCalanoids

#biomassRotifer Colors
# lgClado = rgb(246,210,169, max = 255)
rotif = rgb(245,183,142, max = 255) #biomassRotifers

#Cladoceran Colors
lgClado = rgb(220,113,118, max = 255) #biomassRotifers
smClado = rgb(156,63,93, max = 255) #Allequash

#=====================================================
# STACKED AREA CHART = LAYOUT
windows(height = 8, width = 6.5)
par(mfrow=c(3,2), omi=c(0.75,0.9,0.5,0.5), mai=c(0.2,0.2,0.1,0.1))

#=====================================================
# POND B - Stacked Area Chart
plot(0,0, pch=NA, xlab="", ylab="", xlim=c(115,168), ylim=c(0,3000),xaxt="n", cex.axis=1.1)
axis(1, at=c(120,130,140,150,160,170),labels=c("","","","","",""), cex.axis=1.1)
mtext(side=2, line=2.5, expression(Biomass~"("*mu*g~L^-1*")"))
mtext(side=2, line=4.5, "Ambient", font=2, cex=1.2)

polygon(c(115,123,123,115), c(2700, 2700, 3000, 3000), col="gray20")
lines(c(122.8, 122.8), c(0,3000), lwd=2, lty=2, col="gray20")
text(119, 2850, "Pre", col="white", font=2)
lines(c(129.2, 129.2), c(0,3000), lwd=2, lty=2, col="gray20")
polygon(c(129,168,168,129), c(2700, 2700, 3000, 3000), col="gray20") 
text(149, 2850, "Post Fish", col="white", font=2)

legend("topright", legend=c("Cyclopoid", "Nauplii", "Calanoid", "Rotifer", "Sm Cladoceran", "Lg Cladoceran"),
       pch=15, pt.cex=2.5, col=c(cyclo, naup, cala, rotif, smClado, lgClado), bty='n', cex=1.1, bg="white", inset=c(0,0.15))

#Nauplii + Cyclopoid + Calanoid + Rotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="B", "doy"]), 
    zp_wide[zp_wide$pond=="B", "doy"], 
    max(zp_wide[zp_wide$pond=="B", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="B", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="B", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="B", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="B", "biomassCalanoid"]+
      zp_wide[zp_wide$pond=="B", "biomassCyclopoid"]+
      zp_wide[zp_wide$pond=="B", "biomassNauplii"], 
    0), 
  col=naup , border=F)
#Cyclopoid + Calanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="B", "doy"]), 
    zp_wide[zp_wide$pond=="B", "doy"], 
    max(zp_wide[zp_wide$pond=="B", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="B", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="B", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="B", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="B", "biomassCalanoid"]+
      zp_wide[zp_wide$pond=="B", "biomassCyclopoid"], 
    0), 
  col=cyclo , border=F)
#biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="B", "doy"]), 
    zp_wide[zp_wide$pond=="B", "doy"], 
    max(zp_wide[zp_wide$pond=="B", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="B", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="B", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="B", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="B", "biomassCalanoid"], 
    0), 
  col=cala , border=F)
#biomassRotifers + Sm + Lg Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="B", "doy"]), 
    zp_wide[zp_wide$pond=="B", "doy"], 
    max(zp_wide[zp_wide$pond=="B", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="B", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="B", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="B", "biomassRotifer"], 
    0), 
  col=rotif , border=F)
#Small + Large Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="B", "doy"]), 
    zp_wide[zp_wide$pond=="B", "doy"], 
    max(zp_wide[zp_wide$pond=="B", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="B", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="B", "biomassSmCladocera"], 
    0), 
  col=smClado , border=F)
#Large Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="B", "doy"]), 
    zp_wide[zp_wide$pond=="B", "doy"], 
    max(zp_wide[zp_wide$pond=="B", "doy"])) , 
  c(min(zp_wide[zp_wide$pond=="B", "biomassLgCladocera"]), 
    zp_wide[zp_wide$pond=="B", "biomassLgCladocera"], 
    min(zp_wide[zp_wide$pond=="B", "biomassLgCladocera"])), 
  col=lgClado , border=F)


#=====================================================
# POND F - Stacked Area Chart
plot(0,0, pch=NA, xlab="", ylab="", xlim=c(115,168), ylim=c(0,3000), xaxt="n", yaxt="n")
axis(1, at=c(120,130,140,150,160,170),labels=c("","","","","",""))
axis(2, at=c(0,500,1000,1500,2000,2500),labels=c('','','','','',''))

polygon(c(115,123,123,115), c(2700, 2700, 3000, 3000), col="gray20")
lines(c(122.8, 122.8), c(0,3000), lwd=2, lty=2, col="gray20")
text(119, 2850, "Pre", col="white", font=2)
lines(c(129.2, 129.2), c(0,3000), lwd=2, lty=2, col="gray20")
polygon(c(129,168,168,129), c(2700, 2700, 3000, 3000), col="gray20") 
text(149, 2850, "Post Fish", col="white", font=2)



#Nauplii + Cyclopoid + Calanoid + Rotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="F", "doy"]), 
    zp_wide[zp_wide$pond=="F", "doy"], 
    max(zp_wide[zp_wide$pond=="F", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="F", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="F", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="F", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="F", "biomassCalanoid"]+
      zp_wide[zp_wide$pond=="F", "biomassCyclopoid"]+
      zp_wide[zp_wide$pond=="F", "biomassNauplii"], 
    0), 
  col=naup , border=F)
#Cyclopoid + Calanoid + Rotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="F", "doy"]), 
    zp_wide[zp_wide$pond=="F", "doy"], 
    max(zp_wide[zp_wide$pond=="F", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="F", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="F", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="F", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="F", "biomassCalanoid"]+
      zp_wide[zp_wide$pond=="F", "biomassCyclopoid"], 
    0), 
  col=cyclo , border=F)
#biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="F", "doy"]), 
    zp_wide[zp_wide$pond=="F", "doy"], 
    max(zp_wide[zp_wide$pond=="F", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="F", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="F", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="F", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="F", "biomassCalanoid"], 
    0), 
  col=cala , border=F)
#biomassRotifers + Sm + Lg Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="F", "doy"]), 
    zp_wide[zp_wide$pond=="F", "doy"], 
    max(zp_wide[zp_wide$pond=="F", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="F", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="F", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="F", "biomassRotifer"], 
    0), 
  col=rotif , border=F)
#Small + Large Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="F", "doy"]), 
    zp_wide[zp_wide$pond=="F", "doy"], 
    max(zp_wide[zp_wide$pond=="F", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="F", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="F", "biomassSmCladocera"], 
    0), 
  col=smClado , border=F)
#Large Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="F", "doy"]), 
    zp_wide[zp_wide$pond=="F", "doy"], 
    max(zp_wide[zp_wide$pond=="F", "doy"])) , 
  c(min(zp_wide[zp_wide$pond=="F", "biomassLgCladocera"]), 
    zp_wide[zp_wide$pond=="F", "biomassLgCladocera"], 
    min(zp_wide[zp_wide$pond=="F", "biomassLgCladocera"])), 
  col=lgClado , border=F)

#=====================================================
# POND C - Stacked Area Chart
plot(0,0, pch=NA, xlab="", ylab="", xlim=c(115,168), ylim=c(0,3000), xaxt="n", cex.axis=1.1)
axis(1, at=c(120,130,140,150,160,170),labels=c("","","","","",""), cex.axis=1.1)
mtext(side=2, line=2.5, expression(Biomass~"("*mu*g~L^-1*")"))
mtext(side=2, line=4.5, "Harvested", font=2, cex=1.2)

polygon(c(115,123,123,115), c(2700, 2700, 3000, 3000), col="gray20")
lines(c(122.8, 122.8), c(0,3000), lwd=2, lty=2, col="gray20")
text(119, 2850, "Pre", col="white", font=2)
lines(c(129.2, 129.2), c(0,3000), lwd=2, lty=2, col="gray20")
polygon(c(129,168,168,129), c(2700, 2700, 3000, 3000), col="gray20") 
text(149, 2850, "Post Fish", col="white", font=2)

#biomassNauplii + biomassCyclopoid + biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="C", "doy"]), 
    zp_wide[zp_wide$pond=="C", "doy"], 
    max(zp_wide[zp_wide$pond=="C", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="C", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="C", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="C", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="C", "biomassCalanoid"]+
      zp_wide[zp_wide$pond=="C", "biomassCyclopoid"]+
      zp_wide[zp_wide$pond=="C", "biomassNauplii"], 
    0), 
  col=naup , border=F)
#biomassCyclopoid + biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="C", "doy"]), 
    zp_wide[zp_wide$pond=="C", "doy"], 
    max(zp_wide[zp_wide$pond=="C", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="C", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="C", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="C", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="C", "biomassCalanoid"]+
      zp_wide[zp_wide$pond=="C", "biomassCyclopoid"], 
    0), 
  col=cyclo , border=F)
#biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="C", "doy"]), 
    zp_wide[zp_wide$pond=="C", "doy"], 
    max(zp_wide[zp_wide$pond=="C", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="C", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="C", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="C", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="C", "biomassCalanoid"], 
    0), 
  col=cala , border=F)
#biomassRotifers + Sm + Lg Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="C", "doy"]), 
    zp_wide[zp_wide$pond=="C", "doy"], 
    max(zp_wide[zp_wide$pond=="C", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="C", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="C", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="C", "biomassRotifer"], 
    0), 
  col=rotif , border=F)
#Small + Large Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="C", "doy"]), 
    zp_wide[zp_wide$pond=="C", "doy"], 
    max(zp_wide[zp_wide$pond=="C", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="C", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="C", "biomassSmCladocera"], 
    0), 
  col=smClado , border=F)
#Large Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="C", "doy"]), 
    zp_wide[zp_wide$pond=="C", "doy"], 
    max(zp_wide[zp_wide$pond=="C", "doy"])) , 
  c(min(zp_wide[zp_wide$pond=="C", "biomassLgCladocera"]), 
    zp_wide[zp_wide$pond=="C", "biomassLgCladocera"], 
    min(zp_wide[zp_wide$pond=="C", "biomassLgCladocera"])), 
  col=lgClado , border=F)


#=====================================================
# POND E - Stacked Area Chart
plot(0,0, pch=NA, xlab="", ylab="", xlim=c(115,168), ylim=c(0,3000), xaxt="n", yaxt='n')
axis(1, at=c(120,130,140,150,160,170),labels=c("","","","","",""))
axis(2, at=c(0,500,1000,1500,2000,2500),labels=c('','','','','',''))

polygon(c(115,123,123,115), c(2700, 2700, 3000, 3000), col="gray20")
lines(c(122.8, 122.8), c(0,3000), lwd=2, lty=2, col="gray20")
text(119, 2850, "Pre", col="white", font=2)
lines(c(129.2, 129.2), c(0,3000), lwd=2, lty=2, col="gray20")
polygon(c(129,168,168,129), c(2700, 2700, 3000, 3000), col="gray20") 
text(149, 2850, "Post Fish", col="white", font=2)

#biomassNauplii + biomassCyclopoid + biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="E", "doy"]), 
    zp_wide[zp_wide$pond=="E", "doy"], 
    max(zp_wide[zp_wide$pond=="E", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="E", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="E", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="E", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="E", "biomassCalanoid"]+
      zp_wide[zp_wide$pond=="E", "biomassCyclopoid"]+
      zp_wide[zp_wide$pond=="E", "biomassNauplii"], 
    0), 
  col=naup , border=F)
#biomassCyclopoid + biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="E", "doy"]), 
    zp_wide[zp_wide$pond=="E", "doy"], 
    max(zp_wide[zp_wide$pond=="E", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="E", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="E", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="E", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="E", "biomassCalanoid"]+
      zp_wide[zp_wide$pond=="E", "biomassCyclopoid"], 
    0), 
  col=cyclo , border=F)
#biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="E", "doy"]), 
    zp_wide[zp_wide$pond=="E", "doy"], 
    max(zp_wide[zp_wide$pond=="E", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="E", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="E", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="E", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="E", "biomassCalanoid"], 
    0), 
  col=cala , border=F)
#biomassRotifers + Sm + Lg Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="E", "doy"]), 
    zp_wide[zp_wide$pond=="E", "doy"], 
    max(zp_wide[zp_wide$pond=="E", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="E", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="E", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="E", "biomassRotifer"], 
    0), 
  col=rotif , border=F)
#Small + Large Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="E", "doy"]), 
    zp_wide[zp_wide$pond=="E", "doy"], 
    max(zp_wide[zp_wide$pond=="E", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="E", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="E", "biomassSmCladocera"], 
    0), 
  col=smClado , border=F)
#Large Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="E", "doy"]), 
    zp_wide[zp_wide$pond=="E", "doy"], 
    max(zp_wide[zp_wide$pond=="E", "doy"])) , 
  c(min(zp_wide[zp_wide$pond=="E", "biomassLgCladocera"]), 
    zp_wide[zp_wide$pond=="E", "biomassLgCladocera"], 
    min(zp_wide[zp_wide$pond=="E", "biomassLgCladocera"])), 
  col=lgClado , border=F)

#=====================================================
# POND A - Stacked Area Chart
plot(0,0, pch=NA, xlab="", ylab="", xlim=c(115,168), ylim=c(0,3000), cex.axis=1.1)
mtext(side=2, line=2.5, expression(Biomass~"("*mu*g~L^-1*")"))
mtext(side=2, line=4.5, "Reference", font=2, cex=1.2)
mtext(side=1, line=3, "                                                Day of Year", font=2, cex=1.2)

polygon(c(115,123,123,115), c(2700, 2700, 3000, 3000), col="gray20")
lines(c(122.8, 122.8), c(0,3000), lwd=2, lty=2, col="gray20")
text(119, 2850, "Pre", col="white", font=2)
lines(c(129.2, 129.2), c(0,3000), lwd=2, lty=2, col="gray20")
polygon(c(129,168,168,129), c(2700, 2700, 3000, 3000), col="gray20") 
text(149, 2850, "Post Fish", col="white", font=2)

#biomassNauplii + biomassCyclopoid + biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="A", "doy"]), 
    zp_wide[zp_wide$pond=="A", "doy"], 
    max(zp_wide[zp_wide$pond=="A", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="A", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="A", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="A", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="A", "biomassCalanoid"]+
      zp_wide[zp_wide$pond=="A", "biomassCyclopoid"]+
      zp_wide[zp_wide$pond=="A", "biomassNauplii"], 
    0), 
  col=naup , border=F)
#biomassCyclopoid + biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="A", "doy"]), 
    zp_wide[zp_wide$pond=="A", "doy"], 
    max(zp_wide[zp_wide$pond=="A", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="A", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="A", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="A", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="A", "biomassCalanoid"]+
      zp_wide[zp_wide$pond=="A", "biomassCyclopoid"], 
    0), 
  col=cyclo , border=F)
#biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="A", "doy"]), 
    zp_wide[zp_wide$pond=="A", "doy"], 
    max(zp_wide[zp_wide$pond=="A", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="A", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="A", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="A", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="A", "biomassCalanoid"], 
    0), 
  col=cala , border=F)
#biomassRotifers + Sm + Lg Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="A", "doy"]), 
    zp_wide[zp_wide$pond=="A", "doy"], 
    max(zp_wide[zp_wide$pond=="A", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="A", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="A", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="A", "biomassRotifer"], 
    0), 
  col=rotif , border=F)
#Small + Large Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="A", "doy"]), 
    zp_wide[zp_wide$pond=="A", "doy"], 
    max(zp_wide[zp_wide$pond=="A", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="A", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="A", "biomassSmCladocera"], 
    0), 
  col=smClado , border=F)
#Large Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="A", "doy"]), 
    zp_wide[zp_wide$pond=="A", "doy"], 
    max(zp_wide[zp_wide$pond=="A", "doy"])) , 
  c(min(zp_wide[zp_wide$pond=="A", "biomassLgCladocera"]), 
    zp_wide[zp_wide$pond=="A", "biomassLgCladocera"], 
    min(zp_wide[zp_wide$pond=="A", "biomassLgCladocera"])), 
  col=lgClado , border=F)


#=====================================================
# POND D - Stacked Area Chart
plot(0,0, pch=NA, xlab="", ylab="", xlim=c(115,168), ylim=c(0,3000), yaxt="n", cex.axis=1.1)
axis(2, at=c(0,500,1000,1500,2000,2500),labels=c('','','','','',''), cex.axis=1.1)

polygon(c(115,123,123,115), c(2700, 2700, 3000, 3000), col="gray20")
lines(c(122.8, 122.8), c(0,3000), lwd=2, lty=2, col="gray20")
text(119, 2850, "Pre", col="white", font=2)
lines(c(129.2, 129.2), c(0,3000), lwd=2, lty=2, col="gray20")
polygon(c(129,168,168,129), c(2700, 2700, 3000, 3000), col="gray20") 
text(149, 2850, "Post Fish", col="white", font=2)

#biomassNauplii + biomassCyclopoid + biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="D", "doy"]), 
    zp_wide[zp_wide$pond=="D", "doy"], 
    max(zp_wide[zp_wide$pond=="D", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="D", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="D", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="D", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="D", "biomassCalanoid"]+
      zp_wide[zp_wide$pond=="D", "biomassCyclopoid"]+
      zp_wide[zp_wide$pond=="D", "biomassNauplii"], 
    0), 
  col=naup , border=F)
#biomassCyclopoid + biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="D", "doy"]), 
    zp_wide[zp_wide$pond=="D", "doy"], 
    max(zp_wide[zp_wide$pond=="D", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="D", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="D", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="D", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="D", "biomassCalanoid"]+
      zp_wide[zp_wide$pond=="D", "biomassCyclopoid"], 
    0), 
  col=cyclo , border=F)
#biomassCalanoid + biomassRotifer + Lg + Sm Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="D", "doy"]), 
    zp_wide[zp_wide$pond=="D", "doy"], 
    max(zp_wide[zp_wide$pond=="D", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="D", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="D", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="D", "biomassRotifer"] +
      zp_wide[zp_wide$pond=="D", "biomassCalanoid"], 
    0), 
  col=cala , border=F)
#biomassRotifers + Sm + Lg Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="D", "doy"]), 
    zp_wide[zp_wide$pond=="D", "doy"], 
    max(zp_wide[zp_wide$pond=="D", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="D", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="D", "biomassSmCladocera"] + 
      zp_wide[zp_wide$pond=="D", "biomassRotifer"], 
    0), 
  col=rotif , border=F)
#Small + Large Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="D", "doy"]), 
    zp_wide[zp_wide$pond=="D", "doy"], 
    max(zp_wide[zp_wide$pond=="D", "doy"])) , 
  c(0, 
    zp_wide[zp_wide$pond=="D", "biomassLgCladocera"] + 
      zp_wide[zp_wide$pond=="D", "biomassSmCladocera"], 
    0), 
  col=smClado , border=F)
#Large Cladocerans
polygon(
  c(min(zp_wide[zp_wide$pond=="D", "doy"]), 
    zp_wide[zp_wide$pond=="D", "doy"], 
    max(zp_wide[zp_wide$pond=="D", "doy"])) , 
  c(min(zp_wide[zp_wide$pond=="D", "biomassLgCladocera"]), 
    zp_wide[zp_wide$pond=="D", "biomassLgCladocera"], 
    min(zp_wide[zp_wide$pond=="D", "biomassLgCladocera"])), 
  col=lgClado , border=F)
