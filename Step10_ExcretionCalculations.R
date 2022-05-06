#Bigmouth Buffalo Excretion
library(tidyverse)
excrete_data = read.csv("bmb_excretion.csv")

#================================================
#microgram calculation (for the limnologists)
excrete = excrete_data %>%
  rename(nhx_pre = nxh_pre) %>%
  mutate(nhx_pre = replace(nhx_pre, nhx_pre=="<2.4", "2.4")) %>%
  mutate(nhx_pre = as.numeric(nhx_pre),
         hours = duration_minutes/60) %>%
  select(-srp_pre, - srp_post, -E_C, -pre, -post)

ref = excrete %>%
  filter(type=="reference")

excrete = excrete %>%
  filter(!type=="reference")

tub_vol = 40

#Excretion rate per individual - mass
excrete$excretion_ind = 
  ((excrete$nhx_post - excrete$nhx_pre) * tub_vol)/excrete$hours

#Excretion rate per unit wet mass - mass
excrete$excretion_massSpecific = 
  ((excrete$nhx_post - excrete$nhx_pre) * tub_vol)/excrete$weight / excrete$hours

#==================================================
# Pond calculations
pond_vol = 450000 #liters
pond_area = 550 #square meters
harvest_density = 150 #kg/ha
ambient_density = 450 #kg/ha

fish_mass_harvest_g = (harvest_density * pond_area) / 10
fish_mass_ambient_g = (ambient_density * pond_area) / 10

#Pond-Level excretion rate (ug/L/d)
harvest_pond_excretion = 
  (mean(excrete$excretion_massSpecific) * fish_mass_harvest_g * 24) / pond_vol

# harvest_pond_excretion_sd = 
#   ((mean(excreteg$excretion_mass) + sd(excreteg$excretion_mass)) * fish_mass_harvest_g * 24) / pond_vol

ambient_pond_excretion = 
  (mean(excrete$excretion_massSpecific) * fish_mass_ambient_g * 24) / pond_vol
# ambient_pond_excretion_sd = 
#   ((mean(excreteg$excretion_mass) + sd(excreteg$excretion_mass)) * fish_mass_ambient_g * 24) / pond_vol




par(omi = c(0.5,0.5,0.1,0.1))
plot(log10(excrete$excretion_ind) ~ log10(excrete$weight), 
     xlim = c(log10(1400), log10(4400)), pch = 15,
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = "")
mtext(side = 1, line = 3.5, "Fish Wet Weight (g)")
mtext(side = 2, line = 3.5, expression(Excretion~Rate~"("*mu*mol~ind^-1~h^-1*")"))
axis(side = 1, 
     at = c(log10(1500), log10(1600), log10(1700), log10(1800), log10(1900),
            log10(2000), log10(2100), log10(2200), log10(2300), log10(2400),
            log10(2500), log10(2600), log10(2700), log10(2800), log10(2900),
            log10(3000), log10(3100), log10(3200), log10(3300), log10(3400),
            log10(3500), log10(3600), log10(3700), log10(3800), log10(3900),
            log10(4000), log10(4100)),
     labels = c("1500", "", "", "", "",
                "2000", "", "", "", "",
                "2500", "", "", "", "",
                "3000", "", "", "", "",
                "3500", "", "", "", "",
                "4000", ""), las = 2)
axis(side = 2, 
     at = c(log10(100), log10(200), log10(300), log10(400), log10(500),
            log10(600), log10(700), log10(800)),
     labels = c("100", "200", "300", "400", "500", "600", "700", "800"), las = 2)

