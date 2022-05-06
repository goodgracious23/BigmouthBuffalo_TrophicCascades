## Bigmouth Buffalo Trophic Cascade Project ###
# Code originally written by GM Wilkinson May 2020
# Updated: October 2021
# Run Step1_Munging.R first

zp_wide = as.data.frame(zp_wide)

zp_wide$biomass_sum = 
  zp_wide$biomassCalanoid + 
  zp_wide$biomassCyclopoid +
  zp_wide$biomassLgCladocera + 
  zp_wide$biomassNauplii + 
  zp_wide$biomassRotifer+ 
  zp_wide$biomassSmCladocera

zp_wide$percentCalanoid = 
  (zp_wide$biomassCalanoid/zp_wide$biomass_sum) * 100
zp_wide$percentCyclopoid = 
  (zp_wide$biomassCyclopoid/zp_wide$biomass_sum) * 100
zp_wide$percentLgCladocera = 
  (zp_wide$biomassLgCladocera/zp_wide$biomass_sum) * 100
zp_wide$percentSmCladocera = 
  (zp_wide$biomassSmCladocera/zp_wide$biomass_sum) * 100
zp_wide$percentNauplii = 
  (zp_wide$biomassNauplii/zp_wide$biomass_sum) * 100
zp_wide$percentRotifer = 
  (zp_wide$biomassRotifer/zp_wide$biomass_sum) * 100

#=====================================
cala = "#85c4c9" #rgb(133,196,201, max = 255) #biomassNauplii
cyclo = "#2a5674" #rgb(42,86,116, max = 255) #biomassCyclopoids
naup = "#4f90a6" #rgb(79,144,166, max = 255) #biomassCalanoids
rotif = "#f5b78e" #rgb(245,183,142, max = 255) #biomassRotifers
lgClado = "#dc7176" #rgb(220,113,118, max = 255) #biomassLgClado
smClado = "#9c3f5d" #rgb(156,63,93, max = 255) #biomassSmClado

myColors <- c(cala,cyclo,naup,rotif,lgClado,smClado)

#=====================================================
# STACKED AREA CHART = LAYOUT
windows(height = 4.5, width = 6.5)
library(gridExtra)

# Pond B ================================
zp_B = zp_wide %>%
  filter(pond=="B") %>%
  select(-pond, -biomassCyclopoid:-biomass_sum)

zp_Blong = pivot_longer(zp_B, 
                        c(percentCalanoid:percentRotifer), 
                        names_to = "group")
zp_Blong$group <- factor(zp_Blong$group , 
                         levels=c("percentCalanoid",
                                  "percentCyclopoid",
                                  "percentNauplii",
                                  "percentRotifer",
                                  "percentSmCladocera",
                                  "percentLgCladocera") )

pondB <- ggplot(zp_Blong, aes(x = doy, y = value, fill = group)) + 
  geom_area() +
  scale_fill_manual(values=myColors) +
  theme(legend.position="none",
        panel.background = element_blank(),
        axis.text=element_text(size=8),
        axis.title=element_text(size=10)) +
  xlab("Day of Year") +
  ylab("% Biomass") +
  ggtitle("Ambient")

# Pond F ==================
zp_F = zp_wide %>%
  filter(pond=="F") %>%
  select(-pond, -biomassCyclopoid:-biomass_sum)

zp_Flong = pivot_longer(zp_F, 
                        c(percentCalanoid:percentRotifer), 
                        names_to = "group")
zp_Flong$group <- factor(zp_Flong$group , 
                         levels=c("percentCalanoid",
                                  "percentCyclopoid",
                                  "percentNauplii",
                                  "percentRotifer",
                                  "percentSmCladocera",
                                  "percentLgCladocera") )

pondF <- ggplot(zp_Flong, aes(x = doy, y = value, fill = group)) + 
  geom_area() +
  scale_fill_manual(values=myColors) +
  theme(legend.position="none",
        panel.background = element_blank(),
        axis.text=element_text(size=8),
        axis.title=element_text(size=10)) +
  xlab("Day of Year") +
  ylab("% Biomass")


# Pond C ==================
zp_C = zp_wide %>%
  filter(pond=="C") %>%
  select(-pond, -biomassCyclopoid:-biomass_sum)

zp_Clong = pivot_longer(zp_C, 
                        c(percentCalanoid:percentRotifer), 
                        names_to = "group")
zp_Clong$group <- factor(zp_Clong$group , 
                         levels=c("percentCalanoid",
                                  "percentCyclopoid",
                                  "percentNauplii",
                                  "percentRotifer",
                                  "percentSmCladocera",
                                  "percentLgCladocera") )

pondC <- ggplot(zp_Clong, aes(x = doy, y = value, fill = group)) + 
  geom_area() +
  scale_fill_manual(values=myColors) +
  theme(legend.position="none",
        panel.background = element_blank(),
        axis.text=element_text(size=8),
        axis.title=element_text(size=10)) +
  xlab("Day of Year") +
  ylab("% Biomass") +
  ggtitle("Harvest")

# Pond E ==================
zp_E = zp_wide %>%
  filter(pond=="E") %>%
  select(-pond, -biomassCyclopoid:-biomass_sum)

zp_Elong = pivot_longer(zp_E, 
                        c(percentCalanoid:percentRotifer), 
                        names_to = "group")
zp_Elong$group <- factor(zp_Elong$group , 
                         levels=c("percentCalanoid",
                                  "percentCyclopoid",
                                  "percentNauplii",
                                  "percentRotifer",
                                  "percentSmCladocera",
                                  "percentLgCladocera") )

pondE <- ggplot(zp_Elong, aes(x = doy, y = value, fill = group)) + 
  geom_area() +
  scale_fill_manual(values=myColors) +
  theme(legend.position="none",
        panel.background = element_blank(),
        axis.text=element_text(size=8),
        axis.title=element_text(size=10)) +
  xlab("Day of Year") +
  ylab("% Biomass")


# Pond A ==================
zp_A = zp_wide %>%
  filter(pond=="A") %>%
  select(-pond, -biomassCyclopoid:-biomass_sum)

zp_Along = pivot_longer(zp_A, 
                        c(percentCalanoid:percentRotifer), 
                        names_to = "group")
zp_Along$group <- factor(zp_Along$group , 
                         levels=c("percentCalanoid",
                                  "percentCyclopoid",
                                  "percentNauplii",
                                  "percentRotifer",
                                  "percentSmCladocera",
                                  "percentLgCladocera") )

pondA <- ggplot(zp_Along, aes(x = doy, y = value, fill = group)) + 
  geom_area() +
  scale_fill_manual(values=myColors) +
  theme(legend.position="none",
        panel.background = element_blank(),
        axis.text=element_text(size=8),
        axis.title=element_text(size=10)) +
  xlab("Day of Year") +
  ylab("% Biomass") +
  ggtitle("Reference")


# Pond D ==================
zp_D = zp_wide %>%
  filter(pond=="D") %>%
  select(-pond, -biomassCyclopoid:-biomass_sum)

zp_Dlong = pivot_longer(zp_D, 
                        c(percentCalanoid:percentRotifer), 
                        names_to = "group")
zp_Dlong$group <- factor(zp_Dlong$group , 
                         levels=c("percentCalanoid",
                                  "percentCyclopoid",
                                  "percentNauplii",
                                  "percentRotifer",
                                  "percentSmCladocera",
                                  "percentLgCladocera") )

pondD <- ggplot(zp_Dlong, aes(x = doy, y = value, fill = group)) + 
  geom_area() +
  scale_fill_manual(values=myColors) +
  theme(legend.position="none",
        panel.background = element_blank(),
        axis.text=element_text(size=8),
        axis.title=element_text(size=10)) +
  xlab("Day of Year") +
  ylab("% Biomass")

grid.arrange(pondB, pondC, pondA, pondF, pondE, pondD, 
             nrow = 2, ncol = 3)
