## Bigmouth Buffalo Trophic Cascade Project ###

# Code originally written by GM Wilkinson May 2020
# Updated:February 2022 

#============================================#
# STEP 1: DATA MUNGING
#============================================#
rm(list=ls())
graphics.off()

# Required Libraries for analysis and visualization
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

# setwd("C:/Users/wilkinso/Box/Hort Farm Experiment/2019 Bigmouth Buffalo Experiment/BMB Trophic Cascades Manuscript/Analyses - BMB Trophic Cascade")

# Data sets
bmb = read.csv("BMB_TrophicCascade_FinalData.csv")
zp = read.csv("2019_Hort_ZoopBiomass_03Feb2021.csv")
ab = read.csv("2019_Hort_Zoop_LENGTH_ABUNDANCE.csv")

zp_wide = zp %>%
  #Sum the biomass of indivdiual taxa into groups (e.g. small-bodied cladocerans)
  group_by(LAKE.NO, DOY, GROUP) %>%
  summarize(biomass = sum(BIOMASS.UG.L)) %>%
  ungroup() %>%
  
  #Pivot from the long format to the wide format
  pivot_wider(id_cols = c(LAKE.NO, DOY), 
              names_from = GROUP, 
              values_from = biomass) %>%
  
  #Replace the NAs with 0 as that group was not identified from that pond and DOY, so biomass is zero
  replace_na(replace=list(Cyclopoid = 0,
                          Nauplii = 0,
                          Rotifer = 0,
                          SmCladocera = 0,
                          Calanoid = 0,
                          LgCladocera = 0)) %>%
  #Make the column headers match the 'bmb' data frame for joining
  rename(doy = DOY,
         pond = LAKE.NO) %>%
  rename(biomassCyclopoid = Cyclopoid,
         biomassNauplii = Nauplii,
         biomassRotifer = Rotifer,
         biomassSmCladocera = SmCladocera,
         biomassCalanoid = Calanoid,
         biomassLgCladocera = LgCladocera)

#Join the BMB data frame with the zp_wide data frame by pond and day of year
#This updated 'bmb' data frame will be used in subsequent analyses
bmb = left_join(bmb, zp_wide, by = c("pond", "doy"))

total_zoop = bmb %>%
  group_by(pond, doy) %>%
  summarize(total_zoop = sum(biomassCyclopoid, 
                             biomassCalanoid, 
                             biomassRotifer, 
                             biomassNauplii, 
                             biomassSmCladocera, 
                             biomassLgCladocera))

bmb = left_join(bmb, total_zoop, by = c("pond", "doy"))


#====================================================================
#Tidy up the Zooplankton Length data and weight by biomass
ab = ab %>%
  rename(doy = DOY,
         pond = LAKE.NO,
         taxon = TAXON, 
         group = GROUP)

ab_sum = ab %>%
  group_by(pond, doy) %>%
  summarize(total_abundance = sum(ABUNDANCE.L)) %>%
  ungroup()

ab_join = left_join(ab, ab_sum, by = c("pond", "doy"))

ab_join$frac_abundance = ab_join$ABUNDANCE.L/ab_join$total_abundance
ab_join$weighted_length = ab_join$LENGTH.UM * ab_join$frac_abundance

comm_length = ab_join %>%
  filter(doy<169)%>%
  group_by(pond, doy) %>%
  summarize(commLength = mean(weighted_length),
            commSE = sd(weighted_length)/sqrt(6)) %>%
  ungroup()
comm_length = as.data.frame(comm_length)

bmb = left_join(bmb, comm_length, by = c("pond", "doy"))
bmb$molar_ratio = (bmb$tn/(1000*14.01)) / (bmb$tp/(1000000*30.97))

# ========= BMB COLUMN HEADER METADATA ========= #
# doy = julian day of year
# pond = experimental pond A-F
# treatment = fish density treatment 
  #high = 450 kg/ha, ambient
  #low= 150 kg/ha, harvested
  #no = 0 kg/ha, reference)
# period = experimental period 
  #fish0= prior to fish addition
  #fish1 = after first BMB addition
  #fish2= after second BMB addition)
# temp = water temperature at 0.25 m
# dosat = dissolved oxygen saturation at 0.25 m
# do = dissolved oxygen concentration (mg/L) at 0.25 m
# chl = chlorophyll a concentration at 0.25 m depth
# pc = phycocyanin concentration at 0.25 m depth
# tp = total phosphorus concentration (micrograms per liter)
# tn = total nitrogen concentration (milligrams per liter)
# srp = soluble reactive phosphorus (micrograms per liter)
# no3 = nitrate concetration (milligrams per liter)
# biomassCyclopoid = biomass of cyclopoid copepods, micrograms per liter
# biomassNauplii = biomass of copepod nauplii, micrograms per liter
# biomassRotifer = biomass of all rotifer taxa combined, micrograms per liter
# biomassSmCladocera = biomass of small-bodied cladocerans combined, micrograms per liter
# biomassCalanoid = biomass of calanoid copepods, micrograms per liter
# biomassLgCladocera = biomass of large-bodied cladocerans combined, micrograms per liter
# total_zoop = total biomass of all zoop groups, micrograms per liter