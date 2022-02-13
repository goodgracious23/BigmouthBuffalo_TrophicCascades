## Bigmouth Buffalo Trophic Cascade Project ###
# Code originally written by TJ Butts October 2021

#============================================#
# STEP 6: Zooplankton nMDS & ANOSIM 
#============================================#
graphics.off()

# Required Libraries for analysis and visualization
if (!require(vegan)) install.packages('vegan')
library(vegan)
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
if (!require(gridExtra)) install.packages('gridExtra')
library(gridExtra)

# The goal of NMDS is to represent the original position of communities in
# multidimensional space as accurately as possible using a reduced number 
# of dimensions that can be easily plotted and visualized 

# NMDS does not use the absolute abundances of species in communities, but
# rather their RANK ORDER
# The use of ranks omits some of the issues associated with using absolute
# distance (e.g., sensitivity to transformation), and as a result is much 
# more flexible technique that accepts a variety of types of data

# Load in dataset from previous scripts - zp comes from Step1_Munging.R 
zp # DOY = 'site' and the size zoop groups = 'species' in nmds terms

#======================================================================
# Prepare data object to run nMDS
# Add identifier for Pre- and Post-fish 
## treatment: A&D = Reference, B&F = Ambient, C&E = Harvested 
zp_info = zp %>%
  select(!(SAMPLE.ID)) %>%
  rename(pond = LAKE.NO,
         doy = DOY,
         taxon = TAXON,
         biomass = BIOMASS.UG.L,
         group = GROUP) %>%
  mutate(time = case_when(doy < 122 ~ 'Prefish',
                          doy > 122 ~ 'Postfish')) %>%
  mutate(treat = case_when(pond == 'A' | pond == 'D' ~ 'Reference',
                           pond == 'C' | pond == 'E' ~ 'Harvested',
                           pond == 'B' | pond == 'F' ~ 'Ambient')) %>%
  as_tibble() %>%
  select(pond, doy, time, treat, taxon, group, biomass)
zp_info

# Create a wide data frame
zp_ord = zp_info %>%
  pivot_wider(id_cols = c(pond, doy,time,treat), 
              names_from = taxon, 
              values_from = biomass) %>%
  arrange(doy, pond)
zp_ord

# Replace NAs with 0 # 
zp_ord[is.na(zp_ord)] <- 0
zp_ord # Taxa data = [,5:29]

# Run nMDS with full community data # 
# Convert data into a matrix for the nMDS # 
zp_ord_m = as.matrix(zp_ord[,5:29])

# Perform a data transformation to give low weights to 'species' with low biomass and many zeros # 
zp_hell = decostand(zp_ord_m, method = 'hellinger')

#========================================================================
# Run nmds
# Generally, stress < 0.05 provides an excellent representation in reduced 
# dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a 
# poor representation
zp_nmds = metaMDS(zp_hell, k=2,trymax=100, distance='bray') 
zp_nmds # Stress is 0.15, decent
stressplot(zp_nmds) # Pretty low scatter, good fit  
plot(zp_nmds) # Check it out

ordiplot(zp_nmds, type='n')
orditorp(zp_nmds, display='species') # Remember Calanoids and Large Cladocera are on the right 

# Extract nMDS scores (x and y coordinates, for better plotting)
data.scores = as.data.frame(scores(zp_nmds))

# Add columns to the data frame from original data 
data.scores$pond = zp_ord$pond # pull pond ID identifier
data.scores$doy = as.factor(zp_ord$doy) # pull doy identifier
data.scores$time = zp_ord$time # pull pre v. post fish addition identifier
data.scores$treat = zp_ord$treat # pull BMB density identifier 
data.scores$doypondid <- with(zp_ord, paste0(pond, doy)) # make a unique ID by DOY and pond for the color gradient 

#===================================================================
# ANOSIM test
# ANOSIM (Analysis of Similarity) is an ANOVA-like hypothesis test, however, it is used to evaluated a dissimilarity matrix rather than raw data. The ANOSIM statistic (R) suggests dissimilarity between groups with 0 suggesting a similar community composition between groups. R values below 0 suggest greater dissimilarity within groups than between groups 

#Between Treatments================
# ano = anosim(zp_hell, zp_ord$treat, distance = 'bray', permutations = 1000)
# ano

#Between DOY=======================
# ano2 = anosim(zp_hell, zp_ord$doy, distance = 'bray', permutations = 1000)
# ano2

#=============================================================
#Generate Color Gradient of Dark to Light for DOY in NMDS plotting 
#Used color gradient generator: https://colordesigner.io/gradient-generator

#Reference Ponds (no fish)
pondA = c("#000000", "#0e0e0e", "#181818", "#202020", "#282828",
          "#313131", "#393939", "#434343", "#4c4c4c", "#555555", 
          "#5f5f5f", "#696969", "#737373", "#7d7d7d", "#878787", 
          "#929292", "#9c9c9c", "#a7a7a7", "#b2b2b2", "#bdbdbd")
pondD = c("#000000", "#0e0e0e", "#181818", "#282828",
          "#313131", "#393939", "#4c4c4c", "#555555", 
          "#5f5f5f", "#696969", "#7d7d7d", "#878787", 
          "#929292", "#9c9c9c", "#a7a7a7", "#b2b2b2", "#bdbdbd")

#Harvested Density Ponds
pondC = c("#08101d", "#111927", "#172131", "#263145",
          "#2e3a50", "#36425b", "#3e4b67", "#465472", "#4f5e7e",
          "#57678a", "#607197", "#697aa3", "#7c8ebc",
          "#8598c9", "#8fa3d6", "#99ade3", "#a3b7f1", "#adc2fe")
pondE = c("#08101d", "#111927", "#172131", "#1f293b", "#263145",
          "#2e3a50", "#36425b", "#3e4b67", "#465472", "#4f5e7e",
          "#57678a", "#607197", "#697aa3", "#7284af", "#7c8ebc",
          "#8598c9", "#8fa3d6", "#99ade3", "#a3b7f1", "#adc2fe")

#Ambient Density Ponds
pondB = c("#1f3e32", "#254639", "#325648", "#395f50", "#406758", 
          "#467060", "#548270", "#5c8b78", "#639481", "#6a9d89", 
          "#79b09b", "#80baa4", "#88c3ad", "#90cdb6",  "#9fe1c8", 
          "#a7ebd1", "#b7ffe4")
pondF = c("#1f3e32", "#254639", "#2c4e41", "#325648", "#395f50",
          "#406758", "#467060", "#4d7968", "#548270", "#5c8b78",
          "#639481", "#6a9d89", "#71a792", "#79b09b", "#80baa4",
          "#88c3ad", "#90cdb6", "#97d7bf", "#9fe1c8", "#a7ebd1",
          "#aff5db", "#b7ffe4")

#=====================================================================
# Plot the nMDS
windows(height=4,width=4)

plot_nmds = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) +
  #Plot the individual communities from the ponds using color ramp by DOY
  geom_point(size = 3, aes(shape = treat, colour = doypondid)) +
  xlim(-1.2, 1.2) +
  ylim(-1.2, 1.2) +
  #Do all the ggplot things that make you want to pull your hair out
  theme(axis.text.y = element_text(size = 10), 
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12), 
        legend.position = 'none',
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, size = 1.1)) + 
  #Axis labels
  labs(x = 'NMDS1', shape = 'BMB Density', y = 'NMDS2') +
  #Color the points based on treatment and DOY
  scale_colour_manual(values = c(pondA, pondB, pondC, pondD, pondE, pondF)) 

plot_nmds # plot the figure 

#Save the file without ding bats (because ggplot is the worst) so it can be opened in illustrator while maintaining font fidelity
# ggsave(plot = plot_nmds,
       # height = 4, width = 4,
       # dpi = 200, filename="~/Figure4_NMDS_noDingBats.pdf",
       # useDingbats = FALSE)

#======================================================
# Make gradient Legends
windows(height = 4, width = 2)
par(omi = c(0.1,0.1,0.1,0), mai = c(0,0.2,0,0))

colfunc_ref <- colorRampPalette(c("#BDBDBD", "#000000"))
colfunc_harv <- colorRampPalette(c('#ADC2FE', '#08101D'))
colfunc_amb <- colorRampPalette(c('#B7FFE4', '#1f3e32'))

plot(NA, type = "n", ann = FALSE, 
     xlim = c(1,3), ylim = c(1,4), 
     xaxt = "n", yaxt = "n", bty = "n") # Blank plot
mtext(c('168','115'), side = 2,
      at = c(1.05,3.45), las = 2, cex = 0.8)
mtext(side = 2, line = 0, "Day of Year            ", cex = 1)
text(1.1, 3.6, "Reference", srt = 45, adj = 0)
text(1.7, 3.6, "Harvested", srt = 45, adj = 0)
text(2.3, 3.6, "Ambient", srt = 45, adj = 0)

#Reference ponds color ramp=========================
xl_ref <- 1
yb_ref <- 1
xr_ref <- 1.5
yt_ref <- 3.5

rect(xl_ref,
  head(seq(yb_ref, yt_ref, (yt_ref - yb_ref)/22),-1),
  xr_ref,
  tail(seq(yb_ref, yt_ref, (yt_ref - yb_ref)/22),-1),
  col = colfunc_ref(22), border = colfunc_ref(22)) 

#Harvest Density Ponds Color Ramp======================
xl_harv = 1.6
yb_harv = 1
xr_harv = 2.1
yt_harv = 3.5

points(NA, type = 'n',ann = FALSE) # add on to the blank plot above 
rect(xl_harv,
  head(seq(yb_harv, yt_harv, (yt_harv - yb_harv)/22),-1),
  xr_harv,
  tail(seq(yb_harv, yt_harv, (yt_harv - yb_harv)/22),-1),
  col = colfunc_harv(22), border = colfunc_harv(22)) 

#Ambient ponds color ramp==================================
xl_amb = 2.2
yb_amb = 1
xr_amb = 2.7
yt_amb = 3.5 

points(NA, type = 'n',ann = FALSE) # add on to the blank plot above 
rect(xl_amb,
  head(seq(yb_amb, yt_amb, (yt_amb - yb_amb)/22),-1),
  xr_amb,
  tail(seq(yb_amb, yt_amb, (yt_amb - yb_amb)/22),-1),
  col = colfunc_amb(22), border = colfunc_amb(22))


