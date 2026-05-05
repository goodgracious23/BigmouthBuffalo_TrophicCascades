# Linear Mixed-Effects model of change over time 
# Based on analysis from Wilcots et al. 2025, Ecosystems: https://doi.org/10.1007/s10021-024-00956-3
# 11 Jun 2025

# Load libraries # 
library(tidyverse)
library(nlme)
library(emmeans)
library(conflicted)

#=============================================================
# Be sure to run Step1_Munging.R first
bmb_lme = bmb %>%
  # filter(doy>128) %>%
  mutate(treatment = as_factor(treatment), 
         fishtreat = ordered(treatment, 
                             levels = c("nofish", "harvested", "ambient"))) %>% 
  as_tibble() %>% 
  mutate(log_doy = log(doy), 
         log_chl = log(chl), 
         log_zp = log(total_zoop), 
         log_length = log(commLength))
bmb_lme


#Colors for plots
high1 = "aquamarine3"
high2 = "aquamarine4" 
low1 = "royalblue"
low2 = "royalblue3"
no1 = "gray60"
no2 = "gray30" 

# Model comparing changes in chlorophyll, zooplankton biomass, zooplankton abundance weighted mean-length # 
  # 3 treatments - nofish, ambient, harvested # 
#=========== Chlorophyll #===================
# Load the nlme library
library(nlme)

# Ensure 'pond' and 'treatment' are factors
bmb_lme$pond <- as.factor(bmb_lme$pond)
bmb_lme$treatment <- as.factor(bmb_lme$treatment)

# Model 1: Random Intercept for Pond
mod_intercept <- lme(log_chl ~ treatment * log_doy, 
                     random = ~1 | pond,  # <--- This is the key change
                     weights = varIdent(form = ~1 | treatment),
                     data = bmb_lme, 
                     na.action = na.omit)

summary(mod_intercept)

# Intercept and log_doy have perfect correlation - this is unstable # 
  # Fix by centering log_doy 
mean_log_doy <- mean(bmb_lme$log_doy, na.rm = TRUE)
bmb_lme$log_doy_c <- bmb_lme$log_doy - mean_log_doy

# 3. Re-run your model with the new centered variable
mod1 <- lme(log_chl ~ treatment * log_doy_c,  # <-- Use the new variable
                 random = ~1 | pond,
                 weights = varIdent(form = ~1 | treatment),
                 data = bmb_lme, 
                 na.action = na.omit)

# should account for any linear change in mean over time, 
  # while also accounting for nonlinear among-doy variation arising between ponds 
summary(mod1)

# Residual (within-pond) variability is much higher (SD = 0.70) than between-pond variability (SD = 0.10).
  # This noise (variance) is also unequal: highest in 'nofish' (1.0), lowest in 'ambient' (0.87).

# Slope here is on a log-log scale, so looking at the percent change in chl for a 1% change in DOY.

# Interactions (Note: Intercepts are at the *average* time point, not Day 1)
  # No Fish (reference): Strong decline over time in log_chl (p < 0.0001); Intercept = 0.44, Slope = -7.77
  # Ambient: Significantly higher intercept at avg. time (+0.55, p = 0.026), but shows the slowest decline (Slope = -3.37; interaction p < 0.0001)
  # Harvested: Intercept not different from nofish (p = 0.63), with a moderate decline (Slope = -5.38; interaction p = 0.014)

# Brass Tax
  # The main story is the *change over time* (the slopes):
  #   All ponds show a significant decline in chlorophyll.
  #   The 'nofish' ponds had the fastest decline (slope = -7.77).
  #   The 'harvested' ponds moderated that decline (slope = -5.38).
  #   The 'ambient' ponds had the slowest decline (slope = -3.37).
  #
  # At the *average* time point of the experiment (the "intercept"):
  #   'ambient' ponds had significantly *higher* chlorophyll than 'nofish' ponds.
  #   'harvested' and 'nofish' ponds were not different from each other.

# 1a. Create the x-axis values for prediction
doy_seq <- seq(min(bmb_lme$doy, na.rm = TRUE), 
               max(bmb_lme$doy, na.rm = TRUE), 
               by = 1)

# **CRITICAL STEP**: Get the same mean used to center your original data
mean_log_doy <- mean(bmb_lme$log_doy, na.rm = TRUE)

# Create the new data grid for prediction
newdata1 <- expand.grid(
  doy = doy_seq, # Start with the real x-axis
  treatment = levels(bmb_lme$treatment)
)

# **CRITICAL STEP**: Create the centered predictor variable for the new data
newdata1$log_doy_c <- log(newdata1$doy) - mean_log_doy

# 1b. Fixed-effects estimates and variance-covariance matrix
beta <- fixef(mod1)
Vbeta <- vcov(mod1)

# 1c. Model matrix for newdata1
# **CRITICAL CHANGE**: Update the formula to match your model
Xnew <- model.matrix(~ treatment * log_doy_c, data = newdata1)

# 1d. Predicted fit (fixed effects only) and standard errors
newdata1$fit <- as.vector(Xnew %*% beta)
se_fit <- sqrt( diag( Xnew %*% Vbeta %*% t(Xnew) ) )
newdata1$lower <- newdata1$fit - 1.96 * se_fit
newdata1$upper <- newdata1$fit + 1.96 * se_fit

# 1e. Plot with ggplot2 (This code is unchanged)
# (Assuming you have your color variables no1, high1, etc. in your environment)
windows(height = 6, width = 8)
ggplot(newdata1, aes(x = doy, y = fit, color = treatment, fill = treatment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  geom_line(size = 2) +
  labs(
    x = "Day of Year", 
    y = "Predicted log(Chlorophyll)", # **NOTE**: Your Y-axis is log_chl
    color = "Treatment", fill = "Treatment"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(no1, high1, low1)) +
  scale_fill_manual(values = c(no2, high2, low2))

# 2a. Extract standardized (normalized) residuals and fitted values
data_used1 <- getData(mod1) 
dim(data_used1)

data_used1$resid_std <- resid(mod1, type = "normalized")
data_used1$fitted    <- fitted(mod1)

# 2b. Identify observations with |resid_std| > 2
outliers <- subset(data_used1, abs(resid_std) > 2)
unique(outliers$doy) 

# Residuals vs Fitted
ggplot(data_used1, aes(x = fitted, y = resid_std, color = treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.7) +
  labs(x = "Fitted Value", y = "Standardized Residual") 

# QQ‐plot of standardized residuals
qqnorm(data_used1$resid_std); qqline(data_used1$resid_std)

# Slope Differences (significant interaction) 
chl_trends_compare <- emtrends(mod1, 
                              pairwise ~ treatment,  # Get pairwise differences
                              var = "log_doy_c")

# 2. Convert the *contrasts* to a data frame
slope_comparisons <- as.data.frame(summary(chl_trends_compare$contrasts, infer = T))
pairwise_chla_comparisons <- slope_comparisons

# 3. Create the ball-and-whisker plot
ggplot(slope_comparisons, aes(x = estimate, y = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2) +
  
  # Add a vertical line at 0 (no difference)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  
  labs(
    x = "Estimated Difference in Slopes",
    y = "Treatment Contrast",
    title = " (Interaction)"
  ) +
  theme_minimal()
text(6.75, 6.6, "B", font = 2)

#=========== zooplankton biomass  #===================
mod2 <- lme(log_zp ~ treatment * log_doy_c,  # <-- Use the new variable
                 random = ~1 | pond,
                 weights = varIdent(form = ~1 | treatment),
                 data = bmb_lme, 
                 na.action = na.omit)

# should account for any linear change in mean over time, 
  # while also accounting for nonlinear among-doy variation arising between ponds 
summary(mod2)

# Between-pond variability is effectively zero (SD = 1.34e-05); all variation is residual (within-pond, SD = 0.33).
# Residual noise is nearly equal across all treatments (all parameters ~1.0).

# Slope here is on a log-log scale, so looking at the percent change in length for a 1% change in DOY.

# Interactions (Note: Intercepts are at the *average* time point)
  # No Fish (reference): Strong, significant increase over time (p < 0.0001); Intercept = 3.68, Slope = +5.42
  # Ambient: Intercept significantly lower (–0.46, p = 0.011), with a slope near zero (Slope = +5.42 - 5.27 = +0.15; interaction p < 0.0001)
  # Harvested: Intercept not different from nofish (p = 0.064), with a significant moderate increase (Slope = +5.42 - 2.68 = +2.74; interaction p = 0.0005)

# Brass Tax
  # The main story is the *change over time* (the slopes):
  #   The 'nofish' ponds showed a strong, significant *increase* in length over time.
  #   In contrast, both 'ambient' and 'harvested' ponds showed significantly *slower* rates of increase.
  #   The 'ambient' ponds had the slowest rate of change (slope = +0.15).
  #   The 'harvested' ponds had a moderate rate of increase (slope = +2.74).
  #
  # At the *average* time point of the experiment (the "intercept"):
  #   'ambient' ponds had significantly *lower* length values than 'nofish' ponds.
  #   'harvested' and 'nofish' ponds were not significantly different.

# 1a. Create the x-axis values for prediction
doy_seq <- seq(min(bmb_lme$doy, na.rm = TRUE), 
               max(bmb_lme$doy, na.rm = TRUE), 
               by = 1)

# **CRITICAL STEP**: Get the same mean used to center your original data
mean_log_doy <- mean(bmb_lme$log_doy, na.rm = TRUE)

# Create the new data grid for prediction
newdata2 <- expand.grid(
  doy = doy_seq, # Start with the real x-axis
  treatment = levels(bmb_lme$treatment)
)

# **CRITICAL STEP**: Create the centered predictor variable for the new data
newdata2$log_doy_c <- log(newdata2$doy) - mean_log_doy

# 1b. Fixed-effects estimates and variance-covariance matrix
beta <- fixef(mod2)
Vbeta <- vcov(mod2)

# 2c. Model matrix for newdata1
# **CRITICAL CHANGE**: Update the formula to match your model
Xnew <- model.matrix(~ treatment * log_doy_c, data = newdata2)

# 1d. Predicted fit (fixed effects only) and standard errors
newdata2$fit <- as.vector(Xnew %*% beta)
se_fit <- sqrt( diag( Xnew %*% Vbeta %*% t(Xnew) ) )
newdata2$lower <- newdata2$fit - 1.96 * se_fit
newdata2$upper <- newdata2$fit + 1.96 * se_fit

# 1e. Plot with ggplot2 (This code is unchanged)
# (Assuming you have your color variables no1, high1, etc. in your environment)
windows(height = 6, width = 8)
ggplot(newdata2, aes(x = doy, y = fit, color = treatment, fill = treatment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  geom_line(size = 2) +
  labs(
    x = "Day of Year", 
    y = "Predicted log(Length)", # **NOTE**: Your Y-axis is log_zp
    color = "Treatment", fill = "Treatment"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(no1, high1, low1)) +
  scale_fill_manual(values = c(no2, high2, low2))

# 2a. Extract standardized (normalized) residuals and fitted values
data_used2 <- getData(mod2) 
dim(data_used2)

data_used2$resid_std <- resid(mod2, type = "normalized")
data_used2$fitted    <- fitted(mod2)

# 2b. Identify observations with |resid_std| > 2
outliers <- subset(data_used2, abs(resid_std) > 2)
unique(outliers$doy) 

# Residuals vs Fitted
ggplot(data_used2, aes(x = fitted, y = resid_std, color = treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.7) +
  labs(x = "Fitted Value", y = "Standardized Residual") 

# QQ‐plot of standardized residuals
qqnorm(data_used2$resid_std); qqline(data_used2$resid_std)

# Slope Differences (significant interaction) 
zp_trends_compare <- emtrends(mod2, 
                              pairwise ~ treatment,  # Get pairwise differences
                              var = "log_doy_c")

# 2. Convert the *contrasts* to a data frame
slope_comparisons <- as.data.frame(summary(zp_trends_compare$contrasts, infer = T))
pairwise_zoop_slopes <- slope_comparisons

# 3. Create the ball-and-whisker plot
ggplot(slope_comparisons, aes(x = estimate, y = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2) +
  
  # Add a vertical line at 0 (no difference)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  
  labs(
    x = "Estimated Difference in Slopes",
    y = "Treatment Contrast",
    title = " (Interaction)"
  ) +
  theme_minimal()
text(9.85, 6.6, "D", font = 2)

#=========== Zooplankton Community Length  #===================
mod3 <- lme(log_length ~ treatment * log_doy_c,  # <-- Use the new variable
                 random = ~1 | pond,
                 weights = varIdent(form = ~1 | treatment),
                 data = bmb_lme, 
                 na.action = na.omit)

# should account for any linear change in mean over time, 
  # while also accounting for nonlinear among-doy variation arising between ponds 
summary(mod3)

# Between-pond variability is effectively zero (SD = 1.34e-05); all variation is residual (within-pond, SD = 0.33).
# Residual noise is nearly identical across all three treatments (all parameters ~1.0).

# Slope here is on a log-log scale, so looking at the percent change in length for a 1% change in DOY.

# Interactions (Note: Intercepts are at the *average* time point)
  # No Fish (reference): Strong, significant growth over time (p < 0.001); Intercept = 3.68, Slope = +5.42
  # Ambient: Significantly shorter at avg. time (–0.46, p = 0.011), with growth almost completely halted (Slope = +0.15; interaction p < 0.001)
  # Harvested: Intercept not different from nofish (p = 0.064), with significantly slower growth (Slope = +2.74; interaction p = 0.0005)

# Brass Tax
  # The main story is the *growth rate* (the slopes):
  #   The 'nofish' ponds (control) showed strong, rapid growth over time (slope = +5.42).
  #   The 'harvested' ponds significantly reduced this growth rate by about half (slope = +2.74).
  #   The 'ambient' ponds had the most dramatic effect, almost completely stopping growth (slope = +0.15).
  #
  # At the *average* time point of the experiment (the "intercept"):
  #   Fish in the 'ambient' ponds were significantly *shorter* than fish in the 'nofish' ponds.
  #   Fish in the 'harvested' and 'nofish' ponds were not significantly different in length.

# 1a. Create the x-axis values for prediction
doy_seq <- seq(min(bmb_lme$doy, na.rm = TRUE), 
               max(bmb_lme$doy, na.rm = TRUE), 
               by = 1)

# **CRITICAL STEP**: Get the same mean used to center your original data
mean_log_doy <- mean(bmb_lme$log_doy, na.rm = TRUE)

# Create the new data grid for prediction
newdata3 <- expand.grid(
  doy = doy_seq, # Start with the real x-axis
  treatment = levels(bmb_lme$treatment)
)

# **CRITICAL STEP**: Create the centered predictor variable for the new data
newdata3$log_doy_c <- log(newdata3$doy) - mean_log_doy

# 1b. Fixed-effects estimates and variance-covariance matrix
beta <- fixef(mod3)
Vbeta <- vcov(mod3)

# 1c. Model matrix for newdata1
# **CRITICAL CHANGE**: Update the formula to match your model
Xnew <- model.matrix(~ treatment * log_doy_c, data = newdata3)

# 1d. Predicted fit (fixed effects only) and standard errors
newdata3$fit <- as.vector(Xnew %*% beta)
se_fit <- sqrt( diag( Xnew %*% Vbeta %*% t(Xnew) ) )
newdata3$lower <- newdata3$fit - 1.96 * se_fit
newdata3$upper <- newdata3$fit + 1.96 * se_fit

# 1e. Plot with ggplot2 (This code is unchanged)
# (Assuming you have your color variables no1, high1, etc. in your environment)
windows(height = 6, width = 8)
ggplot(newdata3, aes(x = doy, y = fit, color = treatment, fill = treatment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  geom_line(size = 2) +
  labs(
    x = "Day of Year", 
    y = "Predicted log(lengthorophyll)", # **NOTE**: Your Y-axis is log_length
    color = "Treatment", fill = "Treatment"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(no1, high1, low1)) +
  scale_fill_manual(values = c(no2, high2, low2))

# 2a. Extract standardized (normalized) residuals and fitted values
data_used3 <- getData(mod3) 
dim(data_used3)

data_used3$resid_std <- resid(mod3, type = "normalized")
data_used3$fitted    <- fitted(mod3)

# 2b. Identify observations with |resid_std| > 2
outliers <- subset(data_used3, abs(resid_std) > 2)
unique(outliers$doy) 

# Residuals vs Fitted
ggplot(data_used3, aes(x = fitted, y = resid_std, color = treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.7) +
  labs(x = "Fitted Value", y = "Standardized Residual") 

# QQ‐plot of standardized residuals
qqnorm(data_used3$resid_std); qqline(data_used3$resid_std)

# Slope Differences (significant interaction) 
length_trends_compare <- emtrends(mod3, 
                              pairwise ~ treatment,  # Get pairwise differences
                              var = "log_doy_c")

# 2. Convert the *contrasts* to a data frame
slope_comparisons <- as.data.frame(summary(length_trends_compare$contrasts, infer = T))
pairwise_length_slopes <- slope_comparisons

# 3. Create the ball-and-whisker plot
ggplot(slope_comparisons, aes(x = estimate, y = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2) +
  
  # Add a vertical line at 0 (no difference)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  
  labs(
    x = "Estimated Difference in Slopes",
    y = "Treatment Contrast",
    title = " (Interaction)"
  ) +
  theme_minimal()
text(2.6, 6.6, "F", font = 2)

#========================= Recreate Wilcot Plot ======================================================
# loading in MASS earlier will now cause problems, just click dplyr over stats 
datA <- bmb_lme %>% 
  filter(pond == "A") 
datB <- bmb_lme %>% 
  filter(pond == "B") 
datC <- bmb_lme %>% 
  filter(pond == "C") 
datD <- bmb_lme %>% 
  filter(pond == "D") 
datE <- bmb_lme %>% 
  filter(pond == "E") 
datF <- bmb_lme %>% 
  filter(pond == "F") 

squircle_filled <- function(x0 = 0, y0 = 0, radius, stretch = 1.5, n = 1000, col = "gray", border = NA) {
  r <- function(radius, theta){
    radius/(1 - sin(2*theta)^2/2)^(1/4)
  }
  angle <- seq(0, 2*pi, length.out = n)
  rvec <- r(radius, angle)
  x <- rvec * cos(angle) * stretch + x0  # Apply horizontal stretch
  y <- rvec * sin(angle) + y0            # Leave vertical scale unchanged
  polygon(x, y, col = col, border = border)
}

#========================= Chlorophyll ==================================

# no fish: pond A, D
# harvested: pond C, E
# ambient: pond B, F

windows(height = 6, width = 8)
par(mar = c(1, 3, 2, 0.5), oma = c(4, 4, 0.5, 0.5), tcl = -0.25, mgp = c(2, 0.6, 0))

# no fish 
plot(log_chl ~ doy, data = datA, type = "l", lty = 2, col = no1, ylab = "", xlab = "", 
     ylim = c(log(0.2), log(100)), yaxt = 'n')
mtext("log(Chlorophyll ("~mu*"g"~`L`^1*"))", side = 2, line = 2.2, cex = 1.2)
mtext("Day of Year, 2019", side = 1, line = 2.2, cex = 1.2)
points(log_chl ~ doy, data = datD, type = "l", lty = 2, col = no2)

# harvested 
points(log_chl ~ doy, data = datC, type = "l", lty = 2, col = low1)
points(log_chl ~ doy, data = datE, type = "l", lty = 2, col = low2)

# ambient
points(log_chl ~ doy, data = datB, type = "l", lty = 2, col = high1)
points(log_chl ~ doy, data = datF, type = "l", lty = 2, col = high2)

axis(side=2, 
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), 
          log(0.), log(0.7), log(0.8), log(0.9),
          log(1), log(2), log(3), log(4), log(5), 
          log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40), log(50), 
          log(60), log(70), log(80), log(90), log(100)),
     labels = c("0.1", " ", " ", " ", " ", " ", " ", " ", " ",
                "1", " ", " ", " ", " ", " ", " ", " ", " ",
                "10", " ", " ", " ", " ", " ", " ", " ", " ", "100"), 
     las=2, cex.axis = 1.2)

# add in model fits # 
chl_fit <- newdata1 %>% 
  as_tibble() %>% 
  mutate(bt_chl = exp(fit))
chl_fit

chl_nofish <- chl_fit %>% 
  filter(treatment == "nofish")
chl_harv <- chl_fit %>% 
  filter(treatment == "harvested")
chl_amb <- chl_fit %>% 
  filter(treatment == "ambient")

polygon(
  x = c(chl_nofish$doy, rev(chl_nofish$doy)),
  y = c(chl_nofish$lower, rev(chl_nofish$upper)),
  col = adjustcolor(no1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(chl_harv$doy, rev(chl_harv$doy)),
  y = c(chl_harv$lower, rev(chl_harv$upper)),
  col = adjustcolor(low1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(chl_amb$doy, rev(chl_amb$doy)),
  y = c(chl_amb$lower, rev(chl_amb$upper)),
  col = adjustcolor(high1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

points(fit ~ doy, data = chl_nofish, type = "l", col = no1, lwd = 4)
points(fit ~ doy, data = chl_harv, type = "l", col = low1, lwd = 4)
points(fit ~ doy, data = chl_amb, type = "l", col = high1, lwd = 4)

polygon(c(115,123,123,115), c(log(55), log(55), log(100), log(100)), col="gray20") 
lines(c(123, 123), c(log(0.2), log(100)), lwd=2, lty=3, col="gray20")
text(119, log(75), "Pre Fish", col="white", font=2, cex = 1.2)
lines(c(129.1, 129.1), c(log(0.2), log(100)), lwd=2, lty=3, col="gray20")
polygon(c(129,168,168,129), c(log(55), log(55), log(100), log(100)), col="gray20") 
text(150, log(75), "Post Fish Addition", col="white", font=2, cex = 1.2)

# squircle it up 
squircle_filled(x0 = 152, y0 = log(28), radius = 0.6, stretch = 6, col = no1)
text(152, log(28), "No Fish", col="white", font=2, cex = 1)
squircle_filled(x0 = 159, y0 = log(28), radius = 0.6, stretch = 6, col = low1)
text(159, log(28), "Harvest", col="white", font=2, cex = 1)
squircle_filled(x0 = 166, y0 = log(28), radius = 0.6, stretch = 6, col = high1)
text(166, log(28), "Ambient", col="white", font=2, cex = 1)

text(169.2, log(110), "A", font = 2, cex = 1.5)

#========================= Zooplankton Biomass ==================================

# no fish: pond A, D
# harvested: pond C, E
# ambient: pond B, F

datzA <- bmb_lme %>% 
  filter(pond == "A") %>% 
  mutate(flag = ifelse(is.na(log_zp), 1, 0)) %>% 
  filter(flag == 0)
datzB <- bmb_lme %>% 
  filter(pond == "B") %>% 
  mutate(flag = ifelse(is.na(log_zp), 1, 0)) %>% 
  filter(flag == 0)
datzC <- bmb_lme %>% 
  filter(pond == "C")%>% 
  mutate(flag = ifelse(is.na(log_zp), 1, 0)) %>% 
  filter(flag == 0) 
datzD <- bmb_lme %>% 
  filter(pond == "D") %>% 
  mutate(flag = ifelse(is.na(log_zp), 1, 0)) %>% 
  filter(flag == 0)
datzE <- bmb_lme %>% 
  filter(pond == "E")%>% 
  mutate(flag = ifelse(is.na(log_zp), 1, 0)) %>% 
  filter(flag == 0) 
datzF <- bmb_lme %>% 
  filter(pond == "F") %>% 
  mutate(flag = ifelse(is.na(log_zp), 1, 0)) %>% 
  filter(flag == 0)

windows(height = 6, width = 8)
par(mar = c(1, 3, 2, 0.5), oma = c(4, 4, 0.5, 0.5), tcl = -0.25, mgp = c(2, 0.6, 0))

# no fish 
plot(log_zp ~ doy, data = datzA, type = "l", lty = 2, col = no1, ylab = "", xlab = "", 
     ylim = c(log(5), log(10000)), yaxt = 'n', xlim = c(115, 168))
mtext("log(Zooplankton Biomass ("~mu*"g"~`L`^1*"))", side = 2, line = 3, cex = 1.2)
mtext("Day of Year, 2019", side = 1, line = 2.2, cex = 1.2)
points(log_zp ~ doy, data = datzD, type = "l", lty = 2, col = no2)

# harvested 
points(log_zp ~ doy, data = datzC, type = "l", lty = 2, col = low1)
points(log_zp ~ doy, data = datzE, type = "l", lty = 2, col = low2)

# ambient
points(log_zp ~ doy, data = datzB, type = "l", lty = 2, col = high1)
points(log_zp ~ doy, data = datzF, type = "l", lty = 2, col = high2)

axis(side=2, 
     at=c(log(10), log(20), log(30), log(40), log(50), 
          log(60), log(70), log(80), log(90), log(100), 
          log(200), log(300), log(400), log(500), 
          log(600), log(700), log(800), log(900), log(1000), 
          log(2000), log(3000), log(4000), log(5000), 
          log(6000), log(7000), log(8000), log(9000), log(10000)),
     labels = c("10", " ", " ", " ", " ", " ", " ", " ", " ", 
                "100", " ", " ", " ", " ", " ", " ", " ", " ", 
                "1000"," "," "," "," "," "," "," "," ","10000"), 
     las=2, cex.axis = 1.2)
axis(side=2, 
     at=c(log(100), log(1000), log(10000)),
     labels = c("100", 
                "1000","10000"), 
     las=2, cex.axis = 1.2, tick = F)


# add in model fits # 
zp_fit <- newdata2 %>% 
  as_tibble() %>% 
  mutate(bt_zp = exp(fit))
zp_fit

zp_nofish <- zp_fit %>% 
  filter(treatment == "nofish")
zp_harv <- zp_fit %>% 
  filter(treatment == "harvested")
zp_amb <- zp_fit %>% 
  filter(treatment == "ambient")

polygon(
  x = c(zp_nofish$doy, rev(zp_nofish$doy)),
  y = c(zp_nofish$lower, rev(zp_nofish$upper)),
  col = adjustcolor(no1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(zp_harv$doy, rev(zp_harv$doy)),
  y = c(zp_harv$lower, rev(zp_harv$upper)),
  col = adjustcolor(low1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(zp_amb$doy, rev(zp_amb$doy)),
  y = c(zp_amb$lower, rev(zp_amb$upper)),
  col = adjustcolor(high1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

points(fit ~ doy, data = zp_nofish, type = "l", col = no1, lwd = 4)
points(fit ~ doy, data = zp_harv, type = "l", col = low1, lwd = 4)
points(fit ~ doy, data = zp_amb, type = "l", col = high1, lwd = 4)

polygon(c(115,123,123,115), c(log(5000), log(5000), log(10000), log(10000)), col="gray20") # Pre-Fish
lines(c(122.9, 122.9), c(log(5), log(5000)), lwd=2, lty=3, col="gray20")
text(119, log(7500), "Pre Fish", col="white", font=2, cex = 1.2)
lines(c(129.1, 129.1), c(log(5), log(6000)), lwd=2, lty=3, col="gray20")
polygon(c(129,168,168,129), c(log(5000), log(5000), log(10000), log(10000)), col="gray20") # Second Fish
text(148, log(7500), "Post Fish Addition", col="white", font=2, cex = 1.2)

# squircle it up 

squircle_filled(x0 = 152, y0 = log(2300), radius = 0.7, stretch = 5, col = no1)
text(152, log(2300), "No Fish", col="white", font=2, cex = 1)
squircle_filled(x0 = 159, y0 = log(2300), radius = 0.7, stretch = 5, col = low1)
text(159, log(2300), "Harvest", col="white", font=2, cex = 1)
squircle_filled(x0 = 166, y0 = log(2300), radius = 0.7, stretch = 5, col = high1)
text(166, log(2300), "Ambient", col="white", font=2, cex = 1)

text(169.2, log(11000), "C", font = 2, cex = 1.5)

#========================= Zooplankton Length ==================================

# no fish: pond A, D
# harvested: pond C, E
# ambient: pond B, F

datlA <- bmb_lme %>% 
  filter(pond == "A") %>% 
  mutate(flag = ifelse(is.na(log_length), 1, 0)) %>% 
  filter(flag == 0)
datlB <- bmb_lme %>% 
  filter(pond == "B") %>% 
  mutate(flag = ifelse(is.na(log_length), 1, 0)) %>% 
  filter(flag == 0)
datlC <- bmb_lme %>% 
  filter(pond == "C")%>% 
  mutate(flag = ifelse(is.na(log_length), 1, 0)) %>% 
  filter(flag == 0) 
datlD <- bmb_lme %>% 
  filter(pond == "D") %>% 
  mutate(flag = ifelse(is.na(log_length), 1, 0)) %>% 
  filter(flag == 0)
datlE <- bmb_lme %>% 
  filter(pond == "E")%>% 
  mutate(flag = ifelse(is.na(log_length), 1, 0)) %>% 
  filter(flag == 0) 
datlF <- bmb_lme %>% 
  filter(pond == "F") %>% 
  mutate(flag = ifelse(is.na(log_length), 1, 0)) %>% 
  filter(flag == 0)

windows(height = 6, width = 9)
par(mar = c(1, 3, 2, 0.5), oma = c(4, 4, 0.5, 0.5), tcl = -0.25, mgp = c(2, 0.6, 0))

# no fish 
plot(log_length ~ doy, data = datlA, type = "l", lty = 2, col = no1, ylab = "", xlab = "", 
     ylim = c(log(10), log(200)), yaxt = 'n', xlim = c(115, 168), lwd = 2)
mtext(side=2, line=2.8, expression(Abundance~Weighted), cex=1.2)
mtext(side=2, line=1.8, expression(Mean~Length~"("*mu*m*")"), cex = 1.2)
mtext("Day of Year, 2019", side = 1, line = 2.2, cex = 1.2)
points(log_length ~ doy, data = datlD, type = "l", lty = 2, col = no2, lwd = 2)

# harvested 
points(log_length ~ doy, data = datlC, type = "l", lty = 2, col = low1, lwd = 2)
points(log_length ~ doy, data = datlE, type = "l", lty = 2, col = low2, lwd = 2)

# ambient
points(log_length ~ doy, data = datlB, type = "l", lty = 2, col = high1, lwd = 2)
points(log_length ~ doy, data = datlF, type = "l", lty = 2, col = high2, lwd = 2)

axis(side = 2, las = 2, cex.axis = 1, 
     at=c(log(10), log(20), log(30), log(40), log(50), 
          log(60), log(70), log(80), log(90), 
          log(100), log(200)),
     labels = c("10", " ", " ", " ", "50", 
                " ", " ", " ", " ", "100", ""), cex.axis = 1.2)
axis(side = 2, las = 2, cex.axis = 1, 
     at=c(log(100), log(200)),
     labels = c("100", "200"), cex.axis = 1.2)

# add in model fits # 
length_fit <- newdata3 %>% 
  as_tibble() %>% 
  mutate(bt_length = exp(fit))
length_fit

length_nofish <- length_fit %>% 
  filter(treatment == "nofish")
length_harv <- length_fit %>% 
  filter(treatment == "harvested")
length_amb <- length_fit %>% 
  filter(treatment == "ambient")

polygon(
  x = c(length_nofish$doy, rev(length_nofish$doy)),
  y = c(length_nofish$lower, rev(length_nofish$upper)),
  col = adjustcolor(no1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(length_harv$doy, rev(length_harv$doy)),
  y = c(length_harv$lower, rev(length_harv$upper)),
  col = adjustcolor(low1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(length_amb$doy, rev(length_amb$doy)),
  y = c(length_amb$lower, rev(length_amb$upper)),
  col = adjustcolor(high1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

points(fit ~ doy, data = length_nofish, type = "l", col = no1, lwd = 4)
points(fit ~ doy, data = length_harv, type = "l", col = low1, lwd = 4)
points(fit ~ doy, data = length_amb, type = "l", col = high1, lwd = 4)


polygon(c(115,123,123,115), c(log(150), log(150), log(200), log(200)), col="gray20") # Pre-Fish
lines(c(122.9, 122.9), c(log(10), log(200)), lwd=2, lty=3, col="gray20")
text(119, log(175), "Pre Fish", col="white", font=2, cex = 1.2)
lines(c(129.1, 129.1), c(log(10), log(200)), lwd=2, lty=3, col="gray20")
polygon(c(129,168,168,129), c(log(150), log(150), log(200), log(200)), col="gray20") # Second Fish
text(148, log(175), "Post Fish Addition", col="white", font=2, cex = 1.2)

# squircle it up 
squircle_filled(x0 = 134, y0 = log(114), radius = 0.25, stretch = 12, col = no1)
text(134, log(114), "No Fish", col="white", font=2, cex = 1)
squircle_filled(x0 = 140, y0 = log(114), radius = 0.25, stretch = 12, col = low1)
text(140, log(114), "Harvest", col="white", font=2, cex = 1)
squircle_filled(x0 = 146, y0 = log(114), radius = 0.25, stretch = 12, col = high1)
text(146, log(114), "Ambient", col="white", font=2, cex = 1)

text(169.2, log(210), "E", font = 2, cex = 1.5)

# Put figures together in one strip of code # ======================================

# Plot Dimensions # ========================
windows(height = 8, width = 9)
par(mar = c(1, 3, 2, 0.5), oma = c(4, 4, 0.5, 0.5), tcl = -0.25, mgp = c(2, 0.6, 0))
layout_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)
layout(mat = layout_matrix, heights = c(2.67, 2.67, 2.66))  # SUM = 10 inches high

## Chlorophyll (Plot A) # ========================
# no fish 
plot(log_chl ~ doy, data = datA, type = "l", lty = 2, col = no1, ylab = "", xlab = "", 
     ylim = c(log(0.2), log(100)), yaxt = 'n', lwd = 2)
mtext("log(Chlorophyll ("~mu*"g"~`L`^1*"))", side = 2, line = 2.2, cex = 0.9)
points(log_chl ~ doy, data = datD, type = "l", lty = 2, col = no2, lwd = 2)

# harvested 
points(log_chl ~ doy, data = datC, type = "l", lty = 2, col = low1, lwd = 2)
points(log_chl ~ doy, data = datE, type = "l", lty = 2, col = low2, lwd = 2)

# ambient
points(log_chl ~ doy, data = datB, type = "l", lty = 2, col = high1, lwd = 2)
points(log_chl ~ doy, data = datF, type = "l", lty = 2, col = high2, lwd = 2)

axis(side=2, 
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), 
          log(0.), log(0.7), log(0.8), log(0.9),
          log(1), log(2), log(3), log(4), log(5), 
          log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40), log(50), 
          log(60), log(70), log(80), log(90), log(100)),
     labels = c("0.1", " ", " ", " ", " ", " ", " ", " ", " ",
                "", " ", " ", " ", " ", " ", " ", " ", " ",
                "", " ", " ", " ", " ", " ", " ", " ", " ", "100"), 
     las=2, cex.axis = 1.2)
axis(side=2, 
     at=c(log(1), log(10), log(100)),
     labels = c("1", "10", "100"),
     las=2, cex.axis = 1.2, tick = F)


# add in model fits # 
chl_fit <- newdata1 %>% 
  as_tibble() %>% 
  mutate(bt_chl = exp(fit))
chl_fit

chl_nofish <- chl_fit %>% 
  filter(treatment == "nofish")
chl_harv <- chl_fit %>% 
  filter(treatment == "harvested")
chl_amb <- chl_fit %>% 
  filter(treatment == "ambient")

polygon(
  x = c(chl_nofish$doy, rev(chl_nofish$doy)),
  y = c(chl_nofish$lower, rev(chl_nofish$upper)),
  col = adjustcolor(no1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(chl_harv$doy, rev(chl_harv$doy)),
  y = c(chl_harv$lower, rev(chl_harv$upper)),
  col = adjustcolor(low1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(chl_amb$doy, rev(chl_amb$doy)),
  y = c(chl_amb$lower, rev(chl_amb$upper)),
  col = adjustcolor(high1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

points(fit ~ doy, data = chl_nofish, type = "l", col = no1, lwd = 4)
points(fit ~ doy, data = chl_harv, type = "l", col = low1, lwd = 4)
points(fit ~ doy, data = chl_amb, type = "l", col = high1, lwd = 4)

polygon(c(115,123,123,115), c(log(55), log(55), log(100), log(100)), col="gray20") 
lines(c(123, 123), c(log(0.2), log(100)), lwd=2, lty=3, col="gray20")
text(119, log(75), "Pre Fish", col="white", font=2, cex = 0.9)
lines(c(129, 129), c(log(0.2), log(100)), lwd=2, lty=3, col="gray20")
polygon(c(129,150,150,129), c(log(55), log(55), log(100), log(100)), col="gray20") 
text(140, log(75), "Post Fish Addition", col="white", font=2, cex = 0.9)

# squircle it up 
squircle_filled(x0 = 150, y0 = log(28), radius = 0.6, stretch = 6, col = no1)
text(150, log(28), "No Fish", col="white", font=2, cex = 0.9)
squircle_filled(x0 = 157.5, y0 = log(28), radius = 0.6, stretch = 6, col = low1)
text(157.5, log(28), "Harvest", col="white", font=2, cex = 0.9)
squircle_filled(x0 = 164.75, y0 = log(28), radius = 0.6, stretch = 6, col = high1)
text(164.5, log(28), "Ambient", col="white", font=2, cex = 0.9)

plot_coords <- par("usr")
text(x = plot_coords[2] * 0.99, # 95% to the right edge
     y = plot_coords[4] * 0.96, # 95% to the top edge
     "A", 
     font = 2, 
     cex = 1.2)

## Pairwise Chla (Plot B) # ======================================
chl_trends_compare <- emtrends(mod1, 
                               pairwise ~ treatment,
                               var = "log_doy_c")

# 2. Convert the *SUMMARY* of the contrasts to a data frame with CIs
slope_comparisons <- as.data.frame(summary(chl_trends_compare$contrasts, infer = TRUE))

# 4. Create the main plot area
# We plot points but suppress the axes to draw them manually
plot(x = slope_comparisons$estimate, 
     y = nrow(slope_comparisons):1,  # Plot from top to bottom
     xlim = range(slope_comparisons$lower.CL, slope_comparisons$upper.CL), # Set x-axis limits from data
     ylim = c(0.5, nrow(slope_comparisons) + 0.5), # Add space on y-axis
     xlab = "Estimated Difference in Slopes",
     ylab = "", # Suppress y-axis title
    main = "",
     pch = 16,      # Use solid circles
     cex = 1.5,       # Make points larger
     cex.axis = 1.2,
     yaxt = "n", 
     col = "white")     # Suppress y-axis labels

# 5. Add horizontal error bars (the "whiskers")
arrows(x0 = slope_comparisons$lower.CL, 
       y0 = nrow(slope_comparisons):1, 
       x1 = slope_comparisons$upper.CL, 
       y1 = nrow(slope_comparisons):1,
       angle = 90,     # Makes the ends flat
       code = 3,       # Puts flat ends on both sides
       length = 0.05)  # Sets the size of the flat ends

# 6. Add the vertical reference line at zero
abline(v = 0, lty = 2, col = "gray40")  # Make labels horizontal

# 8. Add the "B" label in the top right corner
# Note: You may need to adjust x and y coordinates depending on your data range
# We use par("usr") to get the coordinates of the plot region
plot_coords <- par("usr")
text(x = plot_coords[2] * 0.95, # 95% to the right edge
     y = plot_coords[4] * 0.97, # 95% to the top edge
     "B", 
     font = 2, 
     cex = 1.2)
# text(x = -0.2, y = plot_coords[4] * 0.97, "*", font = 2, cex = 2)
# text(x = 0.6, y = plot_coords[4] * 0.97, "n.s.", font = 2, cex = 1.5)

# The averaged RGB values
high2_rgb = col2rgb(high2)
low2_rgb = col2rgb(low2)
mixed_color <- "black"

points(x = slope_comparisons$estimate, 
       y = nrow(slope_comparisons):1,
     pch = 21, 
     col = "black",
     lwd = 2,
     bg = c(high2, low2, mixed_color), # indexing weird, but this is the correct order 
     cex = 2)

axis(2, at = c(3,2,1), tick = T, labels = c("A - NF", 
                                            "H - NF", 
                                            "A - H"), 
     las = 0, cex.axis = 1.2)
mtext("Chlorophyll Pairwise Differences", side = 2, line = 2.2, cex = 0.9)

## Zooplankton Biomass (Plot C) =======================================

# no fish 
plot(log_zp ~ doy, data = datzA, type = "l", lty = 2, col = no1, ylab = "", xlab = "", 
     ylim = c(log(5), log(10000)), yaxt = 'n', xlim = c(115, 168), lwd = 2)
mtext("log(Zooplankton Biomass ("~mu*"g"~`L`^1*"))", side = 2, line = 3, cex = 0.9)
points(log_zp ~ doy, data = datzD, type = "l", lty = 2, col = no2, lwd = 2)

# harvested 
points(log_zp ~ doy, data = datzC, type = "l", lty = 2, col = low1, lwd = 2)
points(log_zp ~ doy, data = datzE, type = "l", lty = 2, col = low2, lwd = 2)

# ambient
points(log_zp ~ doy, data = datzB, type = "l", lty = 2, col = high1, lwd = 2)
points(log_zp ~ doy, data = datzF, type = "l", lty = 2, col = high2, lwd = 2)

axis(side=2, 
     at=c(log(10), log(20), log(30), log(40), log(50), 
          log(60), log(70), log(80), log(90), log(100), 
          log(200), log(300), log(400), log(500), 
          log(600), log(700), log(800), log(900), log(1000), 
          log(2000), log(3000), log(4000), log(5000), 
          log(6000), log(7000), log(8000), log(9000), log(10000)),
     labels = c("10", " ", " ", " ", " ", " ", " ", " ", " ", 
                "100", " ", " ", " ", " ", " ", " ", " ", " ", 
                "1000"," "," "," "," "," "," "," "," ","10000"), 
     las=2, cex.axis = 1.2)
axis(side=2, 
     at=c(log(100), log(1000), log(10000)),
     labels = c("100", 
                "1000","10000"), 
     las=2, cex.axis = 1.2, tick = F)


# add in model fits # 
zp_fit <- newdata2 %>% 
  as_tibble() %>% 
  mutate(bt_zp = exp(fit))
zp_fit

zp_nofish <- zp_fit %>% 
  filter(treatment == "nofish")
zp_harv <- zp_fit %>% 
  filter(treatment == "harvested")
zp_amb <- zp_fit %>% 
  filter(treatment == "ambient")

polygon(
  x = c(zp_nofish$doy, rev(zp_nofish$doy)),
  y = c(zp_nofish$lower, rev(zp_nofish$upper)),
  col = adjustcolor(no1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(zp_harv$doy, rev(zp_harv$doy)),
  y = c(zp_harv$lower, rev(zp_harv$upper)),
  col = adjustcolor(low1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(zp_amb$doy, rev(zp_amb$doy)),
  y = c(zp_amb$lower, rev(zp_amb$upper)),
  col = adjustcolor(high1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

points(fit ~ doy, data = zp_nofish, type = "l", col = no1, lwd = 4)
points(fit ~ doy, data = zp_harv, type = "l", col = low1, lwd = 4)
points(fit ~ doy, data = zp_amb, type = "l", col = high1, lwd = 4)

# polygon(c(115,123,123,115), c(log(5000), log(5000), log(10000), log(10000)), col="gray20") # Pre-Fish
lines(c(122.9, 122.9), c(log(5), log(50000)), lwd=2, lty=3, col="gray20")
# text(119, log(7500), "Pre Fish", col="white", font=2, cex = 0.9)
lines(c(129.1, 129.1), c(log(5), log(60000)), lwd=2, lty=3, col="gray20")
# polygon(c(129,150,150,129), c(log(5000), log(5000), log(10000), log(10000)), col="gray20") # Second Fish
# text(140, log(7500), "Post Fish Addition", col="white", font=2, cex = 0.9)

# squircle it up 
# 
# squircle_filled(x0 = 150, y0 = log(2300), radius = 0.6, stretch = 5, col = no1)
# text(150, log(28), "No Fish", col="white", font=2, cex = 0.9)
# squircle_filled(x0 = 157.5, y0 = log(2300), radius = 0.6, stretch = 5, col = low1)
# text(157.5, log(28), "Harvest", col="white", font=2, cex = 0.9)
# squircle_filled(x0 = 164.75, y0 = log(2300), radius = 0.6, stretch = 5, col = high1)
# text(164.5, log(28), "Ambient", col="white", font=2, cex = 0.9)
# 
# text(169, log(100), "A", font = 2, cex = 1.5)
# 
# squircle_filled(x0 = 152, y0 = log(2300), radius = 0.7, stretch = 5, col = no1)
# text(152, log(2300), "No Fish", col="white", font=2, cex = 1)
# squircle_filled(x0 = 159, y0 = log(2300), radius = 0.7, stretch = 5, col = low1)
# text(159, log(2300), "Harvest", col="white", font=2, cex = 1)
# squircle_filled(x0 = 166, y0 = log(2300), radius = 0.7, stretch = 5, col = high1)
# text(166, log(2300), "Ambient", col="white", font=2, cex = 1)

plot_coords <- par("usr")
text(x = plot_coords[2] * 0.99, # 95% to the right edge
     y = plot_coords[4] * 0.96, # 95% to the top edge
     "C", 
     font = 2, 
     cex = 1.2)

## Pairwise Zooplankton Biomass (Plot D) ================================
zp_trends_compare <- emtrends(mod2, 
                               pairwise ~ treatment,
                               var = "log_doy_c")

# 2. Convert the *SUMMARY* of the contrasts to a data frame with CIs
slope_comparisons <- as.data.frame(summary(zp_trends_compare$contrasts, infer = TRUE))

# 4. Create the main plot area
# We plot points but suppress the axes to draw them manually
plot(x = slope_comparisons$estimate, 
     y = nrow(slope_comparisons):1,  # Plot from top to bottom
     xlim = range(slope_comparisons$lower.CL, slope_comparisons$upper.CL), # Set x-axis limits from data
     ylim = c(0.5, nrow(slope_comparisons) + 0.5), # Add space on y-axis
     xlab = "Estimated Difference in Slopes",
     ylab = "", # Suppress y-axis title
     main = "",
     pch = 16,      # Use solid circles
     cex = 1.5,       # Make points larger
     cex.axis = 1.2,
     yaxt = "n", 
     col = "white")     # Suppress y-axis labels

# 5. Add horizontal error bars (the "whiskers")
arrows(x0 = slope_comparisons$lower.CL, 
       y0 = nrow(slope_comparisons):1, 
       x1 = slope_comparisons$upper.CL, 
       y1 = nrow(slope_comparisons):1,
       angle = 90,     # Makes the ends flat
       code = 3,       # Puts flat ends on both sides
       length = 0.05)  # Sets the size of the flat ends

# 6. Add the vertical reference line at zero
abline(v = 0, lty = 2, col = "gray40")  # Make labels horizontal

# 8. Add the "B" label in the top right corner
# Note: You may need to adjust x and y coordinates depending on your data range
# We use par("usr") to get the coordinates of the plot region
plot_coords <- par("usr")
text(x = plot_coords[2] * 0.95, # 95% to the right edge
     y = plot_coords[4] * 0.97, # 95% to the top edge
     "D", 
     font = 2, 
     cex = 1.2)
# text(x = -0.2, y = plot_coords[4] * 0.97, "*", font = 2, cex = 2)
# text(x = 0.6, y = plot_coords[4] * 0.97, "n.s.", font = 2, cex = 1.5)

# The averaged RGB values
high2_rgb = col2rgb(high2)
low2_rgb = col2rgb(low2)
mixed_color <- "black"

points(x = slope_comparisons$estimate, 
       y = nrow(slope_comparisons):1,
     pch = 21, 
     col = "black",
     lwd = 2,
     bg = c(high2, low2, mixed_color), # indexing weird, but this is the correct order 
     cex = 2)

axis(2, at = c(3,2,1), tick = T, labels = c("A - NF", 
                                            "H - NF", 
                                            "A - H"), 
     las = 0, cex.axis = 1.2)
mtext("Zoop Pairwise Differences", side = 2, line = 2.2, cex = 0.9)

## Length (Plot E) ========================================

# no fish 
plot(log_length ~ doy, data = datlA, type = "l", lty = 2, col = no1, ylab = "", xlab = "", 
     ylim = c(log(10), log(200)), yaxt = 'n', xlim = c(115, 168), lwd = 2)
mtext(side=2, line=2.8, expression(Abundance~Weighted), cex=0.9)
mtext(side=2, line=1.8, expression(Mean~Length~"("*mu*m*")"), cex = 0.9)
mtext("Day of Year, 2019", side = 1, line = 2.2, cex = 0.9)
points(log_length ~ doy, data = datlD, type = "l", lty = 2, col = no2, lwd = 2)

# harvested 
points(log_length ~ doy, data = datlC, type = "l", lty = 2, col = low1, lwd = 2)
points(log_length ~ doy, data = datlE, type = "l", lty = 2, col = low2, lwd = 2)

# ambient
points(log_length ~ doy, data = datlB, type = "l", lty = 2, col = high1, lwd = 2)
points(log_length ~ doy, data = datlF, type = "l", lty = 2, col = high2, lwd = 2)

axis(side = 2, las = 2, cex.axis = 1, 
     at=c(log(10), log(20), log(30), log(40), log(50), 
          log(60), log(70), log(80), log(90), 
          log(100), log(200)),
     labels = c("10", " ", " ", " ", "50", 
                " ", " ", " ", " ", "100", ""), cex.axis = 1.2)
axis(side = 2, las = 2, cex.axis = 1, 
     at=c(log(100), log(200)),
     labels = c("100", "200"), cex.axis = 1.2)

# add in model fits # 
length_fit <- newdata3 %>% 
  as_tibble() %>% 
  mutate(bt_length = exp(fit))
length_fit

length_nofish <- length_fit %>% 
  filter(treatment == "nofish")
length_harv <- length_fit %>% 
  filter(treatment == "harvested")
length_amb <- length_fit %>% 
  filter(treatment == "ambient")

polygon(
  x = c(length_nofish$doy, rev(length_nofish$doy)),
  y = c(length_nofish$lower, rev(length_nofish$upper)),
  col = adjustcolor(no1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(length_harv$doy, rev(length_harv$doy)),
  y = c(length_harv$lower, rev(length_harv$upper)),
  col = adjustcolor(low1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(length_amb$doy, rev(length_amb$doy)),
  y = c(length_amb$lower, rev(length_amb$upper)),
  col = adjustcolor(high1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

points(fit ~ doy, data = length_nofish, type = "l", col = no1, lwd = 4)
points(fit ~ doy, data = length_harv, type = "l", col = low1, lwd = 4)
points(fit ~ doy, data = length_amb, type = "l", col = high1, lwd = 4)


# polygon(c(115,123,123,115), c(log(150), log(150), log(200), log(200)), col="gray20") # Pre-Fish
lines(c(122.9, 122.9), c(log(10), log(2000)), lwd=2, lty=3, col="gray20")
# text(119, log(175), "Pre Fish", col="white", font=2, cex = 1.2)
lines(c(129.1, 129.1), c(log(10), log(2000)), lwd=2, lty=3, col="gray20")
# polygon(c(129,168,168,129), c(log(150), log(150), log(200), log(200)), col="gray20") # Second Fish
# text(148, log(175), "Post Fish Addition", col="white", font=2, cex = 1.2)

# squircle it up 
# squircle_filled(x0 = 134, y0 = log(114), radius = 0.25, stretch = 12, col = no1)
# text(134, log(114), "No Fish", col="white", font=2, cex = 1)
# squircle_filled(x0 = 140, y0 = log(114), radius = 0.25, stretch = 12, col = low1)
# text(140, log(114), "Harvest", col="white", font=2, cex = 1)
# squircle_filled(x0 = 146, y0 = log(114), radius = 0.25, stretch = 12, col = high1)
# text(146, log(114), "Ambient", col="white", font=2, cex = 1)

plot_coords <- par("usr")
text(x = plot_coords[2] * 0.99, # 95% to the right edge
     y = plot_coords[4] * 0.96, # 95% to the top edge
     "E", 
     font = 2, 
     cex = 1.2)

## Pairwise Length (Plot F) =================================
length_trends_compare <- emtrends(mod3, 
                               pairwise ~ treatment,
                               var = "log_doy_c")

# 2. Convert the *SUMMARY* of the contrasts to a data frame with CIs
slope_comparisons <- as.data.frame(summary(length_trends_compare$contrasts, infer = TRUE))

# 4. Create the main plot area
# We plot points but suppress the axes to draw them manually
plot(x = slope_comparisons$estimate, 
     y = nrow(slope_comparisons):1,  # Plot from top to bottom
     xlim = range(slope_comparisons$lower.CL, slope_comparisons$upper.CL), # Set x-axis limits from data
     ylim = c(0.5, nrow(slope_comparisons) + 0.5), # Add space on y-axis
     xlab = "Estimated Difference in Slopes",
     ylab = "", # Suppress y-axis title
     main = "",
     pch = 16,      # Use solid circles
     cex = 1.5,       # Make points larger
     cex.axis = 1.2,
     yaxt = "n", 
     col = "white")     # Suppress y-axis labels

# 5. Add horizontal error bars (the "whiskers")
arrows(x0 = slope_comparisons$lower.CL, 
       y0 = nrow(slope_comparisons):1, 
       x1 = slope_comparisons$upper.CL, 
       y1 = nrow(slope_comparisons):1,
       angle = 90,     # Makes the ends flat
       code = 3,       # Puts flat ends on both sides
       length = 0.05)  # Sets the size of the flat ends

# 6. Add the vertical reference line at zero
abline(v = 0, lty = 2, col = "gray40")  # Make labels horizontal

# 8. Add the "B" label in the top right corner
# Note: You may need to adjust x and y coordinates depending on your data range
# We use par("usr") to get the coordinates of the plot region
plot_coords <- par("usr")
text(x = plot_coords[2] * 0.95, # 95% to the right edge
     y = plot_coords[4] * 0.97, # 95% to the top edge
     "F", 
     font = 2, 
     cex = 1.2)
# text(x = -0.2, y = plot_coords[4] * 0.97, "*", font = 2, cex = 2)
# text(x = 0.6, y = plot_coords[4] * 0.97, "n.s.", font = 2, cex = 1.5)

# The averaged RGB values
high2_rgb = col2rgb(high2)
low2_rgb = col2rgb(low2)
mixed_color <- "black"

points(x = slope_comparisons$estimate, 
       y = nrow(slope_comparisons):1,
     pch = 21, 
     col = "black",
     lwd = 2,
     bg = c(high2, low2, mixed_color), # indexing weird, but this is the correct order 
     cex = 2)

axis(2, at = c(3,2,1), tick = T, labels = c("A - NF", 
                                            "H - NF", 
                                            "A - H"), 
     las = 0, cex.axis = 1.2)
mtext("Estimated Difference in Slope", side = 1, line = 2.2, cex = 0.9)
mtext("Length Pairwise Differences", side = 2, line = 2.2, cex = 0.9)


  # Used emmeans() to pull the pairwise contrasts between treatments along with standard errors and confidence intervals 
legend(0.65,1.75, legend = c("Ambient - No Fish", 
                                 "Harvest - No Fish",
                                 "Ambient - Harvest"), 
       pch = 21,
       pt.bg = c(high2, low2, mixed_color),
       col = "black",
       cex = 1.7,
       pt.lwd = 2,
       pt.cex = 2, 
       bty = 'n')

# Re-Run removing pre-fish and fish addition periods to see if effects stay the same # =========================
# Linear Mixed-Effects model of change over time 
# Based on analysis from Wilcots et al. 2025, Ecosystems: https://doi.org/10.1007/s10021-024-00956-3
# 11 Jun 2025

# save the above model runs for comparison # 
real_mod1 <- mod1
real_mod2 <- mod2
real_mod3 <- mod3

# Load libraries # 
library(tidyverse)
library(nlme)
library(emmeans)
library(conflicted)

#=============================================================
# Be sure to run Step1_Munging.R first
bmb_lme = bmb %>%
  filter(doy>128) %>%
  mutate(treatment = as_factor(treatment), 
         fishtreat = ordered(treatment, 
                             levels = c("nofish", "harvested", "ambient"))) %>% 
  as_tibble() %>% 
  mutate(log_doy = log(doy), 
         log_chl = log(chl), 
         log_zp = log(total_zoop), 
         log_length = log(commLength))
bmb_lme


#Colors for plots
#Colors for plots
high1 = "aquamarine3"
high2 = "aquamarine4" 
low1 = "royalblue"
low2 = "royalblue3"
no1 = "gray60"
no2 = "gray30" 

# Model comparing changes in chlorophyll, zooplankton biomass, zooplankton abundance weighted mean-length # 
# 3 treatments - nofish, ambient, harvested # 
#=========== Chlorophyll #===================
# Load the nlme library
library(nlme)

# Ensure 'pond' and 'treatment' are factors
bmb_lme$pond <- as.factor(bmb_lme$pond)
bmb_lme$treatment <- as.factor(bmb_lme$treatment)

# Model 1: Random Intercept for Pond
mod_intercept <- lme(log_chl ~ treatment * log_doy, 
                     random = ~1 | pond,  # <--- This is the key change
                     weights = varIdent(form = ~1 | treatment),
                     data = bmb_lme, 
                     na.action = na.omit)

summary(mod_intercept)

# Intercept and log_doy have perfect correlation - this is unstable # 
# Fix by centering log_doy 
mean_log_doy <- mean(bmb_lme$log_doy, na.rm = TRUE)
bmb_lme$log_doy_c <- bmb_lme$log_doy - mean_log_doy

# 3. Re-run your model with the new centered variable
mod1 <- lme(log_chl ~ treatment * log_doy_c,  # <-- Use the new variable
            random = ~1 | pond,
            weights = varIdent(form = ~1 | treatment),
            data = bmb_lme, 
            na.action = na.omit)

# should account for any linear change in mean over time, 
# while also accounting for nonlinear among-doy variation arising between ponds 
summary(mod1)

# Residual (within-pond) variability is much higher (SD = 0.70) than between-pond variability (SD = 0.10).
# This noise (variance) is also unequal: highest in 'nofish' (1.0), lowest in 'ambient' (0.87).

# Slope here is on a log-log scale, so looking at the percent change in chl for a 1% change in DOY.

# Interactions (Note: Intercepts are at the *average* time point, not Day 1)
# No Fish (reference): Strong decline over time in log_chl (p < 0.0001); Intercept = 0.44, Slope = -7.77
# Ambient: Significantly higher intercept at avg. time (+0.55, p = 0.026), but shows the slowest decline (Slope = -3.37; interaction p < 0.0001)
# Harvested: Intercept not different from nofish (p = 0.63), with a moderate decline (Slope = -5.38; interaction p = 0.014)

# Brass Tax
# The main story is the *change over time* (the slopes):
#   All ponds show a significant decline in chlorophyll.
#   The 'nofish' ponds had the fastest decline (slope = -7.77).
#   The 'harvested' ponds moderated that decline (slope = -5.38).
#   The 'ambient' ponds had the slowest decline (slope = -3.37).
#
# At the *average* time point of the experiment (the "intercept"):
#   'ambient' ponds had significantly *higher* chlorophyll than 'nofish' ponds.
#   'harvested' and 'nofish' ponds were not different from each other.

# 1a. Create the x-axis values for prediction
doy_seq <- seq(min(bmb_lme$doy, na.rm = TRUE), 
               max(bmb_lme$doy, na.rm = TRUE), 
               by = 1)

# **CRITICAL STEP**: Get the same mean used to center your original data
mean_log_doy <- mean(bmb_lme$log_doy, na.rm = TRUE)

# Create the new data grid for prediction
newdata1 <- expand.grid(
  doy = doy_seq, # Start with the real x-axis
  treatment = levels(bmb_lme$treatment)
)

# **CRITICAL STEP**: Create the centered predictor variable for the new data
newdata1$log_doy_c <- log(newdata1$doy) - mean_log_doy

# 1b. Fixed-effects estimates and variance-covariance matrix
beta <- fixef(mod1)
Vbeta <- vcov(mod1)

# 1c. Model matrix for newdata1
# **CRITICAL CHANGE**: Update the formula to match your model
Xnew <- model.matrix(~ treatment * log_doy_c, data = newdata1)

# 1d. Predicted fit (fixed effects only) and standard errors
newdata1$fit <- as.vector(Xnew %*% beta)
se_fit <- sqrt( diag( Xnew %*% Vbeta %*% t(Xnew) ) )
newdata1$lower <- newdata1$fit - 1.96 * se_fit
newdata1$upper <- newdata1$fit + 1.96 * se_fit

# 1e. Plot with ggplot2 (This code is unchanged)
# (Assuming you have your color variables no1, high1, etc. in your environment)
windows(height = 6, width = 8)
ggplot(newdata1, aes(x = doy, y = fit, color = treatment, fill = treatment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  geom_line(size = 2) +
  labs(
    x = "Day of Year", 
    y = "Predicted log(Chlorophyll)", # **NOTE**: Your Y-axis is log_chl
    color = "Treatment", fill = "Treatment"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(no1, high1, low1)) +
  scale_fill_manual(values = c(no2, high2, low2))

# 2a. Extract standardized (normalized) residuals and fitted values
data_used1 <- getData(mod1) 
dim(data_used1)

data_used1$resid_std <- resid(mod1, type = "normalized")
data_used1$fitted    <- fitted(mod1)

# 2b. Identify observations with |resid_std| > 2
outliers <- subset(data_used1, abs(resid_std) > 2)
unique(outliers$doy) 

# Residuals vs Fitted
ggplot(data_used1, aes(x = fitted, y = resid_std, color = treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.7) +
  labs(x = "Fitted Value", y = "Standardized Residual") 

# QQ‐plot of standardized residuals
qqnorm(data_used1$resid_std); qqline(data_used1$resid_std)

# Slope Differences (significant interaction) 
chl_trends_compare <- emtrends(mod1, 
                               pairwise ~ treatment,  # Get pairwise differences
                               var = "log_doy_c")

# 2. Convert the *contrasts* to a data frame
sens_pairwise_chla <- as.data.frame(summary(chl_trends_compare$contrasts, infer = T))
sens_pairwise_chla

# 3. Create the ball-and-whisker plot
ggplot(slope_comparisons, aes(x = estimate, y = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2) +
  
  # Add a vertical line at 0 (no difference)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  
  labs(
    x = "Estimated Difference in Slopes",
    y = "Treatment Contrast",
    title = " (Interaction)"
  ) +
  theme_minimal()
text(6.75, 6.6, "B", font = 2)

#=========== zooplankton biomass  #===================
mod2 <- lme(log_zp ~ treatment * log_doy_c,  # <-- Use the new variable
            random = ~1 | pond,
            weights = varIdent(form = ~1 | treatment),
            data = bmb_lme, 
            na.action = na.omit)

# should account for any linear change in mean over time, 
# while also accounting for nonlinear among-doy variation arising between ponds 
summary(mod2)

# Between-pond variability is effectively zero (SD = 1.34e-05); all variation is residual (within-pond, SD = 0.33).
# Residual noise is nearly equal across all treatments (all parameters ~1.0).

# Slope here is on a log-log scale, so looking at the percent change in length for a 1% change in DOY.

# Interactions (Note: Intercepts are at the *average* time point)
# No Fish (reference): Strong, significant increase over time (p < 0.0001); Intercept = 3.68, Slope = +5.42
# Ambient: Intercept significantly lower (–0.46, p = 0.011), with a slope near zero (Slope = +5.42 - 5.27 = +0.15; interaction p < 0.0001)
# Harvested: Intercept not different from nofish (p = 0.064), with a significant moderate increase (Slope = +5.42 - 2.68 = +2.74; interaction p = 0.0005)

# Brass Tax
# The main story is the *change over time* (the slopes):
#   The 'nofish' ponds showed a strong, significant *increase* in length over time.
#   In contrast, both 'ambient' and 'harvested' ponds showed significantly *slower* rates of increase.
#   The 'ambient' ponds had the slowest rate of change (slope = +0.15).
#   The 'harvested' ponds had a moderate rate of increase (slope = +2.74).
#
# At the *average* time point of the experiment (the "intercept"):
#   'ambient' ponds had significantly *lower* length values than 'nofish' ponds.
#   'harvested' and 'nofish' ponds were not significantly different.

# 1a. Create the x-axis values for prediction
doy_seq <- seq(min(bmb_lme$doy, na.rm = TRUE), 
               max(bmb_lme$doy, na.rm = TRUE), 
               by = 1)

# **CRITICAL STEP**: Get the same mean used to center your original data
mean_log_doy <- mean(bmb_lme$log_doy, na.rm = TRUE)

# Create the new data grid for prediction
newdata2 <- expand.grid(
  doy = doy_seq, # Start with the real x-axis
  treatment = levels(bmb_lme$treatment)
)

# **CRITICAL STEP**: Create the centered predictor variable for the new data
newdata2$log_doy_c <- log(newdata2$doy) - mean_log_doy

# 1b. Fixed-effects estimates and variance-covariance matrix
beta <- fixef(mod2)
Vbeta <- vcov(mod2)

# 2c. Model matrix for newdata1
# **CRITICAL CHANGE**: Update the formula to match your model
Xnew <- model.matrix(~ treatment * log_doy_c, data = newdata2)

# 1d. Predicted fit (fixed effects only) and standard errors
newdata2$fit <- as.vector(Xnew %*% beta)
se_fit <- sqrt( diag( Xnew %*% Vbeta %*% t(Xnew) ) )
newdata2$lower <- newdata2$fit - 1.96 * se_fit
newdata2$upper <- newdata2$fit + 1.96 * se_fit

# 1e. Plot with ggplot2 (This code is unchanged)
# (Assuming you have your color variables no1, high1, etc. in your environment)
windows(height = 6, width = 8)
ggplot(newdata2, aes(x = doy, y = fit, color = treatment, fill = treatment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  geom_line(size = 2) +
  labs(
    x = "Day of Year", 
    y = "Predicted log(Length)", # **NOTE**: Your Y-axis is log_zp
    color = "Treatment", fill = "Treatment"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(no1, high1, low1)) +
  scale_fill_manual(values = c(no2, high2, low2))

# 2a. Extract standardized (normalized) residuals and fitted values
data_used2 <- getData(mod2) 
dim(data_used2)

data_used2$resid_std <- resid(mod2, type = "normalized")
data_used2$fitted    <- fitted(mod2)

# 2b. Identify observations with |resid_std| > 2
outliers <- subset(data_used2, abs(resid_std) > 2)
unique(outliers$doy) 

# Residuals vs Fitted
ggplot(data_used2, aes(x = fitted, y = resid_std, color = treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.7) +
  labs(x = "Fitted Value", y = "Standardized Residual") 

# QQ‐plot of standardized residuals
qqnorm(data_used2$resid_std); qqline(data_used2$resid_std)

# Slope Differences (significant interaction) 
zp_trends_compare <- emtrends(mod2, 
                              pairwise ~ treatment,  # Get pairwise differences
                              var = "log_doy_c")

# 2. Convert the *contrasts* to a data frame
sens_zoop_pairwise <- as.data.frame(summary(zp_trends_compare$contrasts, infer = T))
sens_zoop_pairwise

# 3. Create the ball-and-whisker plot
ggplot(slope_comparisons, aes(x = estimate, y = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2) +
  
  # Add a vertical line at 0 (no difference)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  
  labs(
    x = "Estimated Difference in Slopes",
    y = "Treatment Contrast",
    title = " (Interaction)"
  ) +
  theme_minimal()
text(9.85, 6.6, "D", font = 2)

#=========== Zooplankton Community Length  #===================
mod3 <- lme(log_length ~ treatment * log_doy_c,  # <-- Use the new variable
            random = ~1 | pond,
            weights = varIdent(form = ~1 | treatment),
            data = bmb_lme, 
            na.action = na.omit)

# should account for any linear change in mean over time, 
# while also accounting for nonlinear among-doy variation arising between ponds 
summary(mod3)

# Between-pond variability is effectively zero (SD = 1.34e-05); all variation is residual (within-pond, SD = 0.33).
# Residual noise is nearly identical across all three treatments (all parameters ~1.0).

# Slope here is on a log-log scale, so looking at the percent change in length for a 1% change in DOY.

# Interactions (Note: Intercepts are at the *average* time point)
# No Fish (reference): Strong, significant growth over time (p < 0.001); Intercept = 3.68, Slope = +5.42
# Ambient: Significantly shorter at avg. time (–0.46, p = 0.011), with growth almost completely halted (Slope = +0.15; interaction p < 0.001)
# Harvested: Intercept not different from nofish (p = 0.064), with significantly slower growth (Slope = +2.74; interaction p = 0.0005)

# Brass Tax
# The main story is the *growth rate* (the slopes):
#   The 'nofish' ponds (control) showed strong, rapid growth over time (slope = +5.42).
#   The 'harvested' ponds significantly reduced this growth rate by about half (slope = +2.74).
#   The 'ambient' ponds had the most dramatic effect, almost completely stopping growth (slope = +0.15).
#
# At the *average* time point of the experiment (the "intercept"):
#   Fish in the 'ambient' ponds were significantly *shorter* than fish in the 'nofish' ponds.
#   Fish in the 'harvested' and 'nofish' ponds were not significantly different in length.

# 1a. Create the x-axis values for prediction
doy_seq <- seq(min(bmb_lme$doy, na.rm = TRUE), 
               max(bmb_lme$doy, na.rm = TRUE), 
               by = 1)

# **CRITICAL STEP**: Get the same mean used to center your original data
mean_log_doy <- mean(bmb_lme$log_doy, na.rm = TRUE)

# Create the new data grid for prediction
newdata3 <- expand.grid(
  doy = doy_seq, # Start with the real x-axis
  treatment = levels(bmb_lme$treatment)
)

# **CRITICAL STEP**: Create the centered predictor variable for the new data
newdata3$log_doy_c <- log(newdata3$doy) - mean_log_doy

# 1b. Fixed-effects estimates and variance-covariance matrix
beta <- fixef(mod3)
Vbeta <- vcov(mod3)

# 1c. Model matrix for newdata1
# **CRITICAL CHANGE**: Update the formula to match your model
Xnew <- model.matrix(~ treatment * log_doy_c, data = newdata3)

# 1d. Predicted fit (fixed effects only) and standard errors
newdata3$fit <- as.vector(Xnew %*% beta)
se_fit <- sqrt( diag( Xnew %*% Vbeta %*% t(Xnew) ) )
newdata3$lower <- newdata3$fit - 1.96 * se_fit
newdata3$upper <- newdata3$fit + 1.96 * se_fit

# 1e. Plot with ggplot2 (This code is unchanged)
# (Assuming you have your color variables no1, high1, etc. in your environment)
windows(height = 6, width = 8)
ggplot(newdata3, aes(x = doy, y = fit, color = treatment, fill = treatment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  geom_line(size = 2) +
  labs(
    x = "Day of Year", 
    y = "Predicted log(lengthorophyll)", # **NOTE**: Your Y-axis is log_length
    color = "Treatment", fill = "Treatment"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(no1, high1, low1)) +
  scale_fill_manual(values = c(no2, high2, low2))

# 2a. Extract standardized (normalized) residuals and fitted values
data_used3 <- getData(mod3) 
dim(data_used3)

data_used3$resid_std <- resid(mod3, type = "normalized")
data_used3$fitted    <- fitted(mod3)

# 2b. Identify observations with |resid_std| > 2
outliers <- subset(data_used3, abs(resid_std) > 2)
unique(outliers$doy) 

# Residuals vs Fitted
ggplot(data_used3, aes(x = fitted, y = resid_std, color = treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.7) +
  labs(x = "Fitted Value", y = "Standardized Residual") 

# QQ‐plot of standardized residuals
qqnorm(data_used3$resid_std); qqline(data_used3$resid_std)

# Slope Differences (significant interaction) 
length_trends_compare <- emtrends(mod3, 
                                  pairwise ~ treatment,  # Get pairwise differences
                                  var = "log_doy_c")

# 2. Convert the *contrasts* to a data frame
slope_comparisons <- as.data.frame(summary(length_trends_compare$contrasts, infer = T))
slope_comparisons

# 3. Create the ball-and-whisker plot
ggplot(slope_comparisons, aes(x = estimate, y = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2) +
  
  # Add a vertical line at 0 (no difference)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  
  labs(
    x = "Estimated Difference in Slopes",
    y = "Treatment Contrast",
    title = " (Interaction)"
  ) +
  theme_minimal()
text(2.6, 6.6, "F", font = 2)

#========================= Recreate Wilcot Plot ======================================================
# loading in MASS earlier will now cause problems, just click dplyr over stats 
datA <- bmb_lme %>% 
  filter(pond == "A") 
datB <- bmb_lme %>% 
  filter(pond == "B") 
datC <- bmb_lme %>% 
  filter(pond == "C") 
datD <- bmb_lme %>% 
  filter(pond == "D") 
datE <- bmb_lme %>% 
  filter(pond == "E") 
datF <- bmb_lme %>% 
  filter(pond == "F") 

squircle_filled <- function(x0 = 0, y0 = 0, radius, stretch = 1.5, n = 1000, col = "gray", border = NA) {
  r <- function(radius, theta){
    radius/(1 - sin(2*theta)^2/2)^(1/4)
  }
  angle <- seq(0, 2*pi, length.out = n)
  rvec <- r(radius, angle)
  x <- rvec * cos(angle) * stretch + x0  # Apply horizontal stretch
  y <- rvec * sin(angle) + y0            # Leave vertical scale unchanged
  polygon(x, y, col = col, border = border)
}


# Put figures together in one strip of code # ======================================

# Plot Dimensions # ========================
windows(height = 8, width = 9)
par(mar = c(1, 3, 2, 0.5), oma = c(4, 4, 0.5, 0.5), tcl = -0.25, mgp = c(2, 0.6, 0))
layout_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)
layout(mat = layout_matrix, heights = c(2.67, 2.67, 2.66))  # SUM = 10 inches high

## Chlorophyll (Plot A) # ========================
# no fish 
plot(log_chl ~ doy, data = datA, type = "l", lty = 2, col = no1, ylab = "", xlab = "", 
     ylim = c(log(0.2), log(100)), yaxt = 'n', lwd = 2)
mtext("log(Chlorophyll ("~mu*"g"~`L`^1*"))", side = 2, line = 2.2, cex = 0.9)
points(log_chl ~ doy, data = datD, type = "l", lty = 2, col = no2, lwd = 2)

# harvested 
points(log_chl ~ doy, data = datC, type = "l", lty = 2, col = low1, lwd = 2)
points(log_chl ~ doy, data = datE, type = "l", lty = 2, col = low2, lwd = 2)

# ambient
points(log_chl ~ doy, data = datB, type = "l", lty = 2, col = high1, lwd = 2)
points(log_chl ~ doy, data = datF, type = "l", lty = 2, col = high2, lwd = 2)

axis(side=2, 
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), 
          log(0.), log(0.7), log(0.8), log(0.9),
          log(1), log(2), log(3), log(4), log(5), 
          log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40), log(50), 
          log(60), log(70), log(80), log(90), log(100)),
     labels = c("0.1", " ", " ", " ", " ", " ", " ", " ", " ",
                "", " ", " ", " ", " ", " ", " ", " ", " ",
                "", " ", " ", " ", " ", " ", " ", " ", " ", "100"), 
     las=2, cex.axis = 1.2)
axis(side=2, 
     at=c(log(1), log(10), log(100)),
     labels = c("1", "10", "100"),
     las=2, cex.axis = 1.2, tick = F)


# add in model fits # 
chl_fit <- newdata1 %>% 
  as_tibble() %>% 
  mutate(bt_chl = exp(fit))
chl_fit

chl_nofish <- chl_fit %>% 
  filter(treatment == "nofish")
chl_harv <- chl_fit %>% 
  filter(treatment == "harvested")
chl_amb <- chl_fit %>% 
  filter(treatment == "ambient")

polygon(
  x = c(chl_nofish$doy, rev(chl_nofish$doy)),
  y = c(chl_nofish$lower, rev(chl_nofish$upper)),
  col = adjustcolor(no1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(chl_harv$doy, rev(chl_harv$doy)),
  y = c(chl_harv$lower, rev(chl_harv$upper)),
  col = adjustcolor(low1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(chl_amb$doy, rev(chl_amb$doy)),
  y = c(chl_amb$lower, rev(chl_amb$upper)),
  col = adjustcolor(high1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

points(fit ~ doy, data = chl_nofish, type = "l", col = no1, lwd = 4)
points(fit ~ doy, data = chl_harv, type = "l", col = low1, lwd = 4)
points(fit ~ doy, data = chl_amb, type = "l", col = high1, lwd = 4)

polygon(c(115,123,123,115), c(log(55), log(55), log(100), log(100)), col="gray20") 
lines(c(123, 123), c(log(0.2), log(100)), lwd=2, lty=3, col="gray20")
text(119, log(75), "Pre Fish", col="white", font=2, cex = 0.9)
lines(c(129, 129), c(log(0.2), log(100)), lwd=2, lty=3, col="gray20")
polygon(c(129,150,150,129), c(log(55), log(55), log(100), log(100)), col="gray20") 
text(140, log(75), "Post Fish Addition", col="white", font=2, cex = 0.9)

# squircle it up 
squircle_filled(x0 = 150, y0 = log(28), radius = 0.6, stretch = 6, col = no1)
text(150, log(28), "No Fish", col="white", font=2, cex = 0.9)
squircle_filled(x0 = 157.5, y0 = log(28), radius = 0.6, stretch = 6, col = low1)
text(157.5, log(28), "Harvest", col="white", font=2, cex = 0.9)
squircle_filled(x0 = 164.75, y0 = log(28), radius = 0.6, stretch = 6, col = high1)
text(164.5, log(28), "Ambient", col="white", font=2, cex = 0.9)

plot_coords <- par("usr")
text(x = plot_coords[2] * 0.99, # 95% to the right edge
     y = plot_coords[4] * 0.96, # 95% to the top edge
     "A", 
     font = 2, 
     cex = 1.2)

## Pairwise Chla (Plot B) # ======================================
chl_trends_compare <- emtrends(mod1, 
                               pairwise ~ treatment,
                               var = "log_doy_c")

# 2. Convert the *SUMMARY* of the contrasts to a data frame with CIs
slope_comparisons <- as.data.frame(summary(chl_trends_compare$contrasts, infer = TRUE))

# 4. Create the main plot area
# We plot points but suppress the axes to draw them manually
plot(x = slope_comparisons$estimate, 
     y = nrow(slope_comparisons):1,  # Plot from top to bottom
     xlim = range(slope_comparisons$lower.CL, slope_comparisons$upper.CL), # Set x-axis limits from data
     ylim = c(0.5, nrow(slope_comparisons) + 0.5), # Add space on y-axis
     xlab = "Estimated Difference in Slopes",
     ylab = "", # Suppress y-axis title
     main = "",
     pch = 16,      # Use solid circles
     cex = 1.5,       # Make points larger
     cex.axis = 1.2,
     yaxt = "n", 
     col = "white")     # Suppress y-axis labels

# 5. Add horizontal error bars (the "whiskers")
arrows(x0 = slope_comparisons$lower.CL, 
       y0 = nrow(slope_comparisons):1, 
       x1 = slope_comparisons$upper.CL, 
       y1 = nrow(slope_comparisons):1,
       angle = 90,     # Makes the ends flat
       code = 3,       # Puts flat ends on both sides
       length = 0.05)  # Sets the size of the flat ends

# 6. Add the vertical reference line at zero
abline(v = 0, lty = 2, col = "gray40")  # Make labels horizontal

# 8. Add the "B" label in the top right corner
# Note: You may need to adjust x and y coordinates depending on your data range
# We use par("usr") to get the coordinates of the plot region
plot_coords <- par("usr")
text(x = plot_coords[2] * 0.95, # 95% to the right edge
     y = plot_coords[4] * 0.97, # 95% to the top edge
     "B", 
     font = 2, 
     cex = 1.2)
# text(x = -0.2, y = plot_coords[4] * 0.97, "*", font = 2, cex = 2)
# text(x = 0.6, y = plot_coords[4] * 0.97, "n.s.", font = 2, cex = 1.5)

# The averaged RGB values
high2_rgb = col2rgb(high2)
low2_rgb = col2rgb(low2)
mixed_color <- "black"

points(x = slope_comparisons$estimate, 
       y = nrow(slope_comparisons):1,
       pch = 21, 
       col = "black",
       lwd = 2,
       bg = c(high2, low2, mixed_color), # indexing weird, but this is the correct order 
       cex = 2)

axis(2, at = c(3,2,1), tick = T, labels = c("A - NF", 
                                            "H - NF", 
                                            "A - H"), 
     las = 0, cex.axis = 1.2)
mtext("Chlorophyll Pairwise Differences", side = 2, line = 2.2, cex = 0.9)

## Zooplankton Biomass (Plot C) =======================================

# no fish 
plot(log_zp ~ doy, data = datzA, type = "l", lty = 2, col = no1, ylab = "", xlab = "", 
     ylim = c(log(5), log(10000)), yaxt = 'n', xlim = c(115, 168), lwd = 2)
mtext("log(Zooplankton Biomass ("~mu*"g"~`L`^1*"))", side = 2, line = 3, cex = 0.9)
points(log_zp ~ doy, data = datzD, type = "l", lty = 2, col = no2, lwd = 2)

# harvested 
points(log_zp ~ doy, data = datzC, type = "l", lty = 2, col = low1, lwd = 2)
points(log_zp ~ doy, data = datzE, type = "l", lty = 2, col = low2, lwd = 2)

# ambient
points(log_zp ~ doy, data = datzB, type = "l", lty = 2, col = high1, lwd = 2)
points(log_zp ~ doy, data = datzF, type = "l", lty = 2, col = high2, lwd = 2)

axis(side=2, 
     at=c(log(10), log(20), log(30), log(40), log(50), 
          log(60), log(70), log(80), log(90), log(100), 
          log(200), log(300), log(400), log(500), 
          log(600), log(700), log(800), log(900), log(1000), 
          log(2000), log(3000), log(4000), log(5000), 
          log(6000), log(7000), log(8000), log(9000), log(10000)),
     labels = c("10", " ", " ", " ", " ", " ", " ", " ", " ", 
                "100", " ", " ", " ", " ", " ", " ", " ", " ", 
                "1000"," "," "," "," "," "," "," "," ","10000"), 
     las=2, cex.axis = 1.2)
axis(side=2, 
     at=c(log(100), log(1000), log(10000)),
     labels = c("100", 
                "1000","10000"), 
     las=2, cex.axis = 1.2, tick = F)


# add in model fits # 
zp_fit <- newdata2 %>% 
  as_tibble() %>% 
  mutate(bt_zp = exp(fit))
zp_fit

zp_nofish <- zp_fit %>% 
  filter(treatment == "nofish")
zp_harv <- zp_fit %>% 
  filter(treatment == "harvested")
zp_amb <- zp_fit %>% 
  filter(treatment == "ambient")

polygon(
  x = c(zp_nofish$doy, rev(zp_nofish$doy)),
  y = c(zp_nofish$lower, rev(zp_nofish$upper)),
  col = adjustcolor(no1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(zp_harv$doy, rev(zp_harv$doy)),
  y = c(zp_harv$lower, rev(zp_harv$upper)),
  col = adjustcolor(low1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(zp_amb$doy, rev(zp_amb$doy)),
  y = c(zp_amb$lower, rev(zp_amb$upper)),
  col = adjustcolor(high1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

points(fit ~ doy, data = zp_nofish, type = "l", col = no1, lwd = 4)
points(fit ~ doy, data = zp_harv, type = "l", col = low1, lwd = 4)
points(fit ~ doy, data = zp_amb, type = "l", col = high1, lwd = 4)

# polygon(c(115,123,123,115), c(log(5000), log(5000), log(10000), log(10000)), col="gray20") # Pre-Fish
lines(c(122.9, 122.9), c(log(5), log(50000)), lwd=2, lty=3, col="gray20")
# text(119, log(7500), "Pre Fish", col="white", font=2, cex = 0.9)
lines(c(129.1, 129.1), c(log(5), log(60000)), lwd=2, lty=3, col="gray20")
# polygon(c(129,150,150,129), c(log(5000), log(5000), log(10000), log(10000)), col="gray20") # Second Fish
# text(140, log(7500), "Post Fish Addition", col="white", font=2, cex = 0.9)

# squircle it up 
# 
# squircle_filled(x0 = 150, y0 = log(2300), radius = 0.6, stretch = 5, col = no1)
# text(150, log(28), "No Fish", col="white", font=2, cex = 0.9)
# squircle_filled(x0 = 157.5, y0 = log(2300), radius = 0.6, stretch = 5, col = low1)
# text(157.5, log(28), "Harvest", col="white", font=2, cex = 0.9)
# squircle_filled(x0 = 164.75, y0 = log(2300), radius = 0.6, stretch = 5, col = high1)
# text(164.5, log(28), "Ambient", col="white", font=2, cex = 0.9)
# 
# text(169, log(100), "A", font = 2, cex = 1.5)
# 
# squircle_filled(x0 = 152, y0 = log(2300), radius = 0.7, stretch = 5, col = no1)
# text(152, log(2300), "No Fish", col="white", font=2, cex = 1)
# squircle_filled(x0 = 159, y0 = log(2300), radius = 0.7, stretch = 5, col = low1)
# text(159, log(2300), "Harvest", col="white", font=2, cex = 1)
# squircle_filled(x0 = 166, y0 = log(2300), radius = 0.7, stretch = 5, col = high1)
# text(166, log(2300), "Ambient", col="white", font=2, cex = 1)

plot_coords <- par("usr")
text(x = plot_coords[2] * 0.99, # 95% to the right edge
     y = plot_coords[4] * 0.96, # 95% to the top edge
     "C", 
     font = 2, 
     cex = 1.2)

## Pairwise Zooplankton Biomass (Plot D) ================================
zp_trends_compare <- emtrends(mod2, 
                              pairwise ~ treatment,
                              var = "log_doy_c")

# 2. Convert the *SUMMARY* of the contrasts to a data frame with CIs
slope_comparisons <- as.data.frame(summary(zp_trends_compare$contrasts, infer = TRUE))

# 4. Create the main plot area
# We plot points but suppress the axes to draw them manually
plot(x = slope_comparisons$estimate, 
     y = nrow(slope_comparisons):1,  # Plot from top to bottom
     xlim = range(slope_comparisons$lower.CL, slope_comparisons$upper.CL), # Set x-axis limits from data
     ylim = c(0.5, nrow(slope_comparisons) + 0.5), # Add space on y-axis
     xlab = "Estimated Difference in Slopes",
     ylab = "", # Suppress y-axis title
     main = "",
     pch = 16,      # Use solid circles
     cex = 1.5,       # Make points larger
     cex.axis = 1.2,
     yaxt = "n", 
     col = "white")     # Suppress y-axis labels

# 5. Add horizontal error bars (the "whiskers")
arrows(x0 = slope_comparisons$lower.CL, 
       y0 = nrow(slope_comparisons):1, 
       x1 = slope_comparisons$upper.CL, 
       y1 = nrow(slope_comparisons):1,
       angle = 90,     # Makes the ends flat
       code = 3,       # Puts flat ends on both sides
       length = 0.05)  # Sets the size of the flat ends

# 6. Add the vertical reference line at zero
abline(v = 0, lty = 2, col = "gray40")  # Make labels horizontal

# 8. Add the "B" label in the top right corner
# Note: You may need to adjust x and y coordinates depending on your data range
# We use par("usr") to get the coordinates of the plot region
plot_coords <- par("usr")
text(x = plot_coords[2] * 0.95, # 95% to the right edge
     y = plot_coords[4] * 0.97, # 95% to the top edge
     "D", 
     font = 2, 
     cex = 1.2)
# text(x = -0.2, y = plot_coords[4] * 0.97, "*", font = 2, cex = 2)
# text(x = 0.6, y = plot_coords[4] * 0.97, "n.s.", font = 2, cex = 1.5)

# The averaged RGB values
high2_rgb = col2rgb(high2)
low2_rgb = col2rgb(low2)
mixed_color <- "black"

points(x = slope_comparisons$estimate, 
       y = nrow(slope_comparisons):1,
       pch = 21, 
       col = "black",
       lwd = 2,
       bg = c(high2, low2, mixed_color), # indexing weird, but this is the correct order 
       cex = 2)

axis(2, at = c(3,2,1), tick = T, labels = c("A - NF", 
                                            "H - NF", 
                                            "A - H"), 
     las = 0, cex.axis = 1.2)
mtext("Zoop Pairwise Differences", side = 2, line = 2.2, cex = 0.9)

## Length (Plot E) ========================================

# no fish 
plot(log_length ~ doy, data = datlA, type = "l", lty = 2, col = no1, ylab = "", xlab = "", 
     ylim = c(log(10), log(200)), yaxt = 'n', xlim = c(115, 168), lwd = 2)
mtext(side=2, line=2.8, expression(Abundance~Weighted), cex=0.9)
mtext(side=2, line=1.8, expression(Mean~Length~"("*mu*m*")"), cex = 0.9)
mtext("Day of Year, 2019", side = 1, line = 2.2, cex = 0.9)
points(log_length ~ doy, data = datlD, type = "l", lty = 2, col = no2, lwd = 2)

# harvested 
points(log_length ~ doy, data = datlC, type = "l", lty = 2, col = low1, lwd = 2)
points(log_length ~ doy, data = datlE, type = "l", lty = 2, col = low2, lwd = 2)

# ambient
points(log_length ~ doy, data = datlB, type = "l", lty = 2, col = high1, lwd = 2)
points(log_length ~ doy, data = datlF, type = "l", lty = 2, col = high2, lwd = 2)

axis(side = 2, las = 2, cex.axis = 1, 
     at=c(log(10), log(20), log(30), log(40), log(50), 
          log(60), log(70), log(80), log(90), 
          log(100), log(200)),
     labels = c("10", " ", " ", " ", "50", 
                " ", " ", " ", " ", "100", ""), cex.axis = 1.2)
axis(side = 2, las = 2, cex.axis = 1, 
     at=c(log(100), log(200)),
     labels = c("100", "200"), cex.axis = 1.2)

# add in model fits # 
length_fit <- newdata3 %>% 
  as_tibble() %>% 
  mutate(bt_length = exp(fit))
length_fit

length_nofish <- length_fit %>% 
  filter(treatment == "nofish")
length_harv <- length_fit %>% 
  filter(treatment == "harvested")
length_amb <- length_fit %>% 
  filter(treatment == "ambient")

polygon(
  x = c(length_nofish$doy, rev(length_nofish$doy)),
  y = c(length_nofish$lower, rev(length_nofish$upper)),
  col = adjustcolor(no1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(length_harv$doy, rev(length_harv$doy)),
  y = c(length_harv$lower, rev(length_harv$upper)),
  col = adjustcolor(low1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(length_amb$doy, rev(length_amb$doy)),
  y = c(length_amb$lower, rev(length_amb$upper)),
  col = adjustcolor(high1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

points(fit ~ doy, data = length_nofish, type = "l", col = no1, lwd = 4)
points(fit ~ doy, data = length_harv, type = "l", col = low1, lwd = 4)
points(fit ~ doy, data = length_amb, type = "l", col = high1, lwd = 4)


# polygon(c(115,123,123,115), c(log(150), log(150), log(200), log(200)), col="gray20") # Pre-Fish
lines(c(122.9, 122.9), c(log(10), log(2000)), lwd=2, lty=3, col="gray20")
# text(119, log(175), "Pre Fish", col="white", font=2, cex = 1.2)
lines(c(129.1, 129.1), c(log(10), log(2000)), lwd=2, lty=3, col="gray20")
# polygon(c(129,168,168,129), c(log(150), log(150), log(200), log(200)), col="gray20") # Second Fish
# text(148, log(175), "Post Fish Addition", col="white", font=2, cex = 1.2)

# squircle it up 
# squircle_filled(x0 = 134, y0 = log(114), radius = 0.25, stretch = 12, col = no1)
# text(134, log(114), "No Fish", col="white", font=2, cex = 1)
# squircle_filled(x0 = 140, y0 = log(114), radius = 0.25, stretch = 12, col = low1)
# text(140, log(114), "Harvest", col="white", font=2, cex = 1)
# squircle_filled(x0 = 146, y0 = log(114), radius = 0.25, stretch = 12, col = high1)
# text(146, log(114), "Ambient", col="white", font=2, cex = 1)

plot_coords <- par("usr")
text(x = plot_coords[2] * 0.99, # 95% to the right edge
     y = plot_coords[4] * 0.96, # 95% to the top edge
     "E", 
     font = 2, 
     cex = 1.2)

## Pairwise Length (Plot F) =================================
length_trends_compare <- emtrends(mod3, 
                                  pairwise ~ treatment,
                                  var = "log_doy_c")

# 2. Convert the *SUMMARY* of the contrasts to a data frame with CIs
slope_comparisons <- as.data.frame(summary(length_trends_compare$contrasts, infer = TRUE))

# 4. Create the main plot area
# We plot points but suppress the axes to draw them manually
plot(x = slope_comparisons$estimate, 
     y = nrow(slope_comparisons):1,  # Plot from top to bottom
     xlim = range(slope_comparisons$lower.CL, slope_comparisons$upper.CL), # Set x-axis limits from data
     ylim = c(0.5, nrow(slope_comparisons) + 0.5), # Add space on y-axis
     xlab = "Estimated Difference in Slopes",
     ylab = "", # Suppress y-axis title
     main = "",
     pch = 16,      # Use solid circles
     cex = 1.5,       # Make points larger
     cex.axis = 1.2,
     yaxt = "n", 
     col = "white")     # Suppress y-axis labels

# 5. Add horizontal error bars (the "whiskers")
arrows(x0 = slope_comparisons$lower.CL, 
       y0 = nrow(slope_comparisons):1, 
       x1 = slope_comparisons$upper.CL, 
       y1 = nrow(slope_comparisons):1,
       angle = 90,     # Makes the ends flat
       code = 3,       # Puts flat ends on both sides
       length = 0.05)  # Sets the size of the flat ends

# 6. Add the vertical reference line at zero
abline(v = 0, lty = 2, col = "gray40")  # Make labels horizontal

# 8. Add the "B" label in the top right corner
# Note: You may need to adjust x and y coordinates depending on your data range
# We use par("usr") to get the coordinates of the plot region
plot_coords <- par("usr")
text(x = plot_coords[2] * 0.95, # 95% to the right edge
     y = plot_coords[4] * 0.97, # 95% to the top edge
     "F", 
     font = 2, 
     cex = 1.2)
# text(x = -0.2, y = plot_coords[4] * 0.97, "*", font = 2, cex = 2)
# text(x = 0.6, y = plot_coords[4] * 0.97, "n.s.", font = 2, cex = 1.5)

# The averaged RGB values
high2_rgb = col2rgb(high2)
low2_rgb = col2rgb(low2)
mixed_color <- "black"

points(x = slope_comparisons$estimate, 
       y = nrow(slope_comparisons):1,
       pch = 21, 
       col = "black",
       lwd = 2,
       bg = c(high2, low2, mixed_color), # indexing weird, but this is the correct order 
       cex = 2)

axis(2, at = c(3,2,1), tick = T, labels = c("A - NF", 
                                            "H - NF", 
                                            "A - H"), 
     las = 0, cex.axis = 1.2)
mtext("Estimated Difference in Slope", side = 1, line = 2.2, cex = 0.9)
mtext("Length Pairwise Differences", side = 2, line = 2.2, cex = 0.9)


# Used emmeans() to pull the pairwise contrasts between treatments along with standard errors and confidence intervals 
legend(0.5, 1.75, legend = c("Ambient - No Fish", 
                                 "Harvest - No Fish",
                                 "Ambient - Harvest"), 
       pch = 21,
       pt.bg = c(high2, low2, mixed_color),
       col = "black",
       cex = 1.7,
       pt.lwd = 2,
       pt.cex = 2, 
       bty = 'n')

# Nutrient LMEs # ================================================
bmb_lme = bmb %>%
  # filter(doy>128) %>%
  mutate(treatment = as_factor(treatment), 
         fishtreat = ordered(treatment, 
                             levels = c("nofish", "harvested", "ambient"))) %>% 
  as_tibble() %>% 
  mutate(log_doy = log(doy), 
         log_chl = log(chl), 
         log_zp = log(total_zoop), 
         log_length = log(commLength),
         log_tp = log(tp+1),
         log_tn = log(tn+1))
bmb_lme


#Colors for plots
high1 = rgb(102,205,170, max = 255, alpha = 200)
high2 = "aquamarine3" 
low1 = rgb(65, 105, 225, max = 255, alpha = 180)
low2 = "royalblue"
no1 = rgb(153,153,153, max = 255, alpha = 180)
no2 = "gray60" 

# Model comparing changes in chlorophyll, zooplankton biomass, zooplankton abundance weighted mean-length # 
  # 3 treatments - nofish, ambient, harvested # 
## Total Phosphorus # ======================================
# Load the nlme library
library(nlme)

# Ensure 'pond' and 'treatment' are factors
bmb_lme$pond <- as.factor(bmb_lme$pond)
bmb_lme$treatment <- as.factor(bmb_lme$treatment)

# Model 1: Random Intercept for Pond
mod_intercept <- lme(log_tp ~ treatment * log_doy, 
                     random = ~1 | pond,  # <--- This is the key change
                     weights = varIdent(form = ~1 | treatment),
                     data = bmb_lme, 
                     na.action = na.omit)

summary(mod_intercept)

# Intercept and log_doy have perfect correlation - this is unstable # 
  # Fix by centering log_doy 
mean_log_doy <- mean(bmb_lme$log_doy, na.rm = TRUE)
bmb_lme$log_doy_c <- bmb_lme$log_doy - mean_log_doy

# 3. Re-run your model with the new centered variable
mod1 <- lme(log_tp ~ treatment * log_doy_c,  # <-- Use the new variable
                 random = ~1 | pond,
                 weights = varIdent(form = ~1 | treatment),
                 data = bmb_lme, 
                 na.action = na.omit)

# should account for any linear change in mean over time, 
  # while also accounting for nonlinear among-doy variation arising between ponds 
summary(mod1)

# Residual (within-pond) variability is much higher (SD = 0.70) than between-pond variability (SD = 0.10).
  # This noise (variance) is also unequal: highest in 'nofish' (1.0), lowest in 'ambient' (0.87).

# Slope here is on a log-log scale, so looking at the percent change in chl for a 1% change in DOY.

# Interactions (Note: Intercepts are at the *average* time point, not Day 1)
  # No Fish (reference): Strong decline over time in log_chl (p < 0.0001); Intercept = 0.44, Slope = -7.77
  # Ambient: Significantly higher intercept at avg. time (+0.55, p = 0.026), but shows the slowest decline (Slope = -3.37; interaction p < 0.0001)
  # Harvested: Intercept not different from nofish (p = 0.63), with a moderate decline (Slope = -5.38; interaction p = 0.014)

# Brass Tax
  # The main story is the *change over time* (the slopes):
  #   All ponds show a significant decline in chlorophyll.
  #   The 'nofish' ponds had the fastest decline (slope = -7.77).
  #   The 'harvested' ponds moderated that decline (slope = -5.38).
  #   The 'ambient' ponds had the slowest decline (slope = -3.37).
  #
  # At the *average* time point of the experiment (the "intercept"):
  #   'ambient' ponds had significantly *higher* chlorophyll than 'nofish' ponds.
  #   'harvested' and 'nofish' ponds were not different from each other.

# 1a. Create the x-axis values for prediction
doy_seq <- seq(min(bmb_lme$doy, na.rm = TRUE), 
               max(bmb_lme$doy, na.rm = TRUE), 
               by = 1)

# **CRITICAL STEP**: Get the same mean used to center your original data
mean_log_doy <- mean(bmb_lme$log_doy, na.rm = TRUE)

# Create the new data grid for prediction
newdata1 <- expand.grid(
  doy = doy_seq, # Start with the real x-axis
  treatment = levels(bmb_lme$treatment)
)

# **CRITICAL STEP**: Create the centered predictor variable for the new data
newdata1$log_doy_c <- log(newdata1$doy) - mean_log_doy

# 1b. Fixed-effects estimates and variance-covariance matrix
beta <- fixef(mod1)
Vbeta <- vcov(mod1)

# 1c. Model matrix for newdata1
# **CRITICAL CHANGE**: Update the formula to match your model
Xnew <- model.matrix(~ treatment * log_doy_c, data = newdata1)

# 1d. Predicted fit (fixed effects only) and standard errors
newdata1$fit <- as.vector(Xnew %*% beta)
se_fit <- sqrt( diag( Xnew %*% Vbeta %*% t(Xnew) ) )
newdata1$lower <- newdata1$fit - 1.96 * se_fit
newdata1$upper <- newdata1$fit + 1.96 * se_fit

# 1e. Plot with ggplot2 (This code is unchanged)
# (Assuming you have your color variables no1, high1, etc. in your environment)
windows(height = 6, width = 8)
ggplot(newdata1, aes(x = doy, y = fit, color = treatment, fill = treatment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  geom_line(size = 2) +
  labs(
    x = "Day of Year", 
    y = "Predicted log(Total Phosphorus)", # **NOTE**: Your Y-axis is log_chl
    color = "Treatment", fill = "Treatment"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(no1, high1, low1)) +
  scale_fill_manual(values = c(no2, high2, low2))

# 2a. Extract standardized (normalized) residuals and fitted values
data_used1 <- getData(mod1) 
dim(data_used1)

data_used1$resid_std <- resid(mod1, type = "normalized")
data_used1$fitted    <- fitted(mod1)

# 2b. Identify observations with |resid_std| > 2
outliers <- subset(data_used1, abs(resid_std) > 2)
unique(outliers$doy) 

# Residuals vs Fitted
ggplot(data_used1, aes(x = fitted, y = resid_std, color = treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.7) +
  labs(x = "Fitted Value", y = "Standardized Residual") 

# QQ‐plot of standardized residuals
qqnorm(data_used1$resid_std); qqline(data_used1$resid_std)

# Slope Differences (significant interaction) 
tp_trends_compare <- emtrends(mod1, 
                              pairwise ~ treatment,  # Get pairwise differences
                              var = "log_doy_c")

# 2. Convert the *contrasts* to a data frame
slope_comparisons <- as.data.frame(summary(tp_trends_compare$contrasts, infer = T))
pairwise_tp_comparisons <- slope_comparisons

# 3. Create the ball-and-whisker plot
ggplot(slope_comparisons, aes(x = estimate, y = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2) +
  
  # Add a vertical line at 0 (no difference)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  
  labs(
    x = "Estimated Difference in Slopes",
    y = "Treatment Contrast",
    title = " (Interaction)"
  ) +
  theme_minimal()
text(6.75, 6.6, "B", font = 2)

## Total Nitrogen # ==========================================
mod2 <- lme(log_tn ~ treatment * log_doy_c,  # <-- Use the new variable
                 random = ~1 | pond,
                 weights = varIdent(form = ~1 | treatment),
                 data = bmb_lme, 
                 na.action = na.omit)

# should account for any linear change in mean over time, 
  # while also accounting for nonlinear among-doy variation arising between ponds 
summary(mod2)

# Between-pond variability is effectively zero (SD = 1.34e-05); all variation is residual (within-pond, SD = 0.33).
# Residual noise is nearly equal across all treatments (all parameters ~1.0).

# Slope here is on a log-log scale, so looking at the percent change in length for a 1% change in DOY.

# Interactions (Note: Intercepts are at the *average* time point)
  # No Fish (reference): Strong, significant increase over time (p < 0.0001); Intercept = 3.68, Slope = +5.42
  # Ambient: Intercept significantly lower (–0.46, p = 0.011), with a slope near zero (Slope = +5.42 - 5.27 = +0.15; interaction p < 0.0001)
  # Harvested: Intercept not different from nofish (p = 0.064), with a significant moderate increase (Slope = +5.42 - 2.68 = +2.74; interaction p = 0.0005)

# Brass Tax
  # The main story is the *change over time* (the slopes):
  #   The 'nofish' ponds showed a strong, significant *increase* in length over time.
  #   In contrast, both 'ambient' and 'harvested' ponds showed significantly *slower* rates of increase.
  #   The 'ambient' ponds had the slowest rate of change (slope = +0.15).
  #   The 'harvested' ponds had a moderate rate of increase (slope = +2.74).
  #
  # At the *average* time point of the experiment (the "intercept"):
  #   'ambient' ponds had significantly *lower* length values than 'nofish' ponds.
  #   'harvested' and 'nofish' ponds were not significantly different.

# 1a. Create the x-axis values for prediction
doy_seq <- seq(min(bmb_lme$doy, na.rm = TRUE), 
               max(bmb_lme$doy, na.rm = TRUE), 
               by = 1)

# **CRITICAL STEP**: Get the same mean used to center your original data
mean_log_doy <- mean(bmb_lme$log_doy, na.rm = TRUE)

# Create the new data grid for prediction
newdata2 <- expand.grid(
  doy = doy_seq, # Start with the real x-axis
  treatment = levels(bmb_lme$treatment)
)

# **CRITICAL STEP**: Create the centered predictor variable for the new data
newdata2$log_doy_c <- log(newdata2$doy) - mean_log_doy

# 1b. Fixed-effects estimates and variance-covariance matrix
beta <- fixef(mod2)
Vbeta <- vcov(mod2)

# 2c. Model matrix for newdata1
# **CRITICAL CHANGE**: Update the formula to match your model
Xnew <- model.matrix(~ treatment * log_doy_c, data = newdata2)

# 1d. Predicted fit (fixed effects only) and standard errors
newdata2$fit <- as.vector(Xnew %*% beta)
se_fit <- sqrt( diag( Xnew %*% Vbeta %*% t(Xnew) ) )
newdata2$lower <- newdata2$fit - 1.96 * se_fit
newdata2$upper <- newdata2$fit + 1.96 * se_fit

# 1e. Plot with ggplot2 (This code is unchanged)
# (Assuming you have your color variables no1, high1, etc. in your environment)
windows(height = 6, width = 8)
ggplot(newdata2, aes(x = doy, y = fit, color = treatment, fill = treatment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  geom_line(size = 2) +
  labs(
    x = "Day of Year", 
    y = "Predicted log(Length)", # **NOTE**: Your Y-axis is log_zp
    color = "Treatment", fill = "Treatment"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(no1, high1, low1)) +
  scale_fill_manual(values = c(no2, high2, low2))

# 2a. Extract standardized (normalized) residuals and fitted values
data_used2 <- getData(mod2) 
dim(data_used2)

data_used2$resid_std <- resid(mod2, type = "normalized")
data_used2$fitted    <- fitted(mod2)

# 2b. Identify observations with |resid_std| > 2
outliers <- subset(data_used2, abs(resid_std) > 2)
unique(outliers$doy) 

# Residuals vs Fitted
ggplot(data_used2, aes(x = fitted, y = resid_std, color = treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.7) +
  labs(x = "Fitted Value", y = "Standardized Residual") 

# QQ‐plot of standardized residuals
qqnorm(data_used2$resid_std); qqline(data_used2$resid_std)

# Slope Differences (significant interaction) 
tn_trends_compare <- emtrends(mod2, 
                              pairwise ~ treatment,  # Get pairwise differences
                              var = "log_doy_c")

# 2. Convert the *contrasts* to a data frame
slope_comparisons <- as.data.frame(summary(tn_trends_compare$contrasts, infer = T))
pairwise_tn_slopes <- slope_comparisons

# 3. Create the ball-and-whisker plot
ggplot(slope_comparisons, aes(x = estimate, y = contrast)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2) +
  
  # Add a vertical line at 0 (no difference)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  
  labs(
    x = "Estimated Difference in Slopes",
    y = "Treatment Contrast",
    title = " (Interaction)"
  ) +
  theme_minimal()
text(9.85, 6.6, "D", font = 2)


windows(height = 8, width = 8)
par(mar = c(1, 3, 2, 0.5), oma = c(4, 4, 0.5, 0.5), tcl = -0.25, mgp = c(2, 0.6, 0))
layout_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
layout(mat = layout_matrix, heights = c(2.67, 2.67, 2.66))  # SUM = 10 inches high

## TP (Plot A) # ========================
# no fish 
plot(log_tp ~ doy, data = datA, type = "o", lty = 2, col = no1, ylab = "", xlab = "", 
     ylim = c(log(8), log(200)), yaxt = 'n', lwd = 2)
mtext("log(Total Phosphorus ("~mu*"g"~`L`^1*"))", side = 2, line = 2.2, cex = 0.9)
points(log_tp ~ doy, data = datD, type = "o", lty = 2, col = no2, lwd = 2)

# harvested 
points(log_tp ~ doy, data = datC, type = "o", lty = 2, col = low1, lwd = 2)
points(log_tp ~ doy, data = datE, type = "o", lty = 2, col = low2, lwd = 2)

# ambient
points(log_tp ~ doy, data = datB, type = "o", lty = 2, col = high1, lwd = 2)
points(log_tp ~ doy, data = datF, type = "o", lty = 2, col = high2, lwd = 2)

axis(side=2, 
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), 
          log(0.), log(0.7), log(0.8), log(0.9),
          log(1), log(2), log(3), log(4), log(5), 
          log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40), log(50), 
          log(60), log(70), log(80), log(90), log(100), log(200)),
     labels = c("0.1", " ", " ", " ", " ", " ", " ", " ", " ",
                "", " ", " ", " ", " ", " ", " ", " ", " ",
                "", " ", " ", " ", " ", " ", " ", " ", " ", "100", "200"), 
     las=2, cex.axis = 1.2)
axis(side=2, 
     at=c(log(1), log(10), log(100)),
     labels = c("1", "10", "100"),
     las=2, cex.axis = 1.2, tick = F)


# add in model fits # 
tp_fit <- newdata1 %>% 
  as_tibble() %>% 
  mutate(bt_tp = exp(fit))
tp_fit

tp_nofish <- tp_fit %>% 
  filter(treatment == "nofish")
tp_harv <- tp_fit %>% 
  filter(treatment == "harvested")
tp_amb <- tp_fit %>% 
  filter(treatment == "ambient")

polygon(
  x = c(tp_nofish$doy, rev(tp_nofish$doy)),
  y = c(tp_nofish$lower, rev(tp_nofish$upper)),
  col = adjustcolor(no1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(tp_harv$doy, rev(tp_harv$doy)),
  y = c(tp_harv$lower, rev(tp_harv$upper)),
  col = adjustcolor(low1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(tp_amb$doy, rev(tp_amb$doy)),
  y = c(tp_amb$lower, rev(tp_amb$upper)),
  col = adjustcolor(high1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

points(fit ~ doy, data = tp_nofish, type = "l", col = no1, lwd = 4)
points(fit ~ doy, data = tp_harv, type = "l", col = low1, lwd = 4)
points(fit ~ doy, data = tp_amb, type = "l", col = high1, lwd = 4)

polygon(c(115,123,123,115), c(log(100), log(100), log(200), log(200)), col="gray20") 
lines(c(123, 123), c(log(0.2), log(200)), lwd=2, lty=3, col="gray20")
text(119, log(150), "Pre Fish", col="white", font=2, cex = 0.8)
lines(c(129, 129), c(log(0.2), log(200)), lwd=2, lty=3, col="gray20")
polygon(c(129,150,150,129), c(log(100), log(100), log(200), log(200)), col="gray20") 
text(140, log(150), "Post Fish Addition", col="white", font=2, cex = 0.9)

# squircle it up 
squircle_filled(x0 = 146, y0 = log(90), radius = 0.3, stretch = 16, col = no1)
text(146, log(90), "No Fish", col="white", font=2, cex = 0.9)
squircle_filled(x0 = 156, y0 = log(90), radius = 0.3, stretch = 16, col = low1)
text(156, log(90), "Harvest", col="white", font=2, cex = 0.9)
squircle_filled(x0 = 164.75, y0 = log(90), radius = 0.3, stretch = 16, col = high1)
text(164.75, log(90), "Ambient", col="white", font=2, cex = 0.9)

plot_coords <- par("usr")
text(x = plot_coords[2] * 0.99, # 95% to the right edge
     y = plot_coords[4] * 0.96, # 95% to the top edge
     "A", 
     font = 2, 
     cex = 1.2)

## Pairwise Chla (Plot B) # ======================================
tp_trends_compare <- emtrends(mod1, 
                               pairwise ~ treatment,
                               var = "log_doy_c")

# 2. Convert the *SUMMARY* of the contrasts to a data frame with CIs
slope_comparisons <- as.data.frame(summary(tp_trends_compare$contrasts, infer = TRUE))

# 4. Create the main plot area
# We plot points but suppress the axes to draw them manually
plot(x = slope_comparisons$estimate, 
     y = nrow(slope_comparisons):1,  # Plot from top to bottom
     xlim = range(slope_comparisons$lower.CL, slope_comparisons$upper.CL), # Set x-axis limits from data
     ylim = c(0.5, nrow(slope_comparisons) + 0.5), # Add space on y-axis
     xlab = "Estimated Difference in Slopes",
     ylab = "", # Suppress y-axis title
     main = "",
     pch = 16,      # Use solid circles
     cex = 1.5,       # Make points larger
     cex.axis = 1.2,
     yaxt = "n", 
     col = "white")     # Suppress y-axis labels

# 5. Add horizontal error bars (the "whiskers")
arrows(x0 = slope_comparisons$lower.CL, 
       y0 = nrow(slope_comparisons):1, 
       x1 = slope_comparisons$upper.CL, 
       y1 = nrow(slope_comparisons):1,
       angle = 90,     # Makes the ends flat
       code = 3,       # Puts flat ends on both sides
       length = 0.05)  # Sets the size of the flat ends

# 6. Add the vertical reference line at zero
abline(v = 0, lty = 2, col = "gray40")  # Make labels horizontal

# 8. Add the "B" label in the top right corner
# Note: You may need to adjust x and y coordinates depending on your data range
# We use par("usr") to get the coordinates of the plot region
plot_coords <- par("usr")
text(x = plot_coords[2] * 0.95, # 95% to the right edge
     y = plot_coords[4] * 0.97, # 95% to the top edge
     "B", 
     font = 2, 
     cex = 1.2)
# text(x = -0.2, y = plot_coords[4] * 0.97, "*", font = 2, cex = 2)
# text(x = 0.6, y = plot_coords[4] * 0.97, "n.s.", font = 2, cex = 1.5)

# The averaged RGB values
high2_rgb = col2rgb(high2)
low2_rgb = col2rgb(low2)
mixed_color <- "black"

points(x = slope_comparisons$estimate, 
       y = nrow(slope_comparisons):1,
       pch = 21, 
       col = "black",
       lwd = 2,
       bg = c(high2, low2, mixed_color), # indexing weird, but this is the correct order 
       cex = 2)

axis(2, at = c(3,2,1), tick = T, labels = c("A - NF", 
                                            "H - NF", 
                                            "A - H"), 
     las = 0, cex.axis = 1.2)
mtext("TP Pairwise Differences", side = 2, line = 2.2, cex = 0.9)


## Total Nitrogen (Plot C) =======================================

# no fish 
plot(log_tn ~ doy, data = datA, type = "o", lty = 2, col = no1, ylab = "", xlab = "", 
     ylim = c(log(1), log(50)), yaxt = 'n', xlim = c(115, 168), lwd = 2)
mtext("log(Total Nitrogen ("~m*"g"~`L`^1*"))", side = 2, line = 3, cex = 0.9)
points(log_tn ~ doy, data = datD, type = "o", lty = 2, col = no2, lwd = 2)

# harvested 
points(log_tn ~ doy, data = datC, type = "o", lty = 2, col = low1, lwd = 2)
points(log_tn ~ doy, data = datE, type = "o", lty = 2, col = low2, lwd = 2)

# ambient
points(log_tn ~ doy, data = datB, type = "o", lty = 2, col = high1, lwd = 2)
points(log_tn ~ doy, data = datF, type = "o", lty = 2, col = high2, lwd = 2)

axis(side=2, 
     at=c(log(1), 
          log(2), log(3), log(4), log(5), 
          log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40), log(50)),
     labels = c("1", " ", " ", " ", " ", " ", " ", " ", " ", 
                "10", " ", " ", " ", "50"), 
     las=2, cex.axis = 1.2)
axis(side=2, 
     at=c(log(1), log(10), log(50)),
     labels = c("1", 
                "10","50"), 
     las=2, cex.axis = 1.2, tick = F)


# add in model fits # 
tn_fit <- newdata2 %>% 
  as_tibble() %>% 
  mutate(bt_tn = exp(fit))
tn_fit

tn_nofish <- tn_fit %>% 
  filter(treatment == "nofish")
tn_harv <- tn_fit %>% 
  filter(treatment == "harvested")
tn_amb <- tn_fit %>% 
  filter(treatment == "ambient")

polygon(
  x = c(tn_nofish$doy, rev(tn_nofish$doy)),
  y = c(tn_nofish$lower, rev(tn_nofish$upper)),
  col = adjustcolor(no1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(tn_harv$doy, rev(tn_harv$doy)),
  y = c(tn_harv$lower, rev(tn_harv$upper)),
  col = adjustcolor(low1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

polygon(
  x = c(tn_amb$doy, rev(tn_amb$doy)),
  y = c(tn_amb$lower, rev(tn_amb$upper)),
  col = adjustcolor(high1, alpha.f = 0.3),  # Adjust transparency
  border = NA  # No border for the polygon
)

points(fit ~ doy, data = tn_nofish, type = "l", col = no1, lwd = 4)
points(fit ~ doy, data = tn_harv, type = "l", col = low1, lwd = 4)
points(fit ~ doy, data = tn_amb, type = "l", col = high1, lwd = 4)

# polygon(c(115,123,123,115), c(log(5000), log(5000), log(10000), log(10000)), col="gray20") # Pre-Fish
lines(c(122.9, 122.9), c(log(0.1), log(50000)), lwd=2, lty=3, col="gray20")
# text(119, log(7500), "Pre Fish", col="white", font=2, cex = 0.9)
lines(c(129.1, 129.1), c(log(0.1), log(60000)), lwd=2, lty=3, col="gray20")
# polygon(c(129,150,150,129), c(log(5000), log(5000), log(10000), log(10000)), col="gray20") # Second Fish
# text(140, log(7500), "Post Fish Addition", col="white", font=2, cex = 0.9)

# squircle it up 
# 
# squircle_filled(x0 = 150, y0 = log(2300), radius = 0.6, stretch = 5, col = no1)
# text(150, log(28), "No Fish", col="white", font=2, cex = 0.9)
# squircle_filled(x0 = 157.5, y0 = log(2300), radius = 0.6, stretch = 5, col = low1)
# text(157.5, log(28), "Harvest", col="white", font=2, cex = 0.9)
# squircle_filled(x0 = 164.75, y0 = log(2300), radius = 0.6, stretch = 5, col = high1)
# text(164.5, log(28), "Ambient", col="white", font=2, cex = 0.9)
# 
# text(169, log(100), "A", font = 2, cex = 1.5)
# 
# squircle_filled(x0 = 152, y0 = log(2300), radius = 0.7, stretch = 5, col = no1)
# text(152, log(2300), "No Fish", col="white", font=2, cex = 1)
# squircle_filled(x0 = 159, y0 = log(2300), radius = 0.7, stretch = 5, col = low1)
# text(159, log(2300), "Harvest", col="white", font=2, cex = 1)
# squircle_filled(x0 = 166, y0 = log(2300), radius = 0.7, stretch = 5, col = high1)
# text(166, log(2300), "Ambient", col="white", font=2, cex = 1)

plot_coords <- par("usr")
text(x = plot_coords[2] * 0.99, # 95% to the right edge
     y = plot_coords[4] * 0.96, # 95% to the top edge
     "C", 
     font = 2, 
     cex = 1.2)
mtext("Day of Year, 2019", side = 1, line = 2.2, cex = 0.9)

## Pairwise Zooplankton Biomass (Plot D) ================================
zp_trends_compare <- emtrends(mod2, 
                              pairwise ~ treatment,
                              var = "log_doy_c")

# 2. Convert the *SUMMARY* of the contrasts to a data frame with CIs
slope_comparisons <- as.data.frame(summary(zp_trends_compare$contrasts, infer = TRUE))

# 4. Create the main plot area
# We plot points but suppress the axes to draw them manually
plot(x = slope_comparisons$estimate, 
     y = nrow(slope_comparisons):1,  # Plot from top to bottom
     xlim = range(slope_comparisons$lower.CL, slope_comparisons$upper.CL), # Set x-axis limits from data
     ylim = c(0.5, nrow(slope_comparisons) + 0.5), # Add space on y-axis
     xlab = "Estimated Difference in Slopes",
     ylab = "", # Suppress y-axis title
     main = "",
     pch = 16,      # Use solid circles
     cex = 1.5,       # Make points larger
     cex.axis = 1.2,
     yaxt = "n", 
     col = "white")     # Suppress y-axis labels

# 5. Add horizontal error bars (the "whiskers")
arrows(x0 = slope_comparisons$lower.CL, 
       y0 = nrow(slope_comparisons):1, 
       x1 = slope_comparisons$upper.CL, 
       y1 = nrow(slope_comparisons):1,
       angle = 90,     # Makes the ends flat
       code = 3,       # Puts flat ends on both sides
       length = 0.05)  # Sets the size of the flat ends

# 6. Add the vertical reference line at zero
abline(v = 0, lty = 2, col = "gray40")  # Make labels horizontal

# 8. Add the "B" label in the top right corner
# Note: You may need to adjust x and y coordinates depending on your data range
# We use par("usr") to get the coordinates of the plot region
plot_coords <- par("usr")
text(x = plot_coords[2] * 0.95, # 95% to the right edge
     y = plot_coords[4] * 0.97, # 95% to the top edge
     "D", 
     font = 2, 
     cex = 1.2)
# text(x = -0.2, y = plot_coords[4] * 0.97, "*", font = 2, cex = 2)
# text(x = 0.6, y = plot_coords[4] * 0.97, "n.s.", font = 2, cex = 1.5)

# The averaged RGB values
high2_rgb = col2rgb(high2)
low2_rgb = col2rgb(low2)
mixed_color <- "black"

points(x = slope_comparisons$estimate, 
       y = nrow(slope_comparisons):1,
       pch = 21, 
       col = "black",
       lwd = 2,
       bg = c(high2, low2, mixed_color), # indexing weird, but this is the correct order 
       cex = 2)

axis(2, at = c(3,2,1), tick = T, labels = c("A - NF", 
                                            "H - NF", 
                                            "A - H"), 
     las = 0, cex.axis = 1.2)
mtext("TN Pairwise Differences", side = 2, line = 2.2, cex = 0.9)
mtext("Estimated Difference in Slope", side = 1, line = 2.2, cex = 0.9)

# Nutrient GAMs # ===========================================
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
par(mfrow = c(1,2), 
    omi = c(0.4,0.5,0.1,0.1), 
    mai = c(0.2,0.3,0.2,0.5))

#Colors for GAM plots
high1 = rgb(102,205,170, max = 255, alpha = 200)
high2 = "aquamarine3" 
low1 = rgb(65, 105, 225, max = 255, alpha = 180)
low2 = "royalblue"
no1 = rgb(153,153,153, max = 255, alpha = 180)
no2 = "gray60" 

p_doy <- seq(min(nutrient_gam$doy), max(nutrient_gam$doy), length.out = 200)

# Build a dataframe for each group
nd_no   <- data.frame(doy = p_doy, fishtreat = "nofish", treatment = "nofish")
nd_harv <- data.frame(doy = p_doy, fishtreat = "harvested", treatment = "harvested")
nd_amb  <- data.frame(doy = p_doy, fishtreat = "ambient", treatment = "ambient")

# Predict
p_no   <- predict(tp_gam, newdata = nd_no, se.fit = TRUE)
p_harv <- predict(tp_gam, newdata = nd_harv, se.fit = TRUE)
p_amb  <- predict(tp_gam, newdata = nd_amb, se.fit = TRUE)

plot(p_doy, p_no$fit, type = "n", 
     ylim = c(min(p_no$fit - 2*p_no$se.fit), max(p_no$fit + 2*p_no$se.fit+5)),
     xlab = "Day of Year", ylab = "Total Phosphorus (ug/L)", 
     cex.axis = 1.2, bty = "l")
mtext(side = 2, line = 2.2, "Total Phosphorus (ug/L)")
mtext(side = 1, line = 2, "Day of Year, 2019")

# --- NO FISH (Reference) ---
polygon(c(p_doy, rev(p_doy)), c(p_no$fit + (1.96 * p_no$se.fit), rev(p_no$fit - (1.96 * p_no$se.fit))),
        col = no1, border = NA)
lines(p_doy, p_no$fit, col = no2, lwd = 2)

# --- HARVESTED ---
polygon(c(p_doy, rev(p_doy)), c(p_harv$fit + (1.96 * p_harv$se.fit), rev(p_harv$fit - (1.96 * p_harv$se.fit))),
        col = low1, border = NA)
lines(p_doy, p_harv$fit, col = low2, lwd = 2)

# --- AMBIENT ---
polygon(c(p_doy, rev(p_doy)), c(p_amb$fit + (1.96 * p_amb$se.fit), rev(p_amb$fit - (1.96 * p_amb$se.fit))),
        col = high1, border = NA)
lines(p_doy, p_amb$fit, col = high2, lwd = 2)

legend("topright", legend = c("No Fish", "Harvested", "Ambient"), 
       col = c(no2, low2, high2), lwd = 2, bty = "n")

# Predict using tn_gam
p_no_n   <- predict(tn_gam, newdata = nd_no, se.fit = TRUE)
p_harv_n <- predict(tn_gam, newdata = nd_harv, se.fit = TRUE)
p_amb_n  <- predict(tn_gam, newdata = nd_amb, se.fit = TRUE)

# Plotting TN
plot(p_doy, p_no_n$fit, type = "n", 
     ylim = c(min(p_no_n$fit - 2*p_no_n$se.fit), max(p_no_n$fit + 2*p_no_n$se.fit + 5)),
     xlab = "Day of Year", ylab = "Total Nitrogen", 
     cex.axis = 1.2, bty = "l")
mtext(side = 2, line = 2.2, "Total Nitrogen (mg/L)")
mtext(side = 1, line = 2, "Day of Year, 2019")

# --- NO FISH (Reference) ---
polygon(c(p_doy, rev(p_doy)), c(p_no_n$fit + (1.96 * p_no_n$se.fit), rev(p_no_n$fit - (1.96 * p_no_n$se.fit))),
        col = no1, border = NA)
lines(p_doy, p_no_n$fit, col = no2, lwd = 2)

# --- HARVESTED ---
polygon(c(p_doy, rev(p_doy)), c(p_harv_n$fit + (1.96 * p_harv_n$se.fit), rev(p_harv_n$fit - (1.96 * p_harv_n$se.fit))),
        col = low1, border = NA)
lines(p_doy, p_harv_n$fit, col = low2, lwd = 2)

# --- AMBIENT ---
polygon(c(p_doy, rev(p_doy)), c(p_amb_n$fit + (1.96 * p_amb_n$se.fit), rev(p_amb_n$fit - (1.96 * p_amb_n$se.fit))),
        col = high1, border = NA)
lines(p_doy, p_amb_n$fit, col = high2, lwd = 2)

