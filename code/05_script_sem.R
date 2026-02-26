#' DESCRIPTION:
#' Script for SEM

# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               GGally,
               vegan,
               lavaan,
               lavaanPlot)

#lavaan only applicable to normally distributed data!!!!

#Path analysis - type of SEM, for when you have directly measured all variables
#of interest, and you want to know the direct and indirect effects between those
#variables


url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_foodweb.csv"

df_fw <- read_csv(url)

#visualization
df_fw %>% 
  select(-plot_id) %>%        # remove plot_id column
  ggpairs(                    
    progress = FALSE          # suppress progress messages
  ) +
  theme_bw()
#can tell you what relationships should be in analysis

# Specify the SEM model using lavaan syntax
m1 <- '
mass_herbiv ~ mass_plant + cv_h_plant
  mass_pred ~ mass_herbiv
'

#sem function from lavaan
fit1 <- sem(model = m1,
             data = df_fw)

#interpreting sem output:

#p-value tells you if your model prediction matches raw data, so you want a 
#non significant p-value.
#degrees of freedom represents the number of arrows omitted/excluded from full
#analysis. We excluded two because we had 2 non significant correlations from
#visualization ggpairs step

summary(fit1, standardize = TRUE) #setting standardize to true will give the
#Std.all column which gives standardized coefficients -> allow direct comparison 
#of the relative strength of different paths.

#interpreting summary of sem:
#We can obtain estimates for each path in the model, which quantify the strength
#of the relationships among variables

lavaanPlot(model = fit1, coefs = TRUE, stand = TRUE)

#testing if an arrow is necessary or nah (mass_pred~cv_h_plant in this case):
m2 <- '
mass_herbiv ~ mass_plant + cv_h_plant
  mass_pred ~ mass_herbiv + cv_h_plant
'
(fit2 <- sem(m2,
             data = df_fw))

#compare models
anova(fit1, fit2)

#models not significantly different, use model with lower AIC/less arrows

#SEM incorporates latent variables, path analysis cannot
#Latent variables -> unobserved variables, typically composites of multiple
#observed variables and represent underlying constructs that cannot be measured 
#directly

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_herbivory.csv"

(df_herbv <- read_csv(url))

#visualization
df_herbv %>% 
  ggpairs(
    progress = FALSE,
    columns = c("soil_n",
                "sla",
                "cn_ratio",
                "per_lignin")
  ) +
  theme_bw()

m_sem <- '
# latent variable
  palatability =~ sla + cn_ratio + per_lignin
  
# regression
  palatability ~ soil_n
  herbivory ~ palatability
'

fit_sem <- sem(m_sem,
                data = df_herbv)

summary(fit_sem, standardize=TRUE)
#will give you latent variables section that lets you know what measured variables
#are associated w/ and contributing to the latent variable

# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: Path Analysis and Covariance Visualization
# ============================================================

library(piecewiseSEM)
data("keeley")

# The "keeley" dataset contains fire-related vegetation data
# collected from shrublands in California.
#
# ------------------------------------------------------------
# Column descriptions:
# elev  : Elevation of the site
# slope : Slope steepness
# aspect: Slope aspect (orientation)
# heat  : Heat load index (a function of slope and aspect)
# firesev: Fire severity
# age   : Time since last fire
# cover : Vegetation cover
# rich  : Plant species richness
# ------------------------------------------------------------
#
# In this exercise, you will explore relationships among variables
# using covariance and path analysis. You will replicate a published
# path model and propose an alternative.

# 1. For the variables depicted in Figure 22.1, draw a figure
#    showing the covariance between variables.


keeley %>% 
  select(-elev)%>%
  ggpairs(                    
    progress = F
  ) +
  theme_bw()

# 2. Following Figure 22.1, develop a path model using the
#    same variables and relationships. Examine if this model
#    captures the data structure using a Chi-Square test.

m3 <- '
abiotic~distance
hetero~distance
age~distance
firesev~age
cover~firesev
rich~abiotic+hetero+cover
'

fit3 <- sem(model = m3,
            data = keeley)

summary(fit3, standardize=T)

# 3. Develop an alternative path model that you consider more
#    appropriate based on theory or observed data patterns.

m4 <- '
firesev~distance+abiotic+age
cover~firesev+age
rich~firesev+cover
'

fit4 <- sem(model = m4,
            data = keeley)

summary(fit4, standardize=T)

# 4. Compare the performance of the published model (Figure 22.1)
#    and your alternative model.
#    - Consider fit indices, path coefficients, and interpretability.

anova(fit3, fit4)
