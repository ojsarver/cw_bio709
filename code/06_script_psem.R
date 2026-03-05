#' DESCRIPTION:
#' Script for piecewise SEM

# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               GGally,
               piecewiseSEM,
               glmmTMB)

data("keeley")

(df_keeley <- keeley %>% 
    as_tibble())

# Define individual models
m1 <- lm(abiotic ~ distance, data = df_keeley)
m2 <- lm(hetero ~ distance, data = df_keeley)
m3 <- lm(firesev ~ age, data = df_keeley)
m4 <- lm(cover ~ firesev, data = df_keeley)
m5 <- lm(rich ~ cover + abiotic + hetero, data = df_keeley)

# Combine into piecewise SEM
sem_model <- psem(m1, m2, m3, m4, m5)

# Evaluate
summary(sem_model, .progressBar = FALSE)

#Same process below, but changing m4+m5 (fit to negative binomial GLM)

# m4 added a direct effect of hetero on cover
m4 <- lm(cover ~ firesev + hetero, data = df_keeley)

# m5 now models richness as negative binomial (MASS::glm.nb) and includes direct
#effect of distance on richness
m5 <- MASS::glm.nb(rich ~ cover + abiotic + hetero + distance, 
                   data = df_keeley)  

# Combine into piecewise SEM
sem_model <- psem(m1, m2, m3, m4, m5)

# Evaluate model
summary(sem_model, .progressBar = FALSE)

#Fisher C evaluates model, don't want a large value here

#plot
plot(sem_model)


#Adding random effects
data("shipley")

df_shipley <- shipley %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  drop_na(growth)

df_shipley %>% 
  group_by(site) %>% 
  summarize(n_tree = n_distinct(tree))

#plot
# Pairwise plots of key variables
df_shipley %>% 
  ggpairs(
    columns = c("dd", 
                "date",
                "growth",
                "live"),  # select multiple variables to plot!!
    progress = FALSE
  ) +
  theme_bw()

# m1: y~x, random intercepts for site and tree
m1 <- glmmTMB(date ~ dd + (1 | site) + (1 | tree), 
              data = df_shipley,
              family = "gaussian")

m2 <- glmmTMB(growth ~ date + (1 | site) + (1 | tree), 
              data = df_shipley,
              family = "gaussian")

#m3: variable live is binary, switched the family to binomial -> logistic mixed model
m3 <- glmmTMB(live ~ growth + (1 | site) + (1 | tree), 
              data = df_shipley, 
              family = "binomial")

# Combine models into a piecewise SEM
sem_glmm <- psem(m1, m2, m3)

summary(sem_glmm, .progressBar = FALSE)

#marginal r2 variation explained by fixed effects
#conditional r2 variation explained by fixed and random effects

# lab ---------------------------------------------------------------------

library(piecewiseSEM)
data("meadows")

# =========================================
# EXERCISE: Piecewise SEM with Meadows Data
# =========================================
#
# ------------------------------------------------------------
# Dataset: meadows (from piecewiseSEM package)
# Variables:
#   grazed - 0 = ungrazed, 1 = grazed
#   mass   - plant biomass (g/m²)
#   elev   - plot elevation above sea level
#   rich   - plant species richness per m²
# ------------------------------------------------------------
#
# 1. Explore the dataset (structure, summary, plots).

meadows %>% 
  ggpairs(
    columns = c("grazed", 
                "mass",
                "elev",
                "rich"),  # select multiple variables to plot!!
    progress = FALSE
  ) +
  theme_bw()

# 2. Develop a conceptual model: decide which variables influence others.
#    - Consider direct and indirect effects.
#    - Think about grazing as a disturbance factor.

#grazed + elev impact mass and rich?

# 3. Fit component models (e.g., lm) for each hypothesized relationship.

m1 <- lm(rich ~ elev+grazed, data = meadows)
m2 <- glm(mass ~ grazed+rich+elev, data = meadows)
m3 <- glm(grazed ~ elev, data = meadows, family="binomial")

# 4. Combine models into a piecewise SEM using psem().

sem_dow <- psem(m1, m2,m3)

# 5. Evaluate the SEM: path coefficients, significance, variance explained.

summary(sem_dow, .progressBar = FALSE)

plot(sem_dow)

# 6. Optional: try alternative models if your model deviates from the expectation.
#
# Deliverables:
# - Code for component models and combined SEM
# - Conceptual SEM diagram
# - Short reasoning about your SEM results
