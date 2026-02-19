#' DESCRIPTION:
#' Script for Constrained Ordination

# in-class ----------------------------------------------------------------
pacman::p_load(tidyverse,
               vegan,
               GGally)

data("varespec","varechem")

#rename
m_y<-varespec
colnames(m_y)<-str_to_lower(colnames(m_y))

df_env<-as_tibble(varechem)%>%
  janitor::clean_names()

#visualize
m_y%>%
  ggpairs(
    progress=FALSE,
    column=1:3,
    aes(
      alpha=.25
      )
    )+
    theme_bw()

#redundancy analysis (RDA) variables need to be linear
obj_rda <- rda(m_y ~ n + p + ca,
                data = df_env)
#inertia describes variance in the data
#constrained tells u how much inertia/variance is explained by the predictor variables
#you selected. RDA 1, RDA 2, etc... describe how much variance is explained by that
#axis

##permutation on RDA
anova.cca(obj_rda, 
          by = "margin", 
          permutations = 999)

#specify by="margin" or it defaults to type 1 anova. Found n is significant not
#p or ca

#================
#types of anova

#Type 1: y ~ n + t, tests n then tests t. Changing the order of n and t changes
#what results you get

#type 2: y ~ n + t. Order of n and t will not change the results of the test

#type 3: idk but it exists

#================

#visualization

#community data: how site explains data
df_rda<-scores(obj_rda,
       display="site",
       scaling=2) %>% #gives value for axes RDA 1 and RDA 2
  bind_cols(df_env) %>%
  janitor::clean_names()

#how env factors explain data
df_bp <- scores(obj_rda, 
                display = "bp", 
                scaling = 2) %>% 
  as_tibble(rownames = "variable") %>% 
  janitor::clean_names()

#points represent sites positioned by constrained community composition
#colored by N

df_rda %>% 
  ggplot(aes(x = rda1,
             y = rda2)) +        # color sites by nitrogen level
  geom_point(aes(color = n)) +
  geom_segment(data = df_bp,
               aes(x = 0, xend = rda1 * 10, # 10 is arbitrary scaling for visualization
                   y = 0, yend = rda2 * 10),
               arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_text(data = df_bp,
            aes(x = rda1 * 10.5,    # slightly beyond arrow tip
                y = rda2 * 10.5,
                label = variable),  # or use a variable column
            size = 4) +
  theme_bw() +
  labs(x = "RDA1",
       y = "RDA2",
       color = "Nitrogen") +
  scale_color_viridis_c()

#graph looks like horseshoe, signals that RDA isn't the right method

#dbRDA variables can be nonlinear
obj_db <- dbrda(m_y ~ n + p + ca,
                 data = df_env,
                 distance = "bray")

anova.cca(obj_db,
          by = "margin",
          permutations = 999)

#N is no longer significant

df_db <- scores(obj_db, 
                display = "sites",
                scaling = 2) %>% 
  as_tibble() %>%              
  bind_cols(df_env) %>%        
  janitor::clean_names()

df_bp <- scores(obj_db, 
                display = "bp", 
                scaling = 2) %>% 
  as_tibble(rownames = "variable") %>% 
  janitor::clean_names()

#visualize
df_db %>% 
  ggplot(aes(x = db_rda1,
             y = db_rda2)) +        # color sites by nitrogen level
  geom_point(aes(color = n)) +
  geom_segment(data = df_bp,
               aes(x = 0, xend = db_rda1,
                   y = 0, yend = db_rda2),
               arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_text(data = df_bp,
            aes(x = db_rda1 * 1.1,    # slightly beyond arrow tip
                y = db_rda2 * 1.1,
                label = variable),  # or use a variable column
            size = 4) +
  theme_bw() +
  labs(x = "dbRDA1",
       y = "dbRDA2",
       color = "Nitrogen") +
  scale_color_viridis_c()

# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: Community Ordination and Environmental Gradients
# ============================================================

library(vegan)
data("mite", "mite.env")

# The mite datasets contain information on Oribatid mite communities
# sampled from a small peatland area (2.5 m × 10 m).
#
# There are linked datasets:
# ------------------------------------------------------------
# mite     : Species abundance data (35 mite species × 70 sites)
# mite.env : Environmental variables measured at the same sites
# ------------------------------------------------------------
#
# Environmental variable descriptions (mite.env):
# ------------------------------------------------------------
# SubsDens : Substrate density (g/L)
# WatrCont : Water content of the substrate (g/L)
# Substrate: Substrate type (factor with multiple levels)
# Shrub    : Shrub density (ordered factor: low → high)
# Topo     : Microtopography (Blanket vs Hummock)
# ------------------------------------------------------------

# 1. Explore and visualize interrelationships among species abundances.
#    - Examine patterns of co-occurrence.
#    - Assess whether relationships among species appear linear or nonlinear.


ggpairs(wisconsin(mite),
        columns=1:5,
        progress = FALSE)


mi_y<-mite
colnames(mi_y)<-str_to_lower(colnames(mi_y))

mi_y%>%
  ggpairs(
    progress=FALSE,
    column=1:3,
    aes(
      alpha=.25
    )
  )+
  theme_bw()

#nonlinear?

df_menv<-as_tibble(mite.env)%>%
  janitor::clean_names() %>%
  mutate(num_shrub=as.numeric(shrub))

# 2. Fit a redundancy analysis (RDA) model using environmental variables of your choice.
#    - Visualize the ordination results.
#    - Examine gradients and species–environment relationships.
#    - Evaluate whether the assumptions of RDA are appropriate for these data.

obj_rdam <- rda(mi_y ~ subs_dens + watr_cont + num_shrub,
               data = df_menv)

df_rdam<-scores(obj_rdam,
               display="site",
               scaling=2) %>%
  bind_cols(df_menv) %>%
  as_tibble()%>%
  janitor::clean_names()

df_bpm <- scores(obj_rdam, 
                display = "bp", 
                scaling = 2) %>% 
  as_tibble(rownames = "variable") %>% 
  janitor::clean_names()

df_rdam %>% 
  ggplot(aes(x = rda1,
             y = rda2)) +
  geom_point(aes(color = num_shrub)) +
  geom_segment(data = df_bpm,
               aes(x = 0, xend = rda1 * 5, 
                   y = 0, yend = rda2 * 5),
               arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_text(data = df_bpm,
            aes(x = rda1 * 10.5,
                y = rda2 * 10.5,
                label = variable),
            size = 4) +
  theme_bw() +
  labs(x = "RDA1",
       y = "RDA2") +
  scale_color_viridis_c()
#RDA not good, there is a horseshoe

# 3. Apply alternative ordination methods.
#    - Canonical correspondence analysis (CCA; see ?cca()).
#    - Distance-based RDA (dbRDA).

obj_dbm <- dbrda(mi_y ~ subs_dens + watr_cont + num_shrub,
                data = df_menv,
                distance = "bray")

obj_cca<-cca(mi_y ~ subs_dens + watr_cont+ num_shrub,
    data=df_menv)


# 4. Compare RDA, CCA, and dbRDA.
#    - Perform permutation analysis to examine the significance of predictor variables
#    - Discuss which method is most appropriate for these data and why.

anova.cca(obj_rdam, 
          by = "margin", 
          permutations = 999)

anova.cca(obj_cca, 
          by = "margin", 
          permutations = 999)

anova.cca(obj_dbm, 
          by = "margin", 
          permutations = 999)

#CCA, subs_dens and watr_cont significant but not num_shrub