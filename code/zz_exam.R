# NOTE:
# When instructed to "test XXX", you must report the outcome as comments
# that clearly summarize the relevant statistical results (e.g., effect size,
# direction, significance, and interpretation).
# Providing code alone without documenting and interpreting the results
# in comments will result in point deductions.

# dataset 1 ---------------------------------------------------------------

link1 <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_insect_emergence.rds"
df_emg <- readRDS(url(link1, "rb"))

# This dataset ('df_emg') contains daily measurements of aquatic insect emergence
# from two wetland sites over a full calendar year (Jan 1–Dec 31).

# Data structure:
# t           : Day of the year (integer), where 1 = January 1 and 365 = December 31
# site        : Site identifier (factor), with "s1" and "s2" representing the two wetlands
# emergence   : Emergence flux of aquatic insects (g/day)

# Q1. Visualize seasonal patterns in emergence flux at both sites
#     (e.g., plot emergence vs. day of year, with separate lines or colors for each site).
#     [1 point]

library(tidyverse)

df_emg %>%
  ggplot(aes(x=t,
             y=emergence,
             color=site))+
  geom_point()

# Q2. Test whether emergence flux differs significantly between the two sites,
#     while appropriately accounting for seasonal variation
#     [4 points]

#generalized additive model

library(mgcv)

emg_gam <- gam(emergence ~ site + s(t),
             data = df_emg,
             family = "gaussian")

summary(emg_gam)

#Significant effect of site on emergence (pvalue=6.4e-10),
#site 2 has higher emergence in spring and fall, but lower in summer compared
#to site 1. R^2 of 0.877 
#Day of year also significant (pvalue = <2e-16), 
#as time of year influences emergence at both sites

# dataset 2 ---------------------------------------------------------------

link2 <- "https://raw.githubusercontent.com/aterui/cw_bio709/master/data_fmt/data_lake_invert.rds"
df_inv <- readRDS(url(link2, "rb"))

# This dataset 'df_inv' contains 100 observations from 10 lakes.
# Within each lake, 10 plots were established, spaced ~500 m apart.
# At each plot, the following variables were measured:

# s          : Species richness of invertebrates associated with aquatic plants at each plot
# hb         : Standing biomass of invertebrates associated with aquatic plants at each plot
# prod       : Production rate of aquatic plants (macrophytes), measured as g/month
# substrate  : Median diameter of substrate materials (mm)
# cond       : Water electrical conductivity (µS/cm);
#              a proxy for ionized nutrient levels (higher values may indicate eutrophication)
# lake       : lake ID

# Researcher's hypothesis was that: 
# (a) conductivity influences the productivity of macrophyes.
# (b) macrophyte's production rate ('prod') dictates invertebrate biomass ('hb') through bottom-up effects
# (c) macrophyte's production rate ('prod') dictates invertebrate richness ('s') through bottom-up effects 

# Q1. Create a scatter plot of macrophyte production ('prod', y-axis)
#     versus water conductivity ('cond', x-axis), with points colored by lake identity.
#     [1 point]

df_inv%>%
  ggplot(aes(x=cond,
             y=prod,
             color=lake))+
  geom_point()


# Q2. Create a scatter plot of raw invertebrate biomass ('hb', y-axis)
#     versus macrophyte production ('prod', x-axis), with points colored by lake identity.
#     [1 point]

df_inv%>%
  ggplot(aes(x=prod,
             y=hb,
             color=lake))+
  geom_point()+
  geom_smooth()

# Q3. Create a scatter plot of "log-transformed" invertebrate biomass ('hb', y-axis)
#     versus macrophyte production ('prod', x-axis), with points colored by lake identity.
#     [1 point]

df_inv <- df_inv%>%
  mutate(loghb = log10(hb))

df_inv%>%
  ggplot(aes(x=prod,
             y=loghb,
             color=lake))+
  geom_point()+
  geom_smooth()

# Q4. Test hypothesis (a) by modeling macrophyte production while
#     statistically controlling for potential confounding variables ('substrate', 'lake').
#     [3 points]

library(lavaan)
library(glmmTMB)

m1 <- glmmTMB(prod ~ cond + (1|lake) + substrate,
  data = df_inv)

summary(m1)

#prod highly related to cond (p-value of <2e-16), which supports the hypothesis,
#substrate not significantly related to prod, p-value=0.158, but the 
#original hypothesis doesn't mention that so it overall is supported

# Q5. Test hypotheses (a–c) simultaneously using a unified modeling framework.
#     Based on the resulting statistical tests, determine whether the overarching
#     hypothesis (a–c, combined) is supported or rejected.
#     - Use appropriate probability distributions.
#     - Use variable transformation if appropriate given the data.
#     [4 points]

library(piecewiseSEM)
library(GGally)
library(multcompView)

# (a) conductivity influences the productivity of macrophyes.
# (b) ('hb') ~ ('prod') through bottom-up effects
# (c) ('s') ~ ('prod')through bottom-up effects 

m3<-glmmTMB(hb~prod+(1|lake),data=df_inv)
m4<-glmmTMB(s~prod+(1|lake),data=df_inv,family=poisson()) #add poisson family -
#variable is discrete it is a whole number integer with no decimals,
#it does not have a clear upper limit,
#we don't know of a max species richness/clear limit of species richness

fit2 <- psem(m1,m3,m4)

summary(fit2, .progressbar=F)

#fisher C p-value=0.153

#I think the overall hypothesis is supported as productivity is highly related
#to cond (p-value=0), and productivity is highly related to both 
#hb (p-value 0) and s (p-value=0).

# dataset 3 ---------------------------------------------------------------

link3 <- "https://raw.githubusercontent.com/aterui/cw_bio709/master/data_fmt/nutrient.rds"
nutrient <- readRDS(url(link3, "rb"))

print(trees)

# This dataset ('trees') contains measurements of 31 felled black cherry trees.
# The three variables represent tree diameter, height, and timber volume.
# Note: the variable 'Girth' is actually the diameter measured at 4 ft 6 in above ground.

# Data structure:
# Girth   : Numeric, tree diameter in inches (mislabelled as girth)
# Height  : Numeric, tree height in feet
# Volume  : Numeric, timber volume in cubic feet

# Q1. Visualize relationships among tree diameter ('Girth'), height ('Height'),
#     and timber volume ('Volume') (e.g., using scatterplot matrix or pairwise scatter plots).
#     [1 point]

library(GGally)

df_tree<-as_tibble(trees)

df_tree %>%
  ggpairs(
    progress = FALSE,
    columns = c("Girth",
                "Height",
                "Volume"))+
  theme_bw()

# Q2. Perform an appropriate ordination or dimension reduction method to 
#     summarize these three variables into fewer composite axes.
#     Then, identify and retain axes that explain meaningful variation in the original variables
#     [3 points]

pca <- prcomp(
  x = df_tree,
  center = TRUE,
  scale = TRUE)

summary(pca)

#PC1 explains 80$ of variance, PC2 explains 18.7% of variance, 
#combined they explain 99.06% of variance
#PC3 explains 9.36% of variation

# Q3. If justified, test whether the retained axis (or axes) is significantly 
#     related to "nutrient"; 
#     skip regression if the ordination does not support meaningful interpretation.
#     [1 point]

df_treepca <- bind_cols(
  df_tree,            
  as_tibble(pca$x)
)

treeregression1 <- lm(PC1 ~ nutrient,
        data = df_treepca)

summary(treeregression1)

#PC1 significantly related to nutrient (p-value=7.7e-06)

treeregression2 <- lm(PC2 ~ nutrient,
                      data = df_treepca)

summary(treeregression2)

#PC2 significantly related to nutrient (p-value=3.56e-05)

treeregression3 <- lm(PC3 ~ nutrient,
                      data = df_treepca)

summary(treeregression3)

#PC3 not significantly related to nutrient (p-value=0.85)

# dataset 4 ---------------------------------------------------------------

df_nile <- dplyr::tibble(
  year = time(Nile), # observation year
  discharge = as.numeric(Nile) # discharge
)

df_sunspot <- dplyr::tibble(
  year = time(sunspot.year), # observation year
  sunspots = as.numeric(sunspot.year) # the number of sunspots
)

# These datasets contain:
# - df_nile    : Annual discharge of the Nile River (Nile dataset)
# - df_sunspot : Annual sunspot counts (sunspot.year dataset)

# Q1. Create a combined data frame aligning the observation years
#     (i.e., only include years present in both datasets)
#     [1 point]

df_nile$year<-as.numeric(df_nile$year)

df_sun<-df_sunspot%>%
  filter(year >=1871)%>%
  filter(year <=1970)

df_comb<-left_join(df_sun, df_nile,by='year')


# Q2. Test whether the number of sunspots is significantly related to Nile's discharge
#     [4 points]


library(forecast)

arima <- auto.arima(
  df_comb$discharge,
  xreg = df_comb$sunspots,
  stepwise = FALSE)

confint(arima, level = 0.95)

#95% confidence interval for xreg overlaps with 0, as it is from -.576 to 1.076,
#so sunspots does not significantly impact discharge