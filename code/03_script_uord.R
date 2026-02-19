#' DESCRIPTION:
#' Script for Unconstrained Ordination

# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               GGally,
               vegan)
df_iris<-iris%>%
  as_tibble()%>%
  janitor::clean_names() #replaced periods w/ _ and made all lowercase

df_iris%>%
  ggpairs(progress=FALSE, #so no progress bar appears
          columns=c("sepal_length",
                     "sepal_width",
                     "petal_length",
                     "petal_width"),
          aes(color=species,
              alpha=0.5)
          )+
  theme_bw()

#PCA works best when you have normally distributed variables w/high correlation

#want to PCA petal length and width:
df_petal<-df_iris%>%
  select(starts_with("petal_"))

#PCA - prcomp() function. Will include ALL columns, only use data frame with
#the columns you want in the PCA
obj_pca<-prcomp(x=df_petal,
       center=TRUE, #subtracts mean of each column to center @0
       scale=TRUE)
# scale divide by standard deviation of each column to get unit variance, 
#set to FALSE by default

summary(obj_pca)
#prop of variation tells you how much info was captured by PC1 and PC2

#PC1 is always the most important axis that captures the most, PC2 explains the 2nd most,
#PC3 the third most and so on

#attach PC axes values to iris dataframe
#these PC1 values represent both the petal width and length, combined 2 variables
#into one
df_pca<-df_iris%>%
  bind_cols(obj_pca$x)

#visualize PC1 values between species
df_pca%>%
  ggplot(aes(x=species,
             y=PC1))+
  geom_boxplot()

#=======NMDS===========

#NMDS includes less info than PCA, but useful for non-normal data
data(dune)

dune%>%
  as_tibble()%>%
  select(1:3) %>% #selecting first 3 species cause there are a lot 
  ggpairs()+
  theme_bw()

#to use nonnormal data, NMDS calculates the distance between points, so doesn't
#really look at how the points relate to the axes

#column is species, row is site
m_bray<-vegdist(dune,method="bray")

obj_nmds<-metaMDS(comm=m_bray, #or could just do (comm=dune, distance="bray", k=2)
        k=2)
#k=x where x is the number of axes you want

obj_nmds
#stress=0.12

#visualize

data(dune.env)

df_nmds<-dune.env%>%
  as_tibble()%>%
  bind_cols(obj_nmds$points)%>%
  janitor::clean_names()

df_nmds%>%
  ggplot(aes(x=mds1, #NMDS uses mds1, mds2, etc... instead of PC1, PC2, etc...
             y=mds2,
             color=use))+
  geom_point(size=3)+
  stat_ellipse(level=.95,
               linetype=2)

#NMDS not for stats, it is for visualization. PERMANOVA is the stat test
adonis2(m_bray~use, #response variable goes first
        data=df_nmds)

# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: PCA using the iris dataset
# ============================================================

# In this exercise, you will perform a Principal Component
# Analysis (PCA) using all morphological measurements in the
# iris dataset and visualize multivariate trait patterns
# among species.

# 1. Using all four morphological variables
#    (Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
#    perform a PCA.

df_iris2<-df_iris%>%
  select(-species)
#remove the species column but keep other columns

obj_pca2<-prcomp(x=df_iris2,
                  center=TRUE,
                  scale=TRUE)
  
# 2. Visualize flower morphology in PC axes whose cumulative
#    contribution exceeds 90%; color points by species.

obj_pca2 #tells you what traits contribute most to each PC
summary(obj_pca2)
#PC1 + PC2 cumulative proportion is above 90%

df_pca2<-df_iris%>%
  bind_cols(obj_pca2$x)

#visualize PC1 and PC2 values between species
df_pca2%>%
  ggplot(aes(x=PC1,
             y=PC2,
             color=species))+
  geom_point()

# 3. Which morphological traits contribute most strongly to
#    the first and second principal components? How?

#Petal length and width contribute most to PC1, then sepal length
#sepal width contributes most to PC2

# ============================================================
# EXERCISE: NMDS using the BCI dataset
# ============================================================

# In this exercise, you will perform a Non-metric Multidimensional
# Scaling (NMDS) using the BCI tree community dataset and explore
# patterns in species composition among sites.

data("BCI", "BCI.env")

# 1. Using the BCI dataset, calculate a dissimilarity matrix
#    (e.g., Bray-Curtis) and perform NMDS.

b_bray<-vegdist(BCI,method="bray")

obj_nmds2<-metaMDS(comm=b_bray,
                  k=2)

# 2. Visualize sites in NMDS space.
#    - How are sites positioned relative to each other?
#    - Color or shape points by environmental groups or site
#      characteristics of your choice.

df_nmds2<-BCI.env%>%
  as_tibble()%>%
  bind_cols(obj_nmds2$points)%>%
  janitor::clean_names()

df_nmds2%>%
  ggplot(aes(x=mds1,
             y=mds2,
             color=habitat))+
  geom_point(size=3)+
  stat_ellipse(level=.95,
               linetype=2)

# 3. Perform PERMANOVA to examine if communities are grouped
#    by the environmental variable you selected.

adonis2(b_bray~habitat,
        data=df_nmds2)
