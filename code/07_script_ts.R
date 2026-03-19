#' DESCRIPTION:
#' Script for time-series

# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               forecast,
               lterdatasampler,
               daymetr,
               glarma)

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_ts_anormaly.csv"
(df_ts <- read_csv(url))

#visualize

g_base<-df_ts%>%
  ggplot(aes(x=year,
             y=anormaly))+
  geom_line()+
  geom_point()+
  labs(x="Year",
       y="Anormaly")

#try to fit regression to data

lm_ts<-lm(anormaly~year,
   data=df_ts)

summary(lm_ts)

#estimate describes how much y (anormaly) is changing as x (year) increases

#adding regression line to figure from before

g_base+
  geom_abline(intercept=coef(lm_ts)[1],
              slope=coef(lm_ts)[2])+
  theme_bw()

#regression assumes observations are independent of one another
#time series considers the measurement beforehand, good for when observations
#are not independent, like measuring plant growth week 1, 2, 3, etc...

#generate time series data?
y<-NULL
y[1]<-0
for(i in 1:99){
y[i+1]<-y[i]+rnorm(1,mean=0,sd=1)
}

#plot data

tibble(y=y,
       x=1:length(y))%>%
  ggplot(aes(x=x,
             y=y))+
  geom_point()+
  geom_line()

# AR model - autoregressive - accounts for prior observations
# MA model - moving average - accounts for prior random noise

#repeated measures ANOVA considers nonindependence between points but allows you
#to compare groups - so you can compare multiple plants in the 
#control group for plant growth to the treatments average plant growth
#while time series considers how variables impact a single time
#series/est of observations thru time

df_huron <- tibble(
  year = time(LakeHuron),             
  water_level = as.numeric(LakeHuron)
) %>% 
  arrange(year) #Ensures the data is ordered by year

# Plot Lake Huron time series with a linear trend
df_huron %>% 
  ggplot(aes(x = year, y = water_level)) +
  geom_point(alpha = 0.25) +       
  geom_line(linetype = "dotted") + 
  geom_smooth(method = "lm",  # Linear trend line
              color = "black",
              linewidth = 0.5) +
  theme_bw() +
  labs(x = "Year", y = "Water Level")

########autoregressive model

m_ar1<-Arima(
  df_huron$water_level,
  order =c(1,0,0) #the 1 is telling the model to account for 1 step before
)

#can change the 1 to 2 to tell it to account for 2 steps before and so on

df_huron_ar1<-df_huron%>%
  mutate(fit=fitted(m_ar1)%>%
  as.numeric())

df_huron_ar1%>%
  ggplot() +
  geom_point(aes(x = year, 
                 y = water_level),
             alpha = 0.25) +
  geom_line(aes(x = year, 
                y = fit),
            color = "steelblue") +
  theme_bw() 

##############MA model

m_ma1 <- Arima(
  df_huron$water_level,
  order = c(0, 0, 1)) #MA model puts 1 @ end, means accounting for most recent noise

  #can change 1 to 2 to account for the last two instances of noise
#can combo autoregressive and moving average by making the first number and 
#last number not zero, ex: c(1,0,1)

###########ARIMA model

m_arma1 <- Arima(
  df_huron$water_level,
  order = c(0, 1, 0)) #ARIMA changes middle #, can account for the mean changing
#over time

#model selection
auto.arima(
  y=df_huron$water_level,
  stepwise=F,
  ic="aic")

#runs through to see what combo of numbers is best for the c(x,y,z) part

########### ARIMAX - ARIMA model that includes external predictors 

data("ntl_icecover")

df_ice <- ntl_icecover %>% 
  as_tibble() %>%
  filter(between(year, 1980, 2014),
         lakeid == "Lake Mendota") %>%
  arrange(year)

# Download daily climate data from Daymet for Lake Mendota
list_mendota <- download_daymet(
  site = "Lake_Mendota",   # Arbitrary name you assign to this site
  lat = 43.1,              # Latitude of the lake
  lon = -89.4,             # Longitude of the lake
  start = 1980,            # Start year
  end = 2024,              # End year
  internal = TRUE          # Return the data as an R object rather than saving to disk
)

df_temp <- list_mendota$data %>% 
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate(
    date = as.Date(paste(year, yday, sep = "-"), format = "%Y-%j"),
    month = month(date)
  ) %>% 
  arrange(year, yday) %>% 
  group_by(year) %>%
  summarize(temp_min = round(mean(tmin_deg_c), 2))

# add temp data to ice cover data based on year
df_ice <- df_ice %>% 
  left_join(df_temp, by = "year")

obj_arima <- auto.arima(
  df_ice$ice_duration, # Response variable
  xreg = df_ice$temp_min, # External predictor
  stepwise = FALSE # forces model to do exhaustive rather than piecewise search
)

#if you add or subtract the s.e.*2 to the xreg value and neither adding or 
#subtracting overlaps with zero, then results significant

df_ice%>%
  ggplot(aes(x=temp_min,
             y=ice_duration))+
  geom_point()

# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: Bison Body Mass, Climate, and Time-Series Analysis
# ============================================================

library(lterdatasampler)

# The "knz_bison" dataset contains long-term monitoring data
# on bison captured at Konza Prairie Biological Station.
#
# ------------------------------------------------------------
# Key columns may include:
# rec_year      : Year of capture
# animal_sex    : Sex of the individual (e.g., female, male)
# animal_weight : Body mass of bison
# ------------------------------------------------------------
#
# In this exercise, you will explore long-term trends in bison
# body mass and evaluate how climate variability may influence
# weight dynamics over time.

# 1. Explore the structure of the knz_bison dataset.
#    - Inspect variable types and missing values.
#    - Reformat variables as needed for analysis.

knz_bison1<-knz_bison%>%
  rename_at('rec_year', ~'year')

# 2. Subset the data to include observations from 1994–2012.


knz <- knz_bison1 %>% 
  as_tibble() %>%
  filter(between(year, 1994, 2012)) %>%
  arrange(year)

# 3. Calculate the average body mass for female and male bison
#    for each year in the selected time period.

knz1<-knz%>%
  group_by(year,animal_sex)%>%
  summarize(mu_w=mean(animal_weight))

# 4. Obtain climate data from the daymetr dataset.
#    - Identify relevant climate variables (e.g., temperature,
#      precipitation).
#    - Associate climate data with knz_bison by year.
#    - Coordinates: Lat 39.09300	Lon -96.57500

list_knz <- download_daymet(
  site = "KNZ",
  lat = 39.09300,
  lon = -96.57500,
  start = 1994,
  end = 2012,
  internal = TRUE
)

df_env<-list_knz$data %>% 
  as_tibble() %>%
  janitor::clean_names()%>%
  group_by(year) %>%
  summarize(mu_prec = mean(prcp_mm_day))

knz2 <- knz1 %>% 
  left_join(df_env, by = "year")

# 5. Perform a time-series analysis to examine whether selected
#    climate variables influence annual bison body mass.
#    - Consider temporal autocorrelation and lag effects.
#    - Model males and females separately

knzM<-knz2%>%
  filter(animal_sex=="M")

knzF<-knz2%>%
  filter(animal_sex=="F")

auto.arima(
  knzM$mu_w,
  xreg = knzM$mu_prec,
  stepwise = FALSE
)

auto.arima(
  knzF$mu_w,
  xreg = knzF$mu_prec,
  stepwise = FALSE
)

knz2%>%
  ggplot(aes(x=mu_prec,
             y=mu_w,
             color=animal_sex))+
  geom_point()

# 6. Using your fitted model, compare observed bison body mass
#    with predicted values for the period 2014–2020.
#    - Evaluate model performance and discuss sources of uncertainty.
