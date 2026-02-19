#' DESCRIPTION:
#' Script for GAMs

# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               ggeffects,
               mgcv)

link <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_water_temp.csv"

(df_wt_raw <- read_csv(link))

sapply(df_wt_raw, class)

df_wt<-df_wt_raw%>%
  mutate(date=as.Date(date_time,format= "%m/%d/%Y"),
         year=year(date),
         month=month(date))%>%
  filter(year==2022,
         between(month,left=3,right=10))

df_wt_daily<-df_wt%>%
  group_by(date,
           site)%>%
  summarize(temp=mean(temp,na.rm=TRUE)%>%
              round(3),
            .groups="drop")

df_wt_daily%>%
  ggplot(aes(x=date,
             y=temp,
             color=site))+
  geom_point(alpha=.25)+
  theme_bw()+
  labs(x="date",
       y="water temp",
       color="wetland type")

df_wt_daily<-df_wt_daily%>%
  mutate(j_date=yday(date),
         site=factor(site))

m_glm<-glm(temp ~ site+j_date,
    family="gaussian",
    data=df_wt_daily)

summary(m_glm)

# Generate model predictions across all Julian days and wetland sites
df_pred <- ggpredict(m_glm,
                     terms = c(
                       "j_date [all]",  # Use all observed values of Julian day
                       "site [all]"     # Generate predictions for all levels of the factor 'site'
                     )
) %>% 
  # Rename the default columns to match the original dataset
  rename(site = group,  # 'group' from ggpredict() corresponds to the factor variable 'site'
         j_date = x     # 'x' from ggpredict() corresponds to the predictor 'j_date'
  )

# Plot daily water temperature and overlay model predictions
df_wt_daily %>% 
  ggplot(aes(
    x = j_date,   # Julian day on x-axis
    y = temp,     # Observed daily temperature on y-axis
    color = site  # Color points by wetland type (factor)
  )) +
  geom_point(alpha = 0.25) +
  # Overlay predicted values from the model
  # df_pred contains predictions from ggpredict()
  # aes(y = predicted) maps the model's predicted temperature to y
  geom_line(data = df_pred,
            aes(y = predicted)) +
  theme_bw() +
  labs(x = "Julian Date",         # x-axis label
       y = "Water Temperature",   # y-axis label
       color = "Wetland Type"     # Legend title for site color
  )

m_gam<-gam(temp~site+s(j_date), #s() accounts for j_date being nonlinear
    data=df_wt_daily,
    family="gaussian")

summary(m_gam)

df_pred_gam <- ggpredict(m_gam,
                         terms = c(
                           "j_date [all]", 
                           "site [all]")
) %>% 
  rename(site = group,
         j_date = x)

df_wt_daily %>% 
  ggplot(aes(
    x = j_date,
    y = temp, 
    color = site
  )) +
  geom_point(alpha = 0.25) +
  # Overlay predicted values from the GAM
  geom_line(data = df_pred_gam,
            aes(y = predicted)) +
  theme_bw() +
  labs(x = "Julian Date",         # x-axis label
       y = "Water Temperature",   # y-axis label
       color = "Wetland Type"     # Legend title for site color
  )
# lab ---------------------------------------------------------------------

# 1. Read directly from the raw GitHub URL
url <- "https://raw.githubusercontent.com/aterui/public-proj_restore-aqua-complex/v.1.0/data_raw/data_bat.csv"

# Try reading normally
df_bat <- read_csv(url, show_col_types = FALSE)

# ============================================================
# DATA GUIDE: Bat Detector Data
# ============================================================

# ----------------------------
# Raw data columns
# ----------------------------

# Site
#   Location where bat detectors are deployed.
#   Levels:
#     "RECCON"  = prairie site without wetland
#     "RECWET"  = prairie site with constructed wetland
#     "WOODCON" = woody site without wetland
#     "WOODWET" = woody site with constructed wetland

# DATE
#   Calendar date of each bat pass record.
#   Expected format: YYYY-MM-DD (verify and standardize).

# TIME
#   Time of bat pass detection.
#   Expected format: HH:MM:SS (verify and standardize).

# AUTO ID*
#   Automatically identified bat species.
#   Species IDs may contain misclassifications or unknown labels
#   that should be carefully reviewed during data cleaning.

# ============================================================
# GOAL 1: Clean data
# ============================================================

# 1. Format column names
#   - Convert column names to a clean format

df_bat<-df_bat%>%
  janitor::clean_names()%>%
  mutate(across(.cols = where(is.factor),
                .fns = str_to_lower))

# 2. Examine each column carefully
#   - Check for missing values, inconsistent formats, and typos
#   - Confirm DATE and TIME are properly parsed as date/time objects
#   - Inspect AUTO ID values for NA
#   - Remove or correct invalid or unusable records as needed

sapply(df_bat, FUN=function(x) sum(is.na(x)))

df_bat<-df_bat%>%
  mutate(date=as.Date(date,format= "%m/%d/%Y"),
habitat_type=case_when(site %in% c("RECCON", "RECWET")~"prairie",
                         site %in% c("WOODCON", "WOODWET")~"woody"),
  wetland_status=case_when(site %in% c("RECCON", "WOODCON")~"no_wetland",
                         site %in% c("RECWET", "WOODWET")~"wetland")
)%>%
  drop_na(auto_id)

# New derived columns to create:
# Site-level categories:
#   Prairie sites: "RECCON", "RECWET"
#   Woody sites:   "WOODCON", "WOODWET"

# 3. habitat_type
#   Broad site classification:
#     "prairie" = RECCON, RECWET
#     "woody"   = WOODCON, WOODWET

# 4. wetland_status
#   Presence/absence of wetland:
#     "no_wetland" = RECCON, WOODCON
#     "wetland"    = RECWET, WOODWET

# ============================================================
# GOAL 2: Visualize daily bat activity
# ============================================================

# Objective:
#   Quantify and visualize bat activity as the number of bat passes per day.

df_bat_v<-df_bat%>%
  group_by(site,
           date,
           habitat_type,
           wetland_status)%>%
  summarize(pass=n(), #counts # of rows in each group in group_by
            .groups="drop")%>%
  mutate(month=month(date),
         year=year(date),
         j_date=yday(date))%>%
  filter(year==2021)

df_bat_v%>%
  ggplot(aes(x=date,
             y=pass,
             color=wetland_status))+
  geom_point()+
  facet_wrap(facets =~habitat_type)+
  theme_bw()

# Steps:
#   - Aggregate data to calculate daily bat passes
#   - Convert DATE to Julian date
#   - Plot number of bat passes as a function of Julian date
#   - Optionally:
#       * Color or facet plots by site
#       * Smooth trends to visualize seasonal patterns

# ============================================================
# GOAL 3: Model differences among sites
# ============================================================

# Objective:
#   Test whether bat activity differs among the four detector sites.
#   Does the presence of wetland affect bat activity?
#   Is the effect of wetland is site-dependent?

# Modeling considerations:
#   - Response variable: daily bat passes
#   - Predictors may include:
#       * habitat_type
#       * wetland_status
#       * site (four-level factor)
#       * Julian date (to account for seasonality)
#   - Consider appropriate count models

mean(df_bat_v$pass)
var(df_bat_v$pass)

b_gam_1<-gam(pass~habitat_type+wetland_status+s(j_date),
           data=df_bat_v,
           family="nb")
b_gam_2<-gam(pass~habitat_type*wetland_status+s(j_date),
             data=df_bat_v,
             family="nb")

summary(b_gam_1)
summary(b_gam_2)
