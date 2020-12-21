library(IMFData)
library(tidyverse)
library(lubridate)
library(plm)
library(lmtest)
library(wbstats)
library(Hmisc)
library(corrplot)
library(quantmod)
rm(list = ls())
source("source.R")


#----------------------------
# GDP DATA IMF
#----------------------------
IFS.available.codes <- DataStructureMethod("IFS")
names(IFS.available.codes)
CodeSearch(IFS.available.codes, "CL_INDICATOR_IFS", "GDP")
countries <- IFS.available.codes[[2]]

# Call API
databaseID <- "IFS"
startdate <- "2007-01-01"
enddate <- "2019-12-31"

# Query (National Accounts, Expenditure, Gross Domestic Product, Real, Spliced Historical Series, Index)
# Code 722
queryfilter <- list(CL_FREQ = "Q", CL_AREA_IFS="", CL_INDICATOR_IFS = "NGDP_R_K_XDC")

NGDP_R_K_XDC <- CompactDataMethod(databaseID = databaseID, queryfilter, startdate, enddate,
                                        tidy = T)

# Cantidad de Paises 
length(unique(NGDP_R_K_XDC[["@REF_AREA"]]))
# Check NAs
NGDP_R_K_XDC[is.na(NGDP_R_K_XDC)]

# Select important columns 
NGDP_R_K_XDC <- NGDP_R_K_XDC %>%
  select(`@TIME_PERIOD`,`@OBS_VALUE`, `@REF_AREA`) %>%
  rename("year_qtr" = `@TIME_PERIOD`,
         "gdp" = `@OBS_VALUE`,
         "code_country" = `@REF_AREA`) %>%
  left_join(countries %>% rename("code_country" = CodeValue,
                               "country" = CodeText), by="code_country")


NGDP_R_K_XDC$year <- head(yq(NGDP_R_K_XDC$year_qtr, format = "%Y-Q%q"), -1)


# Check Balanced Panel (not balanced)
NGDP_R_K_XDC %>%
  group_by(country) %>%
  summarise(n())

# Check how many countries (54)
countries_2019 <- NGDP_R_K_XDC %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  filter(n == 52)
countries_2019$country 

NGDP_R_K_XDC_2019 <- NGDP_R_K_XDC %>%
  filter(country %in% unique(countries_2019$country)) %>%
  filter(country != "Euro area (Member States and Institutions of the Euro Area) changing composition")

# Recession = 1 GDP negative during two consecutive quarters 
NGDP_R_K_XDC_2019 <- NGDP_R_K_XDC_2019 %>%
  mutate(gdp = as.numeric(gdp)) %>%
  group_by(country) %>%
  mutate(gdp_growth = c(NA, NA, NA, NA, diff(log(gdp), 4))) %>%
  mutate(recession_gdp = if_else(gdp_growth < 0 & dplyr::lead(gdp_growth, n = 1) < 0, 1, 0)) %>%
  mutate(recession_gdp = if_else(dplyr::lag(recession_gdp) == 1 & gdp_growth < 0, 1, recession_gdp)) %>%
  mutate(recession_gdp = if_else(is.na(recession_gdp), 0, recession_gdp)) %>%
  ungroup()


#----------------------------
# Mobile OS DATA
#----------------------------
mobile <- read_csv("output/mobile_so.csv")
setdiff(unique(NGDP_R_K_XDC_2019$country), unique(mobile$`Country Name`))

# Change countries names 
mobile$`Country Name`[mobile$`Country Name` == "Hong Kong"] <- "Hong Kong, China"
mobile$`Country Name`[mobile$`Country Name` == "Macedonia"] <- "North Macedonia, Republic of"
mobile$`Country Name`[mobile$`Country Name` == "South Korea"] <- "Korea, Republic of"
mobile$`Country Name`[mobile$`Country Name` == "Slovakia (Slovak Republic)"] <- "Slovakia"
mobile$`Country Name`[mobile$`Country Name` == "Bosnia And Herzegovina"] <- "Bosnia and Herzegovina"
mobile$`Country Name`[mobile$`Country Name` == "United States Of America"] <- "United States"


# Replication figure 4 
mobile %>%
  mutate(year = yq(paste0(Year, "-Q", Quarter)),
         diff_andriod_ios = Android - iOS) %>%
  ggplot(aes(x = year, y = diff_andriod_ios, color = `Country Name`)) +
  geom_line() +
  ylab("Market Share Difference Andriod vs iOS") +
  xlab("Quarters") +
  scale_color_manual(values = rep("black", 85)) +
  labs(title = "Market Share Difference (Andriod vs iOS) Evolution per Country, 2009-2019") +
  theme_minimal() +
  theme(legend.position='none') 

mobile <- mobile %>%
  rename("country" = `Country Name`) %>%
  mutate(year = yq(paste0(Year, "-Q", Quarter)))
#----------------------------
# Panel DATA
#----------------------------
panel_data <- NGDP_R_K_XDC_2019 %>%
  left_join(mobile, by=c("country", "year"))

panel_data <- panel_data %>%
  select(year, country, gdp, gdp_growth, recession_gdp, Android, iOS) %>%
  mutate(diff_so = Android - iOS)

#panel_data %>%
#  filter(gdp_growth <= 1) %>%
#  ggplot(aes(gdp_growth, diff_so)) +
#  geom_point() +
#  geom_smooth(method = "lm") +
#  theme_minimal() +
#  theme(legend.position='none') +
#  facet_wrap(~country)

#----------------------------
# WB DATA
#----------------------------
# SP.POP.1564.TO - Population, ages 15-64
population <- wb(indicator = "SP.POP.1564.TO", startdate = 2007, enddate = 2019)
setdiff(unique(panel_data$country), unique(population$country))

population$country[population$country == "Hong Kong SAR, China"] <- "Hong Kong, China"
population$country[population$country == "North Macedonia"] <- "North Macedonia, Republic of"
population$country[population$country == "Korea, Rep."] <- "Korea, Republic of"
population$country[population$country == "Slovak Republic"] <- "Slovakia"

population <- population %>%
  select(date, value, country) %>%
  rename("population" = value) %>%
  mutate(date = as.numeric(date))



panel_data <- panel_data %>%
  mutate(date = year(year)) %>%
  left_join(population) %>%
  select(-date)


# Construct new variables
panel_data <- panel_data %>%
  mutate(gdp = as.numeric(gdp)) %>%
  mutate(gdp_percapita = gdp / population)

panel_data <- panel_data %>%
  group_by(country) %>%
  mutate(gdp_percapita_rate = c(NA, NA, NA, NA, diff(log(gdp_percapita), 4))) %>%
  mutate(recession = if_else(gdp_percapita_rate < 0 & dplyr::lead(gdp_percapita_rate, n = 1) < 0, 1, 0)) %>%
  mutate(recession = if_else(dplyr::lag(recession) == 1 & gdp_percapita_rate < 0, 1, recession)) %>%
  mutate(recession = if_else(is.na(recession), 0, recession)) %>%
  filter(year >= "2009-01-01")


#  IT.CEL.SETS - Mobile cellular subscriptions
mobile_subscriptions  <- wb(indicator = "IT.CEL.SETS", startdate = 2007, enddate = 2019)
setdiff(unique(panel_data$country), unique(mobile_subscriptions$country))

mobile_subscriptions$country[mobile_subscriptions$country == "Hong Kong SAR, China"] <- "Hong Kong, China"
mobile_subscriptions$country[mobile_subscriptions$country == "North Macedonia"] <- "North Macedonia, Republic of"
mobile_subscriptions$country[mobile_subscriptions$country == "Korea, Rep."] <- "Korea, Republic of"
mobile_subscriptions$country[mobile_subscriptions$country == "Slovak Republic"] <- "Slovakia"

mobile_subscriptions <- mobile_subscriptions %>%
  select(date, value, country) %>%
  rename("mobile_subscriptions" = value) 

mobile_subscriptions <- mobile_subscriptions %>%
  arrange(date, country) %>%
  mutate(date = as.numeric(date))

# Interpolate
expand_and_interpolate <- function(x) interpolate_data(expand_data(x))
mobile_subscriptions <- mobile_subscriptions %>% group_by(country) %>% do(expand_and_interpolate(.))

mobile_subscriptions <- mobile_subscriptions %>%
  group_by(country) %>%
  mutate(mobile_growth_rate = c(NA, NA, NA, NA, diff(log(mobile_subscriptions_int), 4)))

mobile_subscriptions <- mobile_subscriptions %>%
  mutate(date = yq(paste0(date, ".", quarter)))

panel_data <- panel_data %>%
  mutate(date = year) %>%
  left_join(mobile_subscriptions, by=c("date", "country")) %>%
  select(-date)




# Create MP variable
panel_data <- panel_data %>%
  mutate(MP = (Android - iOS) - (dplyr::lag(Android) - dplyr::lag(iOS)))

write_csv("output/panel_data.csv", x = panel_data)

# Total de recesiones por paises
panel_data %>%
  group_by(country) %>%
  summarise(total_recession = sum(recession))

for (k in unique(panel_data$country)) {
  df <- panel_data %>%
    filter(country == k)
  
  years_start_recession <- c()
  for (i in nrow(df):1) {
    if (i <= 1) {
      break
    } else {
      if (df[i, ]$recession == 1 & df[(i-1), ]$recession == 0) {
        years_start_recession <- c(years_start_recession, df[i, ]$year)
      } else if (df[i, ]$recession == 1 & df[(i-1), ]$recession == 0 | (i - 1) <= 1) {
        years_start_recession <- c(years_start_recession, df[i-1, ]$year)
      }
    }
  }
  print(k)
  print("Periodos de Inicio Recesión")
  print(as.Date(rev(years_start_recession)))
  years_end_recession <- c()
  for (i in 1:nrow(df)) {
    if (i >= nrow(df)) {
      break
    } else {
      if (df[i, ]$recession == 1 & df[(i+1), ]$recession == 0) {
        years_end_recession <- c(years_end_recession, df[i, ]$year)
      } else if (df[i, ]$recession == 1 & df[(i+1), ]$recession == 0 & (i + 1) >= nrow(df)) {
        years_end_recession <- c(years_end_recession, df[i+1, ]$year)
      }
    }
  }
  print("Periodos de Termino Recesión")
  print(as.Date(years_end_recession))
  print("Duración Crisis (en trimestres)")
  duration_recession <- difftime(as.Date(years_end_recession), as.Date(rev(years_start_recession)), units = "days")
  duration_recession  <- as.double(duration_recession)/365
  print(duration_recession*4)
  decline_gdp_growth_cum <- c()
  for (h in 1:length(years_end_recession)) {
    if (length(years_end_recession)== 0) {
      next
    } else {
    init <- as.Date(rev(years_start_recession[h]))
    end <- as.Date(years_end_recession[h])
    df_filter <- df %>%
      filter(year == init | year == end)
    decline_gdp_growth_cum <- c(decline_gdp_growth_cum, (df_filter[2,]$gdp_percapita /  df_filter[1,]$gdp_percapita)-1)
    }
  }
  print("Caida Acumulada en Recesión")
  print(decline_gdp_growth_cum)
  print("==============================")
  
}

  


#----------------------------
# Regressions
#----------------------------
# Drop El Salvador
panel_data_filter <- panel_data %>%
  filter(country != "El Salvador") %>%
  filter(year <= "2011-12-31")


cor_data <- cor(as.matrix(panel_data_filter %>% select(-year, -country)), use = "complete.obs")
corrplot(cor_data, type = "upper", order = "hclust", 
         tl.col = "black")


# Modelo OLS simple
model_ols <- lm(MP ~ log(gdp_percapita)  + mobile_growth_rate + recession, 
                data = panel_data_filter)

summary(model_ols)
# Modelo con efectos fijos por país
model_fe <- plm(MP ~  log(gdp_percapita) + mobile_growth_rate + recession,
                data = panel_data_filter, index = c("country"))

summary(model_fe)
# Robust standard error 
coeftest(model_fe, vcov. = vcovHC, type = "HC1")




INDPRO <- getSymbols("INDPRO", src = "FRED", auto.assign = F)
INDPRO <- data.frame(date=index(INDPRO), coredata(INDPRO))
INDPRO <- INDPRO %>%
  filter(date >= "2008-12-01")

INDPRO <- INDPRO %>%
  mutate(growth = c(NA, diff(log(INDPRO))))

INDPRO %>%
  ggplot(aes(date, growth)) +
  geom_line()

monthly_mobile <- read_csv("output/mobile_so_monthly.csv")

monthly_mobile <- monthly_mobile %>%
  filter(`Country Name` == "United States Of America") %>%
  select(Date, iOS, Android) %>%
  rename("date" = Date)

df <- monthly_mobile %>%
  left_join(INDPRO) %>%
  arrange(date) %>%
  mutate(MP = (Android - iOS) - (dplyr::lag(Android) - dplyr::lag(iOS)),
         MP =  MP / 100) %>%
  mutate(recession = if_else(growth < 0 & dplyr::lead(growth, n = 1) < 0, 1, 0)) %>%
  mutate(recession = if_else(dplyr::lag(recession) == 1 & growth < 0, 1, recession)) %>%
  mutate(recession = if_else(is.na(recession), 0, recession))

df %>%
  ggplot(aes(date, growth, linetype = "Production")) +
  geom_line() +
  geom_line(aes(date, MP, linetype = "MP"))

ggsave("usa.png")

df %>%
  ggplot(aes(growth, MP)) +
  geom_point() +
  geom_smooth(method = "lm")
ggsave("scatter_usa.png")

model_ols <- lm(MP ~ growth + recession, 
                data = df)

summary(model_ols)







