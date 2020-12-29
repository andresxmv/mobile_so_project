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
  select(year, country, gdp, gdp_growth, recession_gdp, Android, iOS, `BlackBerry OS`, Windows) %>%
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

# Extrapolo y calculo tasa
mobile_subscriptions <- mobile_subscriptions %>%
  group_by(country) %>%
  mutate(mobile_subscriptions_int = zoo::na.spline(mobile_subscriptions_int)) %>%
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


# Leer base de Apple 
apple <- read_csv("output/apple_data.csv") %>%
  rename("r&d_apple" = `r&d`,
         "assets_apple" = assets,
         "liabilities_apple" = liabilities,
         "current_assets_apple" = current_assets,
         "current_liabilities_apple" = current_liabilities,
         "current_ratio_apple" = current_ratio,
         "year" = date) %>%
  mutate(year = as.yearqtr(year))

google <- read_csv("output/google_data.csv") %>%
  rename("r&d_google" = `r&d`,
         "assets_google" = assets,
         "liabilities_google" = liabilities,
         "current_assets_google" = current_assets,
         "current_liabilities_google" = current_liabilities,
         "current_ratio_google" = current_ratio,
         "year" = date) %>%
  mutate(year = as.yearqtr(year))

panel_data <- panel_data %>%
  mutate(year = as.yearqtr(year)) %>%
  left_join(apple, by ="year") %>%
  left_join(google, by= "year")


write_csv("output/panel_data.csv", x = panel_data)

# Total de recesiones por paises
panel_data %>%
  group_by(country) %>%
  summarise(total_recession = sum(recession))

panel <- panel_data %>%
  mutate(year = as.Date(year))

ios <- c()
andriod <- c()
recession_date <- c()
caida_acum <- c()
caida_android <- c()
caida_ios <- c()
gdp_init <- c()
duration <- c()
industria_init <- c()
caida_industria <- c()
for (k in unique(panel$country)) {
  df <- panel %>%
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
  duration <- c(duration, duration_recession*4)
  print(duration_recession*4)
  
  years_start_recession <- rev(years_start_recession)
  decline_gdp_growth_cum <- c()
  decline_industry_cum <- c()
  decline_andriod_cum <- c()
  decline_ios_cum <- c()
  market_share_andriod <- c()
  market_share_ios <- c()
  gdp_inicial <- c()
  industria_inicial <- c()
  for (h in 1:length(years_start_recession)) {
    if (length(years_end_recession)== 0) {
      if (length(duration_recession) == 0) {
        years_start_recession <- 99
        next
      }
    } else {
    init <- as.Date(years_start_recession[h])
    #print(init)
    end <- as.Date(years_end_recession[h])
    #print(end)
    df_filter_1<- df %>%
      filter(year >= init & year <= end) %>%
      arrange(year)
    
    df_filter<- df %>%
      filter(year == init | year == end) %>%
      arrange(year)
    #print(df_filter)
    decline_gdp_growth_cum <- c(decline_gdp_growth_cum, sum(df_filter_1$gdp_percapita_rate, na.rm=T))
    decline_industry_cum <- c(decline_industry_cum, (df_filter[2,]$mobile_subscriptions_int /  df_filter[1,]$mobile_subscriptions_int)-1)
    decline_andriod_cum <- c(decline_andriod_cum, (df_filter[2,]$Android /  df_filter[1,]$Android)-1)
    decline_ios_cum <- c(decline_ios_cum, (df_filter[2,]$iOS /  df_filter[1,]$iOS)-1)
    market_share_andriod <- c(market_share_andriod, df_filter[1, ]$Android)
    market_share_ios <- c(market_share_ios, df_filter[1, ]$iOS)
    gdp_inicial <- c(gdp_inicial, df_filter[1, ]$gdp_percapita)
    industria_inicial <- c(industria_inicial, df_filter[1, ]$mobile_subscriptions_int)
    #print(length(decline_gdp_growth_cum))
    }
  }
  # Llenar vectores
  recession_date <- c(recession_date, as.Date(years_start_recession))
  #duration <- c(duration, duration_recession)
  caida_acum <- c(caida_acum, decline_gdp_growth_cum)
  caida_industria <- c(caida_industria, decline_industry_cum)
  caida_android <- c(caida_android, decline_andriod_cum)
  caida_ios <- c(caida_ios, decline_ios_cum)
  gdp_init <- c(gdp_init, gdp_inicial)
  industria_init <- c(industria_init, industria_inicial)
  ios <- c(ios, market_share_ios)
  andriod <- c(andriod, market_share_andriod)
  
  
  print("Caida Acumulada en Recesión")
  print(decline_gdp_growth_cum)
  print("Caida Acumulada Industria")
  print(decline_industry_cum)
  print("Caida Acumulada Andriod")
  print(decline_andriod_cum)
  print("Caida Acumulada iOS")
  print(decline_ios_cum)
  print("Market Share Andriod")
  print(market_share_andriod)
  print("Market Share iOs")
  print(market_share_ios)
  print("==============================")
  
}


table_recession <- data.frame("period_init" = as.Date(recession_date[recession_date!=99]),
           "duration" = round(duration, 0),
           "decline_gdp" = caida_acum,
           "decline_industry" = caida_industria,
           "decline_android" = caida_android,
           "decline_ios" = caida_ios,
           "gdp_pc_init" = gdp_init,
           "industria_init" = industria_init,
           "android_init" = andriod,
           "ios_init" = ios)

index_to_replace <- which(table_recession$duration < 0)
table_recession[index_to_replace, 2:10] <- NA 

write_csv(table_recession, "output/table_recession.csv")


#----------------------------
# Figures Random
#----------------------------  
# R&D
panel_data %>%
  ggplot(aes(year, `r&d_apple`, color = "Apple")) +
  geom_line() +
  geom_line(aes(year, `r&d_google`, color = "Alphabet")) +
  ylab("R&D") +
  xlab("Year") +
  theme_bw() +
  labs(colour = "")
ggsave("figures/r&d.png")
# Liquidity
panel_data %>%
  ggplot(aes(year, current_ratio_apple, color = "Apple")) +
  geom_line() +
  geom_line(aes(year, current_ratio_google, color = "Alphabet")) +
  ylab("Liquidity") +
  xlab("Year") +
  theme_bw() +
  labs(colour = "")
ggsave("figures/liquidity.png")
# Assets and Liabilities
panel_data %>%
  ggplot(aes(year, assets_apple, color = "Asset Apple")) +
  geom_line() +
  geom_line(aes(year, liabilities_apple, color = "Liabilities Apple")) +
  geom_line(aes(year, assets_google, color = "Asset Alphabet")) +
  geom_line(aes(year, liabilities_google, color = "Liabilities Alphabet")) +
  ylab("") +
  xlab("Year") +
  theme_bw() +
  labs(colour = "")
ggsave("figures/assets_liabilities.png")

# Industry 
panel_data %>%
  ggplot(aes(year, mobile_subscriptions_int)) +
  geom_line() +
  facet_wrap(~country, scales = "free")
ggsave("figures/mobile_sub_levels.png")

panel_data %>%
  ggplot(aes(year, mobile_growth_rate)) +
  geom_line() +
  facet_wrap(~country, scales = "free")
ggsave("figures/mobile_sub_rate.png")
#----------------------------
# Regressions
#----------------------------
# Drop El Salvador
panel_data_filter <- panel_data %>%
  filter(country != "El Salvador")


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










