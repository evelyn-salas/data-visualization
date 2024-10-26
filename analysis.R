######################################
### 1 Load and Understand the Data ###
######################################

#install.packages("wbstats")
library("wbstats")
library("dplyr")
options(scipen = 999)
library("ggplot2")
library("tidyr")
#install.packages("maps")
library("maps")
library("mapproj")

print(nrow(wbindicators()))

# ID for "CO2 emissions (kt)" = EN.ATM.CO2E.KT

co2_countries_df <-
  wb(country = "countries_only", indicator = c("EN.ATM.CO2E.KT"), mrv = 1) %>% 
    arrange(-value) %>% 
  as.data.frame() 


top_10_co2_countries <- 
co2_countries_df[1:10, ]

#View(top_10_co2_countries)

#################################
### 2 CO2 Emissions by Contry ###
#################################

top_10_co2_plot <- 
ggplot(data = top_10_co2_countries) + 
  geom_col(mapping = aes(x = reorder(iso3c, value), y = value)) +
  labs(
    title = "Top 10 Countries by CO2 Emissions",
      x = "Country (iso3)",
      y = "CO2 emissions (kt)"
  )


######################################
### 3 US Income Equality over Time ###
######################################

usa_income <-
wb(country = "USA", indicator = c("SI.DST.10TH.10", "SI.DST.04TH.20",
                                  "SI.DST.FRST.20"), mrv = 20, return_wide = TRUE) %>%
  as.data.frame() 


us_income_years <- 
usa_income %>% 
  rename(wealth_top_10 = SI.DST.10TH.10,
         wealth_4th_20 = SI.DST.04TH.20,
         wealth_bottom_20 = SI.DST.FRST.20
         )%>%  
  mutate( wealth_bottom_40 = (wealth_4th_20 + wealth_bottom_20), 
        date = as.numeric(date)) %>% 
gather(key = "income_group", value = "wealth", c("wealth_top_10", "wealth_bottom_40"))

us_wealth_plot <-
ggplot(data = us_income_years, mapping = aes(x = date, y = wealth, color = income_group)) + 
  geom_point() + geom_line() +
  scale_color_discrete(labels = c("Top 10% of Pop", "Bottom 40% of Pop")) +
  labs(title = "US Wealth Equality Over Time", x = "Year", y = "Percentage of income held")
  
  
########################################
### 4 Health Expenditures by Country ###
########################################

wealthy_countries <-
wbcountries() %>% filter(income == "High income") %>% pull(iso3c) 

updated_cache <- wbcache()

health_costs <- 
wb(country = wealthy_countries, indicator = c("SH.XPD.CHEX.PC.CD", "SH.XPD.GHED.PC.CD",
                                            "SH.XPD.PVTD.PC.CD", "SH.XPD.OOPC.PC.CD"),
  cache = updated_cache, return_wide = T, mrv = 1) %>% 
  rename("Total Spending"= SH.XPD.CHEX.PC.CD,"Government Spending" = SH.XPD.GHED.PC.CD,
         "Private Spending" = SH.XPD.PVTD.PC.CD, "Out of Pocket Costs" = SH.XPD.OOPC.PC.CD) %>% 
  gather(key = "Type", value = "value", c("Total Spending", "Government Spending",
                                                "Private Spending", "Out of Pocket Costs")) %>% 
  arrange(value)

total_health_costs <- 
  health_costs[health_costs$Type == "Total Spending", ] %>% 
    as.data.frame()

health_costs_plot <- 
ggplot(
  data = health_costs, mapping = aes(x= reorder(iso3c, value), y = value, 
                                     color = Type, group = Type)) + 
  geom_linerange(data = total_health_costs, mapping = aes(ymin= 0, ymax= value), 
                show.legend = FALSE) +
 geom_point(aes(shape = Type)) +
  scale_color_brewer(type = "seq", palette = "Dark2", direction = -1, aesthetics = "colour") +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
   theme(legend.position = c(.45, .95), legend.justification = c("right", "top")) +
  labs(title = "Health Care Expenditures (per capita)", x = "Country", y = "Current US$")

#####################################################
### 5 Map Changes in Forestation around the World ###
#####################################################

forest_data <- 
wb(country = "countries_only", indicator = c("AG.LND.FRST.ZS"), mrv = 20) %>% 
  spread(key = date, value = value) %>% as.data.frame() 

forest_area <- 
 forest_data %>%  
mutate(
    first_year = forest_data[["1997"]],
    final_year = forest_data[["2016"]],
    forest_change = (first_year - final_year)
  )

maps_df <- map_data("world")

maps_df$iso3c <- maps_df$region %>% iso.alpha( n=3)

joined_data <- 
left_join(maps_df, forest_area, by = "iso3c") 

world_forest_plot <- 
  ggplot(data = joined_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = forest_change)) + 
  scale_fill_distiller(palette = "RdYlGn", direction = -1) +
  coord_quickmap() +
  labs(fill = "forest_change") +
  theme_void() +
  labs(title = "Change in Forested Area 1997-2016", fill = "Change")
  
#######################
### 6 Your Own Plot ###
#######################
literacy_and_teen_pregnancy <- 
wb(
  country = "IND", indicator = c("SP.ADO.TFRT", "SE.ADT.1524.LT.ZS"),
  return_wide = T, mrv = 30) %>% rename(
    "teen_pregnancy" = SP.ADO.TFRT,
    "literacy_rates" = SE.ADT.1524.LT.ZS
  ) %>% 
  gather(key = "type", value = "rate", c("teen_pregnancy", "literacy_rates")) %>% 
  mutate(date = as.numeric(date)) %>% 
  na.omit()
 
  
cbPalette <- c("#E69F00", "#009E73")

literacy_and_teen_pregnancy_IND_plot <- 
  ggplot(data = literacy_and_teen_pregnancy, mapping = aes(x = date, y = rate, color = type)) +
    geom_point() + geom_line() +
    scale_color_manual(values = cbPalette,
                            name = "Key",
      labels = c("Literacy Rate", "Teen Pregnancy Rate")) + 
  labs(title = "India Literacy and Teen Pregnancy Rates Over Time", x = "Year", y = "Rate")

