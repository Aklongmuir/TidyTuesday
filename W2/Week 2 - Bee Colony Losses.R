library(tidyverse)
library(geojsonio)
library(rgdal)
library(rgeos)
library(broom)
library(scales)
library(extrafont)
library(showtext)

#Read in Data

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

#Summarise Data

chart_df <- colony %>%
  group_by(year,state)%>%
  mutate(sum_loss = sum(colony_lost_pct))%>%
  summarise(year,state, survived_pct = ((100 - sum_loss)/100))%>%
  unique()%>%
  filter(year != "6/")

#Add df for missing states to help keep the chart clean

state <-c("Alaska", "Rhode Island","New Hampshire", "Delaware", "District of Columbia", "Nevada")
year <- c("2015","2015","2015","2015","2015","2015")
survived_pct <- "NA"
Missing_Data <- data.frame(state, year, survived_pct = as.numeric(survived_pct))

chart_df <- rbind(chart_df,Missing_Data)
  

#Load USA Hex Map

# Load this file. (Note: I stored in a folder called DATA)
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# fix the data to work with ggplot

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the center of each hexagon to add the label:

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()


#Join bee Data with Map Data
chart_df_final <- spdf_fortified %>%
  left_join(.,chart_df, by=c("id"="state")) 

#Add Custom Fonts

font_add_google("Roboto Condensed", family = "Roboto Condensed")
showtext_auto()

#Build ggplot

ggplot() +
  geom_polygon(data = chart_df_final,color = "#ffffff", aes(fill =  survived_pct, x = long, y = lat, group = group)) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=1) +
  scale_fill_gradient(trans = "log",low = "#fdea8b", high = "#fb9a0e",na.value = "#d3d3d3",labels = percent, breaks = c(0.75,0.5,0.25,0.1)) +
  theme_void() +
  coord_map()+
  facet_wrap(vars(year))+
  theme(legend.position = "bottom", legend.key.width = unit(3, "line"))+
  labs(fill = "Survival %",title = "According to all known laws of aviation\nthere is no way a bee should be able to fly\n", caption = "Week 2 - Tidy Tuesday 2022\naklongmuir.com|@alyssastweeting|twitch:aklongmuir")+
  theme(text = element_text(family="Roboto Condensed"), plot.title = element_text(hjust =0.5), plot.caption = element_text(hjust = 0.5), plot.margin = margin(1, 1, 1, 1,"cm"))
