library(tidyverse)
library(devtools)
# install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)
library(extrafont)
library(showtext)

#Install Chosen Fonts From Google Fonts

font_add_google("Roboto Condensed", family = "Roboto Condensed")
showtext_auto()

#read data

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')


#Seperate cocoa pecentage

choc_df <- chocolate %>%
  group_by(company_manufacturer)%>%
  mutate(cocoa_percent = as.numeric(substr(cocoa_percent,1,2)))%>%
  mutate(cocoa_percent = mean(cocoa_percent))%>%
  summarise(company_manufacturer, Country = company_location, cocoa_percent, cocoa = "Cocoa", rating = mean(rating))%>%
  unique()

# seperate other ingredients percentage
choc_df2 <- chocolate %>%
  group_by(company_manufacturer)%>%
  mutate(cocoa_percent = as.numeric(substr(cocoa_percent,1,2)))%>%
  mutate(cocoa_percent = 100-mean(cocoa_percent))%>%
  summarise(company_manufacturer, Country = company_location, cocoa_percent, cocoa = "Other", rating = mean(rating))%>%
  unique()

#combine Cocoa and other ingredients

choc_df_final <- rbind(choc_df,choc_df2)

#sort countries by average chocolate rating
choc_df_country <- choc_df_final%>%
  group_by(Country, cocoa)%>%
  mutate(cocoa_percent = mean(cocoa_percent))%>%
  summarise(Country, cocoa_percent, cocoa, rating = mean(rating))%>%
  unique()%>%
  arrange(desc(rating))

#select top 10 countries

choc_df_country <- choc_df_country%>%
  ungroup()%>%
  slice_head(n = 20)%>%
  arrange(cocoa_percent)

#Produce Graph

ggplot(choc_df_country, aes(fill=cocoa, values=cocoa_percent)) +
  geom_waffle(color = "white", size=1, n_rows = 5, flip = TRUE, make_proportional = TRUE)+
  facet_wrap(~Country,strip.position = "bottom", ncol = 5)+
  scale_fill_manual(values = c("#623412","#482683"))+
  theme_void()+
  labs(fill = "Ingredients",title = "Best Rated Countries in the World for Chocolate\n",caption = "\nWeek 3 - Tidy Tuesday 2022\naklongmuir.com|@alyssastweeting|twitch:aklongmuir")+
  theme(text = element_text(family="Roboto Condensed"), plot.title = element_text(hjust =0.5), plot.caption = element_text(hjust = 0.5), plot.margin = margin(1, 1, 1, 1,"cm"), legend.position = "bottom")
