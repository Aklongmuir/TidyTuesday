library(tidyverse)
library(MetBrewer)
library(extrafont)
library(showtext)

#Install Chosen Fonts From Google Fonts

font_add_google("Roboto Condensed", family = "Roboto Condensed")
showtext_auto()

#Read csv ( Data from the Australian Bureau of meteorology (http://www.bom.gov.au/climate/data/))

df <- read.csv("TempData.csv")

#Categorise data by Temperature Bracket

BlanketCat <- df %>%
  summarise(Date = ï..Date, Temperature)%>%
  mutate(Category =  as.factor(ifelse( Temperature > 35, 1, 
                                       ifelse(Temperature > 32, 2,
                                              ifelse(Temperature > 28, 3,
                                                     ifelse(Temperature > 25, 4, 
                                                            ifelse(Temperature > 21, 5,
                                                                   ifelse(Temperature > 15, 6, 7))))))))

#Make Digital Blanket

ggplot(BlanketCat, aes(x = Date))+
  geom_col(aes(fill = Category , y = 100), width = 1.1)+
  scale_fill_manual(values =(met.brewer("Moreau")))+
  labs(title = "2021 Digital Temperature Blanket", caption = "Week 1 - Tidy Tuesday 2022 W1\naklongmuir.com|@alyssastweeting|twitch:aklongmuir")+
  theme_void()+
  theme(text = element_text(family="Roboto Condensed"),legend.position = "none", plot.title = element_text(hjust =0.5), plot.caption = element_text(hjust = 0.5), plot.margin = margin(1, 1, 1, 1,"cm"))

#Save to Publish

ggsave(width = 3, height = 3, dpi = 300, filename = "TidyTuesday2021-W1.png")
