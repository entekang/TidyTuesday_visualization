library(tidyverse)
library(extrafont)
library(ghibli)
library(geofacet)
library(countrycode)

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')


mobile_clean <- mobile %>% 
  filter(continent == "Europe") %>% 
  filter(!is.na(mobile_subs)) 
eu_grid_new <- eu_grid1 %>% 
  filter(name != "Cyprus")

ggplot(mobile_clean) +
  geom_line(aes(x=year, y=mobile_subs, colour = "#B4DAE5FF")) +
  facet_geo(~entity, grid = eu_grid_new, scales = "free_y") +
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill = "#1D2645FF"),
    axis.text = element_text(colour = "white"), 
    panel.grid = element_blank(), 
    legend.position = "none",
    axis.title.x = element_text(size = 16, colour = "white", family = "Georgia"),
    axis.title.y = element_text(size = 16, colour = "white", family = "Georgia", angle = 0, vjust = 0.5),
    plot.title = element_text(size = 22, colour = "white", family = "Georgia", margin = margin(10, 0, 10, 0)),
    plot.caption = element_text(size = 12, colour = "white", family = "Georgia"),
    plot.subtitle = element_text(size = 10, colour = "white", family = "Georgia", margin = margin(10, 0, 20, 0)),
    strip.background = element_rect(fill = "#1D2645FF"), 
    strip.text = element_text(colour = "white"),
    plot.margin = unit(c(1,1,1,1), "cm")
  ) +
  labs(title = "Changes in landline subscriptions over the years", subtitle = "Was there increasing popularity?", caption = "#TidyTuesday Week 46| Ente Kang",
       x = "Year", y = "Subscriptions \n per 100 \n people")

ggsave("landlines.png", width = 300, height = 200, units = "mm")  
