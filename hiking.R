hike <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

library(tidyverse)
library(ghibli)
library(extrafont)

hike_clean <- hike %>% 
  mutate(location = as.factor(word(location, 1, sep = " -- "))) %>% 
  mutate(length = as.numeric(sapply(str_split(length, " "), "[[", 1))) %>%
  unnest(features) %>% 
  group_by(location) %>% 
  summarise(total_miles = sum(length), avg_rating = mean(as.numeric(rating)))

ggplot(hike_clean, aes(x = fct_reorder(location, total_miles), y = total_miles, fill = location)) +
  geom_col() +
  coord_flip() +
  labs(x = "Region", y = "Total Miles", title = "Trail locations", subtitle = "Lengthy hikes for the adventurous",
       caption = "#TidyTuesday Week 48 | Ente Kang") +
  scale_fill_viridis(option = "C", discrete = TRUE) +
  theme(
    legend.position = "none", 
    panel.background = element_blank(), 
    plot.background = element_rect(fill = "#E3D1C3FF"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = "#1D271CFF", family = "Andale Mono", size = 14), 
    axis.title = element_text(colour = "#1D271CFF", family = "Andale Mono", size = 16), 
    axis.title.y = element_text(angle = 0, vjust = 0.5), 
    axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 26, hjust = 0.5, family = "Andale Mono", colour = "#3D4F7DFF", margin = margin(10, 0, 10, 0)), 
    plot.caption = element_text(size = 16, colour = "#1D271CFF", family = "Andale Mono", margin = margin(20, 0, 0, 0)),
    plot.subtitle = element_text(size = 16, hjust = 0.5, family = "Andale Mono", colour = "#CD4F38FF", margin = margin(10, 0, 30, 0)), 
    plot.margin = unit(c(1,3,1,1), "cm")
  )

ggsave("hiking.png", width = 300, height = 200, units = "mm")
