library(tidyverse)
library(treemap)
library(treemapify)
library(ghibli)
library(extrafont)
women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

cat_count <- women %>% 
  group_by(category) %>% 
  summarize(prop = n()/ nrow(women))
ggplot(cat_count, aes(x="", y = prop, fill = category))+
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#E8C4A2FF", "#E75B64FF", "#D8AF39FF", "#5A6F80FF", "#DE7862FF"))

ggplot(cat_count, aes(area = prop, fill = category, label = category))+
  geom_treemap() +
  geom_treemap_text(color = "white", place = "centre", grow = TRUE) +
  scale_fill_manual(values = c("#E8C4A2FF", "#E75B64FF", "#D8AF39FF", "#5A6F80FF", "#DE7862FF")) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 26, family = "Georgia", margin = margin(10, 0, 20, 0)), 
    plot.subtitle = element_text(size = 22, family = "Georgia", margin = margin(10, 0, 20, 0)),
    plot.caption = element_text(size = 16, family = "Georgia", margin = margin(15, 0, 10, 0)), 
    plot.margin = unit(c(1,1,1,1), "cm")
  ) +
  labs(title ="Most Influential Women of 2020", subtitle = "What traits really stood out?", caption = "#TidyTuesday week 50 | Ente Kang")
ggsave("women_2020.png", width = 200, height = 200, units = "mm")
