library(tidyverse)
library(lubridate)
library(ghibli)
library(scales)
library(ggpubr)
library(extrafont)

shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

shelter_clean <- shelters %>% 
  mutate(year = year(ymd(occupancy_date)), month = month(ymd(occupancy_date), label = T, abbr = T), day = day(ymd(occupancy_date))) %>% 
  select(organization_name, sector, occupancy, capacity, year, month, day) %>% 
  filter(!is.na(capacity)) 


# overall trend 
trend <- shelter_clean %>% 
  group_by(organization_name, month) %>% 
  summarize(avg_occ = mean(occupancy))

# by sector (city wide)
sector_trend <- shelter_clean %>% 
  group_by(sector, month) %>% 
  summarize(avg_occ_sec = mean(occupancy))

# min max change each year 
change_year <- shelter_clean %>% 
  group_by(sector, year) %>% 
  summarize(min = min(occupancy), max = max(occupancy), total = sum(occupancy))

# look at by organization
change_year_org <- shelter_clean %>% 
  group_by(organization_name) %>% 
  summarize(min = min(occupancy), max = max(occupancy), total = sum(occupancy))

p <- ggplot(change_year_org)+
  geom_segment(aes(x = min, xend = max, y = fct_reorder(organization_name, max), yend = organization_name)) +
  # geom_point(aes(x = min, y = organization_name, color = organization_name, size = min)) +
  geom_point(aes(x = max, y = organization_name, color = organization_name, size = max)) +
  theme_minimal() +
  labs(x = "Range of Total Occupants", y = "Organizers", title = "Shelters in Toronto", subtitle = "Which shelters are serving the most \n homeless Torontonians") +
  theme(
    legend.position = "none", 
    panel.grid = element_blank(), 
    axis.text = element_text(colour = "#3D4F7DFF", family = "Andale Mono", size = 12), 
    axis.title = element_text(colour = "#1D271CFF", family = "Andale Mono", size = 12), 
    axis.title.y = element_text(angle = 0, vjust = 0.5), 
    axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
    plot.title = element_text(size = 32, hjust = 0.5, family = "Andale Mono", colour = "#3D4F7DFF", margin = margin(10, 0, 10, 0)), 
    plot.subtitle = element_text(size = 20, hjust = 0.5, family = "Andale Mono", colour = "#CD4F38FF", margin = margin(10, 0, 30, 0)), 
    plot.margin = unit(c(1,3,1,1), "cm")
  )

q <- ggplot(change_year, aes(x = year, y = total, fill = sector)) +
  geom_col() +
  facet_wrap(~sector) +
  scale_fill_viridis_d() +
  theme(
    panel.grid = element_blank(), 
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"), 
    strip.text = element_text(colour = "#3D4F7DFF", size = 12), 
    legend.position = "none", 
    axis.title = element_text(colour = "#1D271CFF", family = "Andale Mono", size = 12), 
    axis.title.y = element_text(angle = 0, vjust = 0.5), 
    axis.title.x = element_text(margin = margin(20, 0, 0, 0)), 
    axis.text = element_text(colour = "#3D4F7DFF", family = "Andale Mono", size = 12), 
    plot.caption = element_text(size = 16, colour = "#1D271CFF", family = "Andale Mono", margin = margin(20, 0, 0, 0)),
    plot.margin = unit(c(1,3,1,1), "cm")
  ) +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Total Occupants", caption = "#TidyTuesday Week 49 | Ente Kang")

ggarrange(p, q, ncol = 1, nrow = 2)

ggsave("toronto.png", width = 400, height = 400, units = "mm")
