library(tidyverse)
library(lubridate)
library(ggmap)
library(ggrepel)
library(gridExtra)
library(pander)


###########################
#######Load Data###########
###########################

cities <- read.table("C:/Users/Mrinal/Desktop/data_visualisation/minard/cities.txt",
                     header = TRUE, stringsAsFactors = FALSE)

troops <- read.table("C:/Users/Mrinal/Desktop/data_visualisation/minard/troops.txt",
                     header = TRUE, stringsAsFactors = FALSE)

temps <- read.table("C:/Users/Mrinal/Desktop/data_visualisation/minard/temps.txt",
                    header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(date = dmy(date))  


troops %>% head() %>% pandoc.table(style = "rmarkdown")


ggplot() +
  geom_path(data = troops, aes(x = long, y = lat, group = group, 
                               color = direction, size = survivors),
            lineend = "round") +
  geom_point(data = cities, aes(x = long, y = lat),
             color = "#DC5B44") +
  geom_text_repel(data = cities, aes(x = long, y = lat, label = city),
                  color = "#DC5B44", family = "Open Sans Condensed Bold") +
  scale_size(range = c(0.5, 15)) + 
  scale_colour_manual(values = c("#DFC17E", "#252523")) +
  labs(x = NULL, y = NULL) + 
  guides(color = FALSE, size = FALSE)



march.1812.europe <- c(left = -13.10, bottom = 35.75, right = 41.04, top = 61.86)

# "zoom" ranges from 3 (continent) to 21 (building)
# "where" is a path to a folder where the downloaded tiles are cached
march.1812.europe.map <- get_stamenmap(bbox = march.1812.europe, zoom = 5,
                                       maptype = "terrain", where = "cache")


ggmap(march.1812.europe.map)


march.1812.europe.map.wc <- get_stamenmap(bbox = march.1812.europe, zoom = 5,
                                          maptype = "watercolor", where = "cache")
ggmap(march.1812.europe.map.wc)


ggmap(march.1812.europe.map.wc) +
  geom_path(data = troops, aes(x = long, y = lat, group = group, 
                               color = direction, size = survivors),
            lineend = "round") +
  scale_size(range = c(0.5, 5)) + 
  scale_colour_manual(values = c("#DFC17E", "#252523")) +
  guides(color = FALSE, size = FALSE)+
  theme_nothing()  


march.1812.ne.europe <- c(left = 23.5, bottom = 53.4, right = 38.1, top = 56.3)

march.1812.ne.europe.map <- get_stamenmap(bbox = march.1812.ne.europe, zoom = 8,
                                          maptype = "terrain-background", where = "cache")

march.1812.plot <- ggmap(march.1812.ne.europe.map) +
  geom_path(data = troops, aes(x = long, y = lat, group = group, 
                               color = direction, size = survivors),
            lineend = "round") +
  geom_point(data = cities, aes(x = long, y = lat),
             color = "#DC5B44") +
  geom_text_repel(data = cities, aes(x = long, y = lat, label = city),
                  color = "#DC5B44", family = "Open Sans Condensed Bold") +
  scale_size(range = c(0.5, 10)) + 
  scale_colour_manual(values = c("#DFC17E", "#252523")) +
  guides(color = FALSE, size = FALSE) +
  theme_nothing()


march.1812.ne.europe <- c(left = 23.5, bottom = 53.4, right = 38.1, top = 56.3)

march.1812.ne.europe.map <- get_stamenmap(bbox = march.1812.ne.europe, zoom = 8,
                                          maptype = "terrain-background", where = "cache")

march.1812.plot <- ggmap(march.1812.ne.europe.map) +
  geom_path(data = troops, aes(x = long, y = lat, group = group, 
                               color = direction, size = survivors),
            lineend = "round") +
  geom_point(data = cities, aes(x = long, y = lat),
             color = "#DC5B44") +
  geom_text_repel(data = cities, aes(x = long, y = lat, label = city),
                  color = "#DC5B44", family = "Open Sans Condensed Bold") +
  scale_size(range = c(0.5, 5)) + 
  scale_colour_manual(values = c("#DFC17E", "#252523")) +
  guides(color = FALSE, size = FALSE) +
  theme_nothing()


march.1812.plot


ggplot(data = temps, aes(x = long, y = temp)) +
  geom_line() +
  geom_text(aes(label = temp), vjust = 1.5)


#ggplot_build(march.1812.plot)$layout$panel_ranges[[1]]$x.range


temps.nice <- temps %>%
  mutate(nice.label = paste0(temp, "°, ", month, ". ", day))

temps.1812.plot <- ggplot(data = temps.nice, aes(x = long, y = temp)) +
  geom_line() +
  geom_label(aes(label = nice.label),
             family = "Open Sans Condensed Bold", size = 2.5) + 
  labs(x = NULL, y = "° Celsius") +
  scale_x_continuous(limits = ggplot_build(march.1812.plot)$layout$panel_ranges[[1]]$x.range) +
  scale_y_continuous(position = "right") +
  coord_cartesian(ylim = c(-35, 5)) +  # Add some space above/below
  theme_bw(base_family = "Open Sans Condensed Light") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks = element_blank(),
        panel.border = element_blank())


temps.1812.plot


both.1812.plot <- rbind(ggplotGrob(march.1812.plot),
                        ggplotGrob(temps.1812.plot))


grid::grid.newpage()
grid::grid.draw(both.1812.plot)

panels <- both.1812.plot$layout$t[grep("panel", both.1812.plot$layout$name)]

map.panel.height <- both.1812.plot$heights[panels][1]

map.panel.height

both.1812.plot$heights[panels] <- unit(c(map.panel.height, 0.1), "null")
grid::grid.newpage()
grid::grid.draw(both.1812.plot)