library(dplyr)
library(stringr)
library(ggplot2)
library(ggmap)
library(tidyr)

#Dataset di partenza
map.world = map_data("world")
originalDataset = read.csv("mission_launches.csv")

df = originalDataset %>%
  mutate(Country =str_split_i(originalDataset$Location, ',',-1))%>%
  mutate(Country = str_trim(Country))


df_country = df %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  arrange(-n)


#left join to obtain the longitude and latitude in the library map_data("world") for the geom_polygon
map.world_joined = left_join( map.world, df_country,  by = c( 'region' = 'Country'))
map.world_joined = map.world_joined %>%
  mutate( n = replace_na(n, 0)) #replace the na value with a 0, the value n if is different from 0


map.world_joined$n = as.numeric(map.world_joined$n)


ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = n)) +
  scale_fill_gradient(low = "#abdbe3", high = "#154c79") +
  labs(title = "Lanci missilistici nel mondo", y="Latitudine", x="Longitudine") +
  theme(text = element_text(family = "Times New Roman", color = "black",)
    ,panel.background = element_rect(fill ="#FaFFFF")
    ,plot.background = element_rect(fill = "white")
    ,panel.grid = element_blank()
    ,plot.title = element_text(size = 30)
    ,plot.subtitle = element_text(size = 10)
    ,axis.text = element_blank()
    ,axis.ticks = element_blank()
    ,axis.title.x = element_text(size = 20)
    ,axis.title.y = element_text(size = 20)
    ,legend.position = "none"
  )