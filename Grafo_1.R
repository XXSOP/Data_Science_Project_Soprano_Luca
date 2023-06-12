library(dplyr)
library(stringr)
library(ggplot2)
library(ggmap)
library(tidyr)

#Dataset originali di partenza
map.world = map_data("world")
originalDataset = read.csv("mission_launches.csv")

#Serie di mutate per estrarre il nome della nazione 
df = originalDataset %>%
  mutate(Country =str_split_i(originalDataset$Location, ',',-1))%>%
  mutate(Country = str_trim(Country))

#Conta il numero di volte che un paese compare nel dataset e li ordina in modo decrescente
df_country = df %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  arrange(-n)


#left join per ottenere la longitudine e latitudine nella libreria map_data("world") per il geom_polygon
map.world_joined = left_join( map.world, df_country,  by = c( 'region' = 'Country'))
map.world_joined = map.world_joined %>%
  mutate( n = replace_na(n, 0)) #rimpiazza il valore na con uno 0, usa il valoe n se è diverso da 0#

#Cambio il tipo dei dati nella colonna n creata col mutate
map.world_joined$n = as.numeric(map.world_joined$n)


#Plotta il dataset map.world_joined creando una cartina geografica usando longitudine e latitudine 
# colorando i paesi in colori diversi in base a quante volte compaiono nel dataset (ovvero hanno eseguito diversi lanci missilistici)
ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = n)) +
  scale_fill_gradient(low = "#abdbe3", high = "#154c79") +
  labs(title = "Lanci missilistici nel mondo", y="Latitudine", x="Longitudine") +
  theme(text = element_text(color = "black",)
    ,panel.background = element_rect(fill ="#FaFFFF")
    ,plot.background = element_rect(fill = "white")
    ,panel.grid = element_blank()
    ,plot.title = element_text(size = 20)
    #,axis.text = element_blank()
    #,axis.ticks = element_blank()
    ,axis.title.x = element_text(size = 10)
    ,axis.title.y = element_text(size = 10)
    ,legend.position = "none"
  )
