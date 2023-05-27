library(dplyr)
library(stringr)
library(ggplot2)

originalDataset = read.csv("mission_launches.csv")

df1=originalDataset %>%
  mutate(Year =str_split_i(Date, ',',-1))%>%
  mutate(Year= str_trim(Year))%>%
  mutate(Year =str_split_i(Year, ' ',1))

df2= df1%>%
  group_by(Mission_Status, Year)%>%
  summarise(n = n())

ggplot(df2, mapping = aes(x= Year , y= n))+
  geom_bar(stat = "identity", aes(fill = Mission_Status), width = 0.8, position = position_stack())+
  labs(title = "Number of Missions Status during the following year",
    y="Numbers of occurrences",)+
  theme(text = element_text(color = "black")
    ,panel.background = element_rect(fill ="#FaFFFF")
    ,plot.background = element_rect(fill = "white")
    ,panel.grid = element_blank()
    ,plot.title = element_text(size = 20)
    ,axis.ticks = element_blank()
    ,axis.title.x = element_text(size = 20)
    ,axis.title.y = element_text(size = 20)
  )

