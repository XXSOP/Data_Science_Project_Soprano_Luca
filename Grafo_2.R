library(dplyr)
library(stringr)
library(ggplot2)

#Dataset di partenza
originalDataset = read.csv("mission_launches.csv")

#Tramite una serie di mutate estrae l'anno dalla colonna Date
df1=originalDataset %>%
  mutate(Year =str_split_i(Date, ',',-1))%>%
  mutate(Year= str_trim(Year))%>%
  mutate(Year =str_split_i(Year, ' ',1))

#Raggruppo Mission_Status e Year e li conto così da avere per ogni anno quanto stati di missione di missione ci sono stati
df2= df1%>%
  group_by(Mission_Status, Year)%>%
  summarise(n = n())

#Plotta il subset df2 mappando sulle x gli anni estratti nelle righe dalla 9-12 e sulle y mappa 
# il conto dei vari stati delle missioni
ggplot(df2, mapping = aes(x= Year , y= n))+
  geom_bar(stat = "identity", aes(fill = Mission_Status), width = 0.8, position = position_stack())+ #Colora le varie barre del barplot diversamente in base allo stato delle missioni
  labs(title = "Numero di stati che le missioni hanno assunto",
    y="Numero di occorrenze", x="Anni analizzati (1957-2020)")+
  theme(text = element_text(color = "black") #tra le parentesi del theme() ci sono degli elementi che rendono il grafico più bello e leggibile
    ,panel.background = element_rect(fill ="#FaFFFF")
    ,plot.background = element_rect(fill = "white")
    ,panel.grid = element_blank()
    ,plot.title = element_text(size = 20)
    ,axis.title.x = element_text(size = 10)
    ,axis.title.y = element_text(size = 10)
    ,axis.text.x = element_blank()
  )

