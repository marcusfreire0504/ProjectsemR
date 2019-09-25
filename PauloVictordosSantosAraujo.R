library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(rworldmap)
library(tidyr)
library(scales)

outbreaks <- read.csv('outbreaks.csv',stringsAsFactors = F, header = T)
class(outbreaks)
dim(outbreaks)
colnames(outbreaks)
str(outbreaks)
summary(subset(outbreaks, select = c(Year, Month, State, Location, Food, Ingredient, Species, Serotype.Genotype, Status, Illnesses, Hospitalizations, Fatalities)))

#Sim existe, Unspecified dentro da variável Food e tem os valores indefinidos("") que estão sem nenhuma representação.
print(colMeans(is.na(outbreaks) | outbreaks == "")*100) # Porcentagem de NA's e ("") em cada variável

outbreaks$confirmed <- ifelse(grepl('Confirmed', outbreaks$Status), TRUE, FALSE)

#Variavel usada foi Hospitalization
#Hospitalization por ano
contamined_year <- outbreaks %>% filter(confirmed== TRUE & !is.na(Hospitalizations)) %>% group_by(Year) %>% summarise(num_case=sum(Hospitalizations, na.rm = TRUE)) %>% arrange(desc(num_case))
ggplot(data = contamined_year, aes(x=reorder(Year, desc(num_case)), y=num_case, fill=num_case)) + geom_bar(stat = 'identity') + geom_text(aes(x=reorder(Year, desc(num_case)),y=num_case, label= num_case), nudge_y = 30) + labs(x= "Anos", y= "Numero de casos", title = " Numero de Hospitalizações por ano") + scale_fill_continuous(name="Numero de casos", labels=comma) + scale_y_continuous(labels = comma)

#Hospitalization por mês
contamined_month <- outbreaks %>% filter(confirmed== TRUE & !is.na(Hospitalizations)) %>% group_by(Month) %>% summarise(num_case=sum(Hospitalizations, na.rm = TRUE)) %>% arrange(desc(num_case))
ggplot(data = contamined_month, aes(x=reorder(Month, desc(num_case)), y=num_case, fill=num_case)) + geom_bar(stat = 'identity') + geom_text(aes(x=reorder(Month, desc(num_case)),y=num_case, label= num_case), nudge_y = 30) + labs(x= "Meses", y= "Numero de casos", title = " Numero de Hospitalizações por meses") + scale_fill_continuous(name="Numero de casos", labels=comma) + scale_y_continuous(labels = comma)

#Hospitalization por estado
contamined_state <- outbreaks %>% filter(confirmed== TRUE & !is.na(Hospitalizations)) %>% group_by(State) %>% summarise(num_case=sum(Hospitalizations, na.rm = TRUE)) %>% arrange(desc(num_case))
ggplot(data = contamined_state, aes(x=reorder(State, desc(num_case)), y=num_case, fill=num_case)) + geom_bar(stat = 'identity') + geom_text(aes(x=reorder(State, desc(num_case)),y=num_case, label= num_case), nudge_y = 30) + labs(x= "Estados", y= "Numero de casos", title = " Numero de Hospitalizações por estados") + scale_fill_continuous(name="Numero de casos", labels=comma) + scale_y_continuous(labels = comma)

#Hospitalization por localização
contamined_location <- outbreaks %>% filter(confirmed== TRUE & !is.na(Hospitalizations)) %>% group_by(Location) %>% summarise(num_case=sum(Hospitalizations, na.rm = TRUE)) %>% arrange(desc(num_case))
ggplot(data = contamined_location, aes(x=reorder(Location, desc(num_case)), y=num_case, fill=num_case)) + geom_bar(stat = 'identity') + geom_text(aes(x=reorder(Location, desc(num_case)),y=num_case, label= num_case), nudge_y = 30) + labs(x= "Lugar", y= "Numero de casos", title = " Numero de Hospitalizações por lugar") + scale_fill_continuous(name="Numero de casos", labels=comma) + scale_y_continuous(labels = comma)

#Hospitalization por espécie
contamined_species <- outbreaks %>% filter(confirmed== TRUE & !is.na(Hospitalizations)) %>% group_by(Species) %>% summarise(num_case=sum(Hospitalizations, na.rm = TRUE)) %>% arrange(desc(num_case))
ggplot(data = contamined_species, aes(x=reorder(Species, desc(num_case)), y=num_case, fill=num_case)) + geom_bar(stat = 'identity') + geom_text(aes(x=reorder(Species, desc(num_case)),y=num_case, label= num_case), nudge_y = 30) + labs(x= "Especies", y= "Numero de casos", title = " Numero de Hospitalizações por especies") + scale_fill_continuous(name="Numero de casos", labels=comma) + scale_y_continuous(labels = comma)

#Comparação de número de fatalidades em um determinado estado em cada mês, em um determinado ano

california_2003 <- outbreaks %>%
  filter(confirmed == T & !is.na(Fatalities)) %>%
  select(Year, Month, State, Fatalities) %>%
  filter(State == "California") %>%
  group_by(Month) %>% 
  summarise(num_cases = sum(Fatalities,na.rm = T)) %>%
  arrange(desc(num_cases))

ggplot(data=california_2003,
       aes(x=reorder(Month, desc(num_cases)),
           y=num_cases,
           fill=num_cases))+
  geom_bar(stat='identity') +
  geom_text(aes(x = reorder(Month, desc(num_cases)), 
                y = num_cases, label = num_cases), nudge_y = 10) + 
  labs(x = "Meses", 
       y = "Numero de casos", 
       title = "Numero de fatalidades confirmada na California em 2003") +
  scale_fill_continuous(name= "numero de casos", labels=comma) +
  scale_y_continuous(labels =  comma) 