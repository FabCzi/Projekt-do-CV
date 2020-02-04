# Analiza rynku gier

Co  chcę osiągnąć dzięki analizie pliku csv,który zawiera dane na temat gier.

1.Co łączy gry,które sprzedały się w największej ilości.

2.Sprawdzić czy oceny użytkowników oraz krytyków są skorelowane.

## 1 

 Na początku instalujemy biblioteki,które będą nam potrzebne do analizy danych
 
```
library(tidyverse)
library(lubridate)

```
Wczytujemy plik CSV

```{r}
data=read_csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
```
Sprawdzamy strukturę data frame'u.
```{r}
str(data)

```
![screen1](https://user-images.githubusercontent.com/56741227/73788531-77965580-479d-11ea-897c-5d930e814483.png)

Widzimy,że składa on się z danych typu character oraz numeric,co nie jest optymlane dla nas,gdyż w kolumnach Genre,Rating,Platform oraz Publisher dane możemy zmienić na factory.

Tak prezentuje się struktura data frame'u po transformacji danych.



```{r}
data=data %>%
  mutate_if(is.character,as.factor)
data$Name=as.character(data$Name)
data$Year_of_Release=format(as.Date(data$Year_of_Release,"%Y"),"%Y")
data$User_Score=as.numeric(data$User_Score)

str(data)

```
![screen2](https://user-images.githubusercontent.com/56741227/73788839-0a36f480-479e-11ea-8726-3f078c9ac994.png)

Użyję funckji summary() by pobieżnie,zobaczyć rozkład danych.
```{r}
summary(data)
```
![screen3](https://user-images.githubusercontent.com/56741227/73788875-191da700-479e-11ea-9fcf-e631485c2812.png)

Breaks_rating oraz labels_rating będą mi potrzebne, wykres,który znajduje się poniżej był lepiej czytelny.

```{r}
breaks_rating=c("RP","EC","E","K-A","E10","T","M","AO")
labels_rating=c("Ograniczenie oczekujące","Wczesne dzieciństwo","Każdy","Każdy","Każdy 10+","Nastolatkowie",
                "Dojrzali","Tylko dorośli")
```
Wykres pokazuje jak poszczególne ograniczenia wiekowe korelowały ze światową sprzedażą
```{r}
data %>%
  filter(!is.na(Rating),data$Global_Sales>quantile(data$Global_Sales,.95)) %>%
  group_by(Rating) %>%
   summarise(mean=mean(Global_Sales)) %>%
  ggplot(aes(x=reorder(Rating,mean),y=mean,fill=Rating))+
  geom_bar(stat="identity")+
  scale_fill_discrete(name="Wyjaśnienia",
                         breaks=breaks_rating,
                      labels=labels_rating)+
  labs(title="Średnia sprzedaż na ograniczenie wiekowe na świecie")+
  ylab("sprzedaż w milionach")+
  xlab("Ograniczenie wiekowe")
```
![000003](https://user-images.githubusercontent.com/56741227/73789020-55510780-479e-11ea-8ec2-1f17c4c38f98.png)

```{r}

data %>%
  filter(!is.na(Platform),data$Global_Sales>quantile(data$Global_Sales,.95)) %>%
  group_by(Platform) %>%
   summarise(mean=mean(Global_Sales),count=n()) %>%
    filter(count>quantile(count,0.75)) %>%
  ggplot(aes(x=reorder(Platform,mean),y=mean,fill=Platform))+
  geom_bar(stat="identity")+
  labs(title="Średnia światowa sprzedaż ze względu na platformę")+
  ylab("Sprzedaż w milionach")+
  xlab("Platforma")
```

```{r}
data %>%
  filter(!is.na(Genre),data$Global_Sales>quantile(data$Global_Sales,.95)) %>%
  group_by(Genre) %>%
   summarise(mean=mean(Global_Sales),count=n()) %>%
    filter(count>quantile(count,0.75)) %>%
  ggplot(aes(x=reorder(Genre,mean),y=mean,fill=Genre))+
  geom_bar(stat="identity")+
  labs(title="Średnia światowa sprzedaż ze względu na typ gry")+
    ylab("Sprzedaż w milionach")+
    xlab("Typ gry")
```
