# Analiza rynku gier

Co  chcę osiągnąć dzięki analizie pliku csv,który zawiera dane na temat gier.

1.Co łączy gry,które sprzedały się w największej ilości.



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
![Zrzut ekranu (22)](https://user-images.githubusercontent.com/56741227/74109473-0501ed00-4b84-11ea-8d08-cb5f1495ea44.png)

Widzimy,że składa on się z danych typu character oraz numeric,co nie jest optymlane dla nas,gdyż w kolumnach Genre,Rating,Platform oraz Publisher dane możemy zmienić na factory.Teraz sprawdzimy jak wygląda sytuacja z pustymi danymi.

![Zrzut ekranu (24)](https://user-images.githubusercontent.com/56741227/74109536-91acab00-4b84-11ea-9079-70c65065a0f1.png)




Oczyszczenie oraz transformacja danych

```{r}
data=data %>%
  drop_na()
```



```{r}
data=data %>%
  mutate_if(is.character,as.factor)
data$Name=as.character(data$Name)
data$Year_of_Release=format(as.Date(data$Year_of_Release,"%Y"),"%Y")
data$User_Score=as.numeric(data$User_Score)

str(data)

```
Struktura data frame'u po oczyszczeniu i transformacji danych

![Zrzut ekranu (23)](https://user-images.githubusercontent.com/56741227/74109579-e8b28000-4b84-11ea-9d55-ee3e574ea83b.png)

Użyję funckji summary() by pobieżnie,zobaczyć rozkład danych.
```{r}
summary(data)
```
![Zrzut ekranu (25)](https://user-images.githubusercontent.com/56741227/74109591-fd8f1380-4b84-11ea-95ac-20de7b3d4ae1.png)

Na tym zdjęciu widzimy jak ciężko osiągnąć sukces w tej branży(Kolumny z Sales).

Breaks_rating oraz labels_rating będą mi potrzebne,aby  wykres,który znajduje się poniżej był lepiej czytelny.

```{r}
breaks_rating=c("RP","EC","E","K-A","E10","T","M","AO")
labels_rating=c("Ograniczenie oczekujące","Wczesne dzieciństwo","Każdy","Każdy","Każdy 10+","Nastolatkowie",
                "Dojrzali","Tylko dorośli")
```

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
Wykres pokazuje jak poszczególne ograniczenia wiekowe korelowały ze światową sprzedażą.Chcemy osiągnąć sukces,więc wziąłem pod uwagę tylko 5% najlepszych gier.Najlepiej sprzedają się gry z kategorią dla każdego oraz dla dorosłych.
![wykres1](https://user-images.githubusercontent.com/56741227/74109622-4a72ea00-4b85-11ea-8ea0-c64168610b95.png)

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
Wykres pokazuje jak korelowała światowa sprzedaż z rodzajem platformy na jaką gra została wydana.Widzimy,że Wii i Xbox 360 wypadły tutaj najlepiej.
![wykres2](https://user-images.githubusercontent.com/56741227/74109639-64acc800-4b85-11ea-8918-9030f19693d0.png)

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
Wykres pokazuje jak korelowała światowa sprzedaż z rodzajem gry.Gry sportowe oraz strzelanki były najbardziej popularne.
![000003](https://user-images.githubusercontent.com/56741227/74109656-7a21f200-4b85-11ea-8590-ba4ebd485b7b.png)


Z tej pobieżnej analizy można wywnioskować,aby gra odniosła sukces musi być grą akcji dla każdego wydaną na Nintendo Wii.
