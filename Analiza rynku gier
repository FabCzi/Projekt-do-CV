
```
library(tidyverse)
library(lubridate)

```

```{r}
data=read_csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
```

```{r}
data=data %>%
  mutate_if(is.character,as.factor)
data$Name=as.character(data$Name)
data$Year_of_Release=format(as.Date(data$Year_of_Release,"%Y"),"%Y")
data$User_Score=as.numeric(data$User_Score)
data$Publisher=as.factor(data$Publisher)

str(data)

```

```{r}
summary(data)
```

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
