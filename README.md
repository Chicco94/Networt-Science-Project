---
title: "Presentazione"
author: "Enrico Cominato"
date: "11 febbraio 2019"
output: 
  html_notebook
runtime: shiny
---

<style>
  .col2 {
    columns: 2 200px;         /* number of columns and width in pixels*/
    -webkit-columns: 2 200px; /* chrome, safari */
    -moz-columns: 2 200px;    /* firefox */
  }
  .col3 {
    columns: 3 100px;
    -webkit-columns: 3 100px;
    -moz-columns: 3 100px;
  }
  .shiny-input-checkboxgroup.shiny-input-container-inline label ~ .shiny-options-group, .shiny-input-radiogroup.shiny-input-container-inline label ~ .shiny-options-group {
    margin-top: -1px;
    margin-left: auto;
    margin-right: auto;
    width: 800px !important;
  }
  .irs{
    width: 800px !important;
  }
</style>

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)

# dataset delle partite fatte
dataset_games <- read_csv("dataset/games.csv")
games <- dataset_games

# dataset delle giocate fatte in ogni partita
#dataset_plays <- read_csv("dataset/plays.csv")
#plays <- dataset_plays

# preferisco:
# 1: spaccare data e ora
# 2: mettere il risultato della partita come un booleano
games <- games %>%
  mutate(period = as.Date(added,format = "%Y-%m", tz = "UTc") ) %>%
  mutate(result = if_else(result=="win", TRUE, FALSE)) %>%
  select(everything() , -added)

start <- (games %>%
  select(period) %>%
  group_by(period) %>%
  arrange(period) %>%
  head(1) )[[1]]

end <- (games %>%
  select(period) %>%
  group_by(period) %>%
  arrange(desc(period)) %>%
  head(1))[[1]]

classi <- c("Druid", 
            "Hunter", 
            "Mage",
            "Paladin",
            "Priest",
            "Rogue",
            "Shaman",
            "Warlock",
            "Warrior")

colori <- c("Druid"="brown", 
            "Hunter"="green", 
            "Mage"="aquamarine",
            "Paladin"="yellow",
            "Priest"="white",
            "Rogue"="black",
            "Shaman"="blue",
            "Warlock"="blueviolet",
            "Warrior"="red")

icone <- c(
  "https://hsreplay.net/static/images/class-icons/druid.png",
  "https://hsreplay.net/static/images/class-icons/hunter.png",
  "https://hsreplay.net/static/images/class-icons/mage.png",
  "https://hsreplay.net/static/images/class-icons/paladin.png",
  "https://hsreplay.net/static/images/class-icons/priest.png",
  "https://hsreplay.net/static/images/class-icons/rogue.png",
  "https://hsreplay.net/static/images/class-icons/shaman.png",
  "https://hsreplay.net/static/images/class-icons/warlock.png",
  "https://hsreplay.net/static/images/class-icons/warrior.png"
)
```

## Introduction

In this presentation we will analize the card game Hearthsone.
- We will see how the "meta" has changed through the years, which decks have been played and we will try to figure out why they haev been played.
- After that, there will be an analysis of each single card used in the game not only the frequency of usage during time but also we will try to figure out how a single card can have an impact on the game.
- At the end there will be "mithbuster time" were we try to find if tips and advise from pro player are really useful or just mith.

All the data are available [here](http://www.hearthscry.com/). 

## First analysis
Let's prepare the data. We have already the two dataset loaded (one in games_dataset)

```{r class winrate}
# class winrate
class_hero <- games %>%
  group_by(hero, period) %>%
  summarize(wins = sum(result), games = n()) %>%
  rename(class = hero) %>%
  ungroup()

class_oppo <- games %>%
  group_by(opponent, period) %>%
  summarize(wins = sum(result), games = n()) %>%
  rename(class = opponent) %>%
  ungroup()

class_total <- class_hero %>%
  left_join(class_oppo, by = c("class", "period")) %>%
  group_by(class, period) %>%
  summarize(wins = wins.x+wins.y, games = games.x+games.y, winrate = wins/games)

class_total$class <- as.factor(class_total$class)
# vediamo i dati
class_total
```

## Guardiamo un po' il winrate di ogni classe {#title .emphasized }


    
### Seleziona o più classi
    
```{r}
inputPanel(
  checkboxGroupInput("class_selector", "Classi",
    choiceNames = mapply(classi, icone, FUN = function(classe, iconeUrl) {
      tagList(
        tags$img(src=iconeUrl, width=50, height=50),
        ""
      )
    }, SIMPLIFY = FALSE, USE.NAMES = FALSE),
    choiceValues = classi,
    inline = TRUE
  ),
  textOutput("txt")
)
```
   
### Seleziona un arco di tempo

```{r}
inputPanel(
#Add a Slider Input to select date range
  sliderInput(inputId = "Date_range_selector",
             label ="Select Date Range",
             min = start,
             max = end,
             value = c(start,end),
             step = 30,
             timeFormat="%Y-%m")
)
```   
 
### Analizziamo il winrate delle classi selezionate
    
```{r}
filter_by_date <- function(ds,range ){
return( ds %>%
          filter(
            between(as.Date(period,"%Y-%M"),
                    as.Date(range[1],"%Y-%M"),
                    as.Date(range[2],"%Y-%M")
                    ) 
          )
      )
}
filter_by_class <- function(ds,selector){
  # se non ho selezionato una classe le ritorno tutte
  if (is.null(selector) ){ return(ds) }
  # seleziono solo le classi scelte
  return( ds %>%
    filter( class %in% selector )
  )
}

renderPlot({
  # seleziono solo i dati da plottare
  # if ( input$class_selector_druid)
  #   druid_winrate <- 
  #     class_total %>% 
  #     filter(class == "Druid") %>%
  #     filter_by_date(input$Date_range_selector)%>%
  #   ggplot(aes(x = period, y = winrate)) + geom_point(color="Brown")
  #   
  # if ( input$class_selector_mage){
  #   mage_winrate <- 
  #     class_total %>% 
  #     filter(class = "Mage") %>%
  #     filter_by_date(input$Date_range_selector)%>%
  #   ggplot(aes(x = period, y = winrate)) + geom_point(color="Blue")
  class_total %>%
    filter_by_date(input$Date_range_selector)%>%
      filter_by_class(input$class_selector)%>%
        ggplot(aes(x = period, y = winrate)) + 
          geom_smooth(aes(color = factor(class)), se = FALSE) +
            scale_color_manual( values = colori)
})
```



