---
title: "An Hearthstone decks & cards analysis"
date: 2019-02-11
output: html_document
---
  
library(shiny)
library(tidyverse)

games <- read_csv("games.csv")
plays <- read_csv("plays.csv")


dataset <- games
# preferisco spaccare data e ora
dataset <- dataset %>%
  mutate(period = format(as.Date(added),"%Y-%m")) %>%
  select(everything() ,- added)

# class winrate
class_hero <- dataset %>%
  group_by(hero, period) %>%
  transmute(result = if_else(result=="win", TRUE, FALSE)) %>%
  summarize(wins = sum(result), games = n()) %>%
  rename(class = hero) %>%
  ungroup()

class_oppo <- dataset %>%
  group_by(opponent, period) %>%
  transmute(result = if_else(result=="win", TRUE, FALSE)) %>%
  summarize(wins = sum(result), games = n()) %>%
  rename(class = opponent) %>%
  ungroup()

class_total <- class_hero %>%
  left_join(class_oppo, by = c("class", "period")) %>%
  group_by(class, period) %>%
  summarize(wins = wins.x+wins.y, games = games.x+games.y, winrate = wins/games) %>%
  ungroup() %>%
  mutate(id = row_number()) %>%
  arrange(-winrate)

class_total$class <- as.factor(class_total$class)
ggplot(data = class_total, mapping = aes(x = period, y = winrate, color=class)) + 
  geom_point()+
  geom_smooth()


# archetype winrate
match_hero <- dataset %>%
  group_by(hero, hero_deck, period) %>%
  transmute(result = if_else(result=="win", TRUE, FALSE)) %>%
  summarize(wins = sum(result), games = n()) %>%
  rename(class = hero) %>%
  rename(archetype = hero_deck) %>%
  ungroup()

match_oppo <- dataset %>%
  group_by(opponent, opponent_deck, period) %>%
  transmute(result = if_else(result=="win", TRUE, FALSE)) %>%
  summarize(wins = sum(result), games = n()) %>%
  rename(class = opponent) %>%
  rename(archetype = opponent_deck) %>%
  ungroup()

match_total <- match_hero %>%
  left_join(match_oppo, by = c("class","archetype", "period")) %>%
  group_by(class, archetype, period) %>%
  summarize(wins = wins.x+wins.y, games = games.x+games.y, winrate = wins/games) %>%
  ungroup() %>%
  mutate(id = row_number()) %>%
  filter(games>500, winrate>0.6)

match_total$archetype <- as.factor(match_total$archetype)
match_total$class     <- as.factor(match_total$class)
match_total%>%
  ggplot(aes(x = period, y = winrate, color=class))+
  geom_point()
# da tenere traccia :
# win contro ogni classe: chi vince contro chi?



# quali giocate sono state fatte "in curva"? giocare in curva da un vantaggio?
on_curve <- plays %>%
  filter(turn==mana)%>%
  inner_join(dataset, by = c("game_id"="id"))

on_curve_me <- on_curve %>%
  filter(player=="me") %>%
  count(result)

on_curve_oppo <- on_curve %>%
  filter(player=="opponent") %>%
  count(result)


# giocare fuori curva penalizza davvero?
out_curve <- plays %>%
  filter(turn!=mana)%>%
  inner_join(dataset, by = c("game_id"="id"))

out_curve_me <- out_curve %>%
  filter(player=="me") %>%
  count(result)

out_curve_oppo <- out_curve %>%
  filter(player=="opponent") %>%
  count(result)

# TODO: analizzare correlazione tra giocata in curva e vittoria


# quali sono le carte più vincenti (tolgo HP e coin) e hp potenziato AT...
  winning_cards <- plays %>%
    filter(name != "Fireblast",      name != "Shapeshift",  name != "Totemic Call", 
           name != "Lesser Heal",    name != "Life Tap",    name != "Armor Up!", 
           name != "Dagger Mastery", name != "Steady Shot", name != "Reinforce", 
           name != "The Coin") %>%
  inner_join(dataset %>%
    filter(period>='2018-06')
    , by = c("game_id"="id"))%>%
  select(card_id,name,result)%>%
  group_by(card_id, name)%>%
  transmute(result = if_else(result=="win", TRUE, FALSE))%>%
  summarize(played = n(), winrate = sum(result)/played) %>%
  arrange(-played,-winrate)




# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

