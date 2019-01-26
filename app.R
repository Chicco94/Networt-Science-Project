library(shiny)
library(jsonlite)
library(dplyr)
library(ggplot2)


dataset <- jsonlite::fromJSON("good_dataset/2018-11.json")

# preferisco spaccare data e ora
dataset <- dataset %>%
  mutate(month = format(as.Date(added),"%m"), year = format(as.Date(added),"%Y")) %>%
  select(everything() ,- added)
note <- select(dataset, note != NULL)

dataset <- dataset %>%
  mutate(match_id = row_number()) %>%
  select(id, everything())
# class winrate
class_hero <- dataset %>%
  group_by(hero, year, month) %>%
  transmute(result = if_else(result=="win", TRUE, FALSE)) %>%
  summarize(wins = sum(result), games = n()) %>%
  rename(class = hero) %>%
  ungroup()

class_oppo <- dataset %>%
  group_by(opponent, year, month) %>%
  transmute(result = if_else(result=="win", TRUE, FALSE)) %>%
  summarize(wins = sum(result), games = n()) %>%
  rename(class = opponent) %>%
  ungroup()

class_total <- class_hero %>%
  left_join(class_oppo, by = c("class", "year", "month")) %>%
  group_by(class, year, month) %>%
  summarize(wins = wins.x+wins.y, games = games.x+games.y, winrate = wins/games) %>%
  ungroup() %>%
  mutate(id = row_number())

class_total$class <- as.factor(class_total$class)
x_range <- range(class_total$month)
y_range <- range(class_total$winrate)
class_total%>% group_by(class)
plot(x_range,y_range, color=class)


# generic winrate
match_hero <- dataset %>%
  group_by(hero, hero_deck, year, month) %>%
  transmute(result = if_else(result=="win", TRUE, FALSE)) %>%
  summarize(wins = sum(result), games = n()) %>%
  rename(class = hero) %>%
  rename(archetype = hero_deck) %>%
  ungroup()

match_oppo <- dataset %>%
  group_by(opponent, opponent_deck, year, month) %>%
  transmute(result = if_else(result=="win", TRUE, FALSE)) %>%
  summarize(wins = sum(result), games = n()) %>%
  rename(class = opponent) %>%
  rename(archetype = opponent_deck) %>%
  ungroup()

match_total <- match_hero %>%
  left_join(match_oppo, by = c("class","archetype", "year", "month")) %>%
  group_by(class, archetype, year, month) %>%
  summarize(wins = wins.x+wins.y, games = games.x+games.y, winrate = wins/games) %>%
  ungroup() %>%
  mutate(id = row_number())

match_total$archetype <- as.factor(match_total$archetype)
match_total$class     <- as.factor(match_total$class)
match_total%>%
  ggplot(aes(x = month, y = winrate, color=class))+
  geom_point()
# da tenere traccia :
# win contro ogni classe: chi vince contro chi?

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

