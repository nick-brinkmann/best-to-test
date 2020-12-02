#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(gganimate)
library(dplyr)
library(lubridate)
library(rchess)
all_games <- readRDS("all_games.RDS")

#chss <- Chess$new()
#chss$move("a3")$move("b5")
#chss$fen()

#chsspgn$load_pgn(pgn)

ui <- navbarPage(
    theme = shinytheme("united"),
    "Analyzing My Online Chess Games",
    
    # HOME PANEL
    
    tabPanel("Home",
             fluidPage(
                 titlePanel("Plots"),
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("date",
                                     "Starting date: ",
                                     min = min(all_games$Date),
                                     max = max(all_games$Date),
                                     value = min(all_games$Date)
                                     ),
                         sliderInput("start_end", 
                                     label = h3("Start and End Date"), 
                                     min = min(all_games$Date), 
                                     max = max(all_games$Date), 
                                     value = c(min(all_games$Date), max(all_games$Date))
                                     )
                         ),
                     #mainPanel(chessboardjsOutput('board', width = 300)))
                 mainPanel(plotOutput("plot1"))
                 )
             )
    ),
    
    # VISUALIZATIONS PANEL
    tabPanel("Visualizations",
             fluidPage(
                 titlePanel("Pre-Made Visualizations"),
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons("radio_plot", label = h3("Select Plot"),
                                      choices = list("Elo Rating Over Time" = "smoother_elo_plot.gif", 
                                                     "Most and Least Successful Openings" = "openings.png"
                                                     #"Choice 3" = 3
                                                     ), 
                                      selected = "smoother_elo_plot.gif"),
                         
                         hr(),
                         fluidRow(column(3, verbatimTextOutput("value")))
                     ),
                     mainPanel(
                         plotOutput("vis_plot")
                     )
                 ),
             )
             ),
    
    # DISCUSSION PANEL
    
    tabPanel("Discussion",
             titlePanel("Methodology"),
             p("Using the R package 'bigchess', I was able to download all
               of my games from both websites. I added a few details through
               minor manipulations and cleaning of the data.")),
    
    # ABOUT PANEL
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello! I have been a chess enthusiast since I learned the game at the age of 6.
               Following a competitive stint in my early youth, I took a long hiatus from the game,
               and only became seriously interested in chess again in 2018. At that point,
               I made accounts on the two largest chess websites: chess.com &
               lichess.org. This project visualizes trends from all 8000+ of my games since
               then. \n Over the past two years, my (online) rating has increased
               dramatically, from lows of around 1300 to highs of over 2000 Elo.
               I hope to become an expert (2000 Elo in classical, over-the-board
               chess) sometime in the next few years."),
             h3("About Me"),
             p("My name is Nick Brinkmann and I study Applied Mathematics in the Class of 2023. 
             You can reach me at nickbrinkmann@college.harvard.edu."),
             p("My Github repo  for this Milestone can be found at: https://github.com/nick-brinkmann/milestone-4.")
             )
    )


# Define server logic required to draw a histogram

server <- function(input, output) {
    
    # ELO PLOT
    output$vis_plot <- renderImage({
        list(src = input$radio_plot,
             width = 500,
             height = 500)
    }, deleteFile = FALSE)
    
    
    # RENDERING CHESS BOARD
    
    output$board <- renderChessboardjs({
        #chessboardjs("rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2")
        chss <- Chess$new()
        chss$move("a3")$move("b5")
        chessboardjs(chss$fen())
    })
}

# Run the application 

shinyApp(ui = ui, server = server)
