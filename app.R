library(shiny)
library(shinythemes)
library(tidyverse)
library(gganimate)
library(dplyr)
library(lubridate)
library(rchess)
library(gt)
library(DT)

all_games_dense <- readRDS("all_games_dense.RDS")

#chss <- Chess$new()
#chss$move("a3")$move("b5")
#chss$fen()

#chsspgn$load_pgn(pgn)

# USER INTERFACE

ui <- navbarPage(
    theme = shinytheme("united"),
    "Best To Test: Analyzing My Online Chess Games",
    
    # VISUALIZATIONS PANEL
    
    tabPanel("Home",
             fluidPage(
                 titlePanel("Pre-Made Visualizations"),
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons("radio_plot", label = h3("Select Plot"),
                                      choices = list("Elo Rating Over Time" = "smoothest_elo_plot.gif", 
                                                     "Most and Least Successful Openings" = "openings.png",
                                                     "Performance Rating Over Time" = "quarter_perf.gif",
                                                     "Games Played Per Month" = "game_count.gif"
                                                     ), 
                                      selected = "smoothest_elo_plot.gif"),
                         
                         hr(),
                         fluidRow(column(3, verbatimTextOutput("value")))
                     ),
                     mainPanel(
                         plotOutput("vis_plot")
                     )
                 ),
             )
             ),
    
    
    # MODEL PANEL
    
    tabPanel("Model",
             fluidPage(
                 titlePanel("Model"),
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons("radio_model", label = h3("Select Model"),
                                      choices = list("Opening-Specific Model (Default)" = "stat_sig_openings.png",
                                                     "Lumped Model" = "lumped_openings.png"
                                      ),
                                      selected = "stat_sig_openings.png"),
                         
                     ),
                     mainPanel(
                         plotOutput("model_plot"),
                         
                         conditionalPanel(
                             condition = "input.radio_model == 'stat_sig_openings.png'",
                             h3("Interpretation"),
                             p("The above visualization reflects the result of a linear
                           model that predicts the outcome of games based on the
                           specific opening I use (only a single ECO code),
                           the color I play, and the rating difference between 
                           myself and my opponent. There are 300 ECO codes in my database,
                           so there are 600 possible parameters in this model! (Well,
                           601 if you include rating difference.)
                           I have spared you the gritty details of the model.
                           Instead, I have found the openings and colors which 
                           deviate from my average score of around 53.4% with 
                           statistical significance. More specifically, I have only 
                           plotted those openings and colors for which the model
                           predicts with over 95% confidence that the 'true' 
                           value of my average score in this opening-color pair
                           (when my opponent and I have equal ratings) is either 
                           strictly lower than,
                           or strictly higher than, my average score over all games.
                           The error bars represent a centered 90% confidence
                           interval for my 'true' average score, and the blue
                           dotted line represents my average score over all my games.
                           Since all statistically significant deviations fall 
                           below my average score, this indicates to me that 
                           these are the openings in which I could most benefit from
                           study."),
                             h3("Limitations"),
                             p("As a few caveats, my method here was somewhat restricted
                           by undersampling issues. This model does not consider
                           every single opening and color combination, since there 
                           are a number of openings (ECO codes) for which I have 
                           played very few  games - even none, in some cases - with
                           one or both colors. Trying to predict outcomes based 
                           upon such granular data is like trying to tell whether
                           a coin is biased based on a handful of flips: it just 
                           will not work. Instead, I considered only those
                           openings for which I have played at least 10 games
                           with each color. Not only does this make the model
                           more accurate, but also more useful! I will 
                           likely benefit much more from studying up on popular 
                           openings that show up often, rather than studying 
                           obscure openings which only occur rarely. This approach
                           yielded 41 ECO codes, for a total of 83 parameters."),
                             p("Another important caveat is that this model does not
                           include time. As you might assume (and I would hope!),
                           my knowledge of openings and ability to play each opening
                           well is not static. If I play a game and get destroyed
                           in a particularly unfamiliar opening, I often take a 
                           brief look at how I could improve in that line, and
                           thus hopefully play better the next time. Unfortunately,
                           with only a database of around 5000 games (excluding
                           those openings which occur infrequently; the original
                           dataset includes around 9000 games), and already
                           with nearly 100 parameters, the prospect of including time-related
                           changes in playing ability is likely only to obscure,
                           rather than illuminate, my understanding."),
                             p("An additional shortcoming is that this model uses
                               a Gaussian error term, meaning that the outcome
                               variable is modelled as continuous. In reality,
                               however, only three outcomes are possible in a game:
                               win, lose, or draw (1, 0, or 0.5). While the model
                               is still useful since I mostly consider my *average*
                               score, I would like to improve the model to account
                               for the discrete nature of the outcome."),
                             p("The issue of having to severely prune the 
                           dataset to get this model is also a concern. Since ECO
                           codes are so specific, this model may not fully
                           be able to capture the truth about which openings I 
                           am weakest or strongest in. For example, many openings
                           are near identical, but with just one move not in common, the
                           ECO code can be different. This concern is the main 
                           reason I made the lumped model (click on the Lumped Model
                           radio button to the left!).
                           However, the lumped model also has significant issues,
                           which are addressed in that section.")
                         ),
                         
                         conditionalPanel(
                             condition = "input.radio_model == 'lumped_openings.png'",
                             h3("Interpretation"),
                             p("Instead of trying to find performance differences 
                           in specific ECO codes, this model lumped ECO codes 
                           into roughly homogeneous groups, and tried to predict
                           my average score based on the opening group and the 
                           color I play. Only four of the groups deviate with 95% 
                           confidence from my average score."),
                             p("The first is the group A50-A79 when I play with the 
                           black pieces. This group corresponds with what are known
                           as 'Atypical Indian systems' in the Encyclopedia of 
                           Chess Openings (this is where the abbreviation ECO 
                           comes from), characterized by the first moves 1.d4 
                           Nf6 2.c4 without 2...e6 or 2...g6. This includes
                           the Budapest Gambit and the Benoni Defence, mainly.
                           While there is statistical significance to my success 
                           in this opening, I have only played 11 games in this 
                           group with the black pieces. Perhaps I should play it
                           more often!"),
                             p("The second is the group C40-C59 when I play with the
                           black pieces. This group corresponds with games starting
                           with the moves 1.e4 e5, such as Petrov's Defence,
                           the Scotch Game, the Italian Game, and the Evans Gambit,
                           among a few others. While the last group was lacking
                           somewhat in games, this group certainly isn't - I 
                           have played 623 games with the Black pieces in these 
                           openings, and deviate with confidence, albeit only in 
                           small magnitude, from my average score."),
                             p("Thirdly, I perform slightly lower than average when 
                           I play the openings C30-C39 with the black pieces. This
                           group corresponds to all variations of the King's Gambit,
                           characterized by the moves 1.e4 e5 2.f4. This opening
                           is known to be viciously aggressive, though with best
                           play from Black, to be an inferior opening. Since I
                           often play faster time controls where knowing the opening
                           theory can swiftly decide the game, I am not particularly
                           surprised to see my performance dip below average in this
                           group. However, it means that I should probably brush
                           up on my King's Gambit theory, which is great, as that
                           is something I have started looking into recently! In
                           future, as I add to the 129 games from this group, I
                           will hopefully be better prepared to prove White's
                           disadvantage."),
                             p("Finally, I perform poorly in the group B10-B19 with the
                           black pieces. This corresponds to all variations of
                           the Caro-Kann Defence, characterized by the opening moves
                           1.e4 c6. I rarely play this defence with Black, and 
                           as such, have never seriously studied it. Unsurprisingly,
                           this means I have performed markedly below my regular standard
                           of play in my 27 games in this group."),
                             
                             h3("Caveats"),
                             p("As mentioned, this model also has its limitations.
                               Similarly to the individual-ECO model, this model
                               doesn't consider time-based changes to my performance
                               in certain opening groups, and the model includes 
                               a normally-distributed error term, meaning that the
                               outcome variable is continuous, in contrast to what
                               we know about the outcomes of chess games in reality.
                               Furthermore, we see a 'blurring' effect, where
                               only a very small number of groups deviate with 
                               significance from my average score. This makes sense
                               from a sampling perspective: the larger the sample
                               of games in consideration, the closer my predicted
                               score will be to my average score overall.")
                         )
                     )
                 ),
             )
    ),
    
    
    # ABOUT PANEL
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("The famous American chess genius Bobby Fischer was well-known for
               playing 1.e4 (chess notation for the first move for white being 
               pawn to e4), and coined a popular phrase in the chess community:
               '1.e4: Best by test'. Bobby, forming this opinion over thousands
               of games, clearly embraced empiricism.
               Having been borderline addicted to playing online chess for the past
               several years, I have a trove of nearly 9000 games at my disposal.
               In this project, I follow Bobby's lead and attempt to discover
               trends in my play using empirical, data-driven methods. Along with
               some modeling, I also had plenty of fun teasing out interesting 
               data visualizations concerning my rating, my 
               most and least successful openings, and more."),
             
             h3("Auxiliary Info"),
             p("The R package 'bigchess' allowed me to download all of my games
               directly. Some cleaning was necessary, but overall it was a
               very useful package."),
             
             h3("About Me"),
             p("Hello! My name is Nick Brinkmann and I study Applied Mathematics in the Class of 2023. 
             You can reach me at nickbrinkmann at college.harvard.edu"),
             p("I have been a chess enthusiast since I learned the game at the age of 6.
               Following a competitive stint in my early youth, I took a long hiatus from the game,
               and only became seriously interested in chess again in 2018. At that point,
               I made accounts on the two largest chess websites: chess.com &
               lichess.org. Over the past two years, my (online) rating has increased
               dramatically, from lows of around 1300 to highs of over 2000 Elo.
               I hope to become an expert (2000 Elo in classical, over-the-board
               chess) sometime in the next few years."),
             p("See my Github repo ", tags$a(href = "https://github.com/nick-brinkmann/best-to-test", 
                                      "here"), "."),
             p("See my online profiles on ", 
               tags$a(href = "https://www.chess.com/member/elatedjuvenile",
                      "Chess.com"), 
               "and", 
               tags$a(href = "https://lichess.org/@/elatedjuvenile",
                      "Lichess.org"), 
               ", as well as my ", 
               tags$a(href ="http://www.uschess.org/msa/MbrDtlMain.php?30032067",
                      "US Chess Federation statistics"),
               "."),
             
             
    ),
    
    # Interactive PANEL
    
    # tabPanel("Interactive Plots",
    #          fluidPage(
    #              titlePanel("Interactive Plots"),
    #              sidebarLayout(
    #                  sidebarPanel(
    #                      sliderInput("date",
    #                                  "Starting date: ",
    #                                  min = min(all_games$Date),
    #                                  max = max(all_games$Date),
    #                                  value = min(all_games$Date)
    #                      ),
    #                      sliderInput("start_end",
    #                                  label = h3("Start and End Date"),
    #                                  min = min(all_games$Date),
    #                                  max = max(all_games$Date),
    #                                  value = c(min(all_games$Date), max(all_games$Date))
    #                      )
    #                  ),
    #                  #mainPanel(chessboardjsOutput('board', width = 300)))
    #                  p("To be completed still.")
    #              )
    #          )
    # ),
    
    # BACKGROUND PANEL
    
    tabPanel("Chess Background",
             titlePanel("Wait... What Is Chess Again?"),
             p("For those who are unfamiliar with chess, I will give some brief
               background so you are able to understand the project."),
             h3("Notation"),
             p("In chess, moves are written using 'Algebraic Notation', where
               each piece is given a shorthand, and each square is given a name.
               Kings are denoted K, Queens Q, Rooks R, Bishops B, Knights N,
               and since pawn moves are so common, it is conventional to omit something
               like a 'P' for pawn moves, and instead just say the square to 
               which the pawn moves, as in 'b3'. The one exception is when pawns take another
               piece (and thus move from one file to another), as in 'dxe4', where
               the first letter is the old file, the x denotes a capture, and
               e4 is the square to which the pawn moves. There are special moves
               known as castling, denoted 0-0 for short castling and 0-0-0 for
               long castling, and en-passant, both of which you can look up 
               if you are interested. The squares of the board are labelled by 
               column (file) and row (rank), where files are labelled from a to 
               h, and ranks from 1 to 8 (i.e., the bottom left corner is a1, and
               the top right corner is h8, when looking from the perspective of 
               the player playing with the white pieces.)"),
             
             h3("Openings and ECO codes"),
             p("Chess players like to categorize games they play based on any number
               of the first few moves in the game. To do so, an Encyclopedia of 
               Chess Openings (ECO) has been devised, where openings are categorized
               by a letter from A to E and a number from 00 to 99. Games in the 
               same letter category tend to have similar characteristics.
               You can find more info on the Wikipedia page linked below."),
             tags$a(href="https://en.wikipedia.org/wiki/List_of_chess_openings", 
                    "Read up on ECO codes"),
             
             h3("Time Control"),
             p("Almost all games played online have a time restriction on the players.
               I mostly play games with incremental time, where each player has some
               fixed starting time, and receives additional time for each move played.
               I most frequently play 2+1 games, where each player has 2 minutes,
               and receives an additional 1 second per move. The amount of time
               that the players have to play the game determines the format of the
               game. The most common formats, in decreasing amount of time per
               player, are Classical, Rapid, Blitz, and Bullet. For more information
               about time controls, see ", 
               tags$a(href = "https://lichess.org/faq#time-controls", "here"),
               ". I used the formula linked above to categorize my games into
               their respective formats."),
             
             h3("Rating"),
             p("Each player is given a rating: a single number which measures their
               playing strength relative to other players. Ratings are different
               for each time format, and also differ based on the association through
               which you play. For example, my ratings on Lichess differ from my 
               ratings on Chess.com, which in turn are different from my over-the-board
               (OTB) ratings issued by the US Chess Federation (USCF) and FIDE, the
               International Chess Federation."),
             p("A beginner player typically has a rating of under 1000, while the
               world champion, Magnus Carlsen, has an OTB rating of around 2850.
               Online ratings tend to be inflated compared to OTB ratings. My 
               online ratings are around 2000 on Lichess,
               about 200 points lower on Chess.com, and around 1700-1800 OTB."),
             p("A common metric for measuring how well someone played in a tournament
               or over some time period is known as their 'performance rating'.
               Performance ratings are calculated by a formula which you can find ",
               tags$a(href = "https://en.wikipedia.org/wiki/Elo_rating_system#Performance_rating",
                      "here"),
             ". I used this formula briefly in determining my performance ratings
             over time in the Home panel.")
             
    ),
    
    # PLAY THROUGH GAMES
    
    tabPanel("Games Database",
        fluidPage(
        titlePanel("Play Through Games"),
        p("In the extremely unlikely case that you have any interest in browsing
          my games, here they all are! Since the dataset is so large, this only
          loads games from one month at a time."),
        
                # sliderInput("start_end",
                #             label = h3("Start and End Date"),
                #             min = min(all_games$Date),
                #             max = max(all_games$Date),
                #             value = c(as.Date("2020-10-01"), as.Date("2020-12-01"))
                # ),
        
        selectInput(
            "month",
            label = "Select Month:",
            choices = unique(all_games_dense$month),
            selected = unique(all_games_dense$month)[1]
        ),
        
        DT::dataTableOutput("table")


                # gt_output("table")


        


    )
             
    )
    
)


# Define server logic

server <- function(input, output) {
    
    # ELO PLOT
    
    output$vis_plot <- renderImage({
        list(src = input$radio_plot,
             width = 500,
             height = 500)
    }, deleteFile = FALSE)
    
    # MODEL PLOT
    
    output$model_plot <- renderImage({
        list(src = input$radio_model,
             width = 600,
             height = 370
        )
    }, deleteFile = FALSE)
    
    # GAMES DATABASE TABLE
    # output$table <- render_gt(
    #     expr = all_games %>% 
    #         select(White, Black, my_elo, Result, Movetext, Date) %>%
    #         filter(Date >= input$start_end[1] & Date <= input$start_end[2]) %>% 
    #         gt() %>% 
    #         cols_width(
    #             starts_with("Movetext") ~ px(300),
    #             starts_with("White") ~ px(150),
    #             starts_with("Black") ~ px(150),
    #             everything() ~ px(100)
    #         ),
    #     height = px(1000),
    #     width = px(1000)
    #     )
    
    # output$table <- render_gt(
    #     expr = all_games %>%
    #         filter(month == input$month) %>%
    #         select(White, Black, my_elo, Result, Movetext, Date) %>%
    #         gt() %>%
    #         cols_width(
    #             starts_with("Movetext") ~ px(300),
    #             starts_with("White") ~ px(150),
    #             starts_with("Black") ~ px(150),
    #             everything() ~ px(100)
    #         ),
    #     height = px(1000),
    #     width = px(1000)
    #     )
    
    output$table <- DT::renderDataTable(DT::datatable({
        all_games_dense %>%
            filter(month == input$month) %>% 
            select(White, Black, my_elo, Result, Movetext, Date, format, website)
    }))
    
    
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
