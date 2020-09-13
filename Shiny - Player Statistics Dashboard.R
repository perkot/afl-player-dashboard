# ---------------------
# DEPENDENCIES
# ---------------------

library(ggplot2)
library(shiny)
library(shinyWidgets)
library(plotly)
library(ggthemes)
library(tableHTML)
library(rsconnect)

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/

# https://stackoverflow.com/questions/31268581/shiny-passing-on-text-with-a-space-to-chart-does-not-work-but-it-works-without
# https://stackoverflow.com/questions/45690184/shiny-changing-size-colour-and-font-of-text-boxes

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

# https://rstudio.github.io/shinydashboard/structure.html#sidebar

# ---------------------
# USER INTERFACE
# ---------------------

# Define UI for application that plots features of movies
ui <- fluidPage(
  # strong == bold font 
  titlePanel(strong("AFL player performance on common match day statistics")),
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for x-axis
      selectInput(inputId = "x", label = "Select Horizontal Axis:",
                  choices = c(
                    "Brownlow Votes" = "brownlow_votes",
                    "Behinds" = "behinds",
                    "Bounces" = "bounces",
                    "Centre Clearances" = "centre_clearances",
                    "Clangers" = "clangers",
                    "Clearances" = "clearances",
                    "Contest Defensive Losses" = "contest_def_losses",
                    "Contest Defensive 1 on 1s" = "contest_def_one_on_ones",
                    "Contested Marks" = "contested_marks",
                    "Contest Offensive 1 on 1s" = "contest_off_one_on_ones",
                    "Contest Offensive Wins" = "contest_off_wins",
                    "Contested Possessions" = "contested_possessions",
                    "Defensive Half Pressure Acts" = "def_half_pressure_acts",
                    "Disposals" = "disposals",
                    "Disposal Effficiency" = "disposal_efficiency_percentage",
                    "Effective Kicks" = "effective_kicks",
                    "Forward 50 GroundBall Gets" = "f50_ground_ball_gets",
                    "Frees For" = "free_kicks_for",
                    "Frees Against" = "free_kicks_against",
                    "Goals" = "goals",
                    "Goal Assists" = "goal_assists",
                    "Ground Ball Gets" = "ground_ball_gets",
                    "Handballs" = "handballs",
                    "Hitouts" = "hitouts",
                    "Hitouts To Advantage" = "hitouts_to_advantage",
                    "Inside 50s" = "inside_fifties",
                    "Intercepts" = "intercepts",
                    "Intercept Marks" = "intercept_marks",
                    "Kicks" = "kicks",
                    "Marks Inside 50" = "marks_inside_fifty",
                    "Metres Gained" = "metres_gained",
                    "One Percenters" = "one_percenters",
                    "Pressure Acts" = "pressure_acts",
                    "Rebounds" = "rebounds",
                    "Ruck Contests" = "ruck_contests",
                    "Score Involvements" = "score_involvements",
                    "Score Launches" = "score_launches",
                    "Spoils" = "spoils",
                    "Stoppage Clearances" = "stoppage_clearances",
                    "Tackles" = "tackles",
                    "Tackles Inside 50" = "tackles_inside_fifty",
                    "Time On Ground Percentage" = "time_on_ground_percentage",
                    "Turnovers" = "turnovers",
                    "Uncontested Possessions" = "uncontested_possessions"
                  ),                  
                  selected = "contested_possessions"),
      
      # Select variable for y-axis
      selectInput(inputId = "y", label = "Select Vertical Axis:",
                  choices = c(
                    "Brownlow Votes" = "brownlow_votes",
                    "Behinds" = "behinds",
                    "Bounces" = "bounces",
                    "Centre Clearances" = "centre_clearances",
                    "Clangers" = "clangers",
                    "Clearances" = "clearances",
                    "Contest Defensive Losses" = "contest_def_losses",
                    "Contest Defensive 1 on 1s" = "contest_def_one_on_ones",
                    "Contested Marks" = "contested_marks",
                    "Contest Offensive 1 on 1s" = "contest_off_one_on_ones",
                    "Contest Offensive Wins" = "contest_off_wins",
                    "Contested Possessions" = "contested_possessions",
                    "Defensive Half Pressure Acts" = "def_half_pressure_acts",
                    "Disposals" = "disposals",
                    "Disposal Effficiency" = "disposal_efficiency_percentage",
                    "Effective Kicks" = "effective_kicks",
                    "Forward 50 GroundBall Gets" = "f50_ground_ball_gets",
                    "Frees For" = "free_kicks_for",
                    "Frees Against" = "free_kicks_against",
                    "Goals" = "goals",
                    "Goal Assists" = "goal_assists",
                    "Ground Ball Gets" = "ground_ball_gets",
                    "Handballs" = "handballs",
                    "Hitouts" = "hitouts",
                    "Hitouts To Advantage" = "hitouts_to_advantage",
                    "Inside 50s" = "inside_fifties",
                    "Intercepts" = "intercepts",
                    "Intercept Marks" = "intercept_marks",
                    "Kicks" = "kicks",
                    "Marks Inside 50" = "marks_inside_fifty",
                    "Metres Gained" = "metres_gained",
                    "One Percenters" = "one_percenters",
                    "Pressure Acts" = "pressure_acts",
                    "Rebounds" = "rebounds",
                    "Ruck Contests" = "ruck_contests",
                    "Score Involvements" = "score_involvements",
                    "Score Launches" = "score_launches",
                    "Spoils" = "spoils",
                    "Stoppage Clearances" = "stoppage_clearances",
                    "Tackles" = "tackles",
                    "Tackles Inside 50" = "tackles_inside_fifty",
                    "Time On Ground Percentage" = "time_on_ground_percentage",
                    "Turnovers" = "turnovers",
                    "Uncontested Possessions" = "uncontested_possessions"
                  ),                    
                  selected = "contested_marks"),
      
      sliderInput("Games", "Select # of games played",
                  min(player$Games), 
                  max(player$Games),
                  value = c(22, 86)), # what default values are on slider 
      
      pickerInput("player_team","Select Teams", 
                  choices = levels(player$player_team), # choices == continents 
                  options = list(`actions-box` = TRUE), # action box allows for select/deselect all
                  multiple = T, # allows multiple selections 
                  selected = player$player_team), # defaults to select all
      
      checkboxGroupInput("PositionType", "Select Position",
                   choices = c("Defender",
                               "Forward",
                               "Interchange",
                               "Midfield",
                               "Ruck"
                               ),
                   selected = c("Defender",
                                "Forward",
                                "Interchange",
                                "Midfield",
                                "Ruck"
                   )),
      
      
      tags$small(paste0(
        "This app allows users to select two match day statistics, and explore AFL player 'averages' (mean value) for these metrics on a scatterplot."
      )),
      
      tags$small(paste0(
        "The data summarised is from all games from season 2017 to the present"
      )),
      
    ),
    # Output: Show scatterplot
    mainPanel(
      plotlyOutput(outputId = "scatterplot", 
                   width = 800, height = 600)
    )
  )
)

# ---------------------
# SERVER
# ---------------------

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlotly({
    
    Data <- subset(player,
                   player_team %in% input$player_team & # team as an input
                     PositionType %in% input$PositionType & # position as an input
                     Games >= input$Games[1] & 
                     Games <= input$Games[2]) # games as an input 
    
    # -------
    # PLOT
    # -------
    p <- ggplot(Data, 
                aes(label = Player_Name,
                    label2 = Games,
                    color = PositionType))
    p <- p + geom_point(aes_string(input$x, input$y), 
                        shape = 16, size = 4, alpha = 4/10) + 
      scale_colour_manual(values=c("#b80909", "#0d6cb5", "#e0842d", "#31ad28", "#e3c609")) +
      # scale_colour_manual(values=c("#F93433", "#049A54", "#365DA3", "#E29142", "#EDD537")) +
      # scale_colour_gradient(low = "#b37b7b", high = "#b80909") +
      Plot_Theme2
    final.p <- ggplotly(p)
    final.p
    
  })
}

# Run the application 
shinyApp(ui = ui, 
         server = server)

