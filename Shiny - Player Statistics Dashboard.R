# ---------------------
# DEPENDENCIES
# ---------------------

library(ggplot2)
library(shiny)
library(shinyWidgets)
library(plotly)
library(ggthemes)
library(tableHTML)

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
  titlePanel(strong("Scatterplot of AFL players average performance on common match day statistics")),
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for x-axis
      selectInput(inputId = "x", label = "Select Horizontal Axis:",
                  choices = c(
                    "Brownlow Votes" = "Brownlow_Votes",
                    "Behinds" = "Behinds",
                    "Bounces" = "Bounces",
                    "Centre Clearances" = "Centre_Clearances",
                    "Clangers" = "Clangers",
                    "Clearances" = "Clearances",
                    "Contest Defensive Losses" = "Contest_Def_Losses",
                    "Contest Defensive 1 on 1s" = "Contest_Def_One_On_Ones",
                    "Contested Marks" = "Contested_Marks",
                    "Contest Offensive 1 on 1s" = "Contest_Off_One_On_Ones",
                    "Contest Offensive Wins" = "Contest_Off_Wins",
                    "Contested Possessions" = "Contested_Possessions",
                    "Defensive Half Pressure Acts" = "Def_Half_Pressure_Acts",
                    "Disposals" = "Disposals",
                    "Disposal Effficiency" = "Disposal_Effficiency",
                    "Effective Kicks" = "Effective_Kicks",
                    "Forward 50 GroundBall Gets" = "Forward_50_GroundBall_Gets",
                    "Frees For" = "Frees_For",
                    "Frees Against" = "Frees_Against",
                    "Goals" = "Goals",
                    "Goal Assists" = "Goal_Assists",
                    "GroundBall Gets" = "GroundBall_Gets",
                    "Handballs" = "Handballs",
                    "Hitouts" = "Hitouts",
                    "Hitouts To Advantage" = "Hitouts_To_Advantage",
                    "Inside 50s" = "Inside_50s",
                    "Intercepts" = "Intercepts",
                    "Intercept Marks" = "Intercept_Marks",
                    "Kicks" = "Kicks",
                    "Marks Inside 50" = "Marks_Inside_50",
                    "Metres Gained" = "Metres_Gained",
                    "One Percenters" = "One_Percenters",
                    "Pressure Acts" = "Pressure_Acts",
                    "Rebounds" = "Rebounds",
                    "Ruck Contests" = "Ruck_Contests",
                    "Score Involvements" = "Score_Involvements",
                    "Score Launches" = "Score_Launches",
                    "Spoils" = "Spoils",
                    "Stoppage Clearances" = "Stoppage_Clearances",
                    "Tackles" = "Tackles",
                    "Tackles Inside 50" = "Tackles_Inside_50",
                    "Time On Ground Percentage" = "Time_On_Ground_Percentage",
                    "Turnovers" = "Turnovers",
                    "Uncontested_Possessions" = "Uncontested Possessions"
                  ),                  
                  selected = "Contested_Possessions"),
      
      # Select variable for y-axis
      selectInput(inputId = "y", label = "Select Vertical Axis:",
                  choices = c(
                    "Brownlow Votes" = "Brownlow_Votes",
                    "Behinds" = "Behinds",
                    "Bounces" = "Bounces",
                    "Centre Clearances" = "Centre_Clearances",
                    "Clangers" = "Clangers",
                    "Clearances" = "Clearances",
                    "Contest Defensive Losses" = "Contest_Def_Losses",
                    "Contest Defensive 1 on 1s" = "Contest_Def_One_On_Ones",
                    "Contested Marks" = "Contested_Marks",
                    "Contest Offensive 1 on 1s" = "Contest_Off_One_On_Ones",
                    "Contest Offensive Wins" = "Contest_Off_Wins",
                    "Contested Possessions" = "Contested_Possessions",
                    "Defensive Half Pressure Acts" = "Def_Half_Pressure_Acts",
                    "Disposals" = "Disposals",
                    "Disposal Effficiency" = "Disposal_Effficiency",
                    "Effective Kicks" = "Effective_Kicks",
                    "Forward 50 GroundBall Gets" = "Forward_50_GroundBall_Gets",
                    "Frees For" = "Frees_For",
                    "Frees Against" = "Frees_Against",
                    "Goals" = "Goals",
                    "Goal Assists" = "Goal_Assists",
                    "GroundBall Gets" = "GroundBall_Gets",
                    "Handballs" = "Handballs",
                    "Hitouts" = "Hitouts",
                    "Hitouts To Advantage" = "Hitouts_To_Advantage",
                    "Inside 50s" = "Inside_50s",
                    "Intercepts" = "Intercepts",
                    "Intercept Marks" = "Intercept_Marks",
                    "Kicks" = "Kicks",
                    "Marks Inside 50" = "Marks_Inside_50",
                    "Metres Gained" = "Metres_Gained",
                    "One Percenters" = "One_Percenters",
                    "Pressure Acts" = "Pressure_Acts",
                    "Rebounds" = "Rebounds",
                    "Ruck Contests" = "Ruck_Contests",
                    "Score Involvements" = "Score_Involvements",
                    "Score Launches" = "Score_Launches",
                    "Spoils" = "Spoils",
                    "Stoppage Clearances" = "Stoppage_Clearances",
                    "Tackles" = "Tackles",
                    "Tackles Inside 50" = "Tackles_Inside_50",
                    "Time On Ground Percentage" = "Time_On_Ground_Percentage",
                    "Turnovers" = "Turnovers",
                    "Uncontested_Possessions" = "Uncontested Possessions"
                  ),                    
                  selected = "Contested_Marks"),
      
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
        "This dashboard summarises player 'averages' (mean value)."
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

