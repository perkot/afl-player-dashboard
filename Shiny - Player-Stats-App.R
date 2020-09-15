# ---------------------
# DATA SET-UP
# ---------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(fitzRoy)
library(shiny)
library(shinyWidgets)
library(ggthemes)
library(tableHTML)

# Generic Fitzroy command to get all AFL data 

afl.1 <- get_fryzigg_stats(start = 2017, 
                           end = 2020)
# Explore

str(afl.1)

# Limit to required variables 

afl.2 <- afl.1 %>% 
  select(# player details
    player_id,
    player_first_name,
    player_last_name,
    kicks,
    handballs,
    disposals,
    disposal_efficiency_percentage,
    goals,
    behinds,
    hitouts,
    tackles,
    rebounds,
    inside_fifties,
    clearances,
    clangers,
    free_kicks_for,
    free_kicks_against,
    contested_possessions,
    uncontested_possessions,
    brownlow_votes,
    contested_marks,
    marks_inside_fifty,
    one_percenters,
    bounces,
    goal_assists,
    time_on_ground_percentage,
    centre_clearances,
    stoppage_clearances,
    score_involvements,
    metres_gained,
    turnovers,
    intercepts,
    tackles_inside_fifty,
    contest_def_losses,
    contest_def_one_on_ones,
    contest_off_one_on_ones,
    contest_off_wins,
    def_half_pressure_acts,
    effective_kicks,
    f50_ground_ball_gets,
    ground_ball_gets,
    hitouts_to_advantage,
    intercept_marks,
    pressure_acts,
    ruck_contests,
    score_launches,
    spoils
  )

player <- afl.2 %>% 
  group_by(player_id,
           player_first_name,
           player_last_name) %>%
  dplyr::summarise_at(.vars = colnames(.)[4:47], mean) # calculate mean for all specified columns 

player <- as.data.frame(player)

player <- player %>% mutate_if(is.numeric, ~round(., 2))

# number of games 

# Extract out number of games & join back 

player <- afl.2 %>%
  group_by(player_id) %>% 
  summarise(Games = dplyr::n()) %>% 
  left_join(player, by = "player_id")

# Extract out team and join back 

player <- afl.1 %>% 
  arrange(desc(match_id)) %>% # to get most recent team 
  select(player_id, player_team) %>% 
  distinct(player_id, .keep_all = TRUE) %>% 
  left_join(player, by = "player_id") 

# positions 

position <- afl.1 %>% 
  select(player_id, 
         player_position)

unique_positions <- unique(position$player_position)
unique_positions

# C = Centre
# INT = Interchange
# R = Ruck 
# RR = Ruck Rover
# RK = Ruck 
# FPR = Right Forward Pocket
# FF = Full Forward
# BPL = Left Back Pocket
# FB = Full Back 
# BPR = Right Back Pocket 
# HBFL = Left Half Back Flank
# CHB = Centre Half Back 
# HBFR = Right Half Back Flank
# WL = Left Wing
# WR = Right Wing
# HFFL = Left Half Forward Flank
# FPL = Left Forward Pocket 
# HFFR = Right Half Forward Flank
# CHF" = Centre Half Forward
# SUB = Sub 

# Generic positions 

position$PositionType[
  position$player_position ==  "C" | 
    position$player_position  == "RR" | 
    position$player_position  ==  "R" | 
    position$player_position  ==  "WL" | 
    position$player_position  ==  "WR"] <- 
  "Midfield"

position$PositionType[
  position$player_position ==  "BPL" | 
    position$player_position  == "FB" | 
    position$player_position  ==  "BPR" | 
    position$player_position  ==  "HBFL" | 
    position$player_position  ==  "HBFR" | 
    position$player_position  ==  "CHB"] <- 
  "Defender"

position$PositionType[
  position$player_position ==  "FPR" | 
    position$player_position  == "FF" | 
    position$player_position  ==  "HFFL" | 
    position$player_position  ==  "FPL" | 
    position$player_position  ==  "HFFR" | 
    position$player_position  ==  "CHF"] <- 
  "Forward"

position$PositionType[
  position$player_position ==  "RK"] <- 
  "Ruck"

position$PositionType[
  position$player_position ==  "INT"] <- 
  "Interchange"

# get count of positions per player, then potentially select max 

position2 <- position %>% 
  group_by(player_id, 
           PositionType) %>% 
  summarise(Position_Count = dplyr::n())

# Order by player, then how often they were named in a particular position 
position2 <- position2[with(position2, 
                            order(player_id, Position_Count)), ]

# Issue with interchange players 
# position2 <- position2[!(position2$PositionType == "Interchange"),]


# keep only columns with max value 
position2 <- position2 %>% 
  group_by(player_id) %>% 
  top_n(1, Position_Count)

# keep only distinct
position2 <- position2 %>% distinct(player_id, .keep_all = TRUE)

# remove position count 
position2$Position_Count <- NULL

# Join
player <- left_join(player, position2, by = "player_id")

# Clean 

# remove ID
player$player_id <- NULL

# combine first and last name
player <- player %>% 
  unite(Player_Name,
        c(player_first_name, player_last_name),
        remove = TRUE)

# remove underscore
player$Player_Name <- gsub("_", " ", player$Player_Name, fixed = TRUE)

# re-order 
# Re-order Columns
player <- player[,c(3, 1, 2, 48, 4:47)]

# team as factor
player$player_team <- as.factor(player$player_team)
# position as factor
player$PositionType <- as.factor(player$PositionType)

# games as numeric
player$Games <- as.numeric(player$Games)

# plot theme 

Plot_Theme2 <-
  theme_minimal() + # start with a minimal theme and add what we need
  theme(text = element_text(color = "gray20"),
        legend.position = "top", # position the legend in the upper left 
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.title = element_text(size = 10, color = "gray20"),
        legend.text = element_text(size = 10, color = "gray20"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1, size = 10), # move title away from axis
        axis.title.y = element_text(vjust = 2, size = 10), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray20", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray80", size = 0.5),
        panel.grid.minor = element_line(color = "gray80", size = 0.5),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 11)
  )


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
      
      # text
      tags$small(paste0(
        "This app allows users to select two match day statistics, and explore AFL player 'averages' (mean value) for these metrics on a scatterplot."
      )),
      
      # text
      tags$small(paste0(
        "The data summarised is from all games from season 2017 to the present"
      )),
      
      tags$br(), #space
      tags$br(), # space
      
      # web link
      tags$a(href="https://github.com/perkot/afl-player-dashboard/blob/master/Shiny%20-%20Player-Stats-App.R", "Get Code"),
      
      tags$br(),
      
      tags$a(href="https://perkot.github.io/afl-player-dashboard/", "Walkthrough")
      
    ),
    # Output: Scatterplot
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
                   player_team %in% input$player_team & # team 
                     PositionType %in% input$PositionType & # position 
                     Games >= input$Games[1] & # games
                     Games <= input$Games[2]) 
    
    # -------
    # PLOT
    # -------
    p <- ggplot(Data, 
                aes(label = Player_Name,
                    label2 = Games,
                    color = PositionType))
    p <- p + geom_point(aes_string(input$x, input$y), 
                        shape = 16, size = 4, alpha = 4/10) + 
      scale_colour_manual(values=c("#b80909", 
                                   "#0d6cb5", 
                                   "#e0842d", 
                                   "#31ad28", 
                                   "#e3c609")) +
      Plot_Theme2
    final.p <- ggplotly(p)
    final.p
    
  })
}

# Run the application 
shinyApp(ui = ui,
         server = server)

