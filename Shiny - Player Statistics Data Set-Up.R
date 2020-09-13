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
         player_height_cm,
         player_weight_kg,
         # player_is_retired,
         # player_team,
         # player statistics 
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

# calculate average stats for each player 

# player <- afl.2 %>%
#   group_by(player_id,
#            player_first_name,
#            player_last_name,
#            player_height_cm,
#            player_weight_kg) %>%
#   summarise(
#     Brownlow_Votes = round(mean(brownlow_votes),2),
#     Behinds = round(mean(behinds),2),
#     Bounces = round(mean(bounces),2),
#     Centre_Clearances = round(mean(centre_clearances),2),
#     Clangers = round(mean(clangers),2),
#     Clearances = round(mean(clearances),2),
#     Contest_Def_Losses = round(mean(contest_def_losses),2),
#     Contest_Def_One_On_Ones = round(mean(contest_def_one_on_ones),2),
#     Contested_Marks = round(mean(contested_marks),2),
#     Contest_Off_One_On_Ones = round(mean(contest_off_one_on_ones),2),
#     Contest_Off_Wins = round(mean(contest_off_wins),2),
#     Contested_Possessions = round(mean(contested_possessions),2),
#     Def_Half_Pressure_Acts = round(mean(def_half_pressure_acts),2),
#     Disposals = round(mean(disposals),2),
#     Disposal_Efficiency = round(mean(disposal_efficiency_percentage),2),
#     Effective_Kicks = round(mean(effective_kicks),2),
#     Forward_50_GroundBall_Gets = round(mean(f50_ground_ball_gets),2),
#     Frees_For = round(mean(free_kicks_for),2),
#     Frees_Against = round(mean(free_kicks_against),2),
#     Goals = round(mean(goals),2),
#     Goal_Assists = round(mean(goal_assists),2),
#     GroundBall_Gets = round(mean(ground_ball_gets),2),
#     Handballs = round(mean(handballs),2),
#     Hitouts = round(mean(hitouts),2),
#     Hitouts_To_Advantage = round(mean(hitouts_to_advantage),2),
#     Inside_50s = round(mean(inside_fifties),2),
#     Intercepts = round(mean(intercepts),2),
#     Intercept_Marks = round(mean(intercept_marks),2),
#     Kicks = round(mean(kicks),2),
#     Marks_Inside_50 = round(mean(marks_inside_fifty),2),
#     Metres_Gained = round(mean(metres_gained),2),
#     One_Percenters = round(mean(one_percenters),2),
#     Pressure_Acts = round(mean(pressure_acts),2),
#     Rebounds = round(mean(rebounds),2),
#     Ruck_Contests = round(mean(ruck_contests),2),
#     Score_Involvements = round(mean(score_involvements),2),
#     Score_Launches = round(mean(score_launches),2),
#     Spoils = round(mean(spoils),2),
#     Stoppage_Clearances = round(mean(stoppage_clearances),2),
#     Tackles = round(mean(tackles),2),
#     Tackles_Inside_50 = round(mean(tackles_inside_fifty),2),
#     Time_On_Ground_Percentage = round(mean(time_on_ground_percentage),2),
#     Turnovers = round(mean(turnovers),2),
#     Uncontested_Possessions = round(mean(uncontested_possessions),2)
#   )

player <- afl.2 %>% 
  group_by(player_id,
           player_first_name,
           player_last_name) %>%
  summarise_at(.vars = colnames(.)[6:49], mean) # calculate mean for all specified columns 

player <- as.data.frame(test)

player <- test %>% mutate_if(is.numeric, ~round(., 2))


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
player <- player[,c(3, 1, 2, 50, 4:49)]

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

# Outliers test

# For continuous variable (convert to categorical if needed.)
boxplot(Contested_Possessions ~ Contested_Marks, 
        data = player, 
        main = "Boxplot for metres gained & winning margin")

library(aplpack)
bagplot(cbind(player$Contested_Possessions,
              player$Contested_Marks),pch=16,cex=2)





# ----------------------------------------------

# Variables that can be put on the x and y axes
axis_vars <- c(
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
)
