# https://analysisofafl.netlify.app/fitzroy/2020-06-11-more-data-on-fitzroy-as-featured-on-abc/

Fyfe <- player %>%
  filter(Player_Name %in% c("Nat Fyfe"))

Dusty <- player %>%
  filter(Player_Name %in% c("Dustin Martin"))

Buddy <- player %>%
  filter(Player_Name %in% c("Lance Franklin"))

Stars <- player %>%
  filter(Player_Name %in% c("Lance Franklin",
                            "Dustin Martin",
                            "Lance Franklin",
                            "Nat Fyfe",
                            "Patrick Dangerfield",
                            "Lachie Neale",
                            "Patrick Cripps",
                            "Marcus Bontempelli",
                            "Tom Mitchell"))

# plot template 

p <- player %>% 
  filter(Games >= 22) %>% 
  ggplot(aes(label = Player_Name,
             label2 = Games,
             color = PositionType))
p <- p + geom_point(aes(Contested_Possessions, 
                        Contested_Marks), 
                    shape = 16, 
                    size = 4, 
                    alpha = 4/10) + 
  geom_text(data = Fyfe, 
            size = 3, 
            colour = "#1a5716",
            aes(Contested_Possessions, Contested_Marks)) +
  scale_colour_manual(values=c("#b80909", 
                               "#0d6cb5", 
                               "#e0842d", 
                               "#31ad28", 
                               "#e3c609")) +
  Plot_Theme2
final.p <- ggplotly(p)
final.p


p2 <- player %>% 
  filter(Games >= 22) %>% 
  ggplot(aes(label = Player_Name,
             label2 = Games,
             color = PositionType))
p2 <- p2 + geom_point(aes(Centre_Clearances, 
                        Stoppage_Clearances), 
                    shape = 16, 
                    size = 4, 
                    alpha = 4/10) + 
  geom_text(data = Dusty, 
            size = 3, 
            colour = "#07406b",
            aes(Centre_Clearances, Stoppage_Clearances)) +
  scale_colour_manual(values=c("#b80909", "#0d6cb5", "#e0842d", "#31ad28", "#e3c609")) +
  Plot_Theme2
final.p2 <- ggplotly(p2)
final.p2

p2 <- player %>% 
  filter(Games >= 22) %>% 
  ggplot(aes(label = Player_Name,
             label2 = Games,
             color = PositionType))
p2 <- p2 + geom_point(aes(Centre_Clearances, 
                          Stoppage_Clearances), 
                      shape = 16, 
                      size = 4, 
                      alpha = 4/10) + 
  geom_text(data = Stars, 
            size = 3, 
            colour = "#383e42",
            aes(Centre_Clearances, Stoppage_Clearances)) +
  scale_colour_manual(values=c("#b80909", "#0d6cb5", "#e0842d", "#31ad28", "#e3c609")) +
  Plot_Theme2
final.p2 <- ggplotly(p2)
final.p2


p3 <- player %>% 
  filter(Games >= 22) %>% 
  ggplot(aes(label = Player_Name,
             label2 = Games,
             color = PositionType))
p3 <- p3 + geom_point(aes(Goals, 
                          Behinds), 
                      shape = 16, 
                      size = 4, 
                      alpha = 4/10) + 
  geom_text(data = Buddy, 
            size = 3, 
            colour = "#07406b",
            aes(Goals, Behinds)) +
  scale_colour_manual(values=c("#b80909", "#0d6cb5", "#e0842d", "#31ad28", "#e3c609")) +
  Plot_Theme2
final.p3 <- ggplotly(p3)
final.p3



scale_colour_manual(values=c("#b80909", "#0d6cb5", "#31ad28", "#e3c609", "#e0842d")) +

  
  
# "Brownlow Votes" = "Brownlow_Votes",
# "Behinds" = "Behinds", 
# "Bounces" = "Bounces",
# "Centre Clearances" = "Centre_Clearances",
# "Clangers" = "Clangers",
# "Clearances" = "Clearances",
# "Contest Defensive Losses" = "Contest_Def_Losses",
# "Contest Defensive 1 on 1s" = "Contest_Def_One_On_Ones",
# "Contested Marks" = "Contested_Marks",
# "Contest Offensive 1 on 1s" = "Contest_Off_One_On_Ones",
# "Contest Offensive Wins" = "Contest_Off_Wins",
# "Contested Possessions" = "Contested_Possessions",
# "Defensive Half Pressure Acts" = "Def_Half_Pressure_Acts",
# "Disposals" = "Disposals", 
# "Disposal Effficiency" = "Disposal_Effficiency", 
# "Effective Kicks" = "Effective_Kicks",
# "Forward 50 GroundBall Gets" = "Forward_50_GroundBall_Gets",
# "Frees For" = "Frees_For",
# "Frees Against" = "Frees_Against",
# "Goals" = "Goals",
# "Goal Assists" = "Goal_Assists",
# "GroundBall Gets" = "GroundBall_Gets",
# "Handballs" = "Handballs", 
# "Hitouts" = "Hitouts",
# "Hitouts To Advantage" = "Hitouts_To_Advantage",
# "Inside 50s" = "Inside_50s", 
# "Intercepts" = "Intercepts",
# "Intercept Marks" = "Intercept_Marks",
# "Kicks" = "Kicks", 
# "Marks Inside 50" = "Marks_Inside_50",
# "Metres Gained" = "Metres_Gained",
# "One Percenters" = "One_Percenters",
# "Pressure Acts" = "Pressure_Acts",
# "Rebounds" = "Rebounds",
# "Ruck Contests" = "Ruck_Contests",
# "Score Involvements" = "Score_Involvements",
# "Score Launches" = "Score_Launches",
# "Spoils" = "Spoils",
# "Stoppage Clearances" = "Stoppage_Clearances",
# "Tackles" = "Tackles", 
# "Tackles Inside 50" = "Tackles_Inside_50",
# "Time On Ground Percentage" = "Time_On_Ground_Percentage",
# "Turnovers" = "Turnovers", 
# "Uncontested_Possessions" = "Uncontested Possessions"
  
# https://stackoverflow.com/questions/31268581/shiny-passing-on-text-with-a-space-to-chart-does-not-work-but-it-works-without
  labels_x <- reactive({
    if (input$x =="Brownlow_Votes") return("Brownlow Votes")
    if (input$x =="Behinds") return("Behinds")
    if (input$x=="Bounces") return("Bounces")
    if (input$x=="Centre_Clearances") return("Centre Clearances")
    if (input$x=="Clangers") return("Clangers")
    if (input$x=="Clearances") return("Clearances")
    if (input$x=="Contest_Def_Losses") return("Contest Defensive Losses")
    if (input$x=="Contest_Def_One_On_Ones") return("Contest Defensive 1 on 1s")
    if (input$x=="Contest_Off_Wins") return("Contest Offensive Wins")
    if (input$x=="Contested_Possessions") return("Contested Possessions")
    if (input$x=="Def_Half_Pressure_Acts") return("Defensive Half Pressure Acts")
    if (input$x=="Disposals") return("Disposals")
    if (input$x=="Disposal_Effficiency") return("Disposal Effficiency")
    if (input$x=="Effective_Kicks") return("Effective Kicks")
    if (input$x=="Forward_50_GroundBall_Gets") return("Forward 50 GroundBall Gets")
    if (input$x=="Frees_For") return("Frees For")
    if (input$x=="Frees_Against") return("Frees_Against")
    if (input$x=="Goals") return("Goals")
    if (input$x=="Goal_Assists") return("Goal Assists")
    if (input$x=="GroundBall_Gets") return("GroundBall Gets")
    if (input$x=="Handballs") return("Handballs")
    if (input$x=="Hitouts") return("Hitouts")
    if (input$x=="Hitouts_To_Advantage") return("Hitouts To Advantage")
    if (input$x=="Inside_50s") return("Inside 50s")
    if (input$x=="Intercepts") return("Intercepts")
    if (input$x=="Intercept_Marks") return("Intercept Marks")
    if (input$x=="Kicks") return("Kicks")
    if (input$x=="Marks_Inside_50") return("Marks Inside 50")
    if (input$x=="Metres_Gained") return("Metres Gained")
    if (input$x=="One_Percenters") return("One Percenters")
    if (input$x=="Pressure_Acts") return("Pressure Acts")
    if (input$x=="Rebounds") return("Rebounds")
    if (input$x=="Ruck_Contests") return("Ruck Contests")
    if (input$x=="Score_Involvements") return("Score Involvements")
    if (input$x=="Score_Launches") return("Score Launches")
    if (input$x=="Spoils") return("Spoils")
    if (input$x=="Stoppage_Clearances") return("Stoppage Clearances")
    if (input$x=="Tackles") return("Tackles")
    if (input$x=="Tackles_Inside_50") return("Tackles Inside 50")
    if (input$x=="Time_On_Ground_Percentage") return("Time On Ground Percentage")
    if (input$x=="Turnovers") return("Turnovers")
    if (input$x=="Uncontested_Possessions") return("Uncontested Possessions")
    })

labels_y <- reactive({
  if (input$y =="Brownlow_Votes") return("Brownlow Votes")
  if (input$y =="Behinds") return("Behinds")
  if (input$y=="Bounces") return("Bounces")
  if (input$y=="Centre_Clearances") return("Centre Clearances")
  if (input$y=="Clangers") return("Clangers")
  if (input$y=="Clearances") return("Clearances")
  if (input$y=="Contest_Def_Losses") return("Contest Defensive Losses")
  if (input$y=="Contest_Def_One_On_Ones") return("Contest Defensive 1 on 1s")
  if (input$y=="Contest_Off_Wins") return("Contest Offensive Wins")
  if (input$y=="Contested_Possessions") return("Contested Possessions")
  if (input$y=="Def_Half_Pressure_Acts") return("Defensive Half Pressure Acts")
  if (input$y=="Disposals") return("Disposals")
  if (input$y=="Disposal_Effficiency") return("Disposal Effficiency")
  if (input$y=="Effective_Kicks") return("Effective Kicks")
  if (input$y=="Forward_50_GroundBall_Gets") return("Forward 50 GroundBall Gets")
  if (input$y=="Frees_For") return("Frees For")
  if (input$y=="Frees_Against") return("Frees_Against")
  if (input$y=="Goals") return("Goals")
  if (input$y=="Goal_Assists") return("Goal Assists")
  if (input$y=="GroundBall_Gets") return("GroundBall Gets")
  if (input$y=="Handballs") return("Handballs")
  if (input$y=="Hitouts") return("Hitouts")
  if (input$y=="Hitouts_To_Advantage") return("Hitouts To Advantage")
  if (input$y=="Inside_50s") return("Inside 50s")
  if (input$y=="Intercepts") return("Intercepts")
  if (input$y=="Intercept_Marks") return("Intercept Marks")
  if (input$y=="Kicks") return("Kicks")
  if (input$y=="Marks_Inside_50") return("Marks Inside 50")
  if (input$y=="Metres_Gained") return("Metres Gained")
  if (input$y=="One_Percenters") return("One Percenters")
  if (input$y=="Pressure_Acts") return("Pressure Acts")
  if (input$y=="Rebounds") return("Rebounds")
  if (input$y=="Ruck_Contests") return("Ruck Contests")
  if (input$y=="Score_Involvements") return("Score Involvements")
  if (input$y=="Score_Launches") return("Score Launches")
  if (input$y=="Spoils") return("Spoils")
  if (input$y=="Stoppage_Clearances") return("Stoppage Clearances")
  if (input$y=="Tackles") return("Tackles")
  if (input$y=="Tackles_Inside_50") return("Tackles Inside 50")
  if (input$y=="Time_On_Ground_Percentage") return("Time On Ground Percentage")
  if (input$y=="Turnovers") return("Turnovers")
  if (input$y=="Uncontested_Possessions") return("Uncontested Possessions")
})