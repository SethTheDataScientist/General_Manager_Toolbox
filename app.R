

theme_reach <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      
      # Base elements which are not used directly but inherited by others
      line =              element_line(colour = '#DADADA', size = 0.75, 
                                       linetype = 1, lineend = "butt"),
      rect =              element_rect(fill = "#F0F0F0", colour = "#F0F0F0", 
                                       size = 0.5, linetype = 1),
      text =              element_text(family = base_family, face = "plain",
                                       colour = "#656565", size = base_size,
                                       hjust = 0.5, vjust = 0.5, angle = 0, 
                                       lineheight = 0.9),
      
      # Modified inheritance structure of text element
      plot.title =        element_text(size = rel(1.5), family = '' , 
                                       face = 'bold', hjust = -0.05, 
                                       vjust = 1.5, colour = '#3B3B3B'),
      axis.text =         element_text(),
      
      # Modified inheritance structure of line element
      axis.ticks =        element_line(),
      panel.grid.major =  element_line(),
      panel.grid.minor =  element_blank(),
      
      # Modified inheritance structure of rect element
      plot.background =   element_rect(),
      panel.background =  element_rect(),
      legend.key =        element_rect(colour = '#DADADA'),
      
      # Modifiying legend.position
      legend.position = 'none',
      
      complete = TRUE
    )+
    theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14)
    )
}

#regular libraries
library(tidyverse) 
library(teamcolors) 
library(ggimage)
library(ggrepel)
library(ggthemes)
library(ggplot2)
library(knitr)
library(stringr)
library(shiny)
library(DT)
library(gt)
library(gtExtras)
library(DescTools)
library(lpSolve)
library(lpSolveAPI)
library(ROI)
library(ROI.plugin.glpk)
library(fmsb)

color_palette <- c("#e15759", "#edc948", "#59a14f")


current_season = 2024
NextSeason = current_season + 1
FutureSeason = NextSeason + 1

CollegeComps <- read_rds('data/WRCollegeComps.rds')

DraftCapital <- read_rds("data/DraftCapital.rds")

BigBoardPlayerValues <- read_rds("data/BigBoardPlayerValues.rds")

AllDraftCapital <- read_rds("data/AllDraftCapital.rds") %>% 
  mutate(FilterID = if_else(Season == current_season, paste0(Pick, "-", Code), ""))

DraftPickValueChart <- read_rds("data/DraftPickValueChart.rds")

IWARData <- read_rds("data/IndivWARData.rds") 

FreeAgentsList <- read_rds("data/FreeAgentsList.rds")

IWARname <- IWARData %>% 
  filter(player_id %in% FreeAgentsList) %>% 
  group_by(ID) %>% 
  slice_head(n = 1) %>% 
  select(ID)

DeclaresList <- read_rds("data/DeclaresListJoin.rds")

CWARFULL <- read_rds("data/CollegeWARJoin.rds")

CollegeWARJoin <- read_rds("data/CollegeWARJoinShort.rds")%>% 
  mutate(team_name.x = team_name) 

CWARname <- CollegeWARJoin %>% 
  group_by(ID) %>% 
  slice_head(n = 1) %>% 
  select(ID)

CollegeWRWARFill <- read_rds("data/CollegeWRWARFill.rds") %>% 
  mutate(team_name.x = team_name) 

CWARWRname <- CollegeWRWARFill %>% 
  inner_join(DeclaresList, by = c("player_id")) %>% 
  filter(position == "WR", 
         !is.na(Conference)) %>% 
  group_by(ID) %>% 
  mutate(LastSeason = max(season, na.rm = T),
         team_name.x = team_name) %>% 
  filter(LastSeason >= current_season - 2) %>% 
  slice_head(n = 1) %>% 
  select(ID)

CWARWRnameTE <- CollegeWRWARFill %>% 
  inner_join(DeclaresList, by = c("player_id")) %>% 
  filter(position == "TE", 
         !is.na(Conference)) %>% 
  group_by(ID) %>% 
  mutate(LastSeason = max(season, na.rm = T),
         team_name.x = team_name) %>% 
  filter(LastSeason >= current_season - 2) %>% 
  slice_head(n = 1) %>% 
  select(ID)


ShannonEntropy <- read_rds("data/ShannonEntropy.rds") 

PrimarySecondary <- read_rds("data/PrimarySecondary.rds")%>% 
  mutate(team_name.x = team_name)

PassRush <- read_rds("data/PassRushSummaryfill.rds")%>% 
  mutate(team_name.x = team_name)


PassBlock <- read_rds("data/PassBlockSummaryfill.rds")%>% 
  mutate(team_name.x = team_name)

OffCollegeWARJoin <- read_rds("data/OffCollegeWARJoin.rds")%>% 
  mutate(team_name.x = team_name) %>% 
  filter(position != "WR", position != "QB")

ExplosiveRate <- read_rds("data/ExplosiveRate.rds")%>% 
  mutate(team_name.x = team_name)

TeamDraftCapital <- read_rds("data/TeamDraftCapital.rds")


DraftStrengths <- read_rds("data/DraftStengths.rds")


RosterCap <- read_rds("data/RosterCap.rds")

RosterCap <- RosterCap %>% 
  left_join(TeamDraftCapital, by = c("team" = "Code"))

RosterProjections <- read_rds("data/RosterProjections.rds")

WARplots <- read_rds("data/WARplots.rds")


FreeAgents <- read_rds("data/FreeAgentsShort.rds") 

PositionsOfStrength <- read_rds("data/PositionsOfStrength.rds")


Rounds <- data.frame(
  Rounds = c("None", "1st", "2nd", "3rd", "4th",
            "5th", "6th", "7th"),
  RoundNum = c(0, 1, 2, 3, 4, 5, 6, 7))

Picks <- data.frame(
  Picks = c("",seq(1,265,1)))

Expectations <- data.frame(
  Exp = c("", "Default", "High (Pick 17-32)", "Low (Pick 1-16)"),
  PickNum = c(0, 16, 25, 9)
)


CollegeDefWARFill <- read_rds("data/CollegeDefWARFill.rds")

DefCollegeWAR <- CWARFULL %>% 
  group_by(player_id) %>% 
  summarise(WAR = mean(WAR, na.rm = T))

# Function to filter the dataframe for each value
filter_values <- function(df, value, range) {
  df %>%
    mutate(nearest_value = Rank[which.min(abs(Rank - value))], # Calculate nearest value
           distance_to_nearest = abs(Rank - nearest_value)) %>%  # Calculate distance to nearest value
    filter(between(row_number(), which(Rank == value) - range, which(Rank == value) + range)) %>%
    ungroup()
}


position_list = c(
  "CB",
  'S',
  "DE",
  "DT",
  'LB',
  'OC',
  'OT',
  'OG',
  'RB',
  "WR",
  'TE'
)



# CBMetrics <- CollegeDefWARFill %>% 
#   group_by(player_id) %>% 
#   mutate(CareerGrade = mean(grades_defense, na.rm = T)) %>%  
#   filter(CareerGrade >= 50, snap_counts_coverage >= 20, position == "CB",
#          season >= 2010)  %>% 
#   group_by(player,position, player_id) %>% 
#   summarise(Snaps = mean(snap_counts_coverage),
#             TPS = sum(targets)/Snaps,
#             PMR = sum(pass_break_ups + forced_fumbles + interceptions)/Snaps) %>% 
#   group_by() %>% 
#   mutate(TPS = 1 - percent_rank(TPS),
#          PMR = percent_rank(PMR)) %>% 
#   select(player_id, position, TPS,  PMR) %>%
#   left_join(DefCollegeWAR, by = c("player_id")) %>% 
#   mutate(CollegeWAR = percent_rank(WAR)+5,
#          Metric = percent_rank((TPS^3 * PMR^3 +
#                                   log(CollegeWAR)))) %>% 
#   select(player_id, position, Metric)
# 
# 
# SMetrics <- CollegeDefWARFill %>% 
#   group_by(player_id) %>% 
#   mutate(CareerGrade = mean(grades_defense, na.rm = T)) %>%  
#   filter(CareerGrade >= 50, snap_counts_coverage >= 20, position == "S",
#          season >= 2010)  %>% 
#   group_by(player, position, player_id) %>% 
#   summarise(Snaps = mean(snap_counts_coverage),
#             TPS = sum(targets)/Snaps,
#             TDPS = sum(touchdowns)/Snaps,
#             YAC = sum(yards_after_catch)/Snaps,
#             YPS = sum(yards)/Snaps,
#             PMR = sum(pass_break_ups + forced_fumbles + interceptions)/Snaps,
#             Stops = sum(stops)/Snaps,
#             Miss = sum(missed_tackles)/Snaps) %>% 
#   group_by() %>% 
#   mutate(Snaps = percent_rank(Snaps),
#          TPS = 1 - percent_rank(TPS),
#          TDPS = 1 - percent_rank(TDPS),
#          YAC = 1 - percent_rank(YAC),
#          YPS = 1 - percent_rank(YPS),
#          PMR = percent_rank(PMR),
#          Stops = percent_rank(Stops),
#          Miss = 1 - percent_rank(Miss)) %>% 
#   select(player_id, position, YPS, TDPS, Miss, PMR) %>%
#   left_join(DefCollegeWAR, by = c("player_id")) %>% 
#   mutate(CollegeWAR = percent_rank(WAR)+5,
#          Metric = percent_rank((YPS^4 * TDPS^5 * PMR^10 * Miss^8 + log(CollegeWAR)))) %>% 
#   select(player_id, position, Metric)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("GM Toolbox"),
  
  fluidRow( 
    tabsetPanel(
      tabPanel("Intro Page",
               fluidRow(
                 column(width = 10, offset = 1,
                        helpText(HTML("This shiny app is designed for working with overall team building and draft philosophy. The tabs go into current team construction, free agency, and multiple methods to evaluate draft prospects. An important point to note is that these values are derived from both effectiveness and volume, so during the season these values may seem abnormally low for players.</br>
                        </br>
                        
                                   To check out my other shiny apps, follow one of the following links.</br>
                                   </br>

        <a href='https://seththedatascientist.shinyapps.io/QB_Bayesian_Updating/'>Bayesian Updating of composite metrics for Quarterback play for NFL and College QBs</a></br>

        This shiny app displays both college and pro QBs in two composite metrics that show not only their relative playstyles, but also how those values change over their careers. These values have a high correlation from college to pro for predicting playstyle once in the NFL</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/General_Manager_Toolbox/'>General Manager's Toolbox: A collection of tools to help analyze an NFL Team's Offseason moves.</a></br>

        This shiny app goes over a handful of useful data points that I have found very helpful for analyzing a team's offseason moves, including draft trade calculators (with some linear programming to try and ensure extra value by comparing the Jimmy Johnson trade chart to the Wins Above Replacement values), created metrics to analyze draft prospects in further detail, and team breakdowns of their effective cap and team structure over the coming years.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/Offense_And_Defense_Metrics/'>Collection of Offense & Defense efficiency and playstyle metrics for the NFL</a></br>

        This shiny app includes a number of metrics used to understand Offense and Defense in further detail including down conversion values of how often you are allowing a first down or touchdown based on what down it currently is, explosive play rates, big throws and misses by quarterbacks, and more. Most metrics include a feature to isolate a playcaller's history of that metric across all teams they were the playcaller for.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/Season_EPA_Tracker/'>Timeline of play measuring efficiency metrics, team season-long rankings, and team tier plots</a></br>

        This shiny app includes many iterations of breaking down expected points added (EPA) adjusted based on opponent strength and situation. Season long graphs to see individual team or starting quarterback trends, team plots for offense and defense including splits for passing and rushing, and a metric for team strength based on the relative margin of domination during the game as well as opponent strength.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/WAR_positiongroup/'>Position Wins Above Replacement graphs by team and Watch Index</a></br>

        This shiny app shows Wins Above Replacement (WAR) values and plots for both college and pro broken down into many useful facets like by position group, team, and individual player. Includes receiver custom metrics plotted to compare players both within college and pro, as well as a customizable Watch Index which assigns a values based on relative values of excitement and closeness.</br>
        </br>
                  
                                       To check some of my other work, check out my <a href='https://twitter.com/SethDataScience'>Twitter</a>, <a href='https://www.linkedin.com/in/sethlanza/'>Linkedin</a>, or <a href='https://sites.google.com/view/seth-lanza-portfolio'>Portfolio</a>")),
                 )
               ),
               fluidRow(
                 column(width = 10, offset = 1,
                        textOutput("lastDeploymentTime")
                 )
               )
          ),
      tabPanel("Team Roster Check", 
               fluidRow(
                 column(width = 3,
                        selectizeInput(inputId = "FATeam",
                                       label = "Select Team to Analyze",
                                       choices = sort(unique(RosterCap$team)),
                                       selected = "BUF"),
                 ),
                 column(width = 2, submitButton()),
                 
               ), 
               fluidRow(
                 column(width = 10, offset = 1,
                        helpText(HTML("All Data From Over the Cap. Effective Cap Space is assuming you filled out the remaining players up to 51 roster spots with a cap hit of $1 million. Cap after Cuts and Cap After Easy are the Effective Cap space plus expected cuts or restructures. Only Restrucutre and Cut Targets were selected for this table. This Table also includes Draft Capital in the 2023 Draft for the Selected Team. JJ refers to the Jimmy Johnson Trade Value Chart and WAR refers to the PFF expected WAR by draft pick. Value is the Percentile of Draft Capital or Effective Cap Space")),
                 )
               ),
               fluidRow(
                 column(
                   dataTableOutput("Captable"),
                   width = 10, offset = 1
                 )
               ),
               fluidRow(
                 column(width = 10, offset = 1,
                        helpText(HTML("These are the Players for the Selected team. The Dead Cap and Cap Savings are for the Projection. Check Over the Cap for more detail.")),
                 )
               ),
               fluidRow(
                 column(
                   dataTableOutput("TeamCaptable"),
                   width = 10, offset = 1
                 )
               ),
               fluidRow(
                 column(width = 10, offset = 1,
                        helpText(HTML("The Values below are based on WAR over the last three years to avoid over-valuing one year spikes. The Black Line is the Effective WAR Values assuming that you signed back up to the league average at the position with an average WAR player. If the Black Line Increases it means that the average player at that position is a positve value. </br>
                        The Red line is the players under contract. If the Red Line increases it means that the players who are leaving were negative WAR and should probably leave. If the Red Line decreases it means that the players who are leaving are positve WAR players. </br>
                                      The Black Dashed Line is the Average Team-WAR at that position and the Blue Dashed line is the Max Team-WAR in 2022 at that position.")),
                 )
               ),
               fluidRow(
                 column(
                   plotOutput("TeamPositionPlot", height = 700),
                   width = 10, offset = 1
                 ),
               ),
               fluidRow(
                 column(width = 10, offset = 1,
                        helpText(HTML("Free Agents and Cut/Trade Targets. ExpAPY is a calculation based on a PFF article correlating two year WAR percentile to APY.")),
                 )
               ),
               fluidRow(
                 column(
                   dataTableOutput("FreeAgentstable"),
                   width = 10, offset = 1
                 )
               ),
               fluidRow(
                 column(width = 3, offset = 1,
                        selectizeInput(inputId = "Player",
                                       label = "Player Selection", 
                                       choices = IWARname,
                                       selected = "Jakobi Meyers - 47468")),
                 
                 column(width = 2, submitButton()),
               ),
               fluidRow(
                 column(
                   plotOutput("PlayerWARPlot", height = 700),
                   width = 10, offset = 1
                 ),
               ),
               fluidRow(
                 column(width = 10, offset = 1,
                        helpText(HTML("Positions of Strength in Free Agency")),
                 )
               ),
               fluidRow(
                 column(
                   dataTableOutput("FAStrengthtable"),
                   width = 10, offset = 1
                 )
               ),
      ), 
      tabPanel("Draftable Player Evaluation",
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML("The below WAR Table allows you to search College Players by WAR values for an individual player/season/position/etc. The WARTiles are the percentiles of WAR values per position. <br>
                Transfer and Seasons are based on the data available. So if the player's team in their last year was different than their first year in the dataset they are labelled transfer, and number of seasons is on years of data available as well. There will be players who transferred from the FCS level or players who didn't have enough snaps early in their career to meet the treshold and therefore those years won't show up properly. <br>
                                             Big Board Rank is based on Jack Lichtenstein's (@jacklich10) Consensus Big Board: https://jacklich10.xyz/bigboard/nfl/")))),
               fluidRow(
                 column(
                   dataTableOutput("CWARtable"),
                   width = 10, offset = 1
                 )
               ),
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML("Select a player to see their WAR plot over their College Career.")))),
               fluidRow(
                 column(width = 3, offset = 1,
                        selectizeInput(inputId = "CPlayer",
                                       label = "Player Selection", 
                                       choices = CWARname,
                                       selected = "Xavier Hutchinson - 122518")),
                 
                 column(width = 2, submitButton()),
                 ),
               fluidRow(
                 column(
                   plotOutput("CPlayerWARPlot", height = 700),
                   width = 10, offset = 1)
               ),
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML("This is based on the Consensus Big Board rankings sorted by lowest expected picks with the percent of that position that is Top50/100.")))),
               fluidRow(
                 column(
                   dataTableOutput("DraftStrengthstable"),
                   width = 10, offset = 1
                 )
               ),
      ),
      tabPanel("Offense Extra Evaluation",
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML("This is for my extra analysis on Offense players. This table is the split between the primary and secondary WAR generated. For TEs, primary is Receiving WAR, secondary is Run/PassBlocking WAR. For OL, primary is Pass-Blocking WAR, secondary is Run-Blocking WAR. For HBs, primary is Rushing WAR, secondary is Receiving WAR.")))),
               fluidRow(
                 column(
                   dataTableOutput("PrimarySecondaryOfftable"),
                   width = 10, offset = 1
                 )
               ),
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML("Looking at OL specifically based on their ability and production on True Pass Sets (PFF Term. Basically filters out Screens, Quick Passes, Play-Action, etc.)")))),
               fluidRow(
                 column(
                   dataTableOutput("PassBlocktable"),
                   width = 10, offset = 1
                 )
               ),
               fluidRow(column(width = 10, offset = 1,
                                 helpText(HTML("Running Back analysis consists of Missed Tackle Forced Rate, Explosive Play Rate, and Yards After Contact Per Attempt.")))),
               fluidRow(
                 column(
                   dataTableOutput("ExplosiveTable"),
                   width = 10, offset = 1
                 )
               ),
      ),
      tabPanel("WR/TE Extra Evaluation",
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML("This is for my extra analysis on WR specifically. This is based on a mix of my personal opinions and clustering work done by Kevin Cole while at PFF. The Contested Tile is based on not having Contested Targets, so a Higher Percentile means fewer Contested Targets. I have rough Cutoffs for WR being greater than or equal to 5'11 (71 inches) and above 195lbs. This creates a density (Lbs/Inches) of about 2.75. I prefer my WR to be Separators rather than contested catch monsters and Ideally filling out the Radar Chart as much as possible, but I will lean towards guys who can command targets and efficiency on a per-route basis.</br> </br>
                                    The Filter values refer to different thresholds that I have found filter out a lot of players that don't work out in the league but show up highly in terms of efficiency. Gadget refers to players with an ADOT of less than around the 20th percentile. NonSeparators refer to players that have most of their seasons as non-separators according to contested target rate and adot.</br></br>")))),
               fluidRow(
                 column(width = 3, offset = 1,
                        numericInput(inputId = "target",
                                       label = "Targets Threshold for WR", 
                                     value = 20,
                                       min = 0,
                                     max = 100,
                                     step = 1)
                        ),
                 column(width = 3, offset = 1,
                        numericInput(inputId = "grade",
                                     label = "PFF Grade Threshold for WR", 
                                     value = 60,
                                     min = 30,
                                     max = 80,
                                     step = 5)
                 ),
                 column(width = 2, submitButton()),
               ),
               fluidRow(
                 column(
                   dataTableOutput("Separatortable"),
                   width = 10, offset = 1
                 )
               ),
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML("Select a player to see their Radar plot over their College Career. The Acronymns starting at the top and going counter-clockwise are Routes Run, Targets/Route Run, Yards/Route Run, Touchdowns/Route Run, Average Depth of Target, Yards After Catch/Reception")))),
               fluidRow(
                 column(width = 3, offset = 1,
                        selectizeInput(inputId = "Cplayer2",
                                       label = "Player Selection", 
                                       choices = CWARWRname,
                                       selected = "Marvin Harrison - 145056")),
                 
                 column(width = 2, submitButton()),
                 ),
               fluidRow(
                 column(
                   plotOutput("WRClusterPlot", height = 700),
                   width = 10, offset = 1)
               ),
               fluidRow(
                 column(width = 10, offset = 1,
                        helpText(html("Finally, these are the top 5 NFL player comps based on their athletic profile, the metrics from the separator table, and the metrics from the radar chart. Similarity is the percentile of similarity across all the players, Expected WAR is the NFL player's average WAR times the similarity percentage, and Mean Expected WAR is the average of the top 5 player's comps.")))
               ),
               fluidRow(
                 column(
                   gt_output("WRCompTable"),
                   width = 10, offset = 1)
               ),
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML("I have rough Cutoffs for TE being greater than or equal to 6'4'' (76 inches) and above 250lbs. This creates a density (Lbs/Inches) of about 3.20.")))),
               
               fluidRow(
                 column(
                   dataTableOutput("SeparatortableTE"),
                   width = 10, offset = 1
                 )
               ),
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML("Select a player to see their Radar plot over their College Career. The Acronymns starting at the top and going counter-clockwise are Routes Run, Targets/Route Run, Yards/Route Run, Touchdowns/Route Run, Average Depth of Target, Yards After Catch/Reception")))),
               fluidRow(
                 column(width = 3, offset = 1,
                        selectizeInput(inputId = "Cplayer3",
                                       label = "Player Selection", 
                                       choices = CWARWRnameTE,
                                       selected = "Brock Bowers - 146715")),
                 
                 column(width = 2, submitButton()),
               ),
               fluidRow(
                 column(
                   plotOutput("TEClusterPlot", height = 700),
                   width = 10, offset = 1)
               )
      ),
      tabPanel("Defense Extra Evaluation",
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML("This is for my extra analysis on Defense players. The first table is the Shannon Entropy based on where they line up pre-snap. A higher number means more uncertainty about where they will start, meaning they moved about a lot. Additionally, for Safeties and Corners, there is a Coverage metric which is a percentile of a custom composite metric which has a high correlation from College to Pro.")))),
               fluidRow(
                 column(
                   dataTableOutput("Shannontable"),
                   width = 10, offset = 1
                 )
               ),
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML("This table is the split between the primary and secondary WAR generated. For DBs, primary is Man Coverage WAR, secondary is Zone Coverage WAR. For DL, primary is Pass-Rush WAR, secondary is Run Defense WAR. For LBs, primary is Coverage WAR, secondary is Run Defense WAR. Playmaker rate is the total number of Interceptions and Pass Break Ups a player had divided by their number of targets.")))),
               fluidRow(
                 column(
                   dataTableOutput("PrimarySecondarytable"),
                   width = 10, offset = 1
                 )
               ),
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML("Looking at DL specifically based on their ability and production on True Pass Sets (PFF Term. Basically filters out Screens, Quick Passes, Play-Action, etc.)")))),
               fluidRow(
                 column(
                   dataTableOutput("PassRushtable"),
                   width = 10, offset = 1
                 )
               ),
      ),
      tabPanel("Early Round Draft Optimizer",
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML("Select the inputs for a team, positions you want to consider, and the maximum number of each position you want.                         These are the final selections based on the linear optimization of the team's draft picks in a prospect agnostic model. This is simply a smoothed curve of the average NFL WAR from each position at each pick. For example, players at OC that go around pick 25 have historically earned a career average WAR of about 0.05 per season.")))),
               fluidRow(
                 column(width = 2, offset = 1,
                        selectizeInput(inputId = "TeamOpto",
                                       label = "Team Selection", 
                                       choices = unique(DraftCapital$Code),
                                       selected = "BUF")),
                 
                 column(width = 2,
                        helpText(HTML("Team's Picks you want to Exclude.")),
                        uiOutput("PickExcludeOpto"),
                 ),
                 column(width = 2, offset = 1,
                        checkboxGroupInput(inputId = "PosSelectOpto",
                                       label = "Position Selection", 
                                       choices = position_list,
                                       inline = T,
                                       selected = c("WR", "OT", "CB", "S", "LB"))),
                 column(width = 2, offset = 1,
                        numericInput(inputId = "PosCountOpto",
                                           label = "Max Players at each position", 
                                     value = 2,
                                     min = 1,
                                     max = 3,
                                     step = 1
                                           )),
                 column(width = 2, submitButton()),
               ),
               fluidRow(
                 column(
                   dataTableOutput("FinalSelections"),
                   width = 10, offset = 1
                 )
               ),
               fluidRow(column(width = 10, offset = 1,
                               helpText(HTML(" The following table shows the expected players within a range of the draft picks for said team and their expected value.")))),
               fluidRow(
                 column(
                   dataTableOutput("FilteredSelections"),
                   width = 10, offset = 1
                 )
               ),
      ),
      tabPanel("Draft Trade Calculator", 
               # Sidebar with a slider input for number of bins 
               fluidRow(
                 column(width = 3,
                        helpText(HTML("Pick Numbers of Trade Up")),
                        selectizeInput(inputId = "TeamOutPickNumber1",
                                       label = paste0("First  ", current_season, " Pick"),
                                       choices = Picks$Picks,
                                       selected = "None"),
                        selectizeInput(inputId = "TeamOutPickNumber2",
                                       label = paste0("Second  ", current_season, " Pick"),
                                       choices = Picks$Picks,
                                       selected = "None"),
                        selectizeInput(inputId = "TeamOutPickNumber3",
                                       label = paste0("Third  ", current_season, " Pick"),
                                       choices = Picks$Picks,
                                       selected = "None"),
                        selectizeInput(inputId = "TeamOutPickNumber4",
                                       label = paste0("Fourth  ", current_season, " Pick"),
                                       choices = Picks$Picks,
                                       selected = "None"),
                        selectizeInput(inputId = "TeamOutPickNumber5",
                                       label = paste0("Fifth  ", current_season, " Pick"),
                                       choices = Picks$Picks,
                                       selected = "None"),
                 ),
                 
                 column(width = 3),
                 column(width = 3,
                        helpText(HTML("Pick Numbers of Trade Down")),
                        selectizeInput(inputId = "TeamInPickNumber1",
                                       label = paste0("First  ", current_season, " Pick"),
                                       choices = Picks$Picks,
                                       selected = "None"),
                        selectizeInput(inputId = "TeamInPickNumber2",
                                       label = paste0("Second  ", current_season, " Pick"),                                    
                                       choices = Picks$Picks,
                                       selected = "None"),
                        selectizeInput(inputId = "TeamInPickNumber3",
                                       label = paste0("Third  ", current_season, " Pick"),
                                       choices = Picks$Picks,
                                       selected = "None"),
                        selectizeInput(inputId = "TeamInPickNumber4",
                                       label = paste0("Fourth  ", current_season, " Pick"),
                                       choices = Picks$Picks,
                                       selected = "None"),
                        selectizeInput(inputId = "TeamInPickNumber5",
                                       label = paste0("Fifth  ", current_season, " Pick"),
                                       choices = Picks$Picks,
                                       selected = "None"),
                 ),
                 
                 column(width = 2, submitButton()),
               
               
        ),
        
          
        
        
        checkboxInput(inputId = "NextYear",
                      label = paste0(NextSeason, " Draft Picks Included?")),
        conditionalPanel(
          condition = "input.NextYear == true",
          
          
        fluidRow(
          column(width = 2,
                 helpText(HTML("Future Year Draft Discounted 75% Value")),
                 selectizeInput(inputId = "TeamOutPickRound6",
                                label = paste0("First  ", NextSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
                 selectizeInput(inputId = "TeamOutPickRound7",
                                label = paste0("Second  ", NextSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
                 selectizeInput(inputId = "TeamOutPickRound8",
                                label = paste0("Third  ", NextSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
                 selectizeInput(inputId = "TeamOutPickRound9",
                                label = paste0("Fourth  ", NextSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
          ),
          column(width = 2,
                 helpText(HTML("Season Expectations: High, Low, Default")),
                 selectizeInput(inputId = "TeamOutExp1",
                                label = paste0("First  ", NextSeason, " Pick"),
                                choices = Expectations$Exp,
                                selected = "None"),
                 selectizeInput(inputId = "TeamOutExp2",
                                label = paste0("Second  ", NextSeason, " Pick"),
                                choices = Expectations$Exp,
                                selected = "None"),
                 selectizeInput(inputId = "TeamOutExp3",
                                label = paste0("Third  ", NextSeason, " Pick"),
                                choices = Expectations$Exp,
                                selected = "None"),
                 selectizeInput(inputId = "TeamOutExp4",
                                label = paste0("Fourth  ", NextSeason, " Pick"),
                                choices = Expectations$Exp,
                                selected = "None"),
          ),
          
          column(width = 2),
          column(width = 2,
                 helpText(HTML("Future Year Draft Discounted 75% Value")),
                 selectizeInput(inputId = "TeamInPickRound6",
                                label = paste0("First  ", NextSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
                 selectizeInput(inputId = "TeamInPickRound7",
                                label = paste0("Second  ", NextSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
                 selectizeInput(inputId = "TeamInPickRound8",
                                label = paste0("Third  ", NextSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
                 selectizeInput(inputId = "TeamInPickRound9",
                                label = paste0("Fourth  ", NextSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
          ),
          column(width = 2,
                 helpText(HTML("Season Expectations: High, Low, Default")),
                 selectizeInput(inputId = "TeamInExp1",
                                label = paste0("First  ", NextSeason, " Pick"),
                                choices = Expectations$Exp,
                                selected = "None"),
                 selectizeInput(inputId = "TeamInExp2",
                                label = paste0("Second  ", NextSeason, " Pick"),
                                choices = Expectations$Exp,
                                selected = "None"),
                 selectizeInput(inputId = "TeamInExp3",
                                label = paste0("Third  ", NextSeason, " Pick"),
                                choices = Expectations$Exp,
                                selected = "None"),
                 selectizeInput(inputId = "TeamInExp4",
                                label = paste0("Fourth  ", NextSeason, " Pick"),
                                choices = Expectations$Exp,
                                selected = "None"),
          )
        )
        ),
        
        
        checkboxInput(inputId = "TwoYears",
                      label = paste0(FutureSeason, " Picks Included?")),
        conditionalPanel(
          condition = "input.TwoYears == true",
        
        fluidRow(
          column(width = 2,
                 helpText(HTML("Future Year Draft Discounted 75% Twice")),
                 selectizeInput(inputId = "TeamOutPickRound10",
                                label = paste0("First  ", FutureSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
                 selectizeInput(inputId = "TeamOutPickRound11",
                                label = paste0("Second  ", FutureSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
                 selectizeInput(inputId = "TeamOutPickRound12",
                                label = paste0("Third  ", FutureSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
          ),
          
          column(width = 4),
          column(width = 2,
                 helpText(HTML("Future Year Draft Discounted 75% Twice")),
                 selectizeInput(inputId = "TeamInPickRound10",
                                label = paste0("First  ", FutureSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
                 selectizeInput(inputId = "TeamInPickRound11",
                                label = paste0("Second  ", FutureSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
                 selectizeInput(inputId = "TeamInPickRound12",
                                label = paste0("Third  ", FutureSeason, " Pick"),
                                choices = Rounds$Rounds,
                                selected = "None"),
          ),
        )
        ),
        fluidRow(
          column(width = 10, offset = 1,
                 helpText(HTML("This is from the perspective of the Team Trading Up. A negative Difference means that you are spending that much extra, while positve value means that you are gaining value.")),
          )
        ),
        fluidRow(
          column(
            dataTableOutput("Tradetable"),
            width = 10, offset = 1
          )
        ),
    ),
    
    tabPanel("Trade Up Proposals", 
             fluidRow(
               column(width = 3,
                      helpText(HTML("Team that is Trading Up")),
                      selectizeInput(inputId = "TeamUp",
                                     label = "Trade Up Team",
                                     choices = sort(unique(AllDraftCapital$Code)),
                                     selected = "BUF"),
               ),
               
               column(width = 3),
               column(width = 3,
                      helpText(HTML("Select the Pick you want to Trade Up For")),
                      selectizeInput(inputId = "TeamDown",
                                     label = "Pick Number of Trade",
                                     choices = (AllDraftCapital$FilterID)),
               ),
               
               column(width = 2, submitButton()),
               
               
             ),
             
                      checkboxInput(inputId = "AltSettings",
                                    label = "Adjust Settings of the Optimization"),
                      conditionalPanel(
                        condition = "input.AltSettings == true",
                        
                        
                        fluidRow(
                          column(width = 2,
                                 helpText(HTML("Trade Up Team's Picks you want to Exclude. (Season-Round-Pick)")),
                                 uiOutput("filterdPicksUp"),
                          ),
                          column(width = 2,
                                 helpText(HTML("Trade Down Team's Picks you want to Exclude. (Season-Round-Pick)")),
                                 uiOutput("filterdPicksDown"),
                          ),
                          column(width = 2,
                                 helpText(HTML("Most Picks Allowed by one Team (Default is 4)")),
                                 selectizeInput(inputId = "MaxPicks",
                                                label = "",
                                                choices = c(1:12),
                                                selected = 4),
                          ),
                          column(width = 2,
                                 helpText(HTML("Largest Difference in Picks (Default is 3)")),
                                 selectizeInput(inputId = "PickDiff",
                                                label = "",
                                                choices = c(1:12),
                                                selected = 3),
                          ),
                          column(width = 2,
                                 helpText(HTML("Minimum JJ Value Advantage for the Trade Down Team (From the Trade Up Team's Perspective. Default is -75 points)")),
                                 selectizeInput(inputId = "MinJJ",
                                                label = "",
                                                choices = c(-75, seq(-1000, 1000, 100)),
                                                selected = -75),
                          ),
                          column(width = 2,
                                 helpText(HTML("Maximum JJ Value Advantage for the Trade Down Team (From the Trade Up Team's Perspective. Default is -500 points)")),
                                 selectizeInput(inputId = "MaxJJ",
                                                label = "",
                                                choices = c(seq(-1000, 1000, 100)),
                                                selected = -500),
                          ),
                        )
                      ),
             
             
             fluidRow(
               column(width = 6, offset = 3,
                      helpText(HTML("This is the Trade Package that the model recommends. If there is no data, it means the model could not meet all the constraints. Default Settings are to give the Trade Down team an edge in the Jimmy Johnson Trade Chart, while maximizing the WAR value for the Trade Up Team.")),
               )
             ),
             fluidRow(
               column(
                 dataTableOutput("TradeUptable"),
                 width = 6, offset = 3
               )
             ),
             fluidRow(
               column(width = 10, offset = 1,
                      helpText(HTML("This is from the perspective of the Team Trading Up. A negative Difference means that you are spending that much extra, while positve value means that you are gaining value.")),
               )
             ),
             fluidRow(
               column(
                 dataTableOutput("Tradetable1"),
                 width = 10, offset = 1
               )
             ),
      ),
    
    tabPanel("Trade Down Proposals", 
             fluidRow(
               column(width = 3,
                      helpText(HTML("Team that wants to Trade Down")),
                      selectizeInput(inputId = "DownSelection",
                                     label = "Trade Down Team",
                                     choices = sort(unique(AllDraftCapital$Code)),
                                     selected = "BUF"),
               ),
               
               column(width = 3,
                      helpText(HTML("Select the Pick you want to Trade Down From")),
                      uiOutput("DownPickSelection"),
               ),
               
               column(width = 2, submitButton()),
               
               
             ),
             checkboxInput(inputId = "AltSettings1",
                           label = "Adjust Settings of the Optimization"),
             conditionalPanel(
               condition = "input.AltSettings1 == true",
               
               
               fluidRow(
               column(width = 2,
                      helpText(HTML("Trade Down Team's Picks you want to Exclude. (Season-Round-Pick)")),
                      uiOutput("filtered_optionsDown2"),
               ),
                 column(width = 2,
                        helpText(HTML("Most Picks Allowed by one Team (Default is 4)")),
                        selectizeInput(inputId = "MaxPicks1",
                                       label = "",
                                       choices = c(1:12),
                                       selected = 3),
                 ),
                 column(width = 2,
                        helpText(HTML("Largest Difference in Picks (Default is 3)")),
                        selectizeInput(inputId = "PickDiff1",
                                       label = "",
                                       choices = c(1:12),
                                       selected = 2),
                 ),
                 column(width = 2,
                        helpText(HTML("Minimum JJ Value Advantage for the Trade Down Team (From the Trade Up Team's Perspective. Default is -75 points)")),
                        selectizeInput(inputId = "MinJJ1",
                                       label = "",
                                       choices = c(-75, seq(-1000, 1000, 100)),
                                       selected = -75),
                 ),
                 column(width = 2,
                        helpText(HTML("Maximum JJ Value Advantage for the Trade Down Team (From the Trade Up Team's Perspective. Default is -500 points)")),
                        selectizeInput(inputId = "MaxJJ1",
                                       label = "",
                                       choices = c(seq(-1000, 1000, 100)),
                                       selected = -500),
                 ),
               )
             ),
             
             fluidRow(
               column(width = 10, offset = 1,
                      helpText(HTML("This is from the perspective of the Team Trading Down. All future year's picks are assumed to be in the middle of the round.")),
               )
             ),
             fluidRow(
               column(
                 dataTableOutput("TradeDowntable"),
                 width = 10, offset = 1
               )
             ),
             
    ),
    tabPanel("Draft Pick Value Chart",
                fluidRow(
                  column(width = 10, offset = 1,
                         helpText(HTML("This is a table showing the Current Season's Draft Order, along with the Jimmy Johnson Pick Value and the PFF WAR Pick value and standard deviation. The PFF WAR Values are on a four year scale, so for a single year's expected value from that pick it is roughly 1/4 of the value shown.")),
                  )
                ),
                fluidRow(
                  column(
                    dataTableOutput("DraftPickValueChart"),
                    width = 10, offset = 1
                  )
                )
              ),
    
    )                 
    
    
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  getLastDeploymentTime <- function() {
    timestamp <- tryCatch(
      readLines("deployment_timestamp.txt"),
      error = function(e) NA
    )
    if (!is.na(timestamp)) {
      as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    } else {
      NA
    }
  }
  
  # Display the last deployment time
  output$lastDeploymentTime <- renderText({
    lastDeploymentTime <- getLastDeploymentTime()
    if (!is.na(lastDeploymentTime)) {
      paste("Last Deployment Time: ", format(lastDeploymentTime, "%Y-%m-%d %H:%M:%S"))
    } else {
      "Deployment time not available."
    }
  })
  
  output$Tradetable = DT::renderDataTable({
    
    
    TradeIn <- data.frame(
      Picks = c(input$TeamInPickNumber1,input$TeamInPickNumber2,input$TeamInPickNumber3,input$TeamInPickNumber4,input$TeamInPickNumber5,
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamInPickRound6]-1) * 32) + 
                  Expectations$PickNum[Expectations$Exp == input$TeamInExp1],
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamInPickRound7]-1) * 32) +
                  Expectations$PickNum[Expectations$Exp == input$TeamInExp2],
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamInPickRound8]-1) * 32) +
                  Expectations$PickNum[Expectations$Exp == input$TeamInExp3],
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamInPickRound9]-1) * 32) +
                  Expectations$PickNum[Expectations$Exp == input$TeamInExp4],
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamInPickRound10]-1) * 32) + 16,
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamInPickRound11]-1) * 32) + 16,
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamInPickRound12]-1) * 32) + 16),
      Next = c(0,0,0,0,0,1,1,1,1,0,0,0),
      TwoOut = c(0,0,0,0,0,0,0,0,0,1,1,1)) %>% 
      filter(Picks != 0, Picks > 0, Picks != "")
    
    TradeOut <- data.frame(
      Picks = c(input$TeamOutPickNumber1,input$TeamOutPickNumber2,input$TeamOutPickNumber3,input$TeamOutPickNumber4,input$TeamOutPickNumber5,
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamOutPickRound6]-1) * 32) + 
                  Expectations$PickNum[Expectations$Exp == input$TeamOutExp1],
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamOutPickRound7]-1) * 32) +
                  Expectations$PickNum[Expectations$Exp == input$TeamOutExp2],
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamOutPickRound8]-1) * 32) +
                  Expectations$PickNum[Expectations$Exp == input$TeamOutExp3],
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamOutPickRound9]-1) * 32) +
                  Expectations$PickNum[Expectations$Exp == input$TeamOutExp4],
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamOutPickRound10]-1) * 32) + 16,
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamOutPickRound11]-1) * 32) + 16,
                ((Rounds$RoundNum[Rounds$Rounds == input$TeamOutPickRound12]-1) * 32) + 16),
      Next = c(0,0,0,0,0,1,1,1,1,0,0,0),
      TwoOut = c(0,0,0,0,0,0,0,0,0,1,1,1)) %>% 
      filter(Picks != 0, Picks > 0, Picks != "")
  
  TradeInValue <- DraftCapital[match(TradeIn$Picks, DraftCapital$Pick),]  %>%
    mutate(JJ = case_when(TradeIn$TwoOut == 1 ~ JJDiscount2,
                          TradeIn$Next == 1 ~ JJDiscount,
                          T ~ JJValue),
           WAR = case_when(TradeIn$TwoOut == 1 ~ WARDiscount2,
                           TradeIn$Next == 1 ~ WARDiscount,
                           T ~ WAR)) %>% 
    summarise(JJIn = sum(JJ),
              WARIn = sum(WAR),
              Std = sqrt(sum(`War STD`^2)),
              Value = (rnorm(100000, mean = WARIn, sd = Std))) 
  
  
  TradeOutValue <- DraftCapital[match(TradeOut$Picks, DraftCapital$Pick),] %>%
    mutate(JJ = case_when(TradeOut$TwoOut == 1 ~ JJDiscount2,
                          TradeOut$Next == 1 ~ JJDiscount,
                          T ~ JJValue),
           WAR = case_when(TradeOut$TwoOut == 1 ~ WARDiscount2,
                           TradeOut$Next == 1 ~ WARDiscount,
                           T ~ WAR)) %>% 
    summarise(JJOut = sum(JJ),
              WARIn = sum(WAR),
              Std = sqrt(sum(`War STD`^2)),
              Value = (rnorm(100000, mean = WARIn, sd = Std))) 
  
  TradeTotalValue <- TradeInValue %>% 
    mutate(OutJJ = TradeOutValue$JJOut,
           JJDiff = JJIn - OutJJ,
           OutValue = TradeOutValue$Value,
           Diff = Value - OutValue,
           Win = if_else(Diff > 0 ,0, 1)) %>% 
    group_by() %>% 
    summarise(JJIn = mean(JJIn),
              JJOut = mean(OutJJ),
              JJDiff = mean(JJDiff),
              JJEquivalentPick = DraftCapital$Pick[which.min(abs(DraftCapital$JJValue - abs(JJDiff)))],
              JJEquivalentRound = ceiling(JJEquivalentPick/32),
              InValue = round(mean(Value),2),
              OutValue = round(mean(OutValue),2),
              AvgDiff = round(mean(Diff),2),
              WAREquivalentPick = DraftCapital$Pick[which.min(abs(DraftCapital$WAR - abs(AvgDiff)))],
              WAREquivalentRound = ceiling(WAREquivalentPick/32),
              WinPerc = paste0(round(sum(Win)/100000,4)*100, "% of the Time"))
  
  TradeTable <- data.frame(
    Type = c("JJ", "WAR", "Winner"),
    Up = c(TradeTotalValue$JJOut, TradeTotalValue$OutValue, "JJ"),
    Down = c(TradeTotalValue$JJIn, TradeTotalValue$InValue, if_else(TradeTotalValue$JJEquivalentPick < TradeTotalValue$WAREquivalentPick, ">", "<")),
    Difference = c(TradeTotalValue$JJDiff, TradeTotalValue$AvgDiff, "WAR"),
    EquivalentRound = c(TradeTotalValue$JJEquivalentRound, TradeTotalValue$WAREquivalentRound, "Trade Down Wins WAR Simulation"),
    EquivalentPick = c(TradeTotalValue$JJEquivalentPick, TradeTotalValue$WAREquivalentPick, TradeTotalValue$WinPerc)
  )
  
  
  datatable(TradeTable, rownames = F, options = list(pageLength = 3,
                                                   dom = ''))
  
  })
  
  filtered_optionsUp <- reactive({
    AllDraftCapital %>% filter(AllDraftCapital$Code == input$TeamUp)
  })
  
  output$filterdPicksUp <- renderUI({
    checkboxGroupInput("PicksUpList", "Select Picks you want to Exclude:", filtered_optionsUp()$ID, selected = c(""))
  })
  
  
  filtered_optionsDown <- reactive({
    TradeDownTeam <- substr(input$TeamDown, regexpr("-", input$TeamDown)+1, 300)
    
    AllDraftCapital %>% filter(AllDraftCapital$Code == TradeDownTeam)
  })
  
  
  output$filterdPicksDown <- renderUI({
    checkboxGroupInput("PicksDownList", "Select Picks you want to Exclude:", filtered_optionsDown()$ID, selected = c("2026-1-16"))
  })
  
  
  
  DownPickSelection <- reactive({
    AllDraftCapital %>% filter(AllDraftCapital$Code == input$DownSelection & AllDraftCapital$Season == current_season)
  })
  
  
  
  
  output$DownPickSelection <- renderUI({
    selectizeInput("DownPickSelection1", "Select Pick to Trade Down From:", DownPickSelection()$Pick, selected = c('start'))
  })
  
  # Use a reactive expression to capture the selected value from input$DownPickSelection1
  TradeDownPick1 <- reactive({
    input$DownPickSelection1
  })
  
  observe({
    # Update selected value based on user input
    TradeDownPick1()
  })
  
  filtered_optionsDown2 <- reactive({
    AllDraftCapital %>% filter(AllDraftCapital$Code == input$DownSelection)
  })
  
  
  output$filtered_optionsDown2 <- renderUI({
    checkboxGroupInput("PicksDownList2", "Select Picks you want to Exclude:", filtered_optionsDown2()$ID, selected = c(""))
  })
  
  
  
  df <- reactiveVal(NULL)
  
  output$TradeUptable = DT::renderDataTable({
    
    TradeDownTeam <- substr(input$TeamDown, regexpr("-", input$TeamDown)+1, 300)
    
    TradeDownPick <- as.double(substr(input$TeamDown, 1, regexpr("-", input$TeamDown)-1))
    
    
    if (length(input$PicksDownList) == 0) {
      Exclude_list_down <- '2026-1-16'
    } else {
      Exclude_list_down <- input$PicksDownList
    }
    
    if (length(input$PicksUpList) == 0) {
      Exclude_list_up <- '2026-1-16'
    } else {
      Exclude_list_up <- input$PicksUpList
    }

    

    TradeCombinations <- AllDraftCapital %>% 
      filter(Code == input$TeamUp | Code == TradeDownTeam,
            (Season == current_season & Pick >= TradeDownPick) | Season != current_season) %>% 
    mutate(JJ = if_else(Code == input$TeamUp, -JJ, JJ),
           WAR = if_else(Code == input$TeamUp, WAR, -WAR),
           TradeOrder = if_else(Code == input$TeamUp, 0, 1)
    ) %>%
      filter(!(ID %in% Exclude_list_up | ID %in% Exclude_list_down)) %>% 
    arrange(Pick) %>%
    arrange(Season) %>%
    arrange(TradeOrder)
    

    
    Cols <- dim(TradeCombinations)[1]
    UpIndex <- dim(TradeCombinations[TradeCombinations$Code == input$TeamUp,])[1]
    DownIndex <- dim(TradeCombinations[TradeCombinations$Code == TradeDownTeam,])[1]
    
    maxPicks = as.double(input$MaxPicks)
    pickdiff = as.double(input$PickDiff)
    minjj = as.double(input$MinJJ)
    maxjj = as.double(input$MaxJJ)
    
    lp  <- OP(objective = L_objective(c(TradeCombinations$WAR)),
              constraints = L_constraint(
                L = rbind(c(1, rep(0, (Cols-1))),
                          c(rep(0, UpIndex), 1, rep(0, (DownIndex-1))),
                          c(rep(1, UpIndex), rep(0, DownIndex)),
                          c(rep(1, UpIndex), rep(0, DownIndex)),
                          c(rep(0, UpIndex), rep(1, DownIndex)),
                          c(rep(0, UpIndex), rep(1, DownIndex)),
                          c(rep(1, UpIndex), rep(-1, DownIndex)),
                          c(rep(1, UpIndex), rep(-1, DownIndex)),
                          c(TradeCombinations$JJ),
                          c(TradeCombinations$JJ)
                ), 
                dir = c("==",
                        '==',
                        ">=",
                        "<=",
                        ">=",
                        "<=",
                        "<=",
                        ">=",
                        "<=",
                        ">="
                ),
                rhs = c(1,
                        1,
                        1,
                        maxPicks,
                        1,
                        maxPicks,
                        pickdiff,
                        (-1*pickdiff),
                        minjj,
                        maxjj)
              ),
              bounds = V_bound(li = 1:Cols,
                               ui = 1:Cols,
                               lb = rep(0, Cols),
                               ub = rep(1, Cols)),
              types = c(rep("I", Cols)))
    
    
    solution = ROI_solve(lp)
    
    Package1 = solution(solution)
    
      
      TradePackage <- cbind(TradeCombinations, TradeDownPick)%>%
      filter(Package1 == 1) %>%
        select(Code, Season, Round, Pick) 
    
      
      TradePackageOld <- cbind(TradeCombinations, TradeDownPick)%>%
        filter(Package1 == 1)
      
      
      df(TradePackageOld)
    
    datatable(TradePackage, rownames = F, options = list(pageLength = 40,
                                                         dom = ''))
    
    
  })
  
  
  
  output$Tradetable1 = DT::renderDataTable({
    
  TradePackageOld <- df
     
  
  TradeInValue1 <- TradePackageOld() %>% 
    filter(TradeOrder == 1) %>% 
    reframe(JJIn = sum(JJ),
            WARIn = sum(WAR)*-1,
            Std = sqrt(sum(`War STD`^2)),
            Value = (rnorm(100000, mean = WARIn, sd = Std))) 
  
  
  TradeOutValue1 <- TradePackageOld() %>% 
    filter(TradeOrder == 0) %>% 
    reframe(JJOut = sum(JJ)*-1,
            WARIn = sum(WAR),
            Std = sqrt(sum(`War STD`^2)),
            Value = (rnorm(100000, mean = WARIn, sd = Std))) 
  
  TradeTotalValue1 <- TradeInValue1 %>% 
    mutate(OutJJ = TradeOutValue1$JJOut,
           JJDiff = JJIn - OutJJ,
           OutValue = TradeOutValue1$Value,
           Diff = Value - OutValue,
           Win = if_else(Diff > 0 ,0, 1)) %>% 
    group_by() %>% 
    summarise(JJIn = mean(JJIn),
              JJOut = mean(OutJJ),
              JJDiff = mean(JJDiff),
              JJEquivalentPick = DraftCapital$Pick[which.min(abs(DraftCapital$JJValue - abs(JJDiff)))],
              JJEquivalentRound = ceiling(JJEquivalentPick/32),
              InValue = round(mean(Value),2),
              OutValue = round(mean(OutValue),2),
              AvgDiff = round(mean(Diff),2),
              WAREquivalentPick = DraftCapital$Pick[which.min(abs(DraftCapital$WAR - abs(AvgDiff)))],
              WAREquivalentRound = ceiling(WAREquivalentPick/32),
              WinPerc = paste0(round(sum(Win)/100000,4)*100, "% of the Time"))
  
  TradeTable1 <- data.frame(
    Type = c("JJ", "WAR", "Winner"),
    Up = c(TradeTotalValue1$JJOut, TradeTotalValue1$OutValue, "JJ"),
    Down = c(TradeTotalValue1$JJIn, TradeTotalValue1$InValue, if_else(TradeTotalValue1$JJEquivalentPick < TradeTotalValue1$WAREquivalentPick, ">", "<")),
    Difference = c(TradeTotalValue1$JJDiff, TradeTotalValue1$AvgDiff, "WAR"),
    EquivalentRound = c(TradeTotalValue1$JJEquivalentRound, TradeTotalValue1$WAREquivalentRound, "Trade Down Wins WAR Simulation"),
    EquivalentPick = c(TradeTotalValue1$JJEquivalentPick, TradeTotalValue1$WAREquivalentPick, TradeTotalValue1$WinPerc)
  )
  
  
  
  datatable(TradeTable1, rownames = F, options = list(pageLength = 3,
                                                     dom = ''))
  
  })
  
  
  output$TradeDowntable = DT::renderDataTable({
    
    TradeDownTeam1 <- input$DownSelection
    
    
    output_value <- TradeDownPick1()
    
    if (length(output_value) == 0) {
      TradeDownPick1 <- 28
    } else {
      TradeDownPick1 <- as.double(output_value)
    }
    
    if (length(input$PicksDownList2) == 0) {
      Exclude_list2 <- '2026-1-16'
    } else {
      Exclude_list2 <- input$PicksDownList2
    }
    
    
    TeamList <- unique(AllDraftCapital$Code[AllDraftCapital$Code != TradeDownTeam1])
    
    JJ = list()
    Value = list()
    Package = list()
    PackageList <- data.frame(Team = c(),
                              Package = c())
    
    
for(team in TeamList){
  
  
  TradeUpTeam1 <- team
    
    TradeCombinations2 <- AllDraftCapital %>%
      filter(Code == TradeUpTeam1 | Code == TradeDownTeam1,
     (Season == current_season & Pick >= TradeDownPick1) | Season != current_season) %>%
      mutate(JJ = if_else(Code == TradeUpTeam1, JJ, -JJ),
             WAR = if_else(Code == TradeUpTeam1, -WAR, WAR),
             TradeOrder = if_else(Code == TradeUpTeam1, 0, 1)
      ) %>%
      filter(!(ID %in% Exclude_list2)) %>% 
      arrange(Pick) %>%
      arrange(Season) %>%
      arrange(TradeOrder)
    
    
    
    
    Cols <- dim(TradeCombinations2)[1]
    UpIndex <- dim(TradeCombinations2[TradeCombinations2$Code == TradeUpTeam1,])[1]
    DownIndex <- dim(TradeCombinations2[TradeCombinations2$Code == TradeDownTeam1,])[1]
    
    
    maxPicks1 = as.double(input$MaxPicks1)
    pickdiff1 = as.double(input$PickDiff1)
    minjj1 = as.double(input$MinJJ1)
    maxjj1 = as.double(input$MaxJJ1)
    
    
    lp <- make.lp(10,Cols)
    for(Count in 1:Cols) {
      set.type(lp, Count, "binary")
    }
    
    set.column(lp, 1, c(1,1,1,1,1,
                        TradeCombinations2$JJ[1],
                        TradeCombinations2$JJ[1]),
               indices = c(2:4,7:10))
    
    
    for (Count in 2:UpIndex) {
      set.column(lp, Count, c(1,1,1,1,TradeCombinations2$JJ[Count],TradeCombinations2$JJ[Count]), indices = c(3:4,7:10))
    }
    
    set.column(lp, (UpIndex + 1), c(1,1,1,-1,-1,TradeCombinations2$JJ[(UpIndex + 1)],TradeCombinations2$JJ[(UpIndex + 1)]), indices = c(1, 5:10))
    
    
    
    
    for (Count in 1:(DownIndex - 1)) {
      set.column(lp, (Count + UpIndex + 1), c(1,1,-1,-1,TradeCombinations2$JJ[(Count + UpIndex + 1)],TradeCombinations2$JJ[(Count + UpIndex + 1)]), indices = c(5:10))
    }
    
    
    
    set.constr.type(lp, c("=", '=', ">=", "<=", ">=", "<=", "<=", ">=", "<=", ">="))
    
    set.rhs(lp, c(1,1,1,maxPicks1,1,maxPicks1,pickdiff1,-1*as.numeric(pickdiff1),minjj1,maxjj1))
    
    set.objfn(lp, c(TradeCombinations2$WAR))
    
    
    solve(lp)
    
    
    
    Value[team] <- get.objective(lp)
    Package[team] <- list(get.variables(lp))
    JJ[team] <- get.constraints(lp)[9]
    
    Package1 <- t(t(Package))
    
    Package2 <- as.data.frame(Package1) %>% 
      summarise(Team = rownames(Package1),
                Package = as.list(V1))
    
    testpackage <- TradeCombinations2 %>% 
      left_join(Package2, by = c("Code" = "Team")) %>% 
      mutate(Rownum = seq(1, n(),1),
             Index = flatten(head(Package, 1))) %>% 
      filter(Index == 1) %>% 
      reframe(ID = paste0(Code, "-", ID),
                Team = team,
                Pick = head(Pick, 1)) %>% 
      reframe(List = list(ID),
                Team = Team,
                Highest_Pick = Pick) %>% 
      select(Team, Highest_Pick, List) %>% 
      distinct()
    
    
    PackageList <- rbind(PackageList,testpackage)
    
}
    
    Value1 <- data.frame(t(data.frame(Value) %>% 
                             full_join(data.frame(JJ))))
    
    
    ValueTable <- Value1 %>%
      reframe(Team = rownames(Value1),
                `WAR Value Gained` = round(as.double(X1)*-1,3),
                `JJ Value Gained` = round(as.double(X2),2)) %>% 
      left_join(PackageList) %>% 
      mutate(JJPR = 1-percent_rank(`JJ Value Gained`),
             WARPR = percent_rank(`WAR Value Gained`),
             PickDiff = Highest_Pick - TradeDownPick1,
             PickDiffPR = 1-percent_rank(PickDiff),
             Order = (WARPR * 3 + JJPR + PickDiffPR * 2)/6) %>% 
      arrange(desc(Order)) %>% 
      select(!JJPR) %>% 
      select(!WARPR) %>% 
      select(!PickDiff) %>% 
      select(!PickDiffPR) %>% 
      select(!Order) 
      
    
    WARbrks1 <- quantile(ValueTable$`WAR Value Gained`, probs = seq(.05, .95, .01), na.rm = TRUE)
    WARbrky1 <- round(seq(255, 40, length.out = length(WARbrks1) + 1), 0)
    WARclrs1 <- paste0("rgb(", WARbrky1, "," , 255-WARbrky1 , ",", 0, ")")
     
    
    JJbrks1 <- quantile(ValueTable$`JJ Value Gained`, probs = seq(.05, .95, .01), na.rm = TRUE)
    JJbrky1 <- round(seq(255, 40, length.out = length(JJbrks1) + 1), 0)
    JJclrs1 <- paste0("rgb(", JJbrky1, "," , 255-JJbrky1 , ",", 0, ")")
    
    
    Pickbrks1 <- quantile(ValueTable$Highest_Pick, probs = seq(.05, .95, .01), na.rm = TRUE)
    Pickbrky1 <- round(seq(255, 40, length.out = length(Pickbrks1) + 1), 0)
    Pickclrs1 <- paste0("rgb(", 255-Pickbrky1, "," , Pickbrky1 , ",", 0, ")")
    
    datatable(ValueTable, rownames = F, options = list(pageLength = 40,
                                                         dom = '')) %>%
      formatStyle("WAR Value Gained",
                  backgroundColor = styleInterval(WARbrks1,WARclrs1)
      ) %>%
      formatStyle("JJ Value Gained",
                  backgroundColor = styleInterval(JJbrks1,JJclrs1)
      ) %>%
      formatStyle("Highest_Pick",
                  backgroundColor = styleInterval(Pickbrks1,Pickclrs1)
      )
    


  })
  
  output$DraftPickValueChart = DT::renderDataTable({
    
    DraftPickValueChart = DraftPickValueChart %>% 
    mutate(Round = factor(Round, levels = 1:7),
           Team = factor(Code)) %>% 
      select(Round, Pick, Team, JJValue, WAR, `War STD`)
    
    WARbrks <- quantile(DraftPickValueChart$WAR, probs = seq(.05, .95, .01), na.rm = TRUE)
    WARbrksy <- round(seq(255, 40, length.out = length(WARbrks) + 1), 0)
    WARbrksclrs <- paste0("rgb(", WARbrksy, "," , 255-WARbrksy , ",", 0, ")")
    
    JJValuebrks <- quantile(DraftPickValueChart$JJValue, probs = seq(.05, .95, .01), na.rm = TRUE)
    JJValuebrksy <- round(seq(255, 40, length.out = length(JJValuebrks) + 1), 0)
    JJValuebrksclrs <- paste0("rgb(", JJValuebrksy, "," , 255-JJValuebrksy , ",", 0, ")")
    
    
    datatable(DraftPickValueChart, rownames = F, filter = "top", options = list(pageLength = 50,
                                                      dom = 'Bfrtip')) %>% 
      formatStyle("WAR",
                  backgroundColor = styleInterval(WARbrks,WARbrksclrs)) %>% 
      formatStyle("JJValue",
                  backgroundColor = styleInterval(JJValuebrks,JJValuebrksclrs)
      )
    
    
  })
  
  
  output$Captable = DT::renderDataTable({
    
    RosterCap <-  RosterCap %>% 
      filter(RosterCap$team == input$FATeam) %>% 
      mutate(TotalCap = paste0("$",format(TotalCap, big.mark=",", 
                                           currency=TRUE, trim=TRUE, justify = "left")),
             EffectiveCapSpace = paste0("$",format(EffectiveCapSpace, big.mark=",", 
                                         currency=TRUE, trim=TRUE, justify = "left")),
             CapAfterCuts = paste0("$",format(CapAfterCuts, big.mark=",", 
                                              currency=TRUE, trim=TRUE, justify = "left")),
             CapAfterEasy = paste0("$",format(CapAfterEasy, big.mark=",", 
                                              currency=TRUE, trim=TRUE, justify = "left")),
             )
    
    datatable(RosterCap, rownames = F, options = list(pageLength = 40,
                                                      dom = ''))
    
    
  })
  
  output$TeamCaptable = DT::renderDataTable({
    
    RosterProjections <- RosterProjections %>% 
      filter(RosterProjections$team == input$FATeam) %>% 
      select(!team) %>% 
      select(!OffDef) %>% 
      select(!SideofBall) %>% 
      mutate(CapNumber = paste0("$",format(CapNumber, big.mark=",", 
                                           currency=TRUE, trim=TRUE, justify = "left")),
             DeadCap = paste0("$",format(DeadCap, big.mark=",", 
                                           currency=TRUE, trim=TRUE, justify = "left")),
             CapSavings = paste0("$",format(CapSavings, big.mark=",", 
                                           currency=TRUE, trim=TRUE, justify = "left")),
             WAR = round(WAR, 3),
             WARTile = round(round(WARTile, 4)*100,2),
             careerAvg = round(careerAvg, 3),
             RecentAvg = round(RecentAvg, 3)
      )
    
    datatable(RosterProjections, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 10,
                                                     dom = 'Brtip',                      
                                                     buttons = c('csv', 'excel')))
    
    
  })
  
  output$TeamPositionPlot <- renderPlot({
    
    ggplot(WARplots %>% 
             filter(team == input$FATeam, position != "QB"), aes(x = Season, y = ExpectedWAR))+
      geom_hline(aes(yintercept = TeamPositionalAvg), linetype = "dashed")+
      geom_hline(aes(yintercept = TeamPositionalMax, alpha = 0.5, color = "blue"), linetype = "dashed")+
      geom_point(aes(size = PreviousCount))+
      geom_line()+
      geom_point(aes(size = Count, y = TotalWAR, color = "red"))+
      geom_line(aes(y = TotalWAR, color = "red"))+
      ggrepel::geom_text_repel(aes(y = TotalWAR, label = Count, color = "red"), size = 5,
                               force = 50, max.overlaps = Inf,
                               min.segment.length = 0, direction = "y")+
      facet_wrap(~FacetLabel)+
      scale_fill_identity(aesthetics = c("color", "fill"))+
      theme_reach()
    
    
  })
  
  
  output$FreeAgentstable = DT::renderDataTable({
    
    FreeAgents <-  FreeAgents %>% 
      group_by() %>% 
      summarise(Player = player.x,
             CurrentTeam = team_name.x,
             Projection = as.factor(Projection),
             Position = as.factor(position.x),
             OffDef = as.factor(OffDef.x),
             SideofBall = as.factor(SideofBall.x),
             YearsInNFL = as.integer(YearsInNFL.x),
             Age = as.integer(Age),
             College = Cplleg1,
             DraftPick = if_else(is.na(draft_number) == 1, 300, draft_number),
             RAS = AllTime1,
             Density = round(Density, 2),
             Snaps = Snaps.x,
             WAR = round(WAR.x, 3),
             WARTile = round(round(WARTile.x, 4)*100,4),
             CareerAvg = round(careerAvg.x, 3),
             RecentAvg = round(RecentAvg.x, 3),
             CapNumber = if_else(is.na(CapNumber) == 1, 0, CapNumber),
             CapSavings = Cap,
             CapNumber = paste0("$",format(CapNumber, big.mark=",",currency=TRUE, trim=TRUE, justify = "left")),
             CapSavings = paste0("$",format(Cap, big.mark=",",currency=TRUE, trim=TRUE, justify = "left")),
             ExpAPY = as.numeric(round(ExpAPY,0)),
             ExpAPY = paste0("$",format(ExpAPY, big.mark=",",currency=TRUE, trim=TRUE, justify = "left")),
             # CrudeAPY = if_else(Position == "QB", (RecentAvg * (10000000/256000000)), (RecentAvg * 0.1757813)) * 256000000,
             # CrudeAPY = round(CrudeAPY,0),
             # CrudeAPY = paste0("$",format(CrudeAPY, big.mark=",",currency=TRUE, trim=TRUE, justify = "left")),
      ) %>% 
      arrange(desc(RecentAvg))
    
    datatable(FreeAgents, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 10,
                                                     dom = 'Brtip'))
    
    
  })
  
  
  
  output$PlayerWARPlot <- renderPlot({
    
    
    ggplot(IWARData%>% 
             filter(ID == input$Player) %>% 
             group_by(season, ID) %>% 
             mutate(season = as.character(season),
                    WAR = sum(WAR)),
           aes(x = season, y = WAR)) +
      geom_image(aes(image = url))+
      geom_line(aes(color = primary, group = 1))+
      geom_hline(yintercept = 0, color = "red")+
      geom_hline(aes(yintercept = PosAvg), linetype = "dashed")+
      facet_wrap(~position)+
      theme_reach()+
      scale_color_identity(aesthetics = c("color", "fill"))+
      scale_x_discrete(breaks = seq(2010,2030,1)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      labs(
        y= "Wins Above Replacemen (WAR)",
        x= "Season",
        title= paste0("WAR by Season for ", input$Player),
        subtitle = "Dashed line is Positional Averge, Red Line is 0 WAR",
        caption = "@SethDataScience"
      ) 
  })
  
  output$FAStrengthtable = DT::renderDataTable({
    
    PositionsOfStrength <- PositionsOfStrength %>% 
      group_by() %>% 
      mutate(TotalWAR = round(TotalWAR, 3),
             MeanWAR = round(MeanWAR, 3),
             RecentMeanWAR = round(RecentMeanWAR, 3),
             Top85Percentile = as.factor(Top85Percentile),
             Position = as.factor(position.x),
             Projection = as.factor(Projection)) %>% 
      select(!position.x) %>% 
      select(Position, everything()) %>% 
      arrange(desc(RecentMeanWAR))
    
    datatable(PositionsOfStrength, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 10,
                                                     dom = 'Brtip'))
    
    
  })

  output$CPlayerWARPlot <- renderPlot({

    ggplot(CollegeWARJoin %>%
             filter(ID == input$CPlayer)%>%
             group_by(ID, season) %>% 
             mutate(season = as.character(season),
                    WAR = sum(WAR)),
           aes(x = season, y = WAR)) +
      geom_image(aes(image = CollegeWARJoin$logo[CollegeWARJoin$ID == input$CPlayer]))+
      geom_line(aes(color = CollegeWARJoin$primary[CollegeWARJoin$ID == input$CPlayer], group = 1))+
      geom_hline(yintercept = 0, color = "red")+
      geom_hline(aes(yintercept = PosAvg), linetype = "dashed")+
      facet_wrap(~position)+
      theme_reach()+
      scale_color_identity(aesthetics = c("color", "fill"))+
      scale_x_discrete(breaks = seq(2010,2030,1)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      labs(
        y= "Wins Above Replacemen (WAR)",
        x= "Season",
        title= paste0("WAR by Season for ", input$CPlayer),
        subtitle = "Dashed line is Positional Averge, Red Line is 0 WAR",
        caption = "@SethDataScience"
      )
  })


  output$CWARtable = DT::renderDataTable({

    CWARtable <- CollegeWARJoin %>%
      mutate(WAR = round(WAR, 3),
             WARTile = round(round(WARTile, 3)*100,4),
             AvgWAR = round(AvgWAR, 3),
             WARPerSnap = round(round((WAR/Snaps), 6)*100,4)) %>%
      arrange(desc(WAR)) %>%
      mutate(player = as.factor(player),
             player_id = as.integer(player_id),
             Snaps = as.integer(Snaps),
             season = as.factor(season),
             pos_team = as.factor(pos_team),
             Conference = as.factor(Conference),
             Strength = as.factor(Strength),
             Seasons = as.factor(Seasons),
             Transfer = as.factor(Transfer),
             OffDef = as.factor(OffDef),
             position = as.factor(position))%>%
      select(season, pos_team, Conference, Strength, player,
             player_id, Snaps, Seasons,
             Transfer, OffDef, position, WAR, WARTile, WARPerSnap, BigBoardRank)




    WARbrks <- quantile(CWARtable$WAR, probs = seq(.05, .95, .01), na.rm = TRUE)
    WARbrksy <- round(seq(255, 40, length.out = length(WARbrks) + 1), 0)
    WARbrksclrs <- paste0("rgb(", WARbrksy, "," , 255-WARbrksy , ",", 0, ")")

    WARTilebrks <- quantile(CWARtable$WARTile, probs = seq(.05, .95, .01), na.rm = TRUE)
    WARTilebrksy <- round(seq(255, 40, length.out = length(WARTilebrks) + 1), 0)
    WARTilebrksclrs <- paste0("rgb(", WARTilebrksy, "," , 255-WARTilebrksy , ",", 0, ")")
    
    WARPerSnapbrks <- quantile(CWARtable$WARPerSnap, probs = seq(.05, .95, .01), na.rm = TRUE)
    WARPerSnapbrksy <- round(seq(255, 40, length.out = length(WARPerSnapbrks) + 1), 0)
    WARPerSnapbrksclrs <- paste0("rgb(", WARPerSnapbrksy, "," , 255-WARPerSnapbrksy , ",", 0, ")")

    
    BigBoardRankbrks <- quantile(CWARtable$BigBoardRank, probs = seq(.05, .95, .01), na.rm = TRUE)
    BigBoardRankbrksy <- round(seq(255, 40, length.out = length(BigBoardRankbrks) + 1), 0)
    BigBoardRankbrksclrs <- paste0("rgb(", 255-BigBoardRankbrksy, "," , BigBoardRankbrksy , ",", 0, ")")
    
    

    datatable(CWARtable, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 10,
                                                     dom = 'Bfrtip',                      
                                                     buttons = c('csv', 'excel'))) %>% 
      formatStyle("WAR",
                  backgroundColor = styleInterval(WARbrks,WARbrksclrs)
      )%>%
      formatStyle("WARTile",
                  backgroundColor = styleInterval(WARTilebrks,WARTilebrksclrs)
      )%>%
      formatStyle("WARPerSnap",
                  backgroundColor = styleInterval(WARPerSnapbrks,WARPerSnapbrksclrs)
      )%>%
      formatStyle("BigBoardRank",
                  backgroundColor = styleInterval(BigBoardRankbrks,BigBoardRankbrksclrs)
      )
    
    
  })
  
  
  output$DraftStrengthstable = DT::renderDataTable({
    
    DraftStrengths = DraftStrengths %>% 
      mutate(MeanPos = round(MeanPos, 2),
             Top50Percent = round(Top50Percent, 2),
             Top100Percent = round(Top100Percent, 2),
      )
    
    datatable(DraftStrengths, rownames = F, 
              options = list(pageLength = 20, dom = '')) 
    
    
  })
  
  
  output$Separatortable = DT::renderDataTable({
    
    CollegeWAR <- CWARFULL %>% 
      group_by(player_id) %>% 
      slice_head(n = 1) %>% 
      select(player_id, AvgWAR)
    
    
    Separatortable <- CollegeWRWARFill %>%
      left_join(CollegeWAR, by = c("player_id")) %>% 
      group_by(season, player_id) %>% 
      mutate(ContestedRate = contested_targets/targets,
             yards_after_catch_per_reception = if_else(is.na(yards_after_catch_per_reception) == 1, 0, yards_after_catch_per_reception),
             contested_catch_rate = if_else(is.na(contested_catch_rate) == 1, 0, contested_catch_rate),
             CCR = mean(contested_catch_rate),
             DRP = mean(drop_rate),
             YAC = mean(yards_after_catch_per_reception)
             ) %>% 
      filter(targets >= input$target, grades_offense >= input$grade, position == "WR") %>% 
      distinct() %>% 
      filter(Conference == "ACC" | Conference == "SEC" |
               Conference == "Big 12" | Conference == "Big Ten" | 
               Conference == "Pac-12" | team_name == "NOTRE DAME" |
               team_name == "BUFFALO" | team_name == "UCF"|
               team_name == "BOISE ST" | team_name == "SMU" |
               team_name == "HOUSTON" | player_id == "66272" |
               player_id == "128232") %>% 
      group_by() %>% 
      mutate(ContestedTile = 1-percent_rank(ContestedRate),
             GradeTile = percent_rank(grades_offense),
             ADOTTile = percent_rank(avg_depth_of_target),
             YPRRTile = percent_rank(yprr),
             
             CCR = percent_rank(CCR),
             DRP = 1 - percent_rank(DRP),
             YAC = percent_rank(YAC),
             CollegeWAR = percent_rank(AvgWAR)+0.01,
             Value = (YPRRTile^12 +
                         YAC^3 +
                         DRP^3 + 
                         exp(CCR) + 
                         log(CollegeWAR)),
             Value = percent_rank(Value)
             ) %>% 
      inner_join(DeclaresList, by = c("player_id")) %>%
      arrange(desc(Value)) %>% 
      mutate(NonSeparator = case_when(ContestedTile <= 0.2 ~ 1,
                                      ADOTTile <= 0.15 ~ 1,
                                      ContestedTile <= 0.3 & 
                                        ADOTTile <= 0.3 ~ 1,
                                      ContestedTile <= 0.4 & 
                                        ADOTTile >= 0.8 ~ 1,
                                      T ~ 0)) %>% 
      group_by(player_id) %>% 
      arrange(desc(season)) %>% 
      summarise(player = head(player, 1),
                team_name = head(team_name.x, 1),
                # Ht = head(Ht, 1),
                # Wt = head(Wt, 1),
                # Density = round(head(Density, 1),2),
                # RAS = head(RAS, 1),
                Seasons = n(),
                ContestedTile = round(round(mean(ContestedTile),4)*100, 4),
                GradeTile = round(round(mean(GradeTile),4)*100, 4),
                ADOTTile = round(round(mean(ADOTTile),4)*100,4), 
                YPRRTile = round(round(mean(YPRRTile),4)*100,4), 
                Value = round(round(mean(Value),4)*100, 4),
                TotalNonSepSeasons = sum(NonSeparator),
                NonSepPercent = round(TotalNonSepSeasons/Seasons,2)) %>% 
      mutate(Seasons = factor(Seasons, levels = 1:5),
             Filter = factor(case_when(
               ADOTTile <= 21 ~ "Gadget",
                (TotalNonSepSeasons > 1 &                  NonSepPercent > 0.5) ~ "NonSeparator",
               T ~ "Solid"
                  
             ))
          )%>% 
      arrange(desc(Value)) 
    
    
    ContestedTilebrks <- quantile(Separatortable$ContestedTile, probs = seq(.05, .95, .01), na.rm = TRUE)
    ContestedTilebrksy <- round(seq(255, 40, length.out = length(ContestedTilebrks) + 1), 0)
    ContestedTilebrksclrs <- paste0("rgb(", ContestedTilebrksy, "," , 255-ContestedTilebrksy , ",", 0, ")")
    
    GradeTilebrks <- quantile(Separatortable$GradeTile, probs = seq(.05, .95, .01), na.rm = TRUE)
    GradeTilebrksy <- round(seq(255, 40, length.out = length(GradeTilebrks) + 1), 0)
    GradeTilebrksclrs <- paste0("rgb(", GradeTilebrksy, "," , 255-GradeTilebrksy , ",", 0, ")")
    
    
    
    ADOTTilebrks <- quantile(Separatortable$ADOTTile, probs = seq(.05, .95, .01), na.rm = TRUE)
    ADOTTilebrksy <- round(seq(255, 40, length.out = length(ADOTTilebrks) + 1), 0)
    ADOTTilebrksclrs <- paste0("rgb(", ADOTTilebrksy, "," , 255-ADOTTilebrksy , ",", 0, ")")
    
    
    YPRRTilebrks <- quantile(Separatortable$YPRRTile, probs = seq(.05, .95, .01), na.rm = TRUE)
    YPRRTilebrksy <- round(seq(255, 40, length.out = length(YPRRTilebrks) + 1), 0)
    YPRRTilebrksclrs <- paste0("rgb(", YPRRTilebrksy, "," , 255-YPRRTilebrksy , ",", 0, ")")
    
    
    Valuebrks <- quantile(Separatortable$Value, probs = seq(.05, .95, .01), na.rm = TRUE)
    Valuebrksy <- round(seq(255, 40, length.out = length(Valuebrks) + 1), 0)
    Valuebrksclrs <- paste0("rgb(", Valuebrksy, "," , 255-Valuebrksy , ",", 0, ")")
    
    
    datatable(Separatortable, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 10,
                                                     dom = 'Bfrtip',                      
                                                     buttons = c('csv', 'excel'))) %>% 
      formatStyle("ContestedTile",
                  backgroundColor = styleInterval(ContestedTilebrks,ContestedTilebrksclrs)
      )%>% 
      formatStyle("GradeTile",
                  backgroundColor = styleInterval(GradeTilebrks,GradeTilebrksclrs)
      )%>% 
      formatStyle("ADOTTile",
                  backgroundColor = styleInterval(ADOTTilebrks,ADOTTilebrksclrs)
      )%>% 
      formatStyle("YPRRTile",
                  backgroundColor = styleInterval(YPRRTilebrks,YPRRTilebrksclrs)
      )%>% 
      formatStyle("Value",
                  backgroundColor = styleInterval(Valuebrks,Valuebrksclrs)
      )
    
    
  })
  
  output$WRClusterPlot <- renderPlot({
    
  WRValueCluster <- CollegeWRWARFill %>% 
    group_by(player_id) %>% 
    mutate(CareerGrade = mean(grades_offense, na.rm = T),
           yards_after_catch_per_reception = if_else(is.na(yards_after_catch_per_reception) == 1, 0, yards_after_catch_per_reception)) %>% 
    filter(CareerGrade >= 50, routes >= 20, position == "WR")  %>% 
    filter(Conference == "ACC" | Conference == "SEC" |
             Conference == "Big 12" | Conference == "Big Ten" | 
             Conference == "Pac-12" | team_name == "NOTRE DAME" |
             team_name == "CINCINNATI" | team_name == "N DAK ST" | 
             team_name == "BUFFALO" | team_name == "BOISE ST" |
             team_name == "UCF")%>% 
    group_by(player_id, ID) %>% 
    summarise(RR = mean(routes),
              TPRR = mean(targets)/RR,
              TDPRR = mean(touchdowns)/RR,
              YAC = mean(yards_after_catch_per_reception),
              YPRR = mean(yprr),
              ADOT = mean(avg_depth_of_target)) %>% 
    group_by() %>% 
    mutate(RR = percent_rank(RR),
           TPRR = percent_rank(TPRR),
           TDPRR = percent_rank(TDPRR),
           YAC = percent_rank(YAC),
           YPRR = percent_rank(YPRR),
           ADOT = percent_rank(ADOT)) %>% 
    filter(ID == input$Cplayer2) %>% 
    select(RR, TPRR,  YPRR, TDPRR, ADOT, YAC)
  
  
  WRValueCluster <- rbind(rep(1,6) , rep(0,6) , WRValueCluster)
  
  
  radarchart(WRValueCluster,
             pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4,
             
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
             
             #custom labels
             vlcex=0.8,
             title = paste0(input$Cplayer2, " Career
  Normalized Values from 2018-",current_season, " WRs"))
  
  })
  output$WRCompTable <- render_gt({
    
    gt(CollegeComps %>%
         filter(ID == input$Cplayer2)) %>%
      fmt_number(columns = c("Similarity",'ExpectedWAR', "MeanExpectedWAR"), decimals = 2)%>% 
      data_color(
        columns = c(MeanExpectedWAR),
        colors = scales::col_numeric(
          palette = color_palette,
          domain = c(0.6,0),
          reverse = F  # Set this to TRUE for color reversal
        )) %>% 
      data_color(
        columns = c(ExpectedWAR),
        colors = scales::col_numeric(
          palette = color_palette,
          domain = c(0.75,0),
          reverse = F  # Set this to TRUE for color reversal
        )) %>% 
      data_color(
        columns = c(Similarity),
        colors = scales::col_numeric(
          palette = color_palette,
          domain = c(1,0),
          reverse = F  # Set this to TRUE for color reversal
        )) %>% 
      opt_align_table_header(align = "center") %>% 
      cols_align("center") %>% 
      opt_row_striping() %>%
      gt_theme_espn()  %>%
      tab_options(
      ) %>% 
      tab_header(
        title = md(""),
        subtitle = "")  %>% 
      tab_source_note("Table: @SethDataScience")
    
    
    
  })
  
  output$TEClusterPlot <- renderPlot({
    
    TEValueCluster <- CollegeWRWARFill %>% 
      group_by(player_id) %>% 
      mutate(CareerGrade = mean(grades_offense, na.rm = T),
             yards_after_catch_per_reception = if_else(is.na(yards_after_catch_per_reception) == 1, 0, yards_after_catch_per_reception)) %>% 
      filter(CareerGrade >= 50, routes >= 20, position == "TE")  %>% 
      filter(Conference == "ACC" | Conference == "SEC" |
               Conference == "Big 12" | Conference == "Big Ten" | 
               Conference == "Pac-12" | team_name == "NOTRE DAME" |
               team_name == "CINCINNATI" | team_name == "N DAK ST" | 
               team_name == "BUFFALO" | team_name == "BOISE ST" |
               team_name == "UCF"  )%>% 
      group_by(player_id, ID) %>% 
      summarise(RR = mean(routes),
                TPRR = mean(targets)/RR,
                TDPRR = mean(touchdowns)/RR,
                YAC = mean(yards_after_catch_per_reception),
                YPRR = mean(yprr),
                ADOT = mean(avg_depth_of_target)) %>% 
      group_by() %>% 
      mutate(RR = percent_rank(RR),
             TPRR = percent_rank(TPRR),
             TDPRR = percent_rank(TDPRR),
             YAC = percent_rank(YAC),
             YPRR = percent_rank(YPRR),
             ADOT = percent_rank(ADOT)) %>% 
      filter(ID == input$Cplayer3) %>% 
      select(RR, TPRR,  YPRR, TDPRR, ADOT, YAC)
    
    
    TEValueCluster <- rbind(rep(1,6) , rep(0,6) , TEValueCluster)
    
    
    radarchart(TEValueCluster,
               pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4,
               
               #custom the grid
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
               
               #custom labels
               vlcex=0.8,
               title = paste0(input$Cplayer3, " Career
  Normalized Values from 2018-", current_season, " TEs"))
    
  })
  
  output$SeparatortableTE = DT::renderDataTable({
    
    CollegeWAR <- CWARFULL %>% 
      group_by(player_id) %>% 
      slice_head(n = 1) %>% 
      select(player_id, AvgWAR)
    
    
    SeparatortableTE <- CollegeWRWARFill %>%
      left_join(CollegeWAR, by = c("player_id")) %>% 
      group_by(season, player_id) %>% 
      mutate(ContestedRate = contested_targets/targets,
             yards_after_catch_per_reception = if_else(is.na(yards_after_catch_per_reception) == 1, 0, yards_after_catch_per_reception),
             contested_catch_rate = if_else(is.na(contested_catch_rate) == 1, 0, contested_catch_rate),
             CCR = mean(contested_catch_rate),
             DRP = mean(drop_rate),
             YAC = mean(yards_after_catch_per_reception)
      ) %>% 
      filter(targets >= input$target, grades_offense >= input$grade, position == "TE") %>% 
      distinct() %>% 
      filter(Conference == "ACC" | Conference == "SEC" |
               Conference == "Big 12" | Conference == "Big Ten" | 
               Conference == "Pac-12" | team_name == "NOTRE DAME" |
               team_name == "BUFFALO" | team_name == "UCF"|
               team_name == "BOISE ST" | team_name == "SMU" |
               team_name == "HOUSTON" | player_id == "66272" |
               player_id == "128232") %>% 
      group_by() %>% 
      mutate(ContestedTile = 1-percent_rank(ContestedRate),
             GradeTile = percent_rank(grades_offense),
             ADOTTile = percent_rank(avg_depth_of_target),
             YPRRTile = percent_rank(yprr),
             
             CCR = percent_rank(CCR),
             DRP = 1 - percent_rank(DRP),
             YAC = percent_rank(YAC),
             CollegeWAR = percent_rank(AvgWAR)+5,
             Value = (YPRRTile^2 +
                        ADOTTile +
                        log(CollegeWAR)),
             Value = percent_rank(Value)
      ) %>% 
      inner_join(DeclaresList, by = c("player_id")) %>%
      arrange(desc(Value)) %>% 
      mutate(NonSeparator = case_when(ContestedTile <= 0.2 ~ 1,
                                      ADOTTile <= 0.15 ~ 1,
                                      ContestedTile <= 0.3 & 
                                        ADOTTile <= 0.3 ~ 1,
                                      ContestedTile <= 0.4 & 
                                        ADOTTile >= 0.8 ~ 1,
                                      T ~ 0)) %>% 
      group_by(player_id) %>% 
      arrange(desc(season)) %>% 
      summarise(player = head(player, 1),
                team_name = head(team_name.x, 1),
                # Ht = head(Ht, 1),
                # Wt = head(Wt, 1),
                # Density = round(head(Density, 1),2),
                # RAS = head(RAS, 1),
                Seasons = n(),
                ContestedTile = round(round(mean(ContestedTile),4)*100, 4),
                GradeTile = round(round(mean(GradeTile),4)*100, 4),
                ADOTTile = round(round(mean(ADOTTile),4)*100,4), 
                YPRRTile = round(round(mean(YPRRTile),4)*100,4), 
                Value = round(round(mean(Value),4)*100, 4),
                TotalNonSepSeasons = sum(NonSeparator),
                NonSepPercent = round(TotalNonSepSeasons/Seasons,2)) %>% 
      mutate(Seasons = factor(Seasons, levels = 1:5),
             Filter = factor(case_when(
               ADOTTile <= 21 ~ "Gadget",
               (TotalNonSepSeasons > 1 &                  NonSepPercent > 0.5) ~ "NonSeparator",
               T ~ "Solid"
               
             ))
      )%>% 
      arrange(desc(Value)) 
    
    
    ContestedTilebrks <- quantile(SeparatortableTE$ContestedTile, probs = seq(.05, .95, .01), na.rm = TRUE)
    ContestedTilebrksy <- round(seq(255, 40, length.out = length(ContestedTilebrks) + 1), 0)
    ContestedTilebrksclrs <- paste0("rgb(", ContestedTilebrksy, "," , 255-ContestedTilebrksy , ",", 0, ")")
    
    GradeTilebrks <- quantile(SeparatortableTE$GradeTile, probs = seq(.05, .95, .01), na.rm = TRUE)
    GradeTilebrksy <- round(seq(255, 40, length.out = length(GradeTilebrks) + 1), 0)
    GradeTilebrksclrs <- paste0("rgb(", GradeTilebrksy, "," , 255-GradeTilebrksy , ",", 0, ")")
    
    
    
    ADOTTilebrks <- quantile(SeparatortableTE$ADOTTile, probs = seq(.05, .95, .01), na.rm = TRUE)
    ADOTTilebrksy <- round(seq(255, 40, length.out = length(ADOTTilebrks) + 1), 0)
    ADOTTilebrksclrs <- paste0("rgb(", ADOTTilebrksy, "," , 255-ADOTTilebrksy , ",", 0, ")")
    
    
    YPRRTilebrks <- quantile(SeparatortableTE$YPRRTile, probs = seq(.05, .95, .01), na.rm = TRUE)
    YPRRTilebrksy <- round(seq(255, 40, length.out = length(YPRRTilebrks) + 1), 0)
    YPRRTilebrksclrs <- paste0("rgb(", YPRRTilebrksy, "," , 255-YPRRTilebrksy , ",", 0, ")")
    
    
    Valuebrks <- quantile(SeparatortableTE$Value, probs = seq(.05, .95, .01), na.rm = TRUE)
    Valuebrksy <- round(seq(255, 40, length.out = length(Valuebrks) + 1), 0)
    Valuebrksclrs <- paste0("rgb(", Valuebrksy, "," , 255-Valuebrksy , ",", 0, ")")
    
    
    datatable(SeparatortableTE, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 10,
                                                     dom = 'Bfrtip',                      
                                                     buttons = c('csv', 'excel'))) %>% 
      formatStyle("ContestedTile",
                  backgroundColor = styleInterval(ContestedTilebrks,ContestedTilebrksclrs)
      )%>% 
      formatStyle("GradeTile",
                  backgroundColor = styleInterval(GradeTilebrks,GradeTilebrksclrs)
      )%>% 
      formatStyle("ADOTTile",
                  backgroundColor = styleInterval(ADOTTilebrks,ADOTTilebrksclrs)
      )%>% 
      formatStyle("YPRRTile",
                  backgroundColor = styleInterval(YPRRTilebrks,YPRRTilebrksclrs)
      )%>% 
      formatStyle("Value",
                  backgroundColor = styleInterval(Valuebrks,Valuebrksclrs)
      )
    
    
  })
  
  output$Shannontable = DT::renderDataTable({
    
    
    Shannontable <- CollegeWARJoin %>% 
      inner_join(DeclaresList, by = c("player_id")) %>% 
      left_join(ShannonEntropy, by = c("player_id")) %>% 
      # left_join(CBMetrics, by = c("position", "player_id")) %>% 
      # left_join(SMetrics, by = c("position", "player_id")) %>% 
      filter(Snaps >= 150) %>% 
      distinct() %>%
      filter(Conference == "ACC" | Conference == "SEC" |
               Conference == "Big 12" | Conference == "Big Ten" |
               Conference == "Pac-12" | team_name.x == "NOTRE DAME" |
               team_name.x == "BUFFALO" | team_name.x == "UCF"|
               team_name.x == "BOISE ST" | team_name.x == "SMU" | team_name.x == "TOLEDO" |
               team_name.x == "HOUSTON"| player %in% DeclaresList$player) %>% 
      # mutate(Metric = if_else(position == "CB", Metric.x, Metric.y)) %>% 
      select(season, player_id, player, position,
             #Ht, Wt, Density, RAS,
             team_name.x, AvgWAR, MaxWAR, Ent, 
             # Metric
             ) %>% 
      group_by(player_id) %>% 
      arrange(desc(season)) %>% 
      filter(position %in% c("CB", "S", "LB", "ED", "DI", "DB"))%>%
      summarise(player = head(player, 1),
                team_name = head(team_name.x, 1),
                position = as.factor(head(position, 1)),
                # Ht = head(Ht, 1),
                # Wt = head(Wt, 1),
                # Density = round(head(Density, 1),2),
                # RAS = head(RAS, 1),
                Seasons = factor(n(), levels = 1:7),
                AvgWAR = round(head(AvgWAR, 1), 4),
                MaxWAR = round(head(MaxWAR, 1), 4),
                Entropy = round(head(Ent, 1), 3),
                # CoverageMetric = round(head(Metric, 1)*100, 2)
                ) %>% 
      group_by() %>% 
      arrange(desc(AvgWAR)) 
      


    AvgWARbrks <- quantile(Shannontable$AvgWAR, probs = seq(.05, .95, .01), na.rm = TRUE)
    AvgWARbrksy <- round(seq(255, 40, length.out = length(AvgWARbrks) + 1), 0)
    AvgWARbrksclrs <- paste0("rgb(", AvgWARbrksy, "," , 255-AvgWARbrksy , ",", 0, ")")

    MaxWARbrks <- quantile(Shannontable$MaxWAR, probs = seq(.05, .95, .01), na.rm = TRUE)
    MaxWARbrksy <- round(seq(255, 40, length.out = length(MaxWARbrks) + 1), 0)
    MaxWARbrksclrs <- paste0("rgb(", MaxWARbrksy, "," , 255-MaxWARbrksy , ",", 0, ")")


    Entropybrks <- quantile(Shannontable$Entropy, probs = seq(.05, .95, .01), na.rm = TRUE)
    Entropybrksy <- round(seq(255, 40, length.out = length(Entropybrks) + 1), 0)
    Entropybrksclrs <- paste0("rgb(", Entropybrksy, "," , 255-Entropybrksy , ",", 0, ")")
    
    # CoverageMetricbrks <- quantile(Shannontable$CoverageMetric, probs = seq(.05, .95, .01), na.rm = TRUE)
    # CoverageMetricbrksy <- round(seq(255, 40, length.out = length(CoverageMetricbrks) + 1), 0)
    # CoverageMetricbrksclrs <- paste0("rgb(", CoverageMetricbrksy, "," , 255-CoverageMetricbrksy , ",", 0, ")")
    
    datatable(Shannontable, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 20,
                                                     dom = 'Bfrtip',                      
                                                     buttons = c('csv', 'excel')))     %>%
      formatStyle("AvgWAR",
                  backgroundColor = styleInterval(AvgWARbrks,AvgWARbrksclrs)
      )%>%
      formatStyle("MaxWAR",
                  backgroundColor = styleInterval(MaxWARbrks,MaxWARbrksclrs)
      )%>%
      formatStyle("Entropy",
                  backgroundColor = styleInterval(Entropybrks,Entropybrksclrs)
      )
    # %>%
      # formatStyle("CoverageMetric",
      #             backgroundColor = styleInterval(CoverageMetricbrks,CoverageMetricbrksclrs)
      # )
    
    
  })
  
  output$PrimarySecondarytable = DT::renderDataTable({
    
    
    Primarytable <- PrimarySecondary %>% 
      distinct() %>% 
      select(season, player_id, player.x, position, team_name.x, Primary, Secondary, PlaymakerRate) %>% 
      group_by(player_id) %>% 
      mutate(Seasons = n(),
             season = factor(season, levels = 2017:2023)) %>% 
      group_by(player_id, season) %>% 
      arrange(desc(season)) %>% 
      summarise(player = head(player.x, 1),
                team_name = head(team_name.x, 1),
                position = as.factor(head(position, 1)),
                Seasons = factor(head(Seasons, 1), levels = 1:7),
                Primary = round(head(Primary, 1), 4),
                Secondary = round(head(Secondary, 1), 4),
                PlaymakerRate = round(head(PlaymakerRate, 1), 1)) %>% 
      group_by() %>% 
      arrange(desc(Primary)) 
    
    
    
    Primarybrks <- quantile(Primarytable$Primary, probs = seq(.05, .95, .01), na.rm = TRUE)
    Primarybrksy <- round(seq(255, 40, length.out = length(Primarybrks) + 1), 0)
    Primarybrksclrs <- paste0("rgb(", Primarybrksy, "," , 255-Primarybrksy , ",", 0, ")")
    
    Secondarybrks <- quantile(Primarytable$Secondary, probs = seq(.05, .95, .01), na.rm = TRUE)
    Secondarybrksy <- round(seq(255, 40, length.out = length(Secondarybrks) + 1), 0)
    Secondarybrksclrs <- paste0("rgb(", Secondarybrksy, "," , 255-Secondarybrksy , ",", 0, ")")
    
    
    PlaymakerRatebrks <- quantile(Primarytable$PlaymakerRate[Primarytable$position %in% c("CB", "S", "LB")], probs = seq(.05, .95, .01), na.rm = TRUE)
    PlaymakerRatebrksy <- round(seq(255, 40, length.out = length(PlaymakerRatebrks) + 1), 0)
    PlaymakerRatebrksclrs <- paste0("rgb(", PlaymakerRatebrksy, "," , 255-PlaymakerRatebrksy , ",", 0, ")")
    
    
    datatable(Primarytable, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 20,
                                                     dom = 'Bfrtip',                      
                                                     buttons = c('csv', 'excel')))     %>%
      formatStyle("Primary",
                  backgroundColor = styleInterval(Primarybrks,Primarybrksclrs)
      )%>%
      formatStyle("Secondary",
                  backgroundColor = styleInterval(Secondarybrks,Secondarybrksclrs)
      )%>%
      formatStyle("PlaymakerRate",
                  backgroundColor = styleInterval(PlaymakerRatebrks,PlaymakerRatebrksclrs)
      )
    
    
  })
  
  output$PrimarySecondaryOfftable = DT::renderDataTable({
    
    
    PrimaryOfftable <- OffCollegeWARJoin %>% 
      inner_join(DeclaresList, by = c("player_id")) %>% 
      select(season, player_id, player, position, team_name.x, Primary, Secondary,
             #Ht, Wt, Density, RAS
             ) %>%
      group_by(player_id) %>%
      mutate(Seasons = n(),
             season = factor(season, levels = 2017:2023)) %>%
      mutate(Seasons = factor(Seasons, levels = 1:6)) %>% 
      group_by(player_id, season) %>%
      arrange(desc(season)) %>%
      summarise(player = head(player, 1),
                team_name = head(team_name.x, 1),
                position = as.factor(head(position, 1)),
                Seasons = head(Seasons, 1),
                # Ht = head(Ht, 1),
                # Wt = head(Wt, 1),
                # Density = round(head(Density, 1),2),
                # RAS = head(RAS, 1),
                Primary = round(head(Primary, 1), 4),
                Secondary = round(head(Secondary, 1), 4)) %>%
      group_by() %>%
      arrange(desc(Primary))
    
    
    
    Primarybrks1 <- quantile(PrimaryOfftable$Primary, probs = seq(.05, .95, .01), na.rm = TRUE)
    Primarybrksy1 <- round(seq(255, 40, length.out = length(Primarybrks1) + 1), 0)
    Primarybrksclrs1 <- paste0("rgb(", Primarybrksy1, "," , 255-Primarybrksy1 , ",", 0, ")")

    Secondarybrks1 <- quantile(PrimaryOfftable$Secondary, probs = seq(.05, .95, .01), na.rm = TRUE)
    Secondarybrksy1 <- round(seq(255, 40, length.out = length(Secondarybrks1) + 1), 0)
    Secondarybrksclrs1 <- paste0("rgb(", Secondarybrksy1, "," , 255-Secondarybrksy1 , ",", 0, ")")
    
    
    datatable(PrimaryOfftable, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 20,
                                                     dom = 'Bfrtip',                      
                                                     buttons = c('csv', 'excel')))  %>%
      formatStyle("Primary",
                  backgroundColor = styleInterval(Primarybrks1,Primarybrksclrs1)
      )%>%
      formatStyle("Secondary",
                  backgroundColor = styleInterval(Secondarybrks1,Secondarybrksclrs1)
      )

    
  })
  
  output$ExplosiveTable = DT::renderDataTable({
    
    
    ExplosiveTable <- ExplosiveRate %>% 
      inner_join(DeclaresList, by = c("player_id")) %>% 
      select(season, player_id, player, team_name.x, ForcedMissedTackleRate, ExplosiveRate, YardsAfterContact) %>%
      group_by(player_id) %>%
      mutate(Seasons = factor(n(), levels = 1:3),
             season = factor(season, levels = 2018:2023)) %>%
      group_by(player_id, season) %>%
      arrange(desc(season)) %>%
      summarise(player = head(player, 1),
                team_name = head(team_name.x, 1),
                Seasons = head(Seasons, 1),
                ForcedMissedTackleRate = round(round(head(ForcedMissedTackleRate, 1), 4)*100, 4),
                ExplosiveRate = round(round(head(ExplosiveRate, 1), 4)*100, 4),
                YardsAfterContact = head(YardsAfterContact, 1),
                ) %>%
      group_by() %>%
      arrange(desc(ExplosiveRate))
    
    
    
    ForcedMissedTackleRatebrks1 <- quantile(ExplosiveTable$ForcedMissedTackleRate, probs = seq(.05, .95, .01), na.rm = TRUE)
    ForcedMissedTackleRatebrksy1 <- round(seq(255, 40, length.out = length(ForcedMissedTackleRatebrks1) + 1), 0)
    ForcedMissedTackleRatebrksclrs1 <- paste0("rgb(", ForcedMissedTackleRatebrksy1, "," , 255-ForcedMissedTackleRatebrksy1 , ",", 0, ")")
    
    ExplosiveRatebrks1 <- quantile(ExplosiveTable$ExplosiveRate, probs = seq(.05, .95, .01), na.rm = TRUE)
    ExplosiveRatebrksy1 <- round(seq(255, 40, length.out = length(ExplosiveRatebrks1) + 1), 0)
    ExplosiveRatebrksclrs1 <- paste0("rgb(", ExplosiveRatebrksy1, "," , 255-ExplosiveRatebrksy1 , ",", 0, ")")
    
    
    YardsAfterContactbrks1 <- quantile(ExplosiveTable$YardsAfterContact, probs = seq(.05, .95, .01), na.rm = TRUE)
    YardsAfterContactbrksy1 <- round(seq(255, 40, length.out = length(YardsAfterContactbrks1) + 1), 0)
    YardsAfterContactbrksclrs1 <- paste0("rgb(", YardsAfterContactbrksy1, "," , 255-YardsAfterContactbrksy1 , ",", 0, ")")
    
    
    
    datatable(ExplosiveTable, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 20,
                                                     dom = 'Bfrtip',                      
                                                     buttons = c('csv', 'excel')))  %>%
      formatStyle("ForcedMissedTackleRate",
                  backgroundColor = styleInterval(ForcedMissedTackleRatebrks1,ForcedMissedTackleRatebrksclrs1)
      )%>%
      formatStyle("ExplosiveRate",
                  backgroundColor = styleInterval(ExplosiveRatebrks1,ExplosiveRatebrksclrs1)
      )%>%
      formatStyle("YardsAfterContact",
                  backgroundColor = styleInterval(YardsAfterContactbrks1,YardsAfterContactbrksclrs1)
      )
    
    
  })
  
  output$PassBlocktable = DT::renderDataTable({
    
    
    PassBlockTable <- PassBlock %>% 
      inner_join(DeclaresList, by = c("player_id")) %>% 
      select(season, player_id, player, position, team_name.x, Snaps, WAR, true_pass_set_pbe) %>%
      group_by(player_id) %>%
      mutate(Seasons = n(),
             season = factor(season, levels = 2017:current_season)) %>%
      group_by(player_id, season) %>%
      arrange(desc(season)) %>%
      summarise(player = head(player, 1),
                team_name = head(team_name.x, 1),
                Position = as.factor(head(position, 1)),
                Seasons = factor(head(Seasons, 1), levels = 1:7),
                Snaps = head(Snaps,1),
                TruePassSetWAR = round(head(WAR, 1), 4),
                TruePassSetPBE = head(true_pass_set_pbe, 1),
      ) %>%
      group_by() %>%
      arrange(desc(TruePassSetWAR))
    
    
    
    TruePassSetWARbrks1 <- quantile(PassBlockTable$TruePassSetWAR, probs = seq(.05, .95, .01), na.rm = TRUE)
    TruePassSetWARbrksy1 <- round(seq(255, 40, length.out = length(TruePassSetWARbrks1) + 1), 0)
    TruePassSetWARbrksclrs1 <- paste0("rgb(", TruePassSetWARbrksy1, "," , 255-TruePassSetWARbrksy1 , ",", 0, ")")
    
    TruePassSetWinRatebrks1 <- quantile(PassBlockTable$TruePassSetPBE, probs = seq(.05, .95, .01), na.rm = TRUE)
    TruePassSetWinRatebrksy1 <- round(seq(255, 40, length.out = length(TruePassSetWinRatebrks1) + 1), 0)
    TruePassSetWinRatebrksclrs1 <- paste0("rgb(", TruePassSetWinRatebrksy1, "," , 255-TruePassSetWinRatebrksy1 , ",", 0, ")")
    
    
    
    datatable(PassBlockTable, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 20,
                                                     dom = 'Bfrtip',                      
                                                     buttons = c('csv', 'excel')))  %>%
      formatStyle("TruePassSetWAR",
                  backgroundColor = styleInterval(TruePassSetWARbrks1,TruePassSetWARbrksclrs1)
      )%>%
      formatStyle("TruePassSetPBE",
                  backgroundColor = styleInterval(TruePassSetWinRatebrks1,TruePassSetWinRatebrksclrs1)
      )
    
    
  })
  
  output$PassRushtable = DT::renderDataTable({
    
    
    PassRushTable <- PassRush %>% 
      inner_join(DeclaresList, by = c("player_id")) %>% 
      select(season, player_id, player, position, team_name.x, Snaps, WAR, true_pass_set_pass_rush_win_rate) %>%
      group_by(player_id) %>%
      mutate(Seasons = n(),
             season = factor(season, levels = 2017:2023)) %>%
      group_by(player_id, season) %>%
      arrange(desc(season)) %>%
      summarise(player = head(player, 1),
                team_name = head(team_name.x, 1),
                Position = as.factor(head(position, 1)),
                Seasons = factor(head(Seasons, 1), levels = 1:7),
                Snaps = head(Snaps,1),
                TruePassSetWAR = round(head(WAR, 1), 4),
                TruePassSetWinRate = head(true_pass_set_pass_rush_win_rate, 1),
      ) %>%
      group_by() %>%
      arrange(desc(TruePassSetWAR))
    
    
    
    TruePassSetWARbrks1 <- quantile(PassRushTable$TruePassSetWAR, probs = seq(.05, .95, .01), na.rm = TRUE)
    TruePassSetWARbrksy1 <- round(seq(255, 40, length.out = length(TruePassSetWARbrks1) + 1), 0)
    TruePassSetWARbrksclrs1 <- paste0("rgb(", TruePassSetWARbrksy1, "," , 255-TruePassSetWARbrksy1 , ",", 0, ")")
    
    TruePassSetWinRatebrks1 <- quantile(PassRushTable$TruePassSetWinRate, probs = seq(.05, .95, .01), na.rm = TRUE)
    TruePassSetWinRatebrksy1 <- round(seq(255, 40, length.out = length(TruePassSetWinRatebrks1) + 1), 0)
    TruePassSetWinRatebrksclrs1 <- paste0("rgb(", TruePassSetWinRatebrksy1, "," , 255-TruePassSetWinRatebrksy1 , ",", 0, ")")
    
    
    
    datatable(PassRushTable, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 20,
                                                     dom = 'Bfrtip',                      
                                                     buttons = c('csv', 'excel')))  %>%
      formatStyle("TruePassSetWAR",
                  backgroundColor = styleInterval(TruePassSetWARbrks1,TruePassSetWARbrksclrs1)
      )%>%
      formatStyle("TruePassSetWinRate",
                  backgroundColor = styleInterval(TruePassSetWinRatebrks1,TruePassSetWinRatebrksclrs1)
      )
    
    
  })
  
  
  
  PickChoiceOpto <- reactive({
    AllDraftCapital %>% filter(AllDraftCapital$Code == input$TeamOpto, Season == current_season)
  })
  
  
  output$PickExcludeOpto <- renderUI({
    checkboxGroupInput("PickExclusionOpto", "Select Picks you want to Exclude:", PickChoiceOpto()$Pick, inline  = T, selected = c(248, 204, 200))
  })
  
  
  
  output$FilteredSelections = DT::renderDataTable({
    
    if (length(input$PickExclusionOpto) == 0) {
      Exclude_list_down <-c(248, 204, 200)
    } else {
      Exclude_list_down <- input$PickExclusionOpto
    }
    
    
    # Values to filter
    values_to_filter <- as.list(DraftCapital %>% 
          filter(Code == input$TeamOpto,
                 !(Pick %in% Exclude_list_down)) %>% 
                                  select(Pick))[["Pick"]]
    
    
    # Apply the function to each value and bind the results
    filtered_df <- map_dfr(values_to_filter, ~ filter_values(BigBoardPlayerValues, .x, 5))
    
    
    filtered_df <- filtered_df %>% 
      filter(position %in% input$PosSelectOpto) %>%
      mutate(Value = round(smoothed_value, 2),
             Closest_Pick = nearest_value) %>% 
      select(Rank, Closest_Pick, Player, position, age, Value) %>% 
      arrange(desc(Value)) %>% 
      arrange(Closest_Pick)
    
    
    Valuebrks1 <- quantile(filtered_df$Value, probs = seq(.05, .95, .01), na.rm = TRUE)
    Valuebrksy1 <- round(seq(255, 40, length.out = length(Valuebrks1) + 1), 0)
    Valuebrksclrs1 <- paste0("rgb(", Valuebrksy1, "," , 255-Valuebrksy1 , ",", 0, ")")
    
    
    datatable(filtered_df, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 50,
                                                     dom = 'Bfrtip',                      
                                                     buttons = c('csv', 'excel')))  %>%
      formatStyle("Value",
                  backgroundColor = styleInterval(Valuebrks1,Valuebrksclrs1)
      )
    
    
  })
  
  
  output$FinalSelections = DT::renderDataTable({
    
    if (length(input$PickExclusionOpto) == 0) {
      Exclude_list_down <-c(248, 204, 200)
    } else {
      Exclude_list_down <- input$PickExclusionOpto
    }
    
    
    # Values to filter
    values_to_filter <- as.list(DraftCapital %>% 
                                  filter(Code == input$TeamOpto,
                                         !(Pick %in% Exclude_list_down)) %>% 
                                  select(Pick))[["Pick"]]
    
    
    # Apply the function to each value and bind the results
    filtered_df <- map_dfr(values_to_filter, ~ filter_values(BigBoardPlayerValues, .x, 5))
    
    
    filtered_df <- filtered_df %>% 
      filter(position %in% input$PosSelectOpto) %>%
      mutate(Value = smoothed_value + 
                             ((100-Rank)/10000000),
             Closest_Pick = nearest_value) %>% 
      select(Rank, Closest_Pick, Player, position, age, Value) %>% 
      arrange(desc(Value)) %>% 
      arrange(Closest_Pick)
    
    player_list <- filtered_df %>% 
      select(Player) %>% 
      distinct()
    
    player_list_count <- nrow(player_list)
    
    num_groups <- length(position_list)
    
    num_values <- length(values_to_filter)
    
    num_total_values <- nrow(filtered_df)
    
    constraint_matrix <- matrix(0, nrow = num_values + num_groups + player_list_count, ncol = num_total_values)
    
    for (row in 1:num_values) {
      constraint_matrix[row, ] <- ifelse(filtered_df$Closest_Pick == values_to_filter[row], 1, 0)
    }
    
    for (row in 1:num_groups) {
      constraint_matrix[num_values + row, ] <- ifelse(filtered_df$position == position_list[row], 1, 0)
    }
    
    for (i in 1:player_list_count) {
      player <- player_list$Player[i]
      indices <- which(filtered_df$Player == player)
      constraint_matrix[num_values + num_groups + i, indices] <- 1
    }
    

    # Create a linear optimization model
    lp_model <- lp(direction = "max",
                   objective.in = filtered_df$Value,
                   const.mat = constraint_matrix,
                   const.dir = c(rep("=", num_values),
                                 rep("<=", num_groups),
                                 rep("<=",player_list_count)),
                   const.rhs = c(rep(1, num_values),
                                 rep(input$PosCountOpto, num_groups),
                                 rep(1, player_list_count)),
                   all.bin = TRUE)
    
    
    lp_model$solution
    
    # Extract selected indices
    selected_indices <- which(lp_model$solution == 1)
    
    # Filter the dataframe to include only the selected rows
    Final_selections <- filtered_df[selected_indices, ] %>% mutate(Value = round(Value, 2))
    
    
    Valuebrks1 <- quantile(Final_selections$Value, probs = seq(.05, .95, .01), na.rm = TRUE)
    Valuebrksy1 <- round(seq(255, 40, length.out = length(Valuebrks1) + 1), 0)
    Valuebrksclrs1 <- paste0("rgb(", Valuebrksy1, "," , 255-Valuebrksy1 , ",", 0, ")")
    
    
    datatable(Final_selections, rownames = F, filter = "top",
              extensions = 'Buttons', options = list(pageLength = 50,
                                                     dom = 'Bfrtip',                      
                                                     buttons = c('csv', 'excel')))  %>%
      formatStyle("Value",
                  backgroundColor = styleInterval(Valuebrks1,Valuebrksclrs1)
      )
    
    
  })
  
  
# 
#   output$DefMetrics = DT::renderDataTable({
# 
# 
#     ShannontableFull <- CWARFULL %>%
#       left_join(ShannonEntropy, by = c("player_id")) %>% 
#       left_join(CBMetrics, by = c("position", "player_id")) %>%
#       left_join(SMetrics, by = c("position", "player_id")) %>%
#       filter(Snaps >= 150) %>%
#       distinct() %>% 
#       filter(Conference == "ACC" | Conference == "SEC" |
#                Conference == "Big 12" | Conference == "Big Ten" |
#                Conference == "Pac-12" | team_name == "NOTRE DAME" |
#                team_name == "BUFFALO" | team_name == "UCF"|
#                team_name == "BOISE ST" | team_name == "SMU" | team_name == "TOLEDO" |
#                team_name == "HOUSTON") %>%
#       mutate(Metric = if_else(position == "CB", Metric.x, Metric.y)) %>%
#       select(season, player_id, player, position,
#              #Ht, Wt, Density, RAS,
#              team_name, AvgWAR, MaxWAR, Ent, Metric) %>%
#       group_by(player_id) %>%
#       arrange(desc(season)) %>%
#       filter(position %in% c("S", "CB"))%>%
#       summarise(player = head(player, 1),
#                 team_name = head(team_name, 1),
#                 position = as.factor(head(position, 1)),
#                 # Ht = head(Ht, 1),
#                 # Wt = head(Wt, 1),
#                 # Density = round(head(Density, 1),2),
#                 # RAS = head(RAS, 1),
#                 Seasons = factor(n(), levels = 1:7),
#                 AvgWAR = round(head(AvgWAR, 1), 4),
#                 MaxWAR = round(head(MaxWAR, 1), 4),
#                 Entropy = round(head(Ent, 1), 3),
#                 CoverageMetric = round(head(Metric, 1)*100, 2)) %>%
#       group_by() %>%
#       arrange(desc(CoverageMetric))
# 
# 
# 
#     AvgWARbrks <- quantile(ShannontableFull$AvgWAR, probs = seq(.05, .95, .01), na.rm = TRUE)
#     AvgWARbrksy <- round(seq(255, 40, length.out = length(AvgWARbrks) + 1), 0)
#     AvgWARbrksclrs <- paste0("rgb(", AvgWARbrksy, "," , 255-AvgWARbrksy , ",", 0, ")")
# 
#     MaxWARbrks <- quantile(ShannontableFull$MaxWAR, probs = seq(.05, .95, .01), na.rm = TRUE)
#     MaxWARbrksy <- round(seq(255, 40, length.out = length(MaxWARbrks) + 1), 0)
#     MaxWARbrksclrs <- paste0("rgb(", MaxWARbrksy, "," , 255-MaxWARbrksy , ",", 0, ")")
# 
#     
#     Entropybrks <- quantile(ShannontableFull$Entropy, probs = seq(.05, .95, .01), na.rm = TRUE)
#     Entropybrksy <- round(seq(255, 40, length.out = length(Entropybrks) + 1), 0)
#     Entropybrksclrs <- paste0("rgb(", Entropybrksy, "," , 255-Entropybrksy , ",", 0, ")")
# 
#     CoverageMetricbrks <- quantile(ShannontableFull$CoverageMetric, probs = seq(.05, .95, .01), na.rm = TRUE)
#     CoverageMetricbrksy <- round(seq(255, 40, length.out = length(CoverageMetricbrks) + 1), 0)
#     CoverageMetricbrksclrs <- paste0("rgb(", CoverageMetricbrksy, "," , 255-CoverageMetricbrksy , ",", 0, ")")
# 
#     datatable(ShannontableFull, rownames = F, filter = "top",
#               extensions = 'Buttons', options = list(pageLength = 20,
#                                                      dom = 'Bfrtip',
#                                                      buttons = c('csv', 'excel')))     %>%
#       formatStyle("AvgWAR",
#                   backgroundColor = styleInterval(AvgWARbrks,AvgWARbrksclrs)
#       )%>%
#       formatStyle("MaxWAR",
#                   backgroundColor = styleInterval(MaxWARbrks,MaxWARbrksclrs)
#       )%>%
#       formatStyle("Entropy",
#                   backgroundColor = styleInterval(Entropybrks,Entropybrksclrs)
#       )%>%
#       formatStyle("CoverageMetric",
#                   backgroundColor = styleInterval(CoverageMetricbrks,CoverageMetricbrksclrs)
#       )
# 
# 
#   })
  


}

# Run the application 
shinyApp(ui = ui, server = server)
