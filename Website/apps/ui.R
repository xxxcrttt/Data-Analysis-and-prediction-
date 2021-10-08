#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)

#Competition List
Competition <- as.character(levels(factor(team_data$Competition)))
#Team  List
Team <- as.character(levels(factor(team_data$Team)))
Match_1 <- as.character(levels(factor(data$match_1)))
Match <- as.character(levels(factor(data$match)))
playoffs <- as.character(levels(factor(data$playoffs)))


#Define Header
Header <- dashboardHeader(title = "LOL Esports Stats")
#Define Sidebar
sidebar <- dashboardSidebar({
  sidebarMenu(
    menuItem("Basic Info", tabName = "dashboard", icon = icon("dashboard")),
    selectInput("Competition", "Select a Season:",
                choices = Competition),
    
    sidebarMenu((
      menuItem("Competition Data", tabName = "Widgets", icon = icon("chart-bar"))
    )),
    
    sidebarMenu((
      menuItem("Leaderborad", tabName = "rank", icon = icon("chart-bar"))
    )),

    uiOutput("select_team"),
    
    sidebarMenu((
      menuItem("Team Data", tabName = "teams", icon = icon("chart-bar"))
    )),
    
    uiOutput("select_match"),
    uiOutput("select_game"),
    
    
    sidebarMenu((
      menuItem("General Game Info", tabName = "Games", icon = icon("chart-bar"))
    )),
  
    sidebarMenu((
      menuItem("Players Info", tabName = "Players", icon = icon("chart-bar"))
    ))
     
    
  )
})


#Define Body
body <- dashboardBody(
    tabItems(
        #first tab content
        tabItem(tabName = "dashboard",
        fluidRow(
          #first box content
            box( 
                title = tagList(shiny::icon("gear"), "Basic Information"),
                width = 12,collapsible = TRUE,
                textOutput("basic_introduction"),tags$head(tags$style("#basic_introduction{
                                 color: black;
                                 font-size: 20px;
                                 }"))),
          #second box content
            box(title = tagList(shiny::icon("gear"), "How to use"),
            width = 12,collapsible = TRUE,
            h4('1. Please ',span('Select a Season', style = 'font-weight:bold'),'you want to query in the sidebar. The ',
              span('Competition Data ', style = 'font-weight:bold'),'column shows the data scatter diagram of the teams and players in each position in this season. The ',
              span('Leaderboard',style = 'font-weight:bold'),'column shows the ranking of some important data in this season.'),
            br(),h4('2. After selecting the Season, please ', span('Select a Team', style = 'font-weight:bold'),'you want to query in the sidebar. The ',
              span('Team Data ', style = 'font-weight:bold'),'column shows the overall performance of this team in this season.'),
            br(),h4('3. After selecting the Season and the Team, please', span('Select a Match', style = 'font-weight:bold'), 'you want to query and the corresponding number of game in the sidebar. The ',
              span('General Game Info ', style = 'font-weight:bold'),'column shows the overall match score and specific game data of this match. The ',
              span('Players Info ', style = 'font-weight:bold'),'column shows the data of players participating in this game.')
        ))),
         # Second tab content
        tabItem(tabName = "Widgets",
             fluidRow(
               #first box content
                 box(title = tagList(shiny::icon("gear"), "Technical Terminology"),
                    width = 12,collapsible = TRUE,
                    h4('DPM=damage per minute, GPM=gold per minute, CSPM = creeps per minute,
                       DTPM = damage taken per minute'),
                    h4('DMG_pct = the percentage of damage, DTPM_pct = the percentage of damage taken'),
                    h4('KP = participation rate = (Players kills + assists)/(Teams total kills)'), 
                    h4('WPM = wards per minute, WCPM = wards kills per minute, VSPM = vision score per minute'),
                    h4('Kills = The average number of kills per game in all matches that the team/player participated in this season.'),
                    h4('Deaths = The average number of deaths per game in all matches that the team/player participated in this season.'),
                    h4('Assists = The average number of assists per game in all matches that the team/player participated in this season.'),
                    h4('Towers = The average number of enemy defensive towers destroyed per game in all matches that the team participated in this season.'),
                    h4('Dragons = The average number of times the team has killed the dragons per game in all matches that the team participated in this season.'),
                    h4('Barons = The average number of times the team has killed the barons per game in all matches that the team participated in this season.'),
                    h4(span('For each scatter plot, X and Y labelled the specific information. Teams and players stats can be viewed by hovering over the circle.', style = "color:blue"))
                    )
                    
                    
                    ),
               #second box content
             fluidRow(   
               tabBox(
                 title = tagList(shiny::icon("align-justify"), "Comparision between teams and players"),width = 12,
                 tabPanel("Team",plotlyOutput("Team",height = '1000px', width = '1000px')),
                 tabPanel("Top",plotlyOutput("Top",height = '1000px', width = '1000px')),
                 tabPanel("Jug",plotlyOutput("Jug",height = '1000px', width = '1000px')),
                 tabPanel("Mid",plotlyOutput("Mid",height = '1000px', width = '1000px')),
                 tabPanel("Bot",plotlyOutput("Bot",height = '1000px', width = '1000px')),
                 tabPanel("Sup",plotlyOutput("Sup",height = '1000px', width = '1000px'))
                          
                 )     
               )
             ),
        #Player Data Leaderborad
        tabItem(tabName = "rank",fluidRow(
          box(title = tagList(shiny::icon("gear"),"Introduction"),width = 12, collapsible = TRUE, textOutput("rank_text"),tags$head(tags$style("#rank_text{
                                 text-align:left;
                                 color: black;
                                 font-size: 20px;
                                 }")))),
          fluidRow(
            box(title = tagList(shiny::icon("trophy"),"Winner"),width = 12,background = 'red',collapsible = TRUE, textOutput('Winner_of_season1'),tags$head(tags$style("#Winner_of_season1{
                                 text-align:center;
                                 color: white;
                                 font-size: 30px;
                                 }")),
                textOutput('Winner_of_season2'),tags$head(tags$style("#Winner_of_season2{
                                 text-align:center;
                                 color: white;
                                 font-size: 30px;
                                 }"))
                )),
          fluidRow(
            tabBox(title= " ",width = 12,
              tabPanel("Team", fluidRow(         
                 box(title = tagList(shiny::icon("check-double"),"Team Win Ratio Leaderboard"),width = 6, collapsible = TRUE,textOutput('first_win_rate_text'),tags$head(tags$style("#first_win_rate_text{
                                 text-align:left;
                                 color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")), plotlyOutput("first_win_rate",height = '400px', width = '400px')),
                 box(title = tagList(shiny::icon("check-double"),"Team Kills Leaderboard"),width = 6, collapsible = TRUE,textOutput('first_kills_text'),tags$head(tags$style("#first_kills_text{
                                 text-align:left;
                                 color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")), plotlyOutput("first_kills",height = '400px', width = '400px')),
                 box(title = tagList(shiny::icon("chart-bar"),"Team Vicotry and Defeat Data"),width = 12, collapsible = TRUE, plotlyOutput("Teams_win_rate"))
        )),
              tabPanel("Player", fluidRow(
                box(title = tagList(shiny::icon("check-double"),"Player Kills Leaderboard"),width = 6, collapsible = TRUE, textOutput('player_kills_text'),tags$head(tags$style("#player_kills_text{
                                 text-align:left;
                                 color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")),plotlyOutput("player_kills",height = '400px', width = '400px')),
                box(title = tagList(shiny::icon("check-double"),"Player Deaths Leaderboard"),width = 6, collapsible = TRUE, textOutput('player_deaths_text'),tags$head(tags$style("#player_deaths_text{
                                 text-align:left;
                                 color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")),plotlyOutput("player_deaths",height = '400px', width = '400px'))
              ),
              fluidRow(
                box(title = tagList(shiny::icon("arrow-up"),"Player DPM Leaderboard"),width = 6, collapsible = TRUE, formattableOutput('Player_DPM_Leaderboard')),
                box(title = tagList(shiny::icon("arrow-up"),"Player GPM Leaderboard"),width = 6, collapsible = TRUE, formattableOutput('Player_GPM_Leaderboard'))    
              )
              
              )
        ))
        ),
                
        #Team Data Tab Content
        tabItem(tabName = "teams", 
                fluidRow(
                  #first Tab
                  box(title = tagList(shiny::icon("gear"),"Introduction"),width = 12, collapsible = TRUE,
                      h4('This page displays the KDA data and Towers&Dragons data of the team and all players in this team that has participated in the season.')
                      ),
                  box(title = tagList(shiny::icon("android"),"KDA Radar Plot for Team"),width = 6, collapsible = TRUE, plotlyOutput("radar_KDA_team")),
                  box(title = tagList(shiny::icon("android"),"Dragons and Towers Radar Plot for Team"),width = 6, collapsible = TRUE, plotlyOutput("radar_team")),
                  box(title = tagList(shiny::icon("signal"),"Players KDA for the Season"),width = 12, plotlyOutput("Player",height = '500px', width = '1000px'))
                   )
        ),
  
        #Game Tab Content
        tabItem(tabName = "Games",
                fluidRow(
                  #First Tab
                  box(title = tagList(shiny::icon("trophy"), "Game Result"),width = 6, collapsible = TRUE,background = "red",formattableOutput("Game_result")),
                  box(width = 6, collapsible = TRUE,background = "teal",textOutput("text1"),tags$head(tags$style("#text1{
                                 text-align:center;
                                 color: white;
                                 font-size: 40px;
                                 font-style: bold;
                                 }"
                  )
                  ),
                  textOutput("text2"),tags$head(tags$style("#text2{
                                 text-align:center;
                                 color: white;
                                 font-size: 50px;
                                 font-style: bold;
                                 }"
                  )))
                  ),
                fluidRow(
                  box(title = tagList(shiny::icon("chart-bar"), "Basic Game Info"),width = 12, collapsible = TRUE,formattableOutput("Game_Info"))
                ),
                fluidRow(
                  box(title = tagList(shiny::icon("user-slash"), "Ban"),width = 12, collapsible = TRUE,formattableOutput("Ban_Info"))
                ),
                fluidRow(
                  box(title = tagList(shiny::icon("chart-bar"), "Damage Settlement Panel"), width = 12, collapsible = TRUE, plotlyOutput("Damage"))),
                fluidRow(
                  box(title = tagList(shiny::icon("chart-bar"), "Gold Difference"),width = 12, collapsible = TRUE,plotlyOutput("Gold"))
                )
          
              ),
      
        #Fourth Tab Content
        tabItem(tabName = "Players",
                fluidRow(
                  #First Tab
                  box(title = tagList(shiny::icon("id-badge"), "Players -- Blue Side"),width = 6, collapsible = TRUE,background = "blue",formattableOutput("Competitors1")),
                  box(title = tagList(shiny::icon("id-badge"), "Players -- Red Side"),width = 6, collapsible = TRUE,background = "red",formattableOutput("Competitors2"))
                ),
                fluidRow(
                  #Players Compare
                  tabBox(title = tagList(shiny::icon("user-astronaut"), "General Players Information"),width = 12,
                    tabPanel("Team",fluidRow(
                        box("",width = 12, collapsible = TRUE,
                      formattableOutput("Players_team1"),formattableOutput("Players_team2"))
                        
                         )),
                    
                    tabPanel("Players-DMG",
                             fluidRow(
                             box(title = tagList(shiny::icon("gear"), "Introduction"),width =  12, collapsible = TRUE,
                                 h4('The following plots show the comparison between DPM, GPM and DTPM of the ten players who participated in this game. '),
                                 h4('The left half is the blue side, and the right half is the red side.'),
                                 h4('The same color represents the same position.')),
                             box(title = tagList(shiny::icon("chart-bar"), "Damage to Champions per Minute -- Blue Side"),sund2bOutput("player_dpm1"),width =  6, collapsible = TRUE),
                             box(title = tagList(shiny::icon("chart-bar"), "Damage to Champions per Minute -- Red Side"),sund2bOutput("player_dpm2"),width =  6, collapsible = TRUE),
                             box(title = tagList(shiny::icon("chart-bar"), "Earned Gold per Minute -- Blue Side"),sund2bOutput("player_gpm1"),width =  6, collapsible = TRUE),
                             box(title = tagList(shiny::icon("chart-bar"), "Earned Gold per Minute -- Red Side"),sund2bOutput("player_gpm2"),width =  6, collapsible = TRUE),
                             box(title = tagList(shiny::icon("chart-bar"), "Damage Taken per Minute -- Blue Side"),sund2bOutput("player_dtpm1"),width =  6, collapsible = TRUE),
                             box(title = tagList(shiny::icon("chart-bar"), "Damage Taken per Minute -- Red Side"),sund2bOutput("player_dtpm2"),width =  6, collapsible = TRUE)
                             )
                             
                ),
                    tabPanel("Players-CS/XP",
                             fluidRow(
                               box(title = tagList(shiny::icon("chart-bar"),"Players CS difference at 15"),width = 12,collapsible = TRUE,plotlyOutput("CS")),
                               box(title = tagList(shiny::icon("chart-bar"),"Players XP difference at 15"),width = 12,collapsible = TRUE,plotlyOutput("XP"))
                             
                             )
        )
      
        )
        
      )
)
)
)
    
  


    

  




    
    


ui <- dashboardPage(
    Header,
    sidebar,
    body
)