#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggrepel)
library(plotly)
library(cowplot)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #Basic Info
    output$basic_introduction <- renderText({
      "This is a data interactive website that can search for past match records. 
      This website mainly focuses on the visualization of the game data, 
      aiming to help pro-players and audiences to understand the content and outcome 
      of the game to the greatest extent. Ongoing match series results will be updated regularly."
    })
  
    Team_data <- reactive({
      team_data %>%
        filter(Competition == input$Competition)
    })
    output$Team <- renderPlotly({
      if (is.null(Team_data)) { return(NULL)
      } else {
        p1 <- ggplot(Team_data(),aes(x = DPM,y = GPM,size = CSPM, label = Team)) + 
          geom_point(colour = 'red', alpha = 0.5) +
          scale_size_continuous(range = c(4, 8)) + 
          xlab("DPM") + ylab("GPM") +
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none")) 
        p1 <- ggplotly(p1, tooltip = c("label","x","y","size"))
          
        p2 <- ggplot(Team_data(),aes(x = Kills,y = Deaths,size = KDA, label = Team)) + 
          geom_point(colour = 'green',alpha = 0.5) +
          scale_size_continuous(range = c(4, 8)) +
          labs(x = "Team.Kills", y = "Team.Deaths") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) + 
          theme(legend.position = c("none"))
        p2 <- ggplotly(p2,tooltip = c("label","x","y","size")) 
         
        p3 <- ggplot(Team_data(),aes(x = Towers,y = Barons ,size = Dragons, label = Team)) + 
          geom_point(colour = 'blue', alpha = 0.5) +
          scale_size_continuous(range = c(4,8)) +
          labs(x = "Tower.Kills", y = "Baron.Kills") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none")) 
        p3 <- ggplotly(p3,tooltip = c("label","x","y","size"))
          
        
        p4 <- ggplot(Team_data(),aes(x = WPM,y = WCPM ,size = VSPM, label = Team)) + 
          geom_point(colour = 'yellow',alpha = 0.5) +
          scale_size_continuous(range = c(4, 8)) +
          labs(x = "WPM", y = "WCPM") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none")) 
        p4 <- ggplotly(p4,tooltip = c("label","x","y","size")) 
        subplot(p1,p2,p3,p4,nrows = 2, titleX = TRUE, titleY = TRUE,margin = 0.06)
      }
      })
    
    ##players tabs
    # TOP TAB
    Player_datatop <- reactive({
       player_2020%>%
        filter(Competition == input$Competition) %>%
        filter(Position == 'Top')
    })
    output$Top <- renderPlotly({
      if (is.null(player_2020)) { return(NULL)
      } else {
        p1 <- ggplot(Player_datatop(),aes(x = DPM,y = GPM,size = DTPM, label = Player)) + 
          geom_point(colour = 'red', alpha = 0.5) +
          scale_size_continuous(range = c(2, 8)) + 
          xlab("DPM") + ylab("GPM") +
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none"))
        p1 <- ggplotly(p1, tooltip = c("label","x","y","size"))
        
        p2 <- ggplot(Player_datatop(),aes(x = Kills,y = Deaths,size = Assists, label =Player)) + 
          geom_point(colour = 'green',alpha = 0.5) +
          scale_size_continuous(range = c(2, 8)) +
          labs(x = "Kills", y = "Deaths") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) + 
          theme(legend.position = c("none")) 
        p2 <- ggplotly(p2,tooltip = c("label","x","y","size")) 
        
        p3 <- ggplot(Player_datatop(),aes(x = DMG_Pct,y = DTPM_Pct, size = KP, label = Player)) + 
          geom_point(colour = 'blue', alpha = 0.5) +
          scale_size_continuous(range = c(2,8)) +
          labs(x = "Damage%", y = "Damage Taken%") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none")) 
        p3 <- ggplotly(p3,tooltip = c("label","x","y","size"))
        
        p4 <- ggplot(Player_datatop(),aes(x = WPM, y = WCPM,size = CSPM, label = Player)) + 
          geom_point(colour = 'yellow',alpha = 0.5) + 
          scale_size_continuous(range = c(2,8)) +
          labs(x = "WPM", y = "WCPM") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none")) 
        p4 <- ggplotly(p4,tooltip = c("label","x","y","size")) 
        subplot(p1,p2,p3,p4,nrows = 2, titleX = TRUE, titleY = TRUE,margin = 0.06)
      }
    })
    
    #Jug TAB
    Player_datajug <- reactive({
      player_2020 %>%
        filter(Competition == input$Competition) %>%
        filter(Position == 'Jug')
    })
    output$Jug <- renderPlotly({
      if (is.null(player_2020)) { return(NULL)
      } else {
        p1 <- ggplot(Player_datajug(),aes(x = DPM,y = GPM,size = DTPM, label = Player)) + 
          geom_point(colour = 'red', alpha = 0.5) +
          scale_size_continuous(range = c(4, 8)) + 
          xlab("DPM") + ylab("GPM") +
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none"))
        p1 <- ggplotly(p1, tooltip = c("label","x","y","size"))
        
        p2 <- ggplot(Player_datajug(),aes(x = Kills,y = Deaths,size = Assists, label =Player)) + 
          geom_point(colour = 'green',alpha = 0.5) +
          scale_size_continuous(range = c(4, 8)) +
          labs(x = "Kills", y = "Deaths") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) + 
          theme(legend.position = c("none")) 
        p2 <- ggplotly(p2,tooltip = c("label","x","y","size")) 
        
        p3 <- ggplot(Player_datajug(),aes(x = DMG_Pct,y = DTPM_Pct, size = KP, label = Player)) + 
          geom_point(colour = 'blue', alpha = 0.5) +
          scale_size_continuous(range = c(4,8)) +
          labs(x = "Damage%", y = "Damage Taken%") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none")) 
        p3 <- ggplotly(p3,tooltip = c("label","x","y","size"))
        
        p4 <- ggplot(Player_datajug(),aes(x = WPM, y = WCPM,size = CSPM, label = Player)) + 
          geom_point(colour = 'yellow',alpha = 0.5) + 
          labs(x = "WPM", y = "WCPM") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none"))
        p4 <- ggplotly(p4,tooltip = c("label","x","y","size")) 
        subplot(p1,p2,p3,p4,nrows = 2, titleX = TRUE, titleY = TRUE,margin = 0.06)
      }
    }) 
    
    #Mid TAB
    Player_datamid <- reactive({
      player_2020 %>%
        filter(Competition == input$Competition) %>%
        filter(Position == 'Mid')
    })
    output$Mid <- renderPlotly({
      if (is.null(player_2020)) { return(NULL)
      } else {
        p1 <- ggplot(Player_datamid(),aes(x = DPM,y = GPM,size = DTPM, label = Player)) + 
          geom_point(colour = 'red', alpha = 0.5) +
          scale_size_continuous(range = c(4, 8)) + 
          xlab("DPM") + ylab("GPM") +
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none"))
        p1 <- ggplotly(p1, tooltip = c("label","x","y","size"))
        
        p2 <- ggplot(Player_datamid(),aes(x = Kills,y = Deaths,size = Assists, label =Player)) + 
          geom_point(colour = 'green',alpha = 0.5) +
          scale_size_continuous(range = c(4, 8)) +
          labs(x = "Kills", y = "Deaths") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) + 
          theme(legend.position = c("none"))
        p2 <- ggplotly(p2,tooltip = c("label","x","y","size")) 
        
        p3 <- ggplot(Player_datamid(),aes(x = DMG_Pct,y = DTPM_Pct, size = KP, label = Player)) + 
          geom_point(colour = 'blue', alpha = 0.5) +
          scale_size_continuous(range = c(4,8)) +
          labs(x = "Damage%", y = "Damage Taken%") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none"))
        p3 <- ggplotly(p3,tooltip = c("label","x","y","size"))
        
        p4 <- ggplot(Player_datamid(),aes(x = WPM, y = WCPM,size = CSPM, label = Player)) + 
          geom_point(colour = 'yellow',alpha = 0.5) + 
          labs(x = "WPM", y = "WCPM") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none"))
        p4 <- ggplotly(p4,tooltip = c("label","x","y","size")) 
        subplot(p1,p2,p3,p4,nrows = 2, titleX = TRUE, titleY = TRUE,margin = 0.06)
      }
    }) 
    
    #Bot TAB
    Player_databot <- reactive({
      player_2020 %>%
        filter(Competition == input$Competition) %>%
        filter(Position == 'Bot')
    })
    output$Bot <- renderPlotly({
      if (is.null(player_2020)) { return(NULL)
      } else {
        p1 <- ggplot(Player_databot(),aes(x = DPM,y = GPM,size = DTPM, label = Player)) + 
          geom_point(colour = 'red', alpha = 0.5) +
          scale_size_continuous(range = c(4, 8)) + 
          xlab("DPM") + ylab("GPM") +
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none"))
        p1 <- ggplotly(p1, tooltip = c("label","x","y","size"))
        
        p2 <- ggplot(Player_databot(),aes(x = Kills,y = Deaths,size = Assists, label =Player)) + 
          geom_point(colour = 'green',alpha = 0.5) +
          scale_size_continuous(range = c(4, 8)) +
          labs(x = "Kills", y = "Deaths") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) + 
          theme(legend.position = c("none"))
        p2 <- ggplotly(p2,tooltip = c("label","x","y","size")) 
        
        p3 <- ggplot(Player_databot(),aes(x = DMG_Pct,y = DTPM_Pct, size = KP, label = Player)) + 
          geom_point(colour = 'blue', alpha = 0.5) +
          scale_size_continuous(range = c(4,8)) +
          labs(x = "Damage%", y = "Damage Taken%") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none"))
        p3 <- ggplotly(p3,tooltip = c("label","x","y","size"))
        
        p4 <- ggplot(Player_databot(),aes(x = WPM, y = WCPM,size = CSPM, label = Player)) + 
          geom_point(colour = 'yellow',alpha = 0.5) + 
          labs(x = "WPM", y = "WCPM") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none"))
        p4 <- ggplotly(p4,tooltip = c("label","x","y","size")) 
        subplot(p1,p2,p3,p4,nrows = 2, titleX = TRUE, titleY = TRUE,margin = 0.06)
      }
    })
    
    #Sup TAB
    Player_datasup <- reactive({
      player_2020 %>%
        filter(Competition == input$Competition) %>%
        filter(Position == 'Sup')
    })
    output$Sup <- renderPlotly({
      if (is.null(player_2020)) { return(NULL)
      } else {
        p1 <- ggplot(Player_datasup(),aes(x = DPM,y = GPM,size = DTPM, label = Player)) + 
          geom_point(colour = 'red', alpha = 0.5) +
          scale_size_continuous(range = c(4, 8)) + 
          xlab("DPM") + ylab("GPM") +
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none")) 
        p1 <- ggplotly(p1, tooltip = c("label","x","y","size"))
        
        p2 <- ggplot(Player_datasup(),aes(x = Kills,y = Deaths,size = Assists, label =Player)) + 
          geom_point(colour = 'green',alpha = 0.5) +
          scale_size_continuous(range = c(4, 8)) +
          labs(x = "Kills", y = "Deaths") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) + 
          theme(legend.position = c("none"))
        p2 <- ggplotly(p2,tooltip = c("label","x","y","size")) 
        
        p3 <- ggplot(Player_datasup(),aes(x = DMG_Pct,y = DTPM_Pct, size = KP, label = Player)) + 
          geom_point(colour = 'blue', alpha = 0.5) +
          scale_size_continuous(range = c(4,8)) +
          labs(x = "Damage%", y = "Damage Taken%") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none"))
        p3 <- ggplotly(p3,tooltip = c("label","x","y","size"))
        
        p4 <- ggplot(Player_datasup(),aes(x = WPM, y = WCPM,size = CSPM, label = Player)) + 
          geom_point(colour = 'yellow',alpha = 0.5) + 
          labs(x = "WPM", y = "WCPM") + 
          theme(text=element_text(family="Arial", face="bold", size=12)) +
          theme(legend.position = c("none")) 
        p4 <- ggplotly(p4,tooltip = c("label","x","y","size")) 
        subplot(p1,p2,p3,p4,nrows = 2, titleX = TRUE, titleY = TRUE,margin = 0.06)
      }
    })
    
    #second layer
    select_team <- reactive({
      player_2020 %>%
        filter(Competition == input$Competition) %>%
        select(Team)
    })
    output$select_team <- renderUI({
      selectInput("select_team","Select a Team:", choices = as.character(levels(factor(select_team()$Team))))
    })
    
    #third layer 
    select_match <- reactive({
      data %>%
        filter(Competition == input$Competition) %>%
        filter(Team == input$select_team) %>%
        select(Match)
    })
    output$select_match <- renderUI({
      selectInput("select_match", "Select a Match:", choices = as.character(levels(factor(select_match()$Match))))
    })
    
    #forth layer
    select_game <- reactive({
      data %>%
        filter(Competition == input$Competition) %>%
        filter(Match == input$select_match) %>%
        filter(Team == input$select_team) %>%
        select(match_3)
    })
    output$select_game <- renderUI({
      selectInput("select_game","Select a Game:" , choices = as.character(levels(factor(select_game()$match_3))))
    })
    
    
    #Teams Information for the whole Competition
    radar_KDA_team <- reactive({
      team_data %>%
        filter(Competition == input$Competition) %>%
        filter(Team == input$select_team) %>%
        select('Team','Kills','Deaths','Assists')
    })
    output$radar_KDA_team <- renderPlotly({
      plot_ly(type = 'scatterpolar',mode ='lines+markers', r =(melt(radar_KDA_team()))$value, theta =(melt(radar_KDA_team()))$variable, fill = 'toself')
    })
    radar_team <- reactive({
      team_data %>%
        filter(Competition == input$Competition) %>%
        filter(Team == input$select_team) %>%
        select('Team','Dragons','Barons','Towers','Towers_Opp')
    })
    output$radar_team <- renderPlotly({
      plot_ly(type = 'scatterpolar',mode ='lines+markers', r =(melt(radar_team()))$value, theta =(melt(radar_team()))$variable, fill = 'toself')
    })
    
    Player <- reactive({
      player_2020 %>%
        filter(Competition == input$Competition) %>%
        filter(Team == input$select_team) %>%
        select('Position','Player','Kills',"Deaths","Assists") 
    })
    output$Player <- renderPlotly({
      if (is.null(player_2020)) { return(NULL)
      } else {
      p1 <- ggplot(melt(Player() %>% filter(Position == 'Top')),mapping = aes(x=Player, y =value, fill = variable, label = Position)) +
        geom_bar(stat = 'identity', position="stack", show.legend = FALSE) +
        labs(x = 'Top', y = "KDA") 
      
      p2 <- ggplot(melt(Player() %>% filter(Position == 'Jug')),mapping = aes(x=Player, y =value, fill = variable, label = Position)) +
        geom_bar(stat = 'identity', position="stack",show.legend = FALSE) +
        labs(x = 'Jug', y = "")  
      
      p3 <- ggplot(melt(Player() %>% filter(Position == 'Mid')),mapping = aes(x=Player, y =value, fill = variable, label = Position)) +
        geom_bar(stat = 'identity', position="stack",show.legend = FALSE) +
        labs(x = 'Mid', y = "") 
      
      p4 <- ggplot(melt(Player() %>% filter(Position == 'Bot')),mapping = aes(x=Player, y =value, fill = variable, label = Position)) +
        geom_bar(stat = 'identity', position="stack",show.legend = FALSE) +
        labs(x = 'Bot', y = "")
        
      
      p5 <- ggplot(melt(Player() %>% filter(Position == 'Sup')),mapping = aes(x=Player, y =value, fill = variable, label = Position)) +
        geom_bar(stat = 'identity', position="stack", show.legend = FALSE) +
        labs(x = 'Sup', y = "") 
      
      subplot(p1,p2,p3,p4,p5,titleX = TRUE, titleY = TRUE)
      }  
    })
    
    # Player Data Leader board
    output$rank_text <- renderText({
     paste(" This part shows the main data ranking of teams and players in the season of ", input$Competition, ".")
    })
      
    output$Winner_of_season1 <- renderText({
      paste(input$Competition,'Champion is')
    })
    Winner_of_season2 <- reactive({
      team_data%>%
        filter(Competition == input$Competition) %>%
        select(Champion)
    })
    output$Winner_of_season2 <- renderText({
      paste(as.character(levels(factor(Winner_of_season2()$Champion))))
    })
    
    
    
    #Teams Leader board
    first_win_rate <- reactive({
      team_data %>%
        filter(Competition == input$Competition) %>%
        filter(Win_Rate == max(Win_Rate)) %>%
        select('Team','Victory','Defeat') 
    })
    output$first_win_rate_text <- renderText({
      paste('The highest winning ratio team is:',first_win_rate()$Team)
    })
    output$first_win_rate <- renderPlotly({
      melt(first_win_rate()) %>% 
        group_by(variable) %>%
        plot_ly(labels = ~variable, values = ~value)%>%
        add_pie(hole = 0.5) %>%
        layout(title = "",  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
    })
    
    first_kills <- reactive({
      team_data %>%
        filter(Competition == input$Competition) %>%
        filter(kills == max(kills)) %>%
        select('Team','kills','deaths') 
    })
    output$first_kills_text <- renderText({
      paste('The most kills team is:',first_kills()$Team)
    })
    output$first_kills <- renderPlotly({
      melt(first_kills()) %>% 
        group_by(variable) %>%
        plot_ly(labels = ~variable, values = ~value,textinfo = "text", text = ~value)%>%
        add_pie(hole = 0.5) %>%
        layout(title = "",  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
    #Teams Win Rate
    Teams_win_rate <- reactive({
      team_data %>%
        filter(Competition == input$Competition) %>%
        select('Team','Victory','Defeat','Win_Rate') 
    })
    output$Teams_win_rate <- renderPlotly ({
        ggplot(melt(Teams_win_rate()[,c('Team','Victory','Defeat')]),mapping = aes(x= Team,y = value, group = variable)) +
        geom_bar(melt(Teams_win_rate()[,c('Team','Victory','Defeat')]),mapping = aes(fill = variable), stat = "identity", position = 'dodge', width =0.3) +
        labs(x = 'Teams', y = "Game Numbers") +
        guides(fill=FALSE)
       })
    
    #Player Kills Leaderbroad
    player_kills <- reactive({
      player_2020 %>%
        filter(Competition == input$Competition) %>%
        filter(kills == max(kills)) %>%
        select('Team','Player','kills','deaths') 
    })
    output$player_kills_text <- renderText({
      paste('The most kills player is:',player_kills()$Team,'.',player_kills()$Player)
    })
    output$player_kills <- renderPlotly({
      melt(player_kills()) %>% 
        group_by(variable) %>%
        plot_ly(labels = ~variable, values = ~value,textinfo = "text", text = ~value)%>%
        add_pie(hole = 0.5) %>%
        layout(title = "",  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    #Player Deaths Leaderbroad
    player_deaths <- reactive({
      player_2020 %>%
        filter(Competition == input$Competition) %>%
        filter(deaths == max(deaths)) %>%
        select('Team','Player','kills','deaths') 
    })
    output$player_deaths_text <- renderText({
      paste('The most deaths player is:', player_deaths()$Team,".",player_deaths()$Player)
    })
    output$player_deaths <- renderPlotly({
      melt(player_deaths()) %>% 
        group_by(variable) %>%
        plot_ly(labels = ~variable, values = ~value,textinfo = "text", text = ~value)%>%
        add_pie(hole = 0.5) %>%
        layout(title = "",  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    #Player DPM LeaderBoard
    Player_DPM_Leaderboard <- reactive({
      player_2020 %>%
        filter(Competition == input$Competition) %>%
        select('Team','Player','Position','DPM') %>%
        slice_max(DPM, n = 5)
    })
    
    output$Player_DPM_Leaderboard <- renderFormattable({
      formattable(Player_DPM_Leaderboard(),align = c('l','l','l'))
    })
    
    #Player GPM LeaderBoard
    Player_GPM_Leaderboard <- reactive({
      player_2020 %>%
        filter(Competition == input$Competition) %>%
        select('Team','Player','Position','GPM') %>%
        slice_max(GPM, n = 5)
    })
    
    output$Player_GPM_Leaderboard <- renderFormattable({
      formattable(Player_GPM_Leaderboard(),align = c('l','l','l'))
    })
    
    #Game result
    Game_result <- reactive({
      Result %>%
        filter(Match == input$select_match)
    })
    output$Game_result <- renderFormattable({
       formattable(Game_result(), align = c('l','l','c'), list(digits = "0"))
    })
    
    each_game_result <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == 'team') %>%
        filter(result == '1')
    })
    output$text1 <- renderText({
      paste(input$select_game, "Winner")
    })
    output$text2 <- renderText({
      paste(each_game_result()$team)
    })
    
    Game_Info <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == 'team') %>%
        filter(side == 'Blue') %>%
        select(c('patch',"Duration"))
    })
    output$Game_Info <- renderFormattable({
      if (is.null(Game_Info())) { return(NULL)
      } else {
      formattable(Game_Info(), align = c("l","l"))
    }
    })
    
    Ban_Info <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == 'team') %>%
        select(c('team','side','ban1','ban2','ban3','ban4','ban5'))
    }) 
    output$Ban_Info <- renderFormattable({
      if (is.null(Ban_Info())) { return(NULL)
      } else {
        formattable(Ban_Info(), align = c("l","l",'c','c','c','c'))
      }
    })
    
    
    #Players Information(in data frame)
    Competitors1 <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(side == "Blue") %>%
        filter(position == c("Top","Jug","Mid","Bot","Sup"))%>%
        select(c("team","position","player","champion"))       
    })
    output$Competitors1 <- renderFormattable({
      formattable(Competitors1(), align = c('l','l',"c","c"))
    })
    Competitors2 <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(side == "Red") %>%
        filter(position == c("Top","Jug","Mid","Bot","Sup"))%>%
        select(c("team","position","player","champion"))       
     })
    output$Competitors2 <- renderFormattable({
      formattable(Competitors2(), align = c('l','l',"c","c"))
    })
    
    #Team Information(in data frame)
    Players_team1 <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == c("Top","Jug","Mid","Bot","Sup")) %>%
        filter(side == 'Blue')%>%
        select(c('team','position','player','champion',"KDA",'KP','dpm',"earned_gpm",'wpm','wcpm','vspm',"damagetochampions","totalgold"))
    })
    output$Players_team1 <- renderFormattable({
      formattable(Players_team1(),align = c('c','c',"c",'c','l','l','l','l','l','l','l','c','c'))
    })
    Players_team2 <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == c("Top","Jug","Mid","Bot","Sup")) %>%
        filter(side == 'Red')%>%
        select(c('team','position','player','champion',"KDA",'KP','dpm',"earned_gpm",'wpm','wcpm','vspm',"damagetochampions","totalgold"))
    })
    output$Players_team2 <- renderFormattable({
      formattable(Players_team2(),align = c('c','c',"c",'c','l','l','l','l','l','l','l','c','c'))
    })
    
    #dpm,gpm,dtpm sun-donut plot
    player_dpm1 <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == c("Top","Jug","Mid","Bot","Sup")) %>%
        filter(side == 'Blue')%>%
        select('Position_player','dpm')
    })
    output$player_dpm1 <- renderSund2b({
      sund2b(player_dpm1(), colors = "red")
    })
    player_dpm2 <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == c("Top","Jug","Mid","Bot","Sup")) %>%
        filter(side == 'Red')%>%
        select('Position_player','dpm')
    })
    output$player_dpm2 <- renderSund2b({
      sund2b(player_dpm2(), colors = "red")
    })
    player_gpm1 <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == c("Top","Jug","Mid","Bot","Sup")) %>%
        filter(side == 'Blue')%>%
        select('Position_player','earned_gpm')
    })
    output$player_gpm1 <- renderSund2b({
      sund2b(player_gpm1(), colors = 'red')
    })
    player_gpm2 <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == c("Top","Jug","Mid","Bot","Sup")) %>%
        filter(side == 'Red')%>%
        select('Position_player','earned_gpm')
    })
    output$player_gpm2 <- renderSund2b({
      sund2b(player_gpm2(), colors = "red")
    })
    player_dtpm1 <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == c("Top","Jug","Mid","Bot","Sup")) %>%
        filter(side == 'Blue')%>%
        select('Position_player','damagetakenperminute')
    })
    output$player_dtpm1 <- renderSund2b({
      sund2b(player_dtpm1(), colors = 'blue')
    })
    player_dtpm2 <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == c("Top","Jug","Mid","Bot","Sup")) %>%
        filter(side == 'Red')%>%
        select('Position_player','damagetakenperminute')
    })
    output$player_dtpm2 <- renderSund2b({
      sund2b(player_dtpm2(), colors = "blue")
    })
    
    #Damage bar chart
    Damage <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == c("Top","Jug","Mid","Bot","Sup")) %>%
        select('position','player','Position_player','damagetochampions','team')
    })
    output$Damage <- renderPlotly({
      ggplot(Damage(),mapping = aes(x=position,y = damagetochampions,group = team)) +
        geom_col(aes(fill=team), position="dodge") + 
        scale_fill_manual(values =c('blue','red')) +
        coord_flip() +
        labs(x = '', y = "Damage to Champions") +
        geom_text(aes(label=damagetochampions,y =damagetochampions+1000),position=position_dodge(0.9), stat="identity")+
        geom_text(aes(label=player, y = damagetochampions-1500),position=position_dodge(0.9), stat="identity")
        
    })
    
    #Gold difference line chart
    Gold_Diff <- reactive({
     data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == 'team') %>%
        select('team','goldat10','goldat15','totalgold')
      })
    output$Gold <- renderPlotly({
      ggplot(melt(Gold_Diff())) + 
         geom_point(aes(x=variable, y =value,fill = team, size = 5),stat = 'identity') +
         scale_fill_manual(values =c('blue','red'))+
         labs(x = 'time', y = 'Total Gold')
             
    })
    
    #CS difference
    CS <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == c("Top","Jug","Mid","Bot","Sup")) %>%
        select('position','player','Position_player','csat15','team')
    })
    output$CS <- renderPlotly({
      ggplot(CS(),mapping = aes(x=position,y = csat15,group = team)) +
        geom_col(aes(fill=team), position="dodge") + 
        scale_fill_manual(values =c('blue','red')) +
        coord_flip() +
        labs(x = '', y = "CS at 15") +
        geom_text(aes(label=csat15,y =csat15+10),position=position_dodge(0.9), stat="identity") +
        geom_text(aes(label=player,y =csat15-10),position=position_dodge(0.9), stat="identity")
      
    })
    #XP diff
    XP <- reactive({
      data %>%
        filter(Match == input$select_match) %>%
        filter(match_3 == input$select_game) %>%
        filter(position == c("Top","Jug","Mid","Bot","Sup")) %>%
        select('position','player','Position_player','xpat15','team')
    })
    output$XP <- renderPlotly({
      ggplot(XP(),mapping = aes(x=position,y = xpat15,group = team)) +
        geom_col(aes(fill=team), position="dodge") + 
        scale_fill_manual(values =c('blue','red')) +
        coord_flip() +
        labs(x = '', y = "XP at 15") +
        geom_text(aes(label=xpat15,y =xpat15+400),position=position_dodge(0.9), stat="identity") + 
        geom_text(aes(label=player,y =xpat15-1500),position=position_dodge(0.9), stat="identity")
        
    })
    
    #Word
    

      }

    




