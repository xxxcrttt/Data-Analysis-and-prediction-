library(shiny)
library(ggplot2)
library(ggrepel)
library(plotly)
library(cowplot)
library(formattable)
library(dplyr)
library(sunburstR)
library(tidyverse)
library(reshape2)
library(ECharts2Shiny)
library(radarchart)
library(ggiraph)

data <- read.csv('2020_LoL_esports_match_data.csv')
team_data <- read.csv('wanplus_team.csv')
player_2020 <- read.csv('wanplus_players.csv')
Result <- read.csv('result.csv')


library(tidyr)
data <- unite(data, "Match",date,match_2,remove = FALSE,sep = " ")
data <- unite(data, "Position_player",position,player,remove = FALSE,sep = " ")
data <- unite(data, "KDA",kills,deaths,remove = FALSE,sep = "/")
data <- unite(data, "KDA",KDA,assists,remove = FALSE,sep = "/")
player_2020 <- unite(player_2020, "Position_Player",Position,Player,remove = FALSE,sep = " ")



