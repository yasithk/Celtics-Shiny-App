# load libraries and data
# load libraries
library(shiny)
library(tidyverse)
library(plotly)
library(shiny)
library(tidyverse)
library(dplyr)
library(reshape2)
library(tidyquant)
library(shinydashboard)
# load("nba_shots.RData")
nba_shots  <- read_rds('celtics_data.rds')

# data management
players = nba_shots %>% distinct(player_name) %>% pull()
made = nba_shots %>% distinct(shot_made_flag ) %>% pull()

dashboardPage(
  dashboardHeader(),
  dashboardSidebar( 
                    ),
  dashboardBody()
)

