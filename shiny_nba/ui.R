#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

# load libraries and data

# load("nba_shots.RData")
nba_shots  <- read_rds('celtics_data.rds')

# data management
players = nba_shots %>% distinct(player_name) %>% pull()
made = nba_shots %>% distinct(shot_made_flag ) %>% pull()

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("NBA Shot Attempts"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
    #   textInput(
    #     inputId = 'C:/Users/kariyya/Documents/Training&Admin/Udemy/shiny_nba_complete/jt.png', 
    #     label = 'http://www.espn.com/nba/player/_/id/4065648/jayson-tatum', 
    #     value = "http://placehold.it/300x300")
    # ),
      
      # # drop down menu for player
      selectInput("player_choice", label = h2("Select player"),
                  choices = players, selected = "Jason Tatum") , # uncomment comma to add another widget
      # 
      # # drop down menu for season based on a certain player
      uiOutput("season_choice") ,
      
      # Select 2PT or 3PT field goal 
      selectInput("shot_choice"
                  , label = h4("Shot Type"),
                  choices = list("2PT Field Goal", "3PT Field Goal")
                   # , selected = "2PT Field Goal"
                  )

      # allows the user to filter out made or missed shots:
       # radioButtons("shots_made", label = h3("Shot status"), choices = list("all", "made", "missed"), selected = "all")
      
      # radioButtons("type", label = h4("Shot Type"), choices = list("2pt", "3pt"))
     ),
    
    # Show output based on user selections
    mainPanel(
      h4("Shot Chart of Player"),
      # # spatial plot of shots made
      fluidRow(column=2, plotOutput("court_shots", width = "800", height = "600"), align = "Center"
               ) ,
      # 
      # # box plot of shot distances
      # plotlyOutput("shot_distances")
     
      #headshot
      # htmlOutput('image'),
      #table of stats
      h5("Summary Statstics Comparision"),
      fluidRow(column = 2,tableOutput("table"), align = "Center"),
      
      #plot of hist
      # plotOutput('histplot'),
      
      h4("Comparision of Plus-Minus through first 19 Games"),
      splitLayout(cellwidth = c("50%", "50%"), plotOutput('plusminusPlot2017'),plotOutput('plusminusPlot2018'), width = "800" )
      #plot of plusminus
      # plotOutput('plusminusPlot2017'),
      
      #plot of plusminus2018
      # plotOutput('plusminusPlot2018')
    )
  )
)
)

