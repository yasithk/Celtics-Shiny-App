## app.R ##
library(shiny)
library(shinydashboard)

# load data and source functions to make plot of basketball court
nba_shots  <- read_rds('celtics_data.rds')
source("helpers.R")

# load("nba_shots.RData")
# load("nba_shots.RData")
nba_shots  <- read_rds('celtics_data.rds')

# data management
players = nba_shots %>% distinct(player_name) %>% pull()
made = nba_shots %>% distinct(shot_made_flag ) %>% pull()





#read in summary data
cdata <- read.csv('C:/Users/kariyya/Documents/Training&Admin/Udemy/shiny_nba/cdata.csv', 
                  header = TRUE, stringsAsFactors =  FALSE)
names(cdata)[31] <- 'plusminus'
cdata$plusminus <- as.numeric(cdata$plusminus)
cdata <- cdata %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
cdata$Date <- as.Date(cdata$Date, format = "%d/%m/%Y")
cdata <- cdata %>% mutate( Year = format(Date, "%Y"))
cdata$Year <- as.factor(cdata$Year)
cdata$plusminus <- as.numeric(cdata$plusminus)
cdata <- cdata %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
# define plot of court
gg_court = make_court()

################################################################################


ui <- dashboardPage(
  dashboardHeader(title = '2017 vs 2018 Player Comaprision'),
  dashboardSidebar( 
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
        ),
  dashboardBody(
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

server <- function(input, output) {
  output$season_choice <- renderUI({
  seasons = nba_shots %>% filter(player_name == input$player_choice) %>%
    distinct(season) %>% pull()
  selectizeInput("season_choice", label = h3("Select season"), choices = seasons,
                 selected = seasons[1], multiple = FALSE) }) 
  output$court_shots <- renderPlot({
    #   subset data by selected player and season(s)
    player_data = filter(nba_shots, player_name == input$player_choice, 
                         shot_type == input$shot_choice,
                         #shot_made_flag == input$shots_made,
                         season %in% input$season_choice)
    # 
    #  create plot
    gg_court + geom_point(data = player_data, alpha = 0.75, size = 2.5,
                          aes(loc_x, loc_y, color = shot_made_flag, shape = season)) +
      scale_color_manual("", values = c(made = "blue", missed = "red"))
  })
  
  #create table 
  output$table <- renderTable({
    # nba_shots %>% select(player_name, shot_type, shot_type, shot_zone_basic, shot_made_numeric, shot_value) %>%
    # filter(player_name == input$player_choice)
    #create Year variable
    cdata$Date <- as.Date(cdata$Date, format = "%d/%m/%Y")
    cdata <- cdata %>% mutate( Year = format(Date, "%Y"))
    cdata$Year <- as.factor(cdata$Year)
    
    
    #subset data
    df <- cdata %>% select(Name, G, Year, Opp, Venue, MP, FG, FGA, FG., X3P., X3P,FT., TRB, AST, STL, BLK, TOV, PTS, plusminus )
    
    #convert each chr to num
    df[,6:18] <- df %>% select(MP, FG, FGA, FG., X3P., X3P, FT., TRB, AST, STL, BLK, TOV, PTS, plusminus) %>% 
      mutate_all(funs(as.numeric))
    ### NAs to 0s
    df <- df %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
    
    ###PROBLEM WITH MINUTES PLAYED
    ##Summary table comparision 
    df  %>% select(Name, Year, PTS,  AST, TRB, FG., X3P., TOV, plusminus ) %>% 
      group_by(.dots=c("Name","Year"))  %>% summarise_all(funs(mean)) %>% filter(Name == input$player_choice)
    #%>% order_by(.dots=c("Name","Year"))
  })
  
  #create scatter plot
  output$plusminusPlot2017 <- renderPlot({
    #2018
    first <- cdata %>%  filter(Name == 'Jaylen Brown', Year == 2017) %>% select(G, plusminus, Year)
    first$G <- as.factor(first$G)
    firsplot <- ggplot(data = first, aes(x = G, y = plusminus))  + geom_bar(stat = "identity") + ylim(-15,25) + geom_hline(yintercept = 0) #+
    ggplotly(firsplot)
  })
  
  output$plusminusPlot2018 <- renderPlot({
    #2019
    second <- cdata %>%  filter(Name == 'Jaylen Brown', Year == 2018) %>% select(G, plusminus, Year)
    secondplot <- ggplot(data = second, aes(x = G, y = plusminus)) + geom_bar(stat = 'identity') + geom_hline(yintercept = 0)# +
           #    geom_line(aes(y=rollmean(plusminus, 3, na.pad=TRUE))) + theme_gray()
    ggplotly(secondplot)
  })
  }

shinyApp(ui, server)
