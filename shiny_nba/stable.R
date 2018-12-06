nba_shots <- load("C:/Users/kariyya/Documents/Training&Admin/Udemy/shiny_nba/nba_shots.RData")
#2017
#count  made and missed 
y <- filter(nba_shots, player_name == 'Kyrie Irving', shot_zone_basic != 'Backcourt'
            ,season == '2017-18'
            ) %>% count(shot_zone_basic) %>% group_by(shot_zone_basic) 

#count made
x <-filter(nba_shots, player_name == 'Kyrie Irving', shot_made_flag == 'made', shot_zone_basic != 'Backcourt',
           season == '2017-18') %>% count(shot_zone_basic) %>% group_by(shot_zone_basic) 

transform(y, FGper = round(x$n/y$n*100))

#2018
#count  made and missed 
y1 <- filter(nba_shots, player_name == 'Kyrie Irving', shot_zone_basic != 'Backcourt'
            ,season == '2018-19'
) %>% count(shot_zone_basic) %>% group_by(shot_zone_basic) 

#count made
x1 <-filter(nba_shots, player_name == 'Kyrie Irving', shot_made_flag == 'made', shot_zone_basic != 'Backcourt',
           season == '2018-19') %>% count(shot_zone_basic) %>% group_by(shot_zone_basic) 

transform(y, FGper = round(x1$n/y1$n*100))

###################################3
# library(ballr)

#read in summary data
cdata <- read.csv('C:/Users/kariyya/Documents/Training&Admin/Udemy/shiny_nba/cdata.csv', 
                  header = TRUE, stringsAsFactors =  FALSE)

#create Year variable
cdata$Date <- as.Date(cdata$Date, format = "%d/%m/%Y")
cdata <- cdata %>% mutate( Year = format(Date, "%Y"))
cdata$Year <- as.factor(cdata$Year)

#subset data
df <- cdata %>% select(Name, G, Year, Opp, Venue, MP, FG, FGA, FG., X3P., FT., TRB, AST, STL, BLK, TOV, PTS )

#convert each chr to num
df[,6:17] <- df %>% select(MP, FG, FGA, FG., X3P., FT., TRB, AST, STL, BLK, TOV, PTS) %>% 
  mutate_all(funs(as.numeric))
### NAs to 0s
df <- df %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

###PROBLEM WITH MINUTES PLAYED

##Summary table comparision 
hist1 <-df %>% select(Name, Year, PTS,  AST, TRB, FG., X3P., TOV ) %>% 
  group_by(.dots=c("Name","Year"))  %>% summarise_all(funs(mean))  #%>% order_by(.dots=c("Name","Year"))

##plot
library(reshape2)
hist2 <- melt(hist1)
names(hist2)[3] <- "percent" 

ggplot(hist2, aes(x = Name, y = value, fill = Year )) + geom_bar(stat = 'identity', width = .5, position = 'dodge')

