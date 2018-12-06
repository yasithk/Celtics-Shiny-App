hayward <- 202330
irving <- 202681
tatum <- 1628369
horford <- 201143
brown <- 1627759
smart <- 203935
baynes <- 203382
rozier <- 1626179
morris <- 202694
theis <- 1628464
yabusele <- 1627824

player_id <- as.vector(c(hayward, irving, tatum, horford, brown, smart, baynes, rozier, morris, theis, yabusele))
player_photo_url = function(player_id) {
  paste0("http://stats.nba.com/media/players/230x185/", player_id, ".png")
}

###plus minus plot 
cdata <- read.csv('C:/Users/kariyya/Documents/Training&Admin/Udemy/shiny_nba/cdata.csv', 
                  header = TRUE, stringsAsFactors =  FALSE)
names(cdata)[31] <- 'plusminus'
cdata$Date <- as.Date(cdata$Date, format = "%d/%m/%Y")
cdata <- cdata %>% mutate( Year = format(Date, "%Y"))
cdata$Year <- as.factor(cdata$Year)
cdata$plusminus <- as.numeric(cdata$plusminus)
cdata <- cdata %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

#2018
first <- cdata %>%  filter(Name == 'Jaylen Brown', Year == 2017) %>% select(G, plusminus, Year)
first$G <- as.factor(first$G)
firsplot <- ggplot(data = first, aes(x = G, y = plusminus))  + geom_bar(stat = "identity") + ylim(-15,25) + geom_hline(yintercept = 0) #+
  #geom_ma(ma_fun = SMA, n = 3, na.rm = TRUE)
  #geom_smooth(method=lm, see = FALSE)
  # geom_line(aes(y=rollmean(plusminus, 3, na.pad=TRUE))) + theme_gray()
ggplotly(firsplot)
#2019
second <- cdata %>%  filter(Name == 'Jaylen Brown', Year == 2018) %>% select(G, plusminus, Year)
ggplot(data = second, aes(x = G, y = plusminus)) + geom_bar(stat = 'identity') + geom_hline(yintercept = 0)# +
  #geom_ma(ma_fun = EMA, n = 2, na.rm = TRUE) +
  # geom_line(aes(y=rollmean(plusminus, 3, na.pad=TRUE))) + theme_gray()


plotly(
  x =c(1,2,3),
  y =c(5,6,7),
  type = 'bar',
  mode = 'markers' 
  )
  