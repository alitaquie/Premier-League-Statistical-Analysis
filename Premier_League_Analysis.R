library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)


#scrape data from premier league offical website
#read the HTML of the Premier League fbref.com page and identify the links to use'

page <- read_html("https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures")
links_1 <- unlist(page %>% html_nodes("a") %>% html_attr('href'))
head(links_1)

links_2 <- strsplit(links_1, '"')
links_2[100:120]''

#filter the links using grepl function, only links we want is with premier league matches

links_3 <- links_2[grepl("matches", links_2)]
links_4 <- links_3[grepl("Premier", links_3)]
all_urls <- unique(links_4)

#Since this is an analysis about Arsenal, filter through all the urls regarding only Arsenal

team_urls <- all_urls[grepl("Arsenal", all_urls)]

#initialize tables
all_stat <- NULL
full_stat <- NULL

##data scraping part two
#scrape data for each individual team
selected_urls <- paste("https://fbref.com", team_urls, sep="")


#clean up data for the first 9 days, -1- should equal -01-"
selected_urls <- str_replace(selected_urls, "-1-", "-01-")
selected_urls <- str_replace(selected_urls, "-2-", "-02-")
selected_urls <- str_replace(selected_urls, "-3-", "-03-")
selected_urls <- str_replace(selected_urls, "-4-", "-04-")
selected_urls <- str_replace(selected_urls, "-5-", "-05-")
selected_urls <- str_replace(selected_urls, "-6-", "-06-")
selected_urls <- str_replace(selected_urls, "-7-", "-07-")
selected_urls <- str_replace(selected_urls, "-8-", "-08-")
selected_urls <- str_replace(selected_urls, "-9-", "-09-")



#data cleaning and selection
#loop through each URL in the selected_urls list

for (g in 1:length(selected_urls)){
  # extract game info from the URL
  game_data <- substr(selected_urls[g], 39, nchar(selected_urls[g])-15)
  
  #deal with month names
  game_data <- str_replace(game_data, "January", "Jan")
  game_data <- str_replace(game_data, "February", "Feb")
  game_data <- str_replace(game_data, "March", "Mar")
  game_data <- str_replace(game_data, "April", "Apr")
  game_data <- str_replace(game_data, "June", "Jun")
  game_data <- str_replace(game_data, "July", "Jul")
  game_data <- str_replace(game_data, "August", "Aug")
  game_data <- str_replace(game_data, "September", "Sep")
  game_data <- str_replace(game_data, "October", "Oct")
  game_data <- str_replace(game_data, "November", "Nov")
  game_data <- str_replace(game_data, "December", "Dec")
  
  #deal with derbies
  game_data <- str_replace(game_data, "North-West-London-Derby-", "")
  game_data <- str_replace(game_data, "Merseyside-Derby-", "")
  game_data <- str_replace(game_data, "North-London-Derby-", "")
  game_data <- str_replace(game_data, "Manchester-Derby-", "")
  game_data <- str_replace(game_data, "North-London-Derby-", "")
  
  #extract date and team names from game data
  date <- substr(game_data, nchar(game_data)-10, nchar(game_data))
  #extracts a substring from game_data, starting from the first character and ending 12 characters before the end of the string
  
  #simplify team names 
  
  teams <- substr(game_data, 1, nchar(game_data)-12)
  teams <- str_replace(teams, "Manchester-United", "Manchester Utd")
  teams <- str_replace(teams, "Manchester-City", "Manchester City")
  teams <- str_replace(teams, "Leeds-United", "Leeds United")
  teams <- str_replace(teams, "Crystal-Palace", "Crystal Palace")
  teams <- str_replace(teams, "Leicester-City", "Leicester City")
  teams <- str_replace(teams, "Aston-Villa", "Aston Villa")
  teams <- str_replace(teams, "Norwich-City", "Norwich City")
  teams <- str_replace(teams, "Newcastle-United", "Newcastle Utd")
  teams <- str_replace(teams, "Wolverhampton-Wanderers", "Wolves")
  teams <- str_replace(teams, "West-Ham-United", "West Ham")
  teams <- str_replace(teams, "Brighton-and-Hove-Albion", "Brighton")
  teams <- str_replace(teams, "Tottenham-Hotspur", "Tottenham")
  teams <- str_replace(teams, "Sheffield-United", "Sheffield")
  teams <- str_replace(teams, "Luton-Town", "Luton")
  
  
  teamA <- sub("-.*", "", teams)
  teamB <- sub(".*-", "", teams)
  
  #read the first pair of tables
  url <- selected_urls[g]
  
  #retrieves HTML content of webpage and parses it into XML document
  statA <- curl::curl(url) %>% 
    xml2::read_html() %>%
    #extracts table elements 
    rvest::html_nodes('table') %>%
    #converts table into data frames
    rvest::html_table() %>%
    .[[4]]
  colnames(statA) <- paste0(colnames(statA), " >> ", statA[1, ])#appends ">>" and vals of first row of statA to column name
  names(statA)[1:5] <- paste0(statA[1,1:5])
  statA <- statA[-c(1),]
  statA <- cbind(date, Team=teamA, Opponent=teamB, statA)
  statB <- curl::curl(url)  %>% 
    xml2::read_html() %>%
    rvest::html_nodes('table') %>%
    rvest::html_table() %>%
    .[[11]]
  colnames(statB) <- paste0(colnames(statB), " >> ", statB[1, ])
  names(statB)[1:5] <- paste0(statB[1,1:5])
  statB <- statB[-c(1),]
  statB <- cbind(date, Team=teamB, Opponent=teamA, statB) #combines the extracted stats with data of game and name of teams playinng
  stat_both <- rbind(statA, statB) #combines the statistics from both teams into a single data frame
  #define the game's data frame
  all_stat <- stat_both
  Sys.sleep(15) #prevent overflooding in code
  
  #loop for all other 5 tables related to the game
  for(i in 5:10){
    statA <- curl::curl(url) %>% 
      xml2::read_html() %>%
      rvest::html_nodes('table') %>%
      rvest::html_table() %>%
      .[[i]]
    colnames(statA) <- paste0(colnames(statA), " >> ", statA[1, ])
    names(statA)[1:5] <- paste0(statA[1,1:5])
    statA <- statA[-c(1),]
    statA <- cbind(date, Team=teamA, Opponent=teamB, statA)
    statB <- curl::curl(url)  %>% 
      xml2::read_html() %>%
      rvest::html_nodes('table') %>%
      rvest::html_table() %>%
      .[[i+7]]
    colnames(statB) <- paste0(colnames(statB), " >> ", statB[1, ])
    names(statB)[1:5] <- paste0(statB[1,1:5])
    statB <- statB[-c(1),]
    statB <- cbind(date, Team=teamB, Opponent=teamA, statB)
    stat_both <- rbind(statA, statB)
    all_stat <- merge(all_stat, stat_both, all=T)
    
    #remove any duplicates
    all_stat <- unique(all_stat)
    
    #remove any leading or trailing whitespaces
    all_stat$Player <- str_trim(all_stat$Player, side = c("both", "left", "right"))
    
    #convert all stats into numeric variables
    all_stat <- cbind(all_stat[,1:8], mutate_all(all_stat[,9:ncol(all_stat)], function(x) as.numeric(as.character(x))))
    
    #rename columns such as " >> xG" to "xG"
    #if(i==10){all_stat <- all_stat %>% rename_at(vars(starts_with(" >> ")), funs(str_replace(., " >> ", "")))}
    
    write.csv(all_stat,paste0("premier_league_2021-22_",game_data,".csv"))
    
    Sys.sleep(15)
  }
  #add the game tables to the total data frame
  full_stat <- rbind(full_stat, all_stat)
}


