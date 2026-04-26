###
###  EXTRACT AND CHARTING OF PHILLIES BY-GAME BY-PLAYER STATS
###  HITTING AND PITCHING
###  CODE HERE IS DESIGNED FOR POSTING CHARTS TO BLUESKY.APP
###

##  START TIME AND LOAD LIBRARIES

Sys.setenv(TZ='America/New_York')
starttime <- proc.time()

library(baseballr)
library(jsonlite)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(magick)
library(scales)
library(bskyr)
library(toOrdinal)
library(formattable)
library(sparkline)
library(webshot2)
library(htmlwidgets)
suppressPackageStartupMessages(library(circlize))
library(zoo)

##  USER CONFIGURABLE INPUT PARAMETERS

# Edit these three lines for the respective directory locations
# It is suggested to make images and backups as subdirectories of the main_dir
# i.e., img_dir = /main_dir/images/, bkup_dir = /main_dir/backups/
# Note - you must include the trailing slash and you must leave the quotation marks

main_dir <- "/home/rstudio1/baseball_scripts/"
img_dir <- "/home/rstudio1/baseball_scripts/images/"
bkup_dir <- "/home/rstudio1/baseball_scripts/backups/"

# Replace info here with appropriate login info for your bsky account
# You must leave the single quotation marks

bsky_name <- 'ADD YOUR BSKY USERNAME HERE WITHOUT DOMAIN' (e.g., 'iamvishnurajan')
bsky_fullname <- 'ADD YOUR FULL BSKY USERNAME HERE' (e.g., 'iamvishnurajan.bsky.social')
bsky_pass <- 'ADD AN APP SPECIFIC PASSWORD HERE'
set_bluesky_user(bsky_fullname)
set_bluesky_pass(bsky_pass)
my_auth <- bs_auth(bsky_fullname,bsky_pass)

# Lookup and hardcoding of Team ID and Season

url_teams <- "https://statsapi.mlb.com/api/v1/teams"
resp_teams <- url_teams %>% baseballr:::mlb_api_call()
teams <- jsonlite::fromJSON(jsonlite::toJSON(resp_teams[['teams']]), flatten = TRUE)
teams <- teams[teams$sport.id==1,]

id <- 143
year <- 2025

# Locations of key files that are needed

game_schedule_file <- paste0(main_dir,"game_schedule_player.csv")
game_schedule_file_bkup <- paste0(paste0(bkup_dir,"game_schedule_player_"),format(Sys.time(), "%Y%m%d_%H%M%S"),".csv")
game_schedule_df0 <- read.csv(game_schedule_file, header = TRUE, sep = ",", check.names=FALSE)

game_hitting_file <- paste0(main_dir,"game_hitting.csv")
game_hitting_file_bkup <- paste0(paste0(bkup_dir,"game_hitting_"),format(Sys.time(), "%Y%m%d_%H%M%S"),".csv")
game_hitting_df0 <- read.csv(game_hitting_file, header = TRUE, sep = ",", check.names=FALSE)

game_pitching_file <- paste0(main_dir,"game_pitching.csv")
game_pitching_file_bkup <- paste0(paste0(bkup_dir,"game_pitching_"),format(Sys.time(), "%Y%m%d_%H%M%S"),".csv")
game_pitching_df0 <- read.csv(game_pitching_file, header = TRUE, sep = ",", check.names=FALSE)

# Logging

sink_msgs <- file(paste0(main_dir,"std_msgs_mlb_player_stats.txt"), open="at")
sink(sink_msgs,type=c("message"),append = TRUE)
sink(sink_msgs,type=c("output"),append = TRUE)

###                                  ###
###  DO NOT CHANGE ITEMS BELOW THIS  ###
###                                  ###

##  CREATE API FUNCTION CALLS

mlb_game_logs_std <- function(id,stat_group,year) {
  url <- paste0("http://statsapi.mlb.com/api/v1/teams/",id,"/stats?stats=gameLog&group=",stat_group,"&season=",year,"&language=en")
  resp <- url %>% baseballr:::mlb_api_call()
  df <- jsonlite::fromJSON(jsonlite::toJSON(resp[['stats']]), flatten = TRUE)[[2]][[1]]
  df
}

mlb_game_logs_sitcode <- function(stat_group,sit_code,year) {
  url <- paste0("http://statsapi.mlb.com/api/v1/teams/stats?stats=gameLog,statSplits&group=",stat_group,"&limit=500&sitCodes=",sit_code,"&season=",year,"&language=en")
  resp <- url %>% baseballr:::mlb_api_call()
  df <- jsonlite::fromJSON(jsonlite::toJSON(resp[['stats']]), flatten = TRUE)[[3]][[2]]
  df
}

mlb_team_season <- function(stat_group,year) {
  url <- paste0("http://statsapi.mlb.com/api/v1/teams/stats?stats=season&group=",stat_group,"&limit=500&season=",year,"&language=en")
  resp <- url %>% baseballr:::mlb_api_call()
  df <- jsonlite::fromJSON(jsonlite::toJSON(resp[['stats']]), flatten = TRUE)[[3]][[1]]
  df
}

mlb_team_roster <- function(id) {
  url <- paste0("http://statsapi.mlb.com/api/v1/teams/",id,"/roster")
  resp <- url %>% baseballr:::mlb_api_call()
  df <- jsonlite::fromJSON(jsonlite::toJSON(resp[['roster']]), flatten = TRUE)
  df
}

##  GAME META INFO AND DIFF TO IDENTIFY NEW GAMES PLAYED
##  NOTE: WE BEGIN BY PULLING ACTUAL TO STATS TO KNOW WHICH GAMES HAVE BEEN PLAYED

stat_group <- "hitting"
hitting_std <- mlb_game_logs_std(id,stat_group,year)

game_meta <- merge(hitting_std,teams,by.x=c("opponent.id","season"),by.y=c("id","season"))
game_meta$inningLabel <- ifelse(game_meta$isHome=="TRUE","bottom","top")
game_meta <- game_meta[,c("season","date","game.gamePk","isHome","inningLabel","team.name","team.id","name","opponent.id")]
colnames(game_meta)[colnames(game_meta) == "name"] <- "opponent.name"
game_meta <- game_meta[order(game_meta$date,game_meta$game.gamePk),]
new_game_diff <- anti_join(game_meta,game_schedule_df0,by="game.gamePk")

##  IF NEW GAMES ARE AVAILABLE, WE PROCEED WITH THE FOLLOWING TO PULL STATS AND CREATE CHARTS

if (nrow(new_game_diff)> 0) {
  
  ##  ROSTER DOWNLOAD
  
  roster_df <- mlb_team_roster(id)
  roster_hitter <- roster_df[roster_df$position.abbreviation!="P",][c("person.id","person.fullName","position.abbreviation")]
  roster_pitcher <- roster_df[roster_df$position.abbreviation=="P",][c("person.id","person.fullName","position.abbreviation")]
  
  ##  GAME LOGS BY PLAYER FOR HITTING
  
  player_hitting_tbllist <- list()
  player_hitting_counter <- 0
  
  for (gidx in 1:nrow(new_game_diff)){
    
    url_boxscore <- paste0("https://statsapi.mlb.com/api/v1/game/",new_game_diff[gidx,"game.gamePk"],"/boxscore")
    resp_boxscore <- url_boxscore %>% baseballr:::mlb_api_call()
    game_boxscore <- jsonlite::fromJSON(jsonlite::toJSON(resp_boxscore[['teams']]), flatten = TRUE)
    
    game_schedule_simple <- new_game_diff[gidx,c("season","date","game.gamePk","team.name","team.id","opponent.name","opponent.id","isHome","inningLabel")]
    
    game_boxscore_batters <- if(game_schedule_simple$isHome=="FALSE"){
      as.data.frame(game_boxscore$away$batters)
    } else {
      as.data.frame(game_boxscore$home$batters)
    }
    
    for (pidx in 1:nrow(game_boxscore_batters)){
      
      player_hitting_counter <- player_hitting_counter+1
      
      url_player <- paste0("http://statsapi.mlb.com/api/v1/people/",game_boxscore_batters[pidx,],"/stats/game/",new_game_diff[gidx,"game.gamePk"])
      resp_player <- url_player %>% baseballr:::mlb_api_call()
      game_player <- jsonlite::fromJSON(jsonlite::toJSON(resp_player[['stats']]), flatten = TRUE)
      
      game_player_hitting <- game_player[[1,"splits"]]
      game_player_hitting <- game_player_hitting[game_player_hitting$group=="hitting",]
      
      url_player_detail <- paste0("http://statsapi.mlb.com/api/v1/people/",game_boxscore_batters[pidx,])
      resp_player_detail <- url_player_detail %>% baseballr:::mlb_api_call()
      game_player_detail <- jsonlite::fromJSON(jsonlite::toJSON(resp_player_detail[['people']]), flatten = TRUE)
      game_schedule_simple$person.id <- game_boxscore_batters[pidx,]
      game_schedule_simple$person.fullName <- game_player_detail[,"fullName"]
      
      game_player_hitting <- bind_cols(game_schedule_simple,game_player_hitting)
      
      if(!is.na(game_player_hitting$stat.summary)){
        player_hitting_tbllist[[player_hitting_counter]] <- game_player_hitting
      }
      Sys.sleep(1)
    }
  }
  
  player_hitting_new <- rbindlist(player_hitting_tbllist[1:length(player_hitting_tbllist)],fill=TRUE)
  player_hitting_new$season <- as.integer(player_hitting_new$season)
  player_hitting_new <- player_hitting_new[,c("season","date","game.gamePk","team.name","team.id","opponent.name","opponent.id","isHome","inningLabel","person.id","person.fullName","type","group","stat.plateAppearances","stat.atBats","stat.hits","stat.baseOnBalls","stat.hitByPitch","stat.totalBases","stat.sacFlies","stat.caughtStealing","stat.stolenBases","stat.runs","stat.doubles","stat.triples","stat.homeRuns","stat.rbi","stat.strikeOuts","stat.intentionalWalks","stat.groundIntoDoublePlay")]
  player_hitting_new$stat.ops <- (player_hitting_new$stat.atBats*(player_hitting_new$stat.hits+player_hitting_new$stat.baseOnBalls+player_hitting_new$stat.hitByPitch)+player_hitting_new$stat.totalBases*(player_hitting_new$stat.atBats+player_hitting_new$stat.baseOnBalls+player_hitting_new$stat.sacFlies+player_hitting_new$stat.hitByPitch))/
    (player_hitting_new$stat.atBats*(player_hitting_new$stat.atBats+player_hitting_new$stat.baseOnBalls+player_hitting_new$stat.sacFlies+player_hitting_new$stat.hitByPitch))
  
  Sys.sleep(3)
  
  player_hitting_full <- bind_rows(game_hitting_df0,player_hitting_new)
  player_hitting_full$date <- as.Date(player_hitting_full$date)
  
  ##  GAME LOGS BY PLAYER FOR PITCHING
  
  player_pitching_tbllist <- list()
  player_pitching_counter <- 0
  
  for (gidx in 1:nrow(new_game_diff)){
    
    url_boxscore <- paste0("https://statsapi.mlb.com/api/v1/game/",new_game_diff[gidx,"game.gamePk"],"/boxscore")
    resp_boxscore <- url_boxscore %>% baseballr:::mlb_api_call()
    game_boxscore <- jsonlite::fromJSON(jsonlite::toJSON(resp_boxscore[['teams']]), flatten = TRUE)
    
    game_schedule_simple <- new_game_diff[gidx,c("season","date","game.gamePk","team.name","team.id","opponent.name","opponent.id","isHome","inningLabel")]
    
    game_boxscore_pitchers <- if(game_schedule_simple$isHome=="FALSE"){
      as.data.frame(game_boxscore$away$pitchers)
    } else {
      as.data.frame(game_boxscore$home$pitchers)
    }
    
    for (pidx in 1:nrow(game_boxscore_pitchers)){
      
      player_pitching_counter <- player_pitching_counter+1
      
      url_player <- paste0("http://statsapi.mlb.com/api/v1/people/",game_boxscore_pitchers[pidx,],"/stats/game/",new_game_diff[gidx,"game.gamePk"])
      resp_player <- url_player %>% baseballr:::mlb_api_call()
      game_player <- jsonlite::fromJSON(jsonlite::toJSON(resp_player[['stats']]), flatten = TRUE)
      
      game_player_pitching <- game_player[[1,"splits"]]
      game_player_pitching <- game_player_pitching[game_player_pitching$group=="pitching",]
      
      url_player_detail <- paste0("http://statsapi.mlb.com/api/v1/people/",game_boxscore_pitchers[pidx,])
      resp_player_detail <- url_player_detail %>% baseballr:::mlb_api_call()
      game_player_detail <- jsonlite::fromJSON(jsonlite::toJSON(resp_player_detail[['people']]), flatten = TRUE)
      game_schedule_simple$person.id <- game_boxscore_pitchers[pidx,]
      game_schedule_simple$person.fullName <- game_player_detail[,"fullName"]
      
      game_player_pitching <- bind_cols(game_schedule_simple,game_player_pitching)
      
      player_pitching_tbllist[[player_pitching_counter]] <- game_player_pitching
      
      Sys.sleep(1)
    }
  }
  
  player_pitching_new <- rbindlist(player_pitching_tbllist[1:length(player_pitching_tbllist)],fill=TRUE)
  player_pitching_new$season <- as.integer(player_pitching_new$season)
  player_pitching_new$stat.homeRunsPer9 <- as.numeric(player_pitching_new$stat.homeRunsPer9)
  player_pitching_new$stat.runsScoredPer9 <- as.numeric(player_pitching_new$stat.runsScoredPer9)
  player_pitching_new$stat.inningsPitched <- as.numeric(player_pitching_new$stat.inningsPitched)
  player_pitching_new$stat.inningsPitched <- floor(player_pitching_new$stat.inningsPitched)+10*(player_pitching_new$stat.inningsPitched-floor(player_pitching_new$stat.inningsPitched))/3
  
  player_pitching_new$stat.strikePercentage <- as.numeric(player_pitching_new$stat.strikePercentage)
  
  player_pitching_new <- player_pitching_new[,c("season","date","game.gamePk","team.name","team.id","opponent.name","opponent.id","isHome","inningLabel","person.id","person.fullName","type","group","stat.gamesStarted","stat.hits","stat.baseOnBalls","stat.inningsPitched","stat.numberOfPitches","stat.atBats","stat.battersFaced","stat.runs","stat.homeRuns","stat.earnedRuns","stat.hitByPitch","stat.intentionalWalks","stat.balls","stat.strikeOuts","stat.strikes","stat.strikePercentage","stat.balks","stat.wildPitches","stat.outs","stat.inheritedRunners","stat.inheritedRunnersScored","stat.completeGames","stat.shutouts","stat.runsScoredPer9","stat.homeRunsPer9")]
  player_pitching_new$stat.whip <- (player_pitching_new$stat.baseOnBalls+player_pitching_new$stat.hits)/(player_pitching_new$stat.inningsPitched)
  
  Sys.sleep(3)
  
  player_pitching_full <- bind_rows(game_pitching_df0,player_pitching_new)
  player_pitching_full$date <- as.Date(player_pitching_full$date)
  
  ##  CALCULATION OF BY PLAYER SEASON AND LAST 5 GAME
  ##  COLUMNS: PLAYER NAME, LAST GAME, SEASON AB COUNT, SEASON AVG, LAST 5 AB COUNT, LAST 5 GAMES, TREND UP/DOWN, SPARK LINE
  
  ##  HITTING STATS. NOTE: THE ORDER OF OPERATIONS MATTERS HERE; ENCOUNTERED STRANGE BUGS WHEN OPERATIONS WERE COMBINED
  
  player_hitting_full$orig_seq <- row.names(player_hitting_full)
  
  player_hitting_full <-
    player_hitting_full %>% 
    arrange(date,desc(game.gamePk),orig_seq)
  
  player_hitting_full <-
    player_hitting_full %>% 
    mutate(game_count_all = rleid(game.gamePk))
  
  player_hitting_full <-
    player_hitting_full %>% 
    group_by(person.id,person.fullName) %>%
    mutate(game_count_player = rleid(game.gamePk))
  
  player_hitting_full  <-
    player_hitting_full  %>%
    group_by(person.id,person.fullName) %>%
    mutate(s_atBats = rollapply(stat.atBats,game_count_player,sum,align='right',fill=NA),
           s_hits = rollapply(stat.hits,game_count_player,sum,align='right',fill=NA),
           s_baseOnBalls = rollapply(stat.baseOnBalls,game_count_player,sum,align='right',fill=NA),
           s_hitByPitch = rollapply(stat.hitByPitch,game_count_player,sum,align='right',fill=NA),
           s_totalBases = rollapply(stat.totalBases,game_count_player,sum,align='right',fill=NA),
           s_sacFlies = rollapply(stat.sacFlies,game_count_player,sum,align='right',fill=NA),
           s_OPS = round(((s_atBats*(s_hits+s_baseOnBalls+s_hitByPitch)+s_totalBases*(s_atBats+s_baseOnBalls+s_sacFlies+s_hitByPitch))/(s_atBats*(s_atBats+s_baseOnBalls+s_sacFlies+s_hitByPitch))),3))
  
  player_hitting_full <-
    player_hitting_full %>%
    group_by(person.id,person.fullName) %>%
    mutate(l5_atBats = rollapply(stat.atBats,5,sum,align='right',fill=NA),
           l5_hits = rollapply(stat.hits,5,sum,align='right',fill=NA),
           l5_baseOnBalls = rollapply(stat.baseOnBalls,5,sum,align='right',fill=NA),
           l5_hitByPitch = rollapply(stat.hitByPitch,5,sum,align='right',fill=NA),
           l5_totalBases = rollapply(stat.totalBases,5,sum,align='right',fill=NA),
           l5_sacFlies = rollapply(stat.sacFlies,5,sum,align='right',fill=NA),
           l5_OPS = round(((l5_atBats*(l5_hits+l5_baseOnBalls+l5_hitByPitch)+l5_totalBases*(l5_atBats+l5_baseOnBalls+l5_sacFlies+l5_hitByPitch))/(l5_atBats*(l5_atBats+l5_baseOnBalls+l5_sacFlies+l5_hitByPitch))),3))
  
  player_hitting_full <-
    player_hitting_full %>%
    group_by(person.id,person.fullName) %>%
    mutate(lastGame = max(date))
  
  player_hitting_full_table <- player_hitting_full[player_hitting_full$date==player_hitting_full$lastGame,]
  player_hitting_full_table <- player_hitting_full_table[,c("person.id","person.fullName","lastGame","l5_atBats","l5_OPS","s_atBats","s_OPS")]
  player_hitting_full_table <- player_hitting_full_table[player_hitting_full_table$s_atBats!=0,]
  
  ##  PITCHING STATS. NOTE: THE ORDER OF OPERATIONS MATTERS HERE; ENCOUNTERED STRANGE BUGS WHEN OPERATIONS WERE COMBINED
  
  player_pitching_full$orig_seq <- row.names(player_pitching_full)
  
  player_pitching_full <-
    player_pitching_full %>% 
    arrange(date,desc(game.gamePk),orig_seq)
  
  player_pitching_full <-
    player_pitching_full %>% 
    mutate(game_count_all = rleid(game.gamePk))
  
  player_pitching_full <-
    player_pitching_full %>% 
    group_by(person.id,person.fullName) %>%
    mutate(game_count_player = rleid(game.gamePk))
  
  player_pitching_full  <-
    player_pitching_full  %>%
    group_by(person.id,person.fullName) %>%
    mutate(s_hits = rollapply(stat.hits,game_count_player,sum,align='right',fill=NA),
           s_baseOnBalls = rollapply(stat.baseOnBalls,game_count_player,sum,align='right',fill=NA),
           s_inningsPitched = rollapply(stat.inningsPitched,game_count_player,sum,align='right',fill=NA),
           s_WHIP = round((s_baseOnBalls+s_hits)/(s_inningsPitched),3),
           s_starter = rollapply(stat.gamesStarted,game_count_player,mean,align='right',fill=NA))
  
  player_pitching_full <-
    player_pitching_full %>%
    group_by(person.id,person.fullName) %>%
    mutate(l5_hits = rollapply(stat.hits,5,sum,align='right',fill=NA),
           l5_baseOnBalls = rollapply(stat.baseOnBalls,5,sum,align='right',fill=NA),
           l5_inningsPitched = rollapply(stat.inningsPitched,5,sum,align='right',fill=NA),
           l5_WHIP = round((l5_baseOnBalls+l5_hits)/(l5_inningsPitched),3),
           l5_starter = rollapply(stat.gamesStarted,1,mean,align='right',fill=NA))
  
  player_pitching_full <-
    player_pitching_full %>%
    group_by(person.id,person.fullName) %>%
    mutate(lastGame = max(date))
  
  player_pitching_full_table <- player_pitching_full[player_pitching_full$date==player_pitching_full$lastGame,]
  player_pitching_full_table$l5_starter <- ifelse(player_pitching_full_table$l5_starter==1,"YES","NO")
  player_pitching_full_table <- player_pitching_full_table[,c("person.id","person.fullName","l5_starter","lastGame","l5_inningsPitched","l5_WHIP","s_inningsPitched","s_WHIP")]
  
  ##  FORMATTABLES CREATION
  
  ##  HITTING FORMATTABLE
  ##  We first begin with functions for color coding; to set limits correctly, we custom filter non-essential players
  ##  We only use stats for players that have at least played in 10 games
  ##  We create color coding for both season level and last 5 game stats
  
  player_hitting_full_table <- player_hitting_full_table[player_hitting_full_table$person.fullName %in% roster_hitter$person.fullName,]
  
  hitting_s_lh <- max(player_hitting_full[player_hitting_full$game_count_player>=10,]$s_OPS)
  hitting_s_ll <- min(player_hitting_full[player_hitting_full$game_count_player>=10,]$s_OPS)
  hitting_s_lm <- median(player_hitting_full[player_hitting_full$game_count_player>=10,]$s_OPS)
  hitting_s_color_fun <- colorRamp2(breaks=c(hitting_s_ll,mean(c(hitting_s_ll,hitting_s_lm)),hitting_s_lm,mean(c(hitting_s_lh,hitting_s_lm)),hitting_s_lh),colors=c("#BF3030","#DD9085","white","#B7E7B3","#30BF30"))
  hitting_s_formatter <- formatter("span",
                                   style = x ~ style(display = "block",
                                                     padding = "0 4px",
                                                     'font.weight' = "bold",
                                                     'border-radius' = "4px",
                                                     width="110px",
                                                     'margin-left'="auto",
                                                     'margin-right'="auto",
                                                     'background-color' = hitting_s_color_fun(x),
                                                     'color' = ifelse(x==hitting_s_lh,"#FFFFFF",
                                                                      ifelse(x==hitting_s_ll,"#FFFFFF","#000000"))),
                                   x ~ digits(x,3))
  
  hittingl5_s_lh <- max(player_hitting_full[player_hitting_full$game_count_player>=10,]$l5_OPS)
  hittingl5_s_ll <- min(player_hitting_full[player_hitting_full$game_count_player>=10,]$l5_OPS)
  hittingl5_s_lm <- median(player_hitting_full[player_hitting_full$game_count_player>=10,]$l5_OPS)
  hittingl5_s_color_fun <- colorRamp2(breaks=c(hittingl5_s_ll,mean(c(hittingl5_s_ll,hittingl5_s_lm)),hittingl5_s_lm,mean(c(hittingl5_s_lh,hittingl5_s_lm)),hittingl5_s_lh),colors=c("#BF3030","#DD9085","white","#B7E7B3","#30BF30"))
  hittingl5_s_formatter <- formatter("span",
                                     style = x ~ style(display = "block",
                                                       padding = "0 4px",
                                                       'font.weight' = "bold",
                                                       'border-radius' = "4px",
                                                       width="110px",
                                                       'margin-left'="auto",
                                                       'margin-right'="auto",
                                                       'background-color' = hittingl5_s_color_fun(x),
                                                       'color' = ifelse(x==hittingl5_s_lh,"#FFFFFF",
                                                                        ifelse(x==hittingl5_s_ll,"#FFFFFF","#000000"))),
                                     x ~ digits(x,3))
  
  #  Sparkline creation
  
  player_hitting_spark <- player_hitting_full
  player_hitting_spark <- player_hitting_spark[player_hitting_spark$person.fullName %in% roster_hitter$person.fullName,]
  player_hitting_ftbl_spark <- as.data.table(player_hitting_spark[,c("person.id","game_count_all","l5_OPS")])
  player_hitting_ftbl_spark_max <- max(player_hitting_spark$l5_OPS,na.rm=TRUE)
  player_hitting_ftbl_spark_min <- min(player_hitting_spark$l5_OPS,na.rm=TRUE)
  player_hitting_ftbl_spark <- dcast(player_hitting_ftbl_spark,person.id~game_count_all)
  player_hitting_ftbl_spark$ops_spark <- apply(player_hitting_ftbl_spark[,2:ncol(player_hitting_ftbl_spark)],1,FUN = function(x) as.character(htmltools::as.tags(sparkline(as.numeric(x),type = "line",spotColor="",minSpotColor="",maxSpotColor="",chartRangeMin=player_hitting_ftbl_spark_min,chartRangeMax=player_hitting_ftbl_spark_max))))
  
  player_hitting_ftbl <- merge(player_hitting_full_table,player_hitting_ftbl_spark,by="person.id")[,c(names(player_hitting_full_table),"ops_spark")]
  
  #  Final Formattable creation
  
  names(player_hitting_ftbl)[names(player_hitting_ftbl) == "person.fullName"] <- "Player"
  names(player_hitting_ftbl)[names(player_hitting_ftbl) == "lastGame"] <- "Last Game"
  names(player_hitting_ftbl)[names(player_hitting_ftbl) == "s_atBats"] <- "Season #AtBats"
  names(player_hitting_ftbl)[names(player_hitting_ftbl) == "s_OPS"] <- "Season OPS"
  names(player_hitting_ftbl)[names(player_hitting_ftbl) == "l5_atBats"] <- "Last 5 Games #AtBats"
  names(player_hitting_ftbl)[names(player_hitting_ftbl) == "l5_OPS"] <- "Last 5 OPS"
  names(player_hitting_ftbl)[names(player_hitting_ftbl) == "ops_spark"] <- "Rolling 5 Game OPS"
  
  player_hitting_ftbl <- player_hitting_ftbl[order(-player_hitting_ftbl$"Last 5 OPS",-player_hitting_ftbl$"Season OPS",na.last=TRUE),]
  names(player_hitting_ftbl)[names(player_hitting_ftbl) == "Last 5 OPS"] <- paste0("Last 5 OPS",icontext("arrow-down"))
  rownames(player_hitting_ftbl) <- NULL
  player_hitting_ftbl <- as.htmlwidget(formattable(player_hitting_ftbl,
                                                   align=c("l",rep("c",ncol(player_hitting_ftbl)-1)),
                                                   list("person.id"=FALSE,
                                                        "Player"=formatter("span",style = x ~ style(display="block",
                                                                                                    width="130px",
                                                                                                    'font.weight' = "bold")),
                                                        "Last Game"=formatter("span",style = x ~ style(display = "block",
                                                                                                       'margin-left'="auto",
                                                                                                       'margin-right'="auto",
                                                                                                       width="80px")),
                                                        "Season #AtBats"=formatter("span",style = x ~ style(display = "block",
                                                                                                            'margin-left'="auto",
                                                                                                            'margin-right'="auto",
                                                                                                            width="80px")),
                                                        "Season OPS"=hitting_s_formatter,
                                                        "Last 5 Games #AtBats"=formatter("span",style = x ~ style(display = "block",
                                                                                                            'margin-left'="auto",
                                                                                                            'margin-right'="auto",
                                                                                                            width="80px")),
                                                        "Last 5 OPS<i class=\"glyphicon glyphicon-arrow-down\"></i>"=hittingl5_s_formatter,
                                                        "Rolling 5 Game OPS")))
  player_hitting_ftbl$dependencies = c(player_hitting_ftbl$dependencies,htmlwidgets:::widget_dependencies("sparkline","sparkline"))
  
  ##  PITCHING FORMATTABLE
  ##  We first begin with functions for color coding
  ##  We only use stats for players that have at least played in 5 games
  ##  We create color coding for both season level and last 5 game stats, and by starter and non-starter pitchers
  
  player_pitching_full_table <- player_pitching_full_table[player_pitching_full_table$person.fullName %in% roster_pitcher$person.fullName,]
  
  pitching_ss_lh <- max(player_pitching_full[player_pitching_full$stat.gamesStarted==1 & player_pitching_full$game_count_player>=5,]$s_WHIP)
  pitching_ss_ll <- min(player_pitching_full[player_pitching_full$stat.gamesStarted==1 & player_pitching_full$game_count_player>=5,]$s_WHIP)
  pitching_ss_lm <- median(player_pitching_full[player_pitching_full$stat.gamesStarted==1 & player_pitching_full$game_count_player>=5,]$s_WHIP)
  pitching_ss_color_fun <- colorRamp2(breaks=c(pitching_ss_ll,mean(c(pitching_ss_ll,pitching_ss_lm)),pitching_ss_lm,mean(c(pitching_ss_lh,pitching_ss_lm)),pitching_ss_lh),colors=c("#30BF30","#B7E7B3","white","#DD9085","#BF3030"))
  
  pitching_sr_lh <- max(player_pitching_full[player_pitching_full$stat.gamesStarted!=1 & player_pitching_full$game_count_player>=5,]$s_WHIP)
  pitching_sr_ll <- min(player_pitching_full[player_pitching_full$stat.gamesStarted!=1 & player_pitching_full$game_count_player>=5,]$s_WHIP)
  pitching_sr_lm <- median(player_pitching_full[player_pitching_full$stat.gamesStarted!=1 & player_pitching_full$game_count_player>=5,]$s_WHIP)
  pitching_sr_color_fun <- colorRamp2(breaks=c(pitching_sr_ll,mean(c(pitching_sr_ll,pitching_sr_lm)),pitching_sr_lm,mean(c(pitching_sr_lh,pitching_sr_lm)),pitching_sr_lh),colors=c("#30BF30","#B7E7B3","white","#DD9085","#BF3030"))
  
  pitching_s_formatter <- formatter("span",
                                    style = x ~ style(display = "block",
                                                      padding = "0 4px",
                                                      'font.weight' = "bold",
                                                      'border-radius' = "4px",
                                                      width="110px",
                                                      'margin-left'="auto",
                                                      'margin-right'="auto",
                                                      'background-color' = ifelse(player_pitching_ftbl$Starter=="YES",pitching_ss_color_fun(x),pitching_sr_color_fun(x)),
                                                      'color' = ifelse(x==pitching_ss_lh | x==pitching_sr_lh,"#FFFFFF",
                                                                       ifelse(x==pitching_ss_ll | x==pitching_sr_ll,"#FFFFFF","#000000"))),
                                    x ~ digits(x,3))
  
  pitching_l5s_lh <- max(player_pitching_full[player_pitching_full$stat.gamesStarted==1 & player_pitching_full$game_count_player>=5,]$l5_WHIP)
  pitching_l5s_ll <- min(player_pitching_full[player_pitching_full$stat.gamesStarted==1 & player_pitching_full$game_count_player>=5,]$l5_WHIP)
  pitching_l5s_lm <- median(player_pitching_full[player_pitching_full$stat.gamesStarted==1 & player_pitching_full$game_count_player>=5,]$l5_WHIP)
  pitching_l5s_color_fun <- colorRamp2(breaks=c(pitching_l5s_ll,mean(c(pitching_l5s_ll,pitching_l5s_lm)),pitching_l5s_lm,mean(c(pitching_l5s_lh,pitching_l5s_lm)),pitching_l5s_lh),colors=c("#30BF30","#B7E7B3","white","#DD9085","#BF3030"))
  
  pitching_l5r_lh <- max(player_pitching_full[player_pitching_full$stat.gamesStarted!=1 & player_pitching_full$game_count_player>=5,]$l5_WHIP)
  pitching_l5r_ll <- min(player_pitching_full[player_pitching_full$stat.gamesStarted!=1 & player_pitching_full$game_count_player>=5,]$l5_WHIP)
  pitching_l5r_lm <- median(player_pitching_full[player_pitching_full$stat.gamesStarted!=1 & player_pitching_full$game_count_player>=5,]$l5_WHIP)
  pitching_l5r_color_fun <- colorRamp2(breaks=c(pitching_l5r_ll,mean(c(pitching_l5r_ll,pitching_l5r_lm)),pitching_l5r_lm,mean(c(pitching_l5r_lh,pitching_l5r_lm)),pitching_l5r_lh),colors=c("#30BF30","#B7E7B3","white","#DD9085","#BF3030"))
  
  pitching_l5_formatter <- formatter("span",
                                     style = x ~ style(display = "block",
                                                       padding = "0 4px",
                                                       'font.weight' = "bold",
                                                       'border-radius' = "4px",
                                                       width="110px",
                                                       'margin-left'="auto",
                                                       'margin-right'="auto",
                                                       'background-color' = ifelse(player_pitching_ftbl$Starter=="YES",pitching_l5s_color_fun(x),pitching_l5r_color_fun(x)),
                                                       'color' = ifelse(x==pitching_l5s_lh | x==pitching_l5r_lh,"#FFFFFF",
                                                                        ifelse(x==pitching_l5s_ll | x==pitching_l5r_ll,"#FFFFFF","#000000"))),
                                     x ~ digits(x,3))
  
  #  Sparkline creation
  
  player_pitching_spark <- player_pitching_full
  player_pitching_spark <- player_pitching_spark[player_pitching_spark$person.fullName %in% roster_pitcher$person.fullName,]
  player_pitching_ftbl_spark <- as.data.table(player_pitching_spark[,c("person.id","game_count_all","l5_WHIP")])
  player_pitching_ftbl_spark_max <- max(player_pitching_spark$l5_WHIP,na.rm=TRUE)
  player_pitching_ftbl_spark_min <- min(player_pitching_spark$l5_WHIP,na.rm=TRUE)
  player_pitching_ftbl_spark <- dcast(player_pitching_ftbl_spark,person.id~game_count_all)
  player_pitching_ftbl_spark$whip_spark <- apply(player_pitching_ftbl_spark[,2:ncol(player_pitching_ftbl_spark)],1,FUN = function(x) as.character(htmltools::as.tags(sparkline(as.numeric(x),type = "line",spotColor="",minSpotColor="",maxSpotColor="",chartRangeMin=player_pitching_ftbl_spark_min,chartRangeMax=player_pitching_ftbl_spark_max))))
  
  player_pitching_ftbl <- merge(player_pitching_full_table,player_pitching_ftbl_spark,by="person.id")[,c(names(player_pitching_full_table),"whip_spark")]
  
  player_pitching_ftbl$l5_inningsPitchedf <- round((player_pitching_ftbl$l5_inningsPitched*3)%%3)
  player_pitching_ftbl$l5_inningsPitchedw <- round(player_pitching_ftbl$l5_inningsPitched-(player_pitching_ftbl$l5_inningsPitchedf)/3)
  player_pitching_ftbl$l5_inningsPitched <- ifelse(player_pitching_ftbl$l5_inningsPitchedf==0,player_pitching_ftbl$l5_inningsPitchedw,paste0(player_pitching_ftbl$l5_inningsPitchedw," ",player_pitching_ftbl$l5_inningsPitchedf,"/3"))
  player_pitching_ftbl$s_inningsPitchedf <- round((player_pitching_ftbl$s_inningsPitched*3)%%3)
  player_pitching_ftbl$s_inningsPitchedw <- round(player_pitching_ftbl$s_inningsPitched-(player_pitching_ftbl$s_inningsPitchedf)/3)
  player_pitching_ftbl$s_inningsPitched <- ifelse(player_pitching_ftbl$s_inningsPitchedf==0,player_pitching_ftbl$s_inningsPitchedw,paste0(player_pitching_ftbl$s_inningsPitchedw," ",player_pitching_ftbl$s_inningsPitchedf,"/3"))
  player_pitching_ftbl$l5_inningsPitchedf <- NULL
  player_pitching_ftbl$l5_inningsPitchedw <- NULL
  player_pitching_ftbl$s_inningsPitchedf <- NULL
  player_pitching_ftbl$s_inningsPitchedw <- NULL
  
  #  Final Formattable creation
  
  names(player_pitching_ftbl)[names(player_pitching_ftbl) == "person.fullName"] <- "Player"
  names(player_pitching_ftbl)[names(player_pitching_ftbl) == "l5_starter"] <- "Starter"
  names(player_pitching_ftbl)[names(player_pitching_ftbl) == "lastGame"] <- "Last Game"
  names(player_pitching_ftbl)[names(player_pitching_ftbl) == "s_inningsPitched"] <- "Season Innings Pitched"
  names(player_pitching_ftbl)[names(player_pitching_ftbl) == "s_WHIP"] <- "Season WHIP"
  names(player_pitching_ftbl)[names(player_pitching_ftbl) == "l5_inningsPitched"] <- "Last 5 Games Innings Pitched"
  names(player_pitching_ftbl)[names(player_pitching_ftbl) == "l5_WHIP"] <- "Last 5 WHIP"
  names(player_pitching_ftbl)[names(player_pitching_ftbl) == "whip_spark"] <- "Rolling 5 Game WHIP"
  
  player_pitching_ftbl <- player_pitching_ftbl[order(desc(player_pitching_ftbl$"Starter"),player_pitching_ftbl$"Last 5 WHIP",player_pitching_ftbl$"Season WHIP",na.last=TRUE),]
  names(player_pitching_ftbl)[names(player_pitching_ftbl) == "Last 5 WHIP"] <- paste0("Last 5 WHIP",icontext("arrow-down"))
  rownames(player_pitching_ftbl) <- NULL
  player_pitching_ftbl <- as.htmlwidget(formattable(player_pitching_ftbl,
                                                    align=c("l",rep("c",ncol(player_pitching_ftbl)-1)),
                                                    list("person.id"=FALSE,
                                                         "Player"=formatter("span",style = x ~ style(display="block",
                                                                                                     width="140px",
                                                                                                     'font.weight' = "bold")),
                                                         "Starter"=formatter("span",style = x ~ style(display="block",
                                                                                                      width="80px",
                                                                                                      'font.weight' = "bold")),
                                                         "Last Game"=formatter("span",style = x ~ style(display = "block",
                                                                                                        'margin-left'="auto",
                                                                                                        'margin-right'="auto",
                                                                                                        width="80px")),
                                                         "Season Innings Pitched"=formatter("span",style = x ~ style(display = "block",
                                                                                                                     'margin-left'="auto",
                                                                                                                     'margin-right'="auto",
                                                                                                                     width="80px")),
                                                         "Season WHIP"=pitching_s_formatter,
                                                         "Last 5 Games Innings Pitched"=formatter("span",style = x ~ style(display = "block",
                                                                                                                     'margin-left'="auto",
                                                                                                                     'margin-right'="auto",
                                                                                                                     width="80px")),
                                                         "Last 5 WHIP<i class=\"glyphicon glyphicon-arrow-down\"></i>"=pitching_l5_formatter,
                                                         "Rolling 5 Game WHIP")))
  player_pitching_ftbl$dependencies = c(player_pitching_ftbl$dependencies,htmlwidgets:::widget_dependencies("sparkline","sparkline"))
  
  ##  FINAL FILE SAVING AND FORMATTING
  
  maxdate <- max(player_hitting_full$date)
  
  ftbl_file1_html <- paste0(img_dir,"player_hitting_ftbl_",Sys.Date(),".html")
  ftbl_file1_png <- paste0(img_dir,"player_hitting_ftbl_",Sys.Date(),".png")
  
  saveWidget(player_hitting_ftbl,ftbl_file1_html)
  
  webshot(ftbl_file1_html,ftbl_file1_png,vwidth=900,vheight=600,zoom=2)
  
  ftbl_1 <- image_read(ftbl_file1_png)
  ftbl_1 <- image_trim(ftbl_1)
  ftbl_1 <- image_border(ftbl_1,"white","0x100")
  ftbl_1 <- image_annotate(ftbl_1,paste0("Phillies Hitting Stats by Player [",maxdate,"]"),gravity="north",size=60,weight=700)
  ftbl_1 <- image_annotate(ftbl_1,paste0("Strength of color is relative to team current season best for each statistic category. White font is a team current season record."),gravity="southwest",size=20,weight=700)
  ftbl_1 <- image_annotate(ftbl_1,paste0("Source: statsapi.mlb.com\n@iamvishnurajan.bsky.social"),gravity="southeast",size=20,weight=700)
  ftbl_1 <- image_trim(ftbl_1)
  ftbl_1 <- image_border(ftbl_1,"white","20x20")
  image_write(ftbl_1,path=ftbl_file1_png,format="png")
  
  ftbl_file2_html <- paste0(img_dir,"player_pitching_ftbl_",Sys.Date(),".html")
  ftbl_file2_png <- paste0(img_dir,"player_pitching_ftbl_",Sys.Date(),".png")
  
  saveWidget(player_pitching_ftbl,ftbl_file2_html)
  
  webshot(ftbl_file2_html,ftbl_file2_png,vwidth=1050,vheight=600,zoom=2)
  
  ftbl_2 <- image_read(ftbl_file2_png)
  ftbl_2 <- image_trim(ftbl_2)
  ftbl_2 <- image_border(ftbl_2,"white","0x110")
  ftbl_2 <- image_annotate(ftbl_2,paste0("Phillies Pitching Stats by Player [",maxdate,"]"),gravity="north",size=60,weight=700)
  ftbl_2 <- image_annotate(ftbl_2,paste0("Strength of color is relative to team current season best for each statistic category. White font is a team current season record."),gravity="southwest",size=20,weight=700)
  ftbl_2 <- image_annotate(ftbl_2,paste0("Source: statsapi.mlb.com\n@iamvishnurajan.bsky.social"),gravity="southeast",size=20,weight=700)
  ftbl_2 <- image_trim(ftbl_2)
  ftbl_2 <- image_border(ftbl_2,"white","20x20")
  image_write(ftbl_2,path=ftbl_file2_png,format="png")
  
  ##  WE NOW POST TO BSKY WITH TEXT AND ATTACHMENTS OF CHARTS
  
  Sys.sleep(3)
  
  season_best_ops <- player_hitting_full[player_hitting_full$s_OPS==hitting_s_lh & !is.na(player_hitting_full$s_OPS) & player_hitting_full$game_count_player>=10,]
  season_best_ops <- paste0(season_best_ops$person.fullName,", ",season_best_ops$date,", ",season_best_ops$s_OPS)
  l5_best_ops <- player_hitting_full[player_hitting_full$l5_OPS==hittingl5_s_lh & !is.na(player_hitting_full$l5_OPS) & player_hitting_full$game_count_player>=10,]
  l5_best_ops <- paste0(l5_best_ops$person.fullName,", ",l5_best_ops$date,", ",l5_best_ops$l5_OPS)
  
  season_best_whip_s <- player_pitching_full[player_pitching_full$s_WHIP==pitching_ss_ll & !is.na(player_pitching_full$s_WHIP) & player_pitching_full$stat.gamesStarted==1 & player_pitching_full$game_count_player>=5,]
  season_best_whip_s <- paste0(season_best_whip_s$person.fullName,", ",season_best_whip_s$date,", ",season_best_whip_s$s_WHIP)
  season_best_whip_r <- player_pitching_full[player_pitching_full$s_WHIP==pitching_sr_ll & !is.na(player_pitching_full$s_WHIP) & player_pitching_full$stat.gamesStarted!=1 & player_pitching_full$game_count_player>=5,]
  season_best_whip_r <- paste0(season_best_whip_r$person.fullName,", ",season_best_whip_r$date,", ",season_best_whip_r$s_WHIP)
  l5_best_whip_s <- player_pitching_full[player_pitching_full$l5_WHIP==pitching_l5s_ll & !is.na(player_pitching_full$l5_WHIP) & player_pitching_full$stat.gamesStarted==1 & player_pitching_full$game_count_player>=5,]
  l5_best_whip_s <- paste0(l5_best_whip_s$person.fullName,", ",l5_best_whip_s$date,", ",l5_best_whip_s$l5_WHIP)
  l5_best_whip_r <- player_pitching_full[player_pitching_full$l5_WHIP==pitching_l5r_ll & !is.na(player_pitching_full$l5_WHIP) & player_pitching_full$stat.gamesStarted!=1 & player_pitching_full$game_count_player>=5,]
  l5_best_whip_r <- paste0(l5_best_whip_r$person.fullName,", ",l5_best_whip_r$date,", ",l5_best_whip_r$l5_WHIP)
  
  bstext <- paste0("*Automated Post*\n\nPhiladelphia Phillies Daily Stat Charts by Player\nMost recent game date: ",format(maxdate,"%b %d, %Y"),"\n\n")
  
  alt_text1 <- paste0("Hitting: OPS by-player trend",
                      "\nSeason Best OPS: ",season_best_ops,
                      "\nLast 5 Best OPS: ",l5_best_ops)
  alt_text2 <- paste0("Pitching: WHIP by-player trend, separated by starter and reliever",
                      "\nSeason Best WHIP (starter): ",season_best_whip_s,
                      "\nSeason Best WHIP (reliever): ",season_best_whip_r,
                      "\nLast 5 Best WHIP (s): ",l5_best_whip_s,
                      "\nLast 5 Best WHIP (r): ",l5_best_whip_r)
  
  bs_post(
    text = bstext,
    images = c(ftbl_file1_png,ftbl_file2_png),
    images_alt = c(alt_text1,alt_text2)
  )
  
  ##  WE NOW WRITE OUT DATA FILES, INCLUDING BACKUPS OF ORIGINALS
  
  Sys.sleep(3)
  
  write.csv(game_hitting_df0,file=game_hitting_file_bkup,row.names=FALSE)
  write.csv(player_hitting_full,file=game_hitting_file,row.names=FALSE)
  
  write.csv(game_pitching_df0,file=game_pitching_file_bkup,row.names=FALSE)
  write.csv(player_pitching_full,file=game_pitching_file,row.names=FALSE)
  
  write.csv(game_schedule_df0,file=game_schedule_file_bkup,row.names=FALSE)
  write.csv(game_meta,file=game_schedule_file,row.names=FALSE)
  
  
}

##  FINAL LOGGING

endtime <- proc.time() - starttime
endsecs <- endtime[3]
print(endsecs)
print(Sys.time())
cat("\n\n")

sink(type="message")
sink(type="output")
close(sink_msgs)
