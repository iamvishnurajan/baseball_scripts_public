###
###  EXTRACT AND CHARTING OF PHILLIES BY-GAME STATS
###  HITTING, PITCHING, AND FIELDING
###  NOTE: BY-GAME RISP CALCLATIONS ARE MANUAL AND MAY NOT MATCH OFFICIAL STATS
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

bsky_fullname <- 'ADD YOUR FULL BSKY HANDLE HERE (e.g., iamvishnurajan.bsky.social)'
bsky_pass <- 'ADD AN APP SPECIFIC BSKY PASSWORD'

set_bluesky_user(bsky_fullname)
set_bluesky_pass(bsky_pass)
my_auth <- bs_auth(bsky_fullname,bsky_pass)

# Lookup and hardcoding of Team ID and Season Year

url_teams <- "https://statsapi.mlb.com/api/v1/teams"
resp_teams <- url_teams %>% baseballr:::mlb_api_call()
teams <- jsonlite::fromJSON(jsonlite::toJSON(resp_teams[['teams']]), flatten = TRUE)
teams <- teams[teams$sport.id==1,]

id <- 143
year <- 2024

# Locations of key files that are needed

game_schedule_file <- paste0(main_dir,"game_schedule.csv")
game_detail_full_file <- paste0(main_dir,"game_detail_full.csv")
result_event_lookup_file <- paste0(main_dir,"result_event_lookup.csv")
game_schedule_file_bkup <- paste0(paste0(bkup_dir,"game_schedule_"),format(Sys.time(), "%Y%m%d_%H%M%S"),".csv")
game_detail_full_file_bkup <- paste0(paste0(bkup_dir,"game_detail_full_"),format(Sys.time(), "%Y%m%d_%H%M%S"),".csv")

game_schedule_df0 <- read.csv(game_schedule_file, header = TRUE, sep = ",", check.names=FALSE)
game_detail_full_df0 <- read.csv(game_detail_full_file, header = TRUE, sep = ",", check.names=FALSE)
result_event_lookup <- read.csv(result_event_lookup_file, header = TRUE, sep = ",", check.names=FALSE)

# Logging

sink_msgs <- file(paste0(main_dir,"std_msgs_mlb_stats.txt"), open="at")
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

##  META LOOKUPS IF OTHER FIELDS OR STAT TYPES ARE NEEDED
# url1 <- "https://statsapi.mlb.com/api/v1/statGroups"
# resp1 <- url1 %>% baseballr:::mlb_api_call()
# 
# url2 <- "https://statsapi.mlb.com/api/v1/statTypes"
# resp2 <- url2 %>% baseballr:::mlb_api_call()
# 
# url3 <- "https://statsapi.mlb.com/api/v1/situationCodes"
# resp3 <- url3 %>% baseballr:::mlb_api_call()

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
  
  ##  BY-GAME AND SEASON-TO-DATE LOGS
  stat_group <- "hitting"
  hitting_std$date <- as.Date(hitting_std$date)
  for (hidx in 1:ncol(hitting_std)) {
    if (grepl("stat.",colnames(hitting_std[hidx]))){
      hitting_std[,hidx] <- as.numeric(hitting_std[,hidx])
    }
  }
  hitting_all <- mlb_team_season(stat_group,year)
  hitting_all <- hitting_all[hitting_all$team.id %in% teams$id,]
  for (haidx in 1:ncol(hitting_all)) {
    if (grepl("stat.",colnames(hitting_all[haidx]))){
      hitting_all[,haidx] <- as.numeric(hitting_all[,haidx])
    }
  }
  hitting_all_max <- hitting_all[hitting_all$stat.ops==max(hitting_all$stat.ops),c("team.id","team.name","stat.ops")]
  hitting_all_min <- hitting_all[hitting_all$stat.ops==min(hitting_all$stat.ops),c("team.id","team.name","stat.ops")]
  hitting_all_avg <- round(mean(hitting_all$stat.ops),3)
  
  stat_group <- "pitching"
  pitching_std <- mlb_game_logs_std(id,stat_group,year)
  pitching_std$date <- as.Date(pitching_std$date)
  for (pidx in 1:ncol(pitching_std)) {
    if (grepl("stat.",colnames(pitching_std[pidx]))){
      pitching_std[,pidx] <- as.numeric(pitching_std[,pidx])
    }
  }
  pitching_all <- mlb_team_season(stat_group,year)
  pitching_all <- pitching_all[pitching_all$team.id %in% teams$id,]
  for (paidx in 1:ncol(pitching_all)) {
    if (grepl("stat.",colnames(pitching_all[paidx]))){
      pitching_all[,paidx] <- as.numeric(pitching_all[,paidx])
    }
  }
  pitching_all_max <- pitching_all[pitching_all$stat.whip==max(pitching_all$stat.whip),c("team.id","team.name","stat.whip")]
  pitching_all_min <- pitching_all[pitching_all$stat.whip==min(pitching_all$stat.whip),c("team.id","team.name","stat.whip")]
  pitching_all_avg <- round(mean(pitching_all$stat.whip),3)
  
  stat_group <- "fielding"
  fielding_std <- mlb_game_logs_std(id,stat_group,year)
  fielding_std$date <- as.Date(fielding_std$date)
  for (fidx in 1:ncol(fielding_std)) {
    if (grepl("stat.",colnames(fielding_std[fidx]))){
      fielding_std[,fidx] <- as.numeric(fielding_std[,fidx])
    }
  }
  fielding_all <- mlb_team_season(stat_group,year)
  fielding_all <- fielding_all[fielding_all$team.id %in% teams$id,]
  for (faidx in 1:ncol(fielding_all)) {
    if (grepl("stat.",colnames(fielding_all[faidx]))){
      fielding_all[,faidx] <- as.numeric(fielding_all[,faidx])
    }
  }
  fielding_all_max <- fielding_all[fielding_all$stat.rangeFactorPer9Inn==max(fielding_all$stat.rangeFactorPer9Inn),c("team.id","team.name","stat.rangeFactorPer9Inn")]
  fielding_all_min <- fielding_all[fielding_all$stat.rangeFactorPer9Inn==min(fielding_all$stat.rangeFactorPer9Inn),c("team.id","team.name","stat.rangeFactorPer9Inn")]
  fielding_all_avg <- round(mean(fielding_all$stat.rangeFactorPer9Inn),2)
  
  stat_group <- "hitting"
  sit_code <- "risp"
  hitting_risp_all <- mlb_game_logs_sitcode(stat_group,sit_code,year)
  hitting_risp_all <- hitting_risp_all[hitting_risp_all$team.id %in% teams$id,]
  for (hridx in 1:ncol(hitting_risp_all)) {
    if (grepl("stat.",colnames(hitting_risp_all[hridx]))){
      hitting_risp_all[,hridx] <- as.numeric(hitting_risp_all[,hridx])
    }
  }
  hitting_risp_all_max <- hitting_risp_all[hitting_risp_all$stat.avg==max(hitting_risp_all$stat.avg),c("team.id","team.name","stat.avg")]
  hitting_risp_all_min <- hitting_risp_all[hitting_risp_all$stat.avg==min(hitting_risp_all$stat.avg),c("team.id","team.name","stat.avg")]
  hitting_risp_all_avg <- round(mean(hitting_risp_all$stat.avg),3)
  
  ##  LARGE SEQUENCE FOR PULLING PLAY-BY-PLAY INDIVIDUAL GAME DETAIL
  ##  THIS IS FOR CALCUATION OF BA WITH RISP BY GAME AS IT DOES NOT SEEM TO BE AVAILABLE OTHERWISE
  ##  ONLY SEASON-TO-DATE BA WITH RISP APPEARS TO BE AVAILABLE VIA API
  
  # We first extract play-by-play data, clean it, add necessary calculated columns, and join it to the existing data.frame
  # This section is error-prone and is only a best guesstimate of how the situational statistics are calculated by other stat sources
  # The method itself is self-consistent but will likely have minor differences to other statistical sources
  # Improvements for this section are welcomed. In particular for how to determine RISP from the MLB play-by-play game logs.
  
  game_detail_tbllist <- list()
  for (gidx in 1:nrow(new_game_diff)){
    url4 <- paste0("https://statsapi.mlb.com/api/v1/game/",new_game_diff[gidx,"game.gamePk"],"/playByPlay")
    resp4 <- url4 %>% baseballr:::mlb_api_call()
    game_detail <- jsonlite::fromJSON(jsonlite::toJSON(resp4[['allPlays']]), flatten = TRUE)
    if(!("matchup.postOnFirst.fullName" %in% colnames(game_detail))) {
      game_detail$matchup.postOnFirst.fullName <- NA;
    }
    if(!("matchup.postOnSecond.fullName" %in% colnames(game_detail))) {
      game_detail$matchup.postOnSecond.fullName <- NA;
    }
    if(!("matchup.postOnThird.fullName" %in% colnames(game_detail))) {
      game_detail$matchup.postOnThird.fullName <- NA;
    }
    game_detail$advBase <- ifelse(grepl("stolen",game_detail[,4]),"STOLEN",
                                  ifelse(grepl("wild",game_detail[,4]),"WILD PITCH",
                                         ifelse(grepl("passed",game_detail[,4]),"PASSED BALL","NONE")))
    game_detail_simple <- game_detail[,c("atBatIndex","result.event","result.eventType","result.description",
                                         "result.rbi","result.awayScore","result.homeScore","about.inning","about.halfInning","count.outs",
                                         "matchup.batter.id","matchup.batter.fullName","matchup.splits.menOnBase","advBase","matchup.postOnFirst.fullName","matchup.postOnSecond.fullName","matchup.postOnThird.fullName")]
    game_detail_simple <-
      game_detail_simple %>%
      group_by(about.inning,about.halfInning) %>%
      mutate(matchup.splits.menOnBase = lag(matchup.splits.menOnBase,1,order_by=atBatIndex),
             matchup.postOnFirst.fullName = lag(matchup.postOnFirst.fullName,1,order_by=atBatIndex),
             matchup.postOnSecond.fullName = lag(matchup.postOnSecond.fullName,1,order_by=atBatIndex),
             matchup.postOnThird.fullName = lag(matchup.postOnThird.fullName,1,order_by=atBatIndex))
    game_detail_simple$matchup.splits.menOnBase <- ifelse(is.na(game_detail_simple$matchup.splits.menOnBase),"Empty",game_detail$matchup.splits.menOnBase)
    game_detail_simple$season <- new_game_diff$season[gidx]
    game_detail_simple$date <- new_game_diff$date[gidx]
    game_detail_simple$game.gamePk <- new_game_diff$game.gamePk[gidx]
    game_detail_simple$isHome <- new_game_diff$isHome[gidx]
    game_detail_simple$inningLabel <- new_game_diff$inningLabel[gidx]
    game_detail_simple$team.name <- new_game_diff$team.name[gidx]
    game_detail_simple$team.id <- new_game_diff$team.id[gidx]
    game_detail_simple$opponent.name <- new_game_diff$opponent.name[gidx]
    game_detail_simple$opponent.id <- new_game_diff$opponent.id[gidx]
    game_detail_simple <- game_detail_simple[,c("season","date","game.gamePk","isHome","inningLabel","team.name","team.id","opponent.name","opponent.id",
                                                "atBatIndex","result.event","result.eventType","result.description","result.rbi",
                                                "result.awayScore","result.homeScore","about.inning","about.halfInning","count.outs","matchup.batter.id",
                                                "matchup.batter.fullName","matchup.splits.menOnBase","advBase","matchup.postOnFirst.fullName","matchup.postOnSecond.fullName","matchup.postOnThird.fullName")]
    game_detail_tbllist[[gidx]] <- game_detail_simple
    Sys.sleep(3)
  }
  
  game_detail_new <- rbindlist(game_detail_tbllist[1:length(game_detail_tbllist)],fill=TRUE)
  game_detail_new <- game_detail_new[game_detail_new$inningLabel==game_detail_new$about.halfInning,]
  game_detail_new <- merge(game_detail_new,result_event_lookup,by="result.event")
  game_detail_new <- game_detail_new[,c("season","date","game.gamePk","isHome","inningLabel","team.name","team.id","opponent.name","opponent.id",
                                        "atBatIndex","result.event","result.eventType","result.description","result.rbi",
                                        "result.awayScore","result.homeScore","about.inning","about.halfInning","count.outs","matchup.batter.id",
                                        "matchup.batter.fullName","matchup.splits.menOnBase","advBase","matchup.postOnFirst.fullName","matchup.postOnSecond.fullName","matchup.postOnThird.fullName",
                                        "is.atBat","is.hit")]
  game_detail_new$risp.calc <- ifelse((game_detail_new$advBase!="NONE"|!is.na(game_detail_new$matchup.postOnSecond.fullName)|!is.na(game_detail_new$matchup.postOnThird.fullName)),"yes","no")
  game_detail_new <- game_detail_new[order(game_detail_new$date,game_detail_new$game.gamePk,game_detail_new$atBatIndex),]
  game_detail_new$season <- as.integer(game_detail_new$season)
  
  game_detail_full <- bind_rows(game_detail_full_df0,game_detail_new)
  
  # With the full data.frame assembled we now run create a BA with RISP game log
  
  game_log_risp_detail <- game_detail_full[game_detail_full$risp.calc=="yes",]
  game_log_risp <-
    game_log_risp_detail %>%
    group_by(season,date,game.gamePk,team.name,team.id,opponent.name,opponent.id,isHome,inningLabel,) %>%
    summarise(atBat = sum(is.atBat=="yes"),
              hit = sum(is.hit=="yes"),
              BA_w_RISP = round(hit/atBat,3))
  game_log_risp$date <- as.Date(game_log_risp$date)
    
  ##  WE NOW HAVE ALL DATA.FRAMES AND STATISTICS CALCULATED
  ##  THE FOLLOWING IS FOR FORMATTING OF DATA.FRAMES FOR EASE OF CHARTING AND ACTUAL CHARTING 

  baseball_chart_file <- 0

  hitting_max_date <- max(hitting_std$date)
  hitting_min_date <- min(hitting_std$date)
  hitting_team_value <- round(hitting_all[hitting_all$team.id==id,"stat.ops"],3)
  hitting_team_label <- paste0(hitting_all[hitting_all$team.id==id,"team.name"]," Season Avg: ",sprintf("%.3f",hitting_team_value))
  hitting_max_value <- round(unique(hitting_all_max$stat.ops),3)
  hitting_max_label <- paste0("Best Season Avg: ",paste0(hitting_all_max$team.name,collapse=",")," [",sprintf("%.3f",hitting_max_value),"]")
  hitting_min_value <- round(unique(hitting_all_min$stat.ops),3)
  hitting_min_label <- paste0("Worst Season Avg: ",paste0(hitting_all_min$team.name,collapse=",")," [",sprintf("%.3f",hitting_min_value),"]")
  hitting_trend <- 
    hitting_std %>% ggplot() + 
    scale_x_date(date_breaks = "1 week",date_labels = "%b %d") +
    geom_line(aes(x=date,y=stat.ops),linewidth=1.25,alpha=0.2,color="#6F263D",show.legend = FALSE) +
    geom_point(aes(x=date,y=stat.ops),color="#6F263D",alpha=0.2,show.legend = FALSE) +
    stat_smooth(aes(x=date,y=stat.ops),color="#6F263D",method = "loess",formula=y~x,size=0.5) +
    geom_hline(yintercept=hitting_team_value,linetype="dashed",col="#6F263D") +
    geom_text_repel(aes(x=hitting_max_date,y=hitting_team_value,label=hitting_team_label),data=hitting_all_max[1,],size=3.25,hjust=1,vjust=0,fontface="bold",col="#6F263D") +
    geom_hline(yintercept=hitting_max_value,linetype="dashed",col="gray30") +
    geom_text_repel(aes(x=hitting_max_date,y=hitting_max_value,label=hitting_max_label),data=hitting_all_max[1,],size=3.25,hjust=1,vjust=1,fontface="bold",col="gray30") +
    geom_hline(yintercept=hitting_min_value,linetype="dashed",col="gray30") +
    geom_text_repel(aes(x=hitting_max_date,y=hitting_min_value,label=hitting_min_label),data=hitting_all_max[1,],size=3.25,hjust=1,vjust=0,fontface="bold",col="gray30") +
    geom_hline(yintercept=hitting_all_avg,linetype="dashed",col="gray30") +
    geom_text_repel(aes(x=hitting_min_date,y=hitting_all_avg,label=paste0("League Avg: ",sprintf("%.3f",hitting_all_avg))),data=hitting_all_max[1,],size=3.25,hjust=0,vjust=0.5,fontface="bold",col="gray30") +
    scale_y_continuous(breaks = pretty(hitting_std$stat.ops,n=10)) +
    labs(x="\nDate",tag="Source: https://statsapi.mlb.com") +
    ylab(paste0("Hitting: On-Base Plus Slugging (OPS)\n")) +
    labs(title=paste0("[Hitting] On-Base Plus Slugging (OPS): ",format(hitting_max_date,"%b %d, %Y"),"\n(Higher is better)\n")) +
    theme(plot.title = element_text(hjust = 0.5,face="bold",size="12"),
          strip.text.x = element_text(face="bold",size="10"),
          axis.text.x = element_text(angle=90,size="12"),
          axis.text.y = element_text(size="12"),
          axis.title.y = element_text(face="bold",size="12"),
          axis.title = element_text(face="bold",size="12"),
          plot.tag = element_text(hjust=1,vjust=0,face="bold",size="8"),
          plot.tag.position = c(1,0),
          panel.background = element_rect(fill='white',color="black"),
          panel.grid.major = element_line(color = "gray90"))
  baseball_chart_file[1] <- paste0(img_dir,"hitting_",hitting_max_date,".png")
  ggsave(baseball_chart_file[1],width=8,height=6)
  
  pitching_max_date <- max(pitching_std$date)
  pitching_min_date <- min(pitching_std$date)
  pitching_team_value <- round(pitching_all[pitching_all$team.id==id,"stat.whip"],3)
  pitching_team_label <- paste0(pitching_all[pitching_all$team.id==id,"team.name"]," Season Avg: ",sprintf("%.3f",pitching_team_value))
  pitching_max_value <- round(unique(pitching_all_max$stat.whip),3)
  pitching_max_label <- paste0("Worst Season Avg: ",paste0(pitching_all_max$team.name,collapse=",")," [",sprintf("%.3f",pitching_max_value),"]")
  pitching_min_value <- round(unique(pitching_all_min$stat.whip),3)
  pitching_min_label <- paste0("Best Season Avg: ",paste0(pitching_all_min$team.name,collapse=",")," [",sprintf("%.3f",pitching_min_value),"]")
  pitching_trend <- 
    pitching_std %>% ggplot() + 
    scale_x_date(date_breaks = "1 week",date_labels = "%b %d") +
    geom_line(aes(x=date,y=stat.whip),linewidth=1.25,alpha=0.2,color="#6F263D",show.legend = FALSE) +
    geom_point(aes(x=date,y=stat.whip),color="#6F263D",alpha=0.2,show.legend = FALSE) +
    stat_smooth(aes(x=date,y=stat.whip),color="#6F263D",method = "loess",formula=y~x,size=0.5) +
    geom_hline(yintercept=pitching_team_value,linetype="dashed",col="#6F263D") +
    geom_text_repel(aes(x=pitching_max_date,y=pitching_team_value,label=pitching_team_label),data=pitching_all_max[1,],size=3.25,hjust=1,vjust=1,fontface="bold",col="#6F263D") +
    geom_hline(yintercept=pitching_max_value,linetype="dashed",col="gray30") +
    geom_text_repel(aes(x=pitching_max_date,y=pitching_max_value,label=pitching_max_label),data=pitching_all_max[1,],size=3.25,hjust=1,vjust=1,fontface="bold",col="gray30") +
    geom_hline(yintercept=pitching_min_value,linetype="dashed",col="gray30") +
    geom_text_repel(aes(x=pitching_max_date,y=pitching_min_value,label=pitching_min_label),data=pitching_all_max[1,],size=3.25,hjust=1,vjust=0,fontface="bold",col="gray30") +
    geom_hline(yintercept=pitching_all_avg,linetype="dashed",col="gray30") +
    geom_text_repel(aes(x=pitching_min_date,y=pitching_all_avg,label=paste0("League Avg: ",sprintf("%.3f",pitching_all_avg))),data=pitching_all_max[1,],size=3.25,hjust=0,vjust=1,fontface="bold",col="gray30") +
    scale_y_continuous(breaks = pretty(pitching_std$stat.whip,n=10)) +
    labs(x="\nDate",tag="Source: https://statsapi.mlb.com") +
    ylab(paste0("Pitching: Walks and Hits Per Inning Pitched (WHIP)\n")) +
    labs(title=paste0("[Pitching] Walks and Hits Per Inning Pitched (WHIP): ",format(pitching_max_date,"%b %d, %Y"),"\n(Lower is better)\n")) +
    theme(plot.title = element_text(hjust = 0.5,face="bold",size="12"),
          strip.text.x = element_text(face="bold",size="10"),
          axis.text.x = element_text(angle=90,size="12"),
          axis.text.y = element_text(size="12"),
          axis.title.y = element_text(face="bold",size="12"),
          axis.title = element_text(face="bold",size="12"),
          plot.tag = element_text(hjust=1,vjust=0,face="bold",size="8"),
          plot.tag.position = c(1,0),
          panel.background = element_rect(fill='white',color="black"),
          panel.grid.major = element_line(color = "gray90"))
  baseball_chart_file[2] <- paste0(img_dir,"pitching_",pitching_max_date,".png")
  ggsave(baseball_chart_file[2],width=8,height=6)
  
  fielding_max_date <- max(fielding_std$date)
  fielding_min_date <- min(fielding_std$date)
  fielding_team_value <- round(fielding_all[fielding_all$team.id==id,"stat.rangeFactorPer9Inn"],2)
  fielding_team_label <- paste0(fielding_all[fielding_all$team.id==id,"team.name"]," Season Avg: ",sprintf("%.2f",fielding_team_value))
  fielding_max_value <- round(unique(fielding_all_max$stat.rangeFactorPer9Inn),2)
  fielding_max_label <- paste0("Best Season Avg: ",paste0(fielding_all_max$team.name,collapse=",")," [",sprintf("%.2f",fielding_max_value),"]")
  fielding_min_value <- round(unique(fielding_all_min$stat.rangeFactorPer9Inn),2)
  fielding_min_label <- paste0("Worst Season Avg: ",paste0(fielding_all_min$team.name,collapse=",")," [",sprintf("%.2f",fielding_min_value),"]")
  fielding_trend <- 
    fielding_std %>% ggplot() + 
    scale_x_date(date_breaks = "1 week",date_labels = "%b %d") +
    geom_line(aes(x=date,y=stat.rangeFactorPer9Inn),linewidth=1.25,alpha=0.2,color="#6F263D",show.legend = FALSE) +
    geom_point(aes(x=date,y=stat.rangeFactorPer9Inn),color="#6F263D",alpha=0.2,show.legend = FALSE) +
    stat_smooth(aes(x=date,y=stat.rangeFactorPer9Inn),color="#6F263D",method = "loess",formula=y~x,size=0.5) +
    geom_hline(yintercept=fielding_team_value,linetype="dashed",col="#6F263D") +
    geom_text_repel(aes(x=fielding_max_date,y=fielding_team_value,label=fielding_team_label),data=fielding_all_max[1,],size=3.25,hjust=1,vjust=1,fontface="bold",col="#6F263D") +
    geom_hline(yintercept=fielding_max_value,linetype="dashed",col="gray30") +
    geom_text_repel(aes(x=fielding_max_date,y=fielding_max_value,label=fielding_max_label),data=fielding_all_max[1,],size=3.25,hjust=1,vjust=1,fontface="bold",col="gray30") +
    geom_hline(yintercept=fielding_min_value,linetype="dashed",col="gray30") +
    geom_text_repel(aes(x=fielding_max_date,y=fielding_min_value,label=fielding_min_label),data=fielding_all_max[1,],size=3.25,hjust=1,vjust=0,fontface="bold",col="gray30") +
    geom_hline(yintercept=fielding_all_avg,linetype="dashed",col="gray30") +
    geom_text_repel(aes(x=fielding_min_date,y=fielding_all_avg,label=paste0("League Avg: ",sprintf("%.3f",fielding_all_avg))),data=fielding_all_max[1,],size=3.25,hjust=0,vjust=0,fontface="bold",col="gray30") +
    scale_y_continuous(breaks = pretty(fielding_std$stat.rangeFactorPer9Inn,n=10)) +
    labs(x="\nDate",tag="Source: https://statsapi.mlb.com") +
    ylab(paste0("Fielding: Range Factor Per 9 Innings\n")) +
    labs(title=paste0("[Fielding] Range Factor Per 9 Innings: ",format(fielding_max_date,"%b %d, %Y"),"\n(Higher is better)\n")) +
    theme(plot.title = element_text(hjust = 0.5,face="bold",size="12"),
          strip.text.x = element_text(face="bold",size="10"),
          axis.text.x = element_text(angle=90,size="12"),
          axis.text.y = element_text(size="12"),
          axis.title.y = element_text(face="bold",size="12"),
          axis.title = element_text(face="bold",size="12"),
          plot.tag = element_text(hjust=1,vjust=0,face="bold",size="8"),
          plot.tag.position = c(1,0),
          panel.background = element_rect(fill='white',color="black"),
          panel.grid.major = element_line(color = "gray90"))
  baseball_chart_file[3] <- paste0(img_dir,"fielding_",fielding_max_date,".png")
  ggsave(baseball_chart_file[3],width=8,height=6)
  
  game_log_risp$qualified <- ifelse(game_log_risp$atBat>=3,"yes","no")
  barisp_max_date <- max(game_log_risp$date)
  barisp_min_date <- min(game_log_risp$date)
  barisp_team_value <- round(hitting_risp_all[hitting_risp_all$team.id==id,"stat.avg"],3)
  barisp_team_label <- paste0(hitting_risp_all[hitting_risp_all$team.id==id,"team.name"]," Season Avg: ",sprintf("%.3f",barisp_team_value))
  barisp_max_value <- round(unique(hitting_risp_all_max$stat.avg),3)
  barisp_max_label <- paste0("Best Season Avg: ",paste0(hitting_risp_all_max$team.name,collapse=",")," [",sprintf("%.3f",barisp_max_value),"]")
  barisp_min_value <- round(unique(hitting_risp_all_min$stat.avg),3)
  barisp_min_label <- paste0("Worst Season Avg: ",paste0(hitting_risp_all_min$team.name,collapse=",")," [",sprintf("%.3f",barisp_min_value),"]")
  barisp_trend <- 
    game_log_risp %>% ggplot() + 
    scale_x_date(date_breaks = "1 week",date_labels = "%b %d") +
    geom_line(aes(x=date,y=BA_w_RISP),linewidth=1.25,alpha=0.2,color="#6F263D",show.legend = FALSE) +
    geom_point(aes(x=date,y=BA_w_RISP,color=qualified),alpha=0.2,show.legend = FALSE) +
    stat_smooth(aes(x=date,y=BA_w_RISP),color="#6F263D",method = "loess",formula=y~x,size=0.5) +
    scale_color_manual(breaks = c("yes", "no"),values=c("#6F263D", "gray80")) +
    geom_hline(yintercept=barisp_team_value,linetype="dashed",col="#6F263D") +
    geom_text_repel(aes(x=barisp_max_date,y=barisp_team_value,label=barisp_team_label),data=hitting_risp_all_max[1,],size=3.25,hjust=1,vjust=0,fontface="bold",col="#6F263D") +
    geom_hline(yintercept=barisp_max_value,linetype="dashed",col="gray30") +
    geom_text_repel(aes(x=barisp_max_date,y=barisp_max_value,label=barisp_max_label),data=hitting_risp_all_max[1,],size=3.25,hjust=1,vjust=1,fontface="bold",col="gray30") +
    geom_hline(yintercept=barisp_min_value,linetype="dashed",col="gray30") +
    geom_text_repel(aes(x=barisp_max_date,y=barisp_min_value,label=barisp_min_label),data=hitting_risp_all_max[1,],size=3.25,hjust=1,vjust=0,fontface="bold",col="gray30") +
    geom_hline(yintercept=hitting_risp_all_avg,linetype="dashed",col="gray30") +
    geom_text_repel(aes(x=barisp_min_date,y=hitting_risp_all_avg,label=paste0("League Avg: ",sprintf("%.3f",hitting_risp_all_avg))),data=fielding_all_max[1,],size=3.25,hjust=0,vjust=0,fontface="bold",col="gray30") +
    coord_cartesian(ylim = c(0,0.4)) +
    labs(x="\nDate",tag="Source: https://statsapi.mlb.com") +
    ylab(paste0("Hitting: Batting Avg. w/ Runners in Scoring Position\n")) +
    labs(title=paste0("**BETA**\n[Hitting] Batting Avg. w./ Runners in Scoring Position: ",format(barisp_max_date,"%b %d, %Y"),"\nNote: Games with less than 3 qualifying at-bats in gray\n(Higher is better)\n")) +
    theme(plot.title = element_text(hjust = 0.5,face="bold",size="12"),
          strip.text.x = element_text(face="bold",size="10"),
          axis.text.x = element_text(angle=90,size="12"),
          axis.text.y = element_text(size="12"),
          axis.title.y = element_text(face="bold",size="12"),
          axis.title = element_text(face="bold",size="12"),
          plot.tag = element_text(hjust=1,vjust=0,face="bold",size="8"),
          plot.tag.position = c(1,0),
          panel.background = element_rect(fill='white',color="black"),
          panel.grid.major = element_line(color = "gray90"))
  baseball_chart_file[4] <- paste0(img_dir,"barisp_",barisp_max_date,".png")
  ggsave(baseball_chart_file[4],width=8,height=6)
  
  ##  WE NOW POST TO BSKY WITH TEXT AND ATTACHMENTS OF CHARTS
  
  bstext <- paste0("Philadelphia Phillies Daily Stat Charts\nMost recent game date: ",hitting_max_date,"\n\nNote: Batting Avg. w/ RISP is beta and manually calculated as it does not exist by-game from MLB")
  
  bs_post(
    text = bstext, 
    images = c(baseball_chart_file[1],baseball_chart_file[4],baseball_chart_file[2],baseball_chart_file[3]), 
    images_alt = c('Hitting: OPS by-game trend','Batting Avg. w/ RISP by-game trend','Pitching: WHIP by-game trend','Fielding: Range Factor Per 9 Innings by-game trend')
  )
  
  ##  WE NOW WRITE OUT DATA FILES, INCLUDING BACKUPS OF ORIGINALS
  
  write.csv(game_schedule_df0,file=game_schedule_file_bkup,row.names=FALSE)
  write.csv(game_meta,file=game_schedule_file,row.names=FALSE)
  
  write.csv(game_detail_full_df0,file=game_detail_full_file_bkup,row.names=FALSE)
  write.csv(game_detail_full,file=game_detail_full_file,row.names=FALSE)
  
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

