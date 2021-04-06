library(nbastatR)
library(tidyverse)

player_game_stats_season <- function(player_name, season){

    pp <- game_logs(season)
    return(dplyr::filter(pp, namePlayer == player_name))

}


player_game_stats_multiple_seasons <- function(player_name, first_season, last_season){

    res <- c()

    for(i in first_season:last_season){

        res <- rbind(res, game_logs(i))

    }

    return(dplyr::filter(res, namePlayer == player_name))

}

team_game_stats_season <- function(team_name, season){

    pp <- game_logs(season, result_types = "team")
    return(dplyr::filter(pp, nameTeam == team_name))

}

team_averages_last_n_games_df <- function(team_stats_season, n){

    df <- dplyr::arrange(team_stats_season, dateGame)

    infocols <- dplyr::select(df, 1:9)
    numericcols <- dplyr::select(df, 23:29, 31:46)
    res <- c()

    for (i in 1:nrow(df)) {

        dfslice <- slice(df, 1:i)
        pp <- dplyr::select(dfslice, 23:29, 31:46)
        averages <- c()

        if(i <= n){
            for (c in 1:ncol(pp)) {
                averages <- c(averages, mean(unlist(pp[c])))
            }
        } else{
            dfslice2 <- dfslice %>% arrange(desc(dateGame)) %>% slice(1:10)
            pp <- dplyr::select(dfslice2, 23:29, 31:46)
            for (c in 1:ncol(pp)) {
                averages <- c(averages, mean(unlist(pp[c])))
            }
        }

        res <- rbind(res, averages)

    }

    res <- as.data.frame(res)
    colnames(res) <- colnames(numericcols)
    res <- cbind(infocols, res)

    return(res)

}

team_code_to_names <- function(codes){

    team_names <- character(length(codes))

    for(i in 1:length(codes)){
        team_names[i] <- dplyr::case_when(
            codes[i] == "ATL" ~ "Atlanta Hawks",
            codes[i] == "BKN" ~ "Brooklyn Nets",
            codes[i] == "BOS" ~ "Boston Celtics",
            codes[i] == "CHA" ~ "Charlotte Hornets",
            codes[i] == "CHI" ~ "Chicago Bulls",
            codes[i] == "CLE" ~ "Cleveland Cavaliers",
            codes[i] == "DAL" ~ "Dallas Mavericks",
            codes[i] == "DEN" ~ "Denver Nuggets",
            codes[i] == "DET" ~ "Detroit Pistons",
            codes[i] == "GSW" ~ "Golden State Warriors",
            codes[i] == "HOU" ~ "Houston Rockets",
            codes[i] == "IND" ~ "Indiana Pacers",
            codes[i] == "LAC" ~ "LA Clippers",
            codes[i] == "LAL" ~ "Los Angeles Lakers",
            codes[i] == "MEM" ~ "Memphis Grizzlies",
            codes[i] == "MIA" ~ "Miami Heat",
            codes[i] == "MIL" ~ "Milwaukee Bucks",
            codes[i] == "MIN" ~ "Minnesota Timberwolves",
            codes[i] == "NOP" ~ "New Orleans Pelicans",
            codes[i] == "NYK" ~ "New York Knicks",
            codes[i] == "OKC" ~ "Oklahoma City Thunder",
            codes[i] == "ORL" ~ "Orlando Magic",
            codes[i] == "PHI" ~ "Philadelphia 76ers",
            codes[i] == "PHX" ~ "Phoenix Suns",
            codes[i] == "POR" ~ "Portland Trail Blazers",
            codes[i] == "SAC" ~ "Sacramento Kings",
            codes[i] == "SAS" ~ "San Antonio Spurs",
            codes[i] == "TOR" ~ "Toronto Raptors",
            codes[i] == "UTA" ~ "Utah Jazz",
            codes[i] == "WAS" ~ "Washington Wizards"
        )
    }

    return(team_names)

}
