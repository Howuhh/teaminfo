library(dplyr)
library(stringr)
library(lubridate)
library(rjson)
library(purrr)
# library(tm)

#* @get /info
info = function() {
  list(status="I am working")
}


df = data.frame(id = c(1883502, 2163, 1375614, 36, 2586976, 15),
                name = c("Virtus.pro", "Team Liquid", "Newbee", "Na'Vi", "OG", "PSG.LGD"))

#* @get /GetTeamStruct
team_inf = function(team_name) {
  if (team_name %in% df$name) {
    url = "https://api.opendota.com/api/teams/"
    url_pl = "https://api.opendota.com/api/proPlayers"
    t_id = df %>% filter(name == team_name) %>% select(id) %>% as.numeric()
    req = str_c(url, t_id, "/players")
    json = fromJSON(file = req)
    roaster = data.frame()
    for (i in 1:length(json)) {
      if (is.null(json[[i]]$is_current_team_member) == FALSE) {
        a = as.data.frame(json[[i]])
      }
      roaster = rbind(roaster, a)
    }
    roaster_current = roaster %>% filter(roaster[5] == TRUE)
    roaster_current$winrate = round(roaster_current$wins / roaster_current$games_played, digits = 2)
    roaster_current = roaster_current %>% select(account_id, name, winrate)
    proplayers = data.frame()
    j = fromJSON(file = url_pl)
    for (i in 1:length(j)) {
      try({
        pur = as.data.frame(t(as.matrix(j[[i]])))
        players = select(pur, account_id, loccountrycode)
        proplayers = rbind(proplayers, players)
      })
    }
    proplayers$account_id = as.numeric(proplayers$account_id)
    proplayers$loccountrycode = as.character(proplayers$loccountrycode)
    proplayers$loccountrycode = ifelse(proplayers$loccountrycode == "NULL", "Unknown", proplayers$loccountrycode)
    roaster_current = left_join(roaster_current, proplayers, by = "account_id")
    colnames(roaster_current) = c("PlayerID", "PlayerName", "WinRate", "Country")
    list(roaster_current)
  } else {
    # не спрашивайте зачем
    list(list(list("PlayerID"="sry", "PlayerName"="no", "WinRate"="such", "Country"="team")))
  }
}

#Team data
x_team_struct = data.frame(
  team_id = c(33, 55, 66, 22, 88),
  player_1_id = c(11, 3, 84, 97, 88),
  player_2_id = c(23, 35, 45, 67, 56),
  player_3_id = c(2, 1, 5, 6, 7), 
  # player_4_id = c(65, 91, 92, 93, 94),
  # player_5_id = c(12, 13, 14, 15, 16),
  stringsAsFactors = FALSE
)

# Lookup a team members info
#* @get /example
get_team_info = function(t_id){
  # Expect incoming JSON with "letter" field in it
  team <- x_team_struct %>% filter(team_id == t_id)
  
  list(team)
}