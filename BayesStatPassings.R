library(readr)
library(plyr)
library(dplyr)
library(purrr)
library(broom)
library(ggplot2)
library(nflscrapR)

est_mean = function(prior_mu, prior_sigma, data_mu, data_sigma, n) {
  return(((prior_mu/prior_sigma^2) + ((n * data_mu)/data_sigma^2))/((1/prior_sigma^2) + (n/data_sigma^2)))
}

est_sd = function(prior_sigma, data_sigma, n) {
  return(sqrt(1/((1/prior_sigma^2) + (n/data_sigma^2))))
}

pbp_2009 = read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2009.csv")
pbp_2010 = read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2010.csv")
pbp_2011 = read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2011.csv")
pbp_2012 = read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2012.csv")
pbp_2013 = read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2013.csv")
pbp_2014 = read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2014.csv")
pbp_2015 = read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2015.csv")
pbp_2016 = read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2016.csv")
pbp_2017 = read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv")
pbp_2018 = read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
pbp_2019 = read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2019.csv")

pbp = rbind(pbp_2009,pbp_2010,pbp_2011,pbp_2012,pbp_2013,pbp_2014, pbp_2015, pbp_2016, pbp_2017, pbp_2018, pbp_2019)

rm(pbp_2009,pbp_2010,pbp_2011,pbp_2012,pbp_2013, pbp_2014, pbp_2015, pbp_2016, pbp_2017, pbp_2018, pbp_2019)

games = read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv")

pbp = pbp %>% 
  left_join(., games %>% select(game_id, season), by = c("game_id"))

passes = pbp %>%
  mutate(play_type = ifelse(play_type == 'no_play' & interception == 1, 'pass', play_type)) %>%
  filter(play_type == 'pass', two_point_attempt != 1) %>%
  group_by(passer_player_id) %>%
  mutate(attempt = ifelse(sack == 1, 0, 1),
         same_td = ifelse(td_team == posteam, 1, 0)
         ) %>%
  summarise(est_draft_year = min(season),
            dropbacks = n(),
            attempts = sum(attempt),
            pass_yards = sum(attempt * yards_gained, na.rm = T),
            pass_tds = sum(attempt * same_td * touchdown, na.rm = T),
            ints = sum(interception)
            ) %>%
  mutate(int.rate = ints/attempts) %>%
  left_join(., pbp %>%
              mutate(play_type = ifelse(play_type == 'no_play' & interception == 1, 'pass', play_type)) %>%
              filter(play_type == 'pass', two_point_attempt != 1, sack == 0) %>%
              group_by(passer_player_id) %>%
              summarise(ypa = mean(yards_gained, na.rm = T),
                        data_sigma = sd(yards_gained, na.rm = T)), by = c("passer_player_id"))

m = MASS::fitdistr(passes %>% filter(attempts > 100) %>% pull(ypa), 'normal')

passes = passes %>%
  filter(attempts > 10) %>%
  mutate(
    prior_ypa_mu = as.numeric(m$estimate[1]),
    prior_ypa_sd = as.numeric(m$estimate[2])
  ) %>%
  mutate(
    eb_ypa = est_mean(prior_ypa_mu, prior_ypa_sd, ypa, data_sigma, attempts),
    eb_ypa_sd = est_sd(prior_ypa_sd, data_sigma, attempts)
  )

m <- MASS::fitdistr(passes %>% filter(attempts > 100) %>% pull(int.rate), dbeta,
                    start = list(shape1 = 0.001, shape2 = 10), lower = 0.01)

passes = passes %>%
  mutate(int_alpha0 = as.numeric(m$estimate[1]),
         int_beta0 = as.numeric(m$estimate[2]),
         int_alpha1 = ints + int_alpha0,
         int_beta1 = attempts + int_alpha0 + int_beta0,
         est_int.rate = int_alpha1/int_beta1)

sacks = pbp %>%
  filter(play_type == 'pass', two_point_attempt != 1) %>%
  group_by(passer_player_id) %>%
  summarise(dropbacks = n(),
            sacks = sum(sack),
            sack_fum = sum(sack * fumble_lost, na.rm = T)) %>%
  mutate(sack.rate = sacks/dropbacks)

m <- MASS::fitdistr(sacks %>% filter(dropbacks > 100) %>% pull(sack.rate), dbeta,
                    start = list(shape1 = 0.001, shape2 = 10), lower = 0.01)

sacks = sacks %>%
  mutate(sack_alpha0 = as.numeric(m$estimate[1]),
         sack_beta0 = as.numeric(m$estimate[2]),
         sack_alpha1 = sacks + sack_alpha0,
         sack_beta1 = dropbacks + sack_alpha0 + sack_beta0,
         est_sack.rate = sack_alpha1/sack_beta1)

rushes = pbp %>%
  filter(play_type == 'run', two_point_attempt != 1, !is.na(rusher_player_id), !is.na(rusher_player_name)) %>%
  mutate(same_td = ifelse(td_team == posteam, 1, 0)) %>%
  group_by(rusher_player_id) %>%
  summarise(carries = n(),
            rush_yards = sum(yards_gained, na.rm = T),
            rush_tds = sum(same_td * touchdown, na.rm = T),
            rush_fum = sum(fumble_lost),
            rush_sd = sd(yards_gained, na.rm = T)
            ) %>%
  mutate(ypc = rush_yards/carries,
         rush.td.perc = rush_tds/carries,
         rush.fum.perc = rush_fum/carries
         )

m = MASS::fitdistr(rushes %>% filter(carries > 300) %>% pull(ypc), 'normal')

rushes = rushes %>%
  mutate(
    prior_rush_mu = as.numeric(m$estimate[1]),
    prior_rush_sd = as.numeric(m$estimate[2])
  ) %>%
  mutate(
    eb_ypc = est_mean(prior_rush_mu, prior_rush_sd, ypc, rush_sd, carries),
    eb_ypc_sd = est_sd(prior_rush_sd, rush_sd, carries)
  ) %>%
  filter(carries > 15)

qbs = passes %>%
  left_join(., rushes, by = c("passer_player_id"="rusher_player_id")) %>%
  filter(carries > 15) %>%
  left_join(., sacks %>% select(-dropbacks), by = c("passer_player_id")) %>%
  ungroup()

sim_df = qbs %>% ungroup() %>% dplyr::select(passer_player_id, dropbacks, eb_ypa, est_int.rate, est_sack.rate, eb_ypc) %>% filter(dropbacks > 50)
sim_matrix = as.data.frame(as.matrix(dist(sim_df[,c(3:6)])))
rownames(sim_matrix) = sim_df$passer_player_name
colnames(sim_matrix) = sim_df$passer_player_name

this_name = "L.Jackson"
rownames(sim_matrix[order(sim_matrix[rownames(sim_matrix) == this_name]),])[2:6]


team_colors = read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/teamcolors.csv") %>%
  mutate(color = ifelse(team == "JAX", color2, 
                        ifelse(team == "LAC", color2, color))) %>%
  mutate(color2 = ifelse(team == "BAL", color3,
                         ifelse(team == "NYJ", "#FFFFFF",
                                ifelse(team == "IND", '#FFFFFF',
                                       ifelse(team == "JAX", color3, 
                                              ifelse(team == "LAC", color3, color2))))))

rosters = purrr::map_dfr(2019:2009, teams = pbp %>% distinct(posteam) %>% pull(posteam), nflscrapR::get_season_rosters)

rosters_copy = rosters_copy %>% ungroup()
rosters = rosters_copy[!duplicated(rosters_copy[,c(7)]),] %>%
  select(gsis_id, full_player_name, team) %>%
  mutate(team = ifelse(team == "JAC", "JAX",
                       ifelse(team == "LA", "LAR", team))) %>%
  left_join(., team_colors %>% select(team, color, color2) %>% rename(team_color=color, sec_color=color2), by = "team")

qbs = qbs %>% left_join(., rosters, by = c("passer_player_id"="gsis_id"))


