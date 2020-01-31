library(readr)
library(dplyr)
library(broom)
library(ggplot2)

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

pbp = rbind(pbp_2009,pbp_2010,pbp_2011,pbp_2012,pbp_2013, pbp_2014, pbp_2015, pbp_2016, pbp_2017, pbp_2018, pbp_2019) #%>%
  #filter(play_type == 'pass')

passes = pbp %>%
  mutate(passer_player_name = ifelse(passer_player_name == 'Jos.Allen', 'J.Allen',
                                         ifelse(passer_player_name == "R.Griffin III", "R.Griffin",
                                                ifelse(passer_player_name == "G.Minshew II", "G.Minshew", passer_player_name)))) %>%
  mutate(play_type = ifelse(play_type == 'no_play' & interception == 1, 'pass', play_type)) %>%
  filter(play_type == 'pass', two_point_attempt != 1) %>%
  group_by(passer_player_id, passer_player_name) %>%
  mutate(attempt = ifelse(sack == 1, 0, 1),
         same_td = ifelse(td_team == posteam, 1, 0)
         ) %>%
  summarise(dropbacks = n(),
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

passes %<>%
  filter(attempts > 10) %>%
  mutate(
    p_y_m = as.numeric(m$estimate[1]),
    p_y_s = as.numeric(m$estimate[2])
  ) %>%
  mutate(
    eb_ypa = est_mean(p_y_m, p_y_s, ypa, data_sigma, attempts),
    eb_ypa_sd = est_sd(p_y_s, data_sigma, attempts)
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
  left_join(., sacks %>% select(-dropbacks), by = c("passer_player_id"))


sim_df = qbs %>% ungroup() %>% dplyr::select(passer_player_name, dropbacks, eb_ypa, est_int.rate, est_sack.rate, eb_ypc) %>% filter(dropbacks > 50)
sim_matrix = as.data.frame(as.matrix(dist(sim_df[,c(3:6)])))
rownames(sim_matrix) = sim_df$passer_player_name
colnames(sim_matrix) = sim_df$passer_player_name

this_name = "L.Jackson"
rownames(sim_matrix[order(sim_matrix[rownames(sim_matrix) == this_name]),])[2:6]


yankee_1998 <- c("K.Murray", "D.Haskins", "D.Lock", "G.Minshew",
                 "B.Mayfield", "S.Darnold", "J.Allen", "J.Rosen", "L.Jackson")

yankee_1998_career <- qbs %>%
  filter(passer_player_name %in% yankee_1998)

yankee_beta <- yankee_1998_career %>%
  tidyr::crossing(x = seq(4, 10, 0.02)) %>%
  ungroup() %>%
  mutate(density = dnorm(x, eb_ypa, eb_ypa_sd),
         prior = dnorm(x, p_y_m, p_y_s),
         type = 'ypa'
  ) %>%
  bind_rows(
    yankee_1998_career %>%
      tidyr::crossing(x = seq(0, 0.1, 0.0002)) %>%
      ungroup() %>%
      mutate(density = dbeta(x, int_alpha1, int_beta1),
             prior = dbeta(x, int_alpha0, int_beta0),
             type = 'int.rate')
  ) %>%
  bind_rows(
    yankee_1998_career %>%
      tidyr::crossing(x = seq(0, 0.15, 0.0002)) %>%
      ungroup() %>%
      mutate(density = dbeta(x, sack_alpha1, sack_beta1),
             prior = dbeta(x, sack_alpha0, sack_beta0),
             type = 'sack.rate')
  ) %>%
  bind_rows(
    yankee_1998_career %>%
      tidyr::crossing(x = seq(1, 9, 0.02)) %>%
      ungroup() %>%
      mutate(density = dnorm(x, eb_ypc, eb_ypc_sd),
             prior = dnorm(x, prior_rush_mu, prior_rush_sd),
             type = 'ypc')
  )

ggplot(yankee_beta) +
  geom_line(aes(x, density, color = passer_player_name)) +
  geom_line(aes(x, prior), color = 'black', lty = 2) +
  facet_wrap(~ type, scales = "free") +
  theme_light() +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position="bottom"
  )

