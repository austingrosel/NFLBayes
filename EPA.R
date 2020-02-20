p = pbp %>%
  mutate(play_type = ifelse(play_type == 'no_play' & interception == 1, 'pass', play_type)) %>%
  filter(two_point_attempt != 1, qb_dropback == 1) %>%
  mutate(player_id = ifelse(qb_scramble == 1, rusher_player_id, passer_player_id)) %>%
  group_by(player_id) %>%
  summarise(dropbacks = n(),
            scrambles = sum(qb_scramble),
            epa_db = mean(epa, na.rm = T),
            epa_db_sd = sd(epa, na.rm = T))

m = MASS::fitdistr(p %>% filter(dropbacks > 100) %>% pull(epa_db), 'normal')
m

hist(rnorm(10000, m$estimate[1], m$estimate[2]))

p = p %>%
  mutate(
    prior_epa_db_mu = as.numeric(m$estimate[1]),
    prior_epa_db_sd = as.numeric(m$estimate[2]),
  ) %>%
  mutate(
    est_pass_epa = round(est_mean(prior_epa_db_mu, prior_epa_db_sd, epa_db, epa_db_sd, dropbacks), 3),
    est_pass_epa_sd = est_sd(prior_epa_db_sd, epa_db_sd, dropbacks)
  ) %>%
  filter(dropbacks > 15)

r = pbp %>%
  filter(play_type == 'run', two_point_attempt != 1, qb_scramble == 0, !is.na(rusher_player_id), !is.na(rusher_player_name)) %>%
  mutate(same_td = ifelse(td_team == posteam, 1, 0)) %>%
  group_by(rusher_player_id) %>%
  summarise(carries = n(),
            rush_yards = sum(yards_gained, na.rm = T),
            rush_tds = sum(same_td * touchdown, na.rm = T),
            rush_fum = sum(fumble_lost),
            rush_sd = sd(yards_gained, na.rm = T),
            rush_epa = sum(epa, na.rm = T),
            rush_epa_sd = sd(epa, na.rm = T)
  ) %>%
  mutate(ypc = rush_yards/carries,
         epa.rush = rush_epa/carries,
         rush.td.perc = rush_tds/carries,
         rush.fum.perc = rush_fum/carries
  ) %>%
  left_join(., rosters %>% select(gsis_id, position, full_player_name), by = c("rusher_player_id"="gsis_id")) %>% 
  filter(position == 'QB')

m = MASS::fitdistr(r %>% filter(carries > 30) %>% pull(epa.rush), 'normal')
m

hist(rnorm(10000, m$estimate[1], m$estimate[2]))

r = r %>%
  mutate(
    prior_rush_epa_mu = as.numeric(m$estimate[1]),
    prior_rush_epa_sd = as.numeric(m$estimate[2]),
  ) %>%
  mutate(
    est_rush_epa = round(est_mean(prior_rush_epa_mu, prior_rush_epa_sd, epa.rush, rush_epa_sd, carries), 3),
    est_rush_epa_sd = est_sd(prior_rush_epa_sd, rush_epa_sd, carries)
  )

q = p %>%
  left_join(., r, by = c("player_id"="rusher_player_id")) %>%
  filter(carries > 5) %>%
  select(full_player_name, est_pass_epa, est_rush_epa, everything()) %>%
  arrange(-est_pass_epa)
  
