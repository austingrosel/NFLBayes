library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(JLutils)

sim_norm = function(player1, player2, df, mu, sd) {
  set.seed(123)
  sim1 = rnorm(
    1e5,
    df %>% dplyr::filter(full_player_name == player1) %>% dplyr::pull(mu),
    df %>% dplyr::filter(full_player_name == player1) %>% dplyr::pull(sd)
  )
  sim2 = rnorm(
    1e5,
    df %>% dplyr::filter(full_player_name == player2) %>% dplyr::pull(mu),
    df %>% dplyr::filter(full_player_name == player2) %>% dplyr::pull(sd)
  )
  mean(sim1 > sim2)
}

sim_beta = function(player1, player2, df, alpha, beta) {
  set.seed(123)
  sim1 = rbeta(
    1e5,
    df %>% dplyr::filter(full_player_name == player1) %>% dplyr::pull(alpha),
    df %>% dplyr::filter(full_player_name == player1) %>% dplyr::pull(beta)
  )
  sim2 = rbeta(
    1e5,
    df %>% dplyr::filter(full_player_name == player2) %>% dplyr::pull(alpha),
    df %>% dplyr::filter(full_player_name == player2) %>% dplyr::pull(beta)
  )
  mean(sim1 > sim2)
}

player1 = "Lamar Jackson"
player2 = "Baker Mayfield"

plotting_df = data.frame(
  stat = c(rep("YPA", 2),
           rep("YPC", 2),
           rep("INT%",2),
           rep("SK%", 2)),
  player = c(rep(c(player1, player2), 4)),
  value = c(sim_norm(player1, player2, qbs, 'eb_ypa', 'eb_ypa_sd'), 1 - sim_norm(player1, player2, qbs, 'eb_ypa', 'eb_ypa_sd'),
            sim_norm(player1, player2, qbs, 'eb_ypc', 'eb_ypc_sd'), 1 - sim_norm(player1, player2, qbs, 'eb_ypc', 'eb_ypc_sd'),
            1 - sim_beta(player1, player2, qbs, 'int_alpha1', 'int_beta1'), sim_beta(player1, player2, qbs, 'int_alpha1', 'int_beta1'),
            1 - sim_beta(player1, player2, qbs, 'sack_alpha1', 'sack_beta1'), sim_beta(player1, player2, qbs, 'sack_alpha1', 'sack_beta1'))) %>% 
  dplyr::mutate(player = as.character(player),
         value = value * 100) %>%
  dplyr::left_join(., qbs %>% dplyr::select(full_player_name, team_color), by = c("player"="full_player_name"))

ggplot(plotting_df, aes(x = stat, fill = team_color, weight = value)) +
  geom_bar(position="fill") +
  coord_flip() +
  scale_y_continuous(labels=percent) +
  scale_color_identity(aesthetics = "fill") +
  theme_minimal() +
  geom_label(stat = "fill_labels", color = 'white')
