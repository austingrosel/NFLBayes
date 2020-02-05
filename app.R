library(shiny)
library(DT)
library(scales)
library(JLutils)

ui <- fluidPage(
    titlePanel("NFL QB Comparisons"),
    
    fluidRow(
        column(3,
               selectInput("select_draft", "Draft Year:", 
                           choices = c("", qbs %>%
                                           filter(!is.na(est_draft_year)) %>%
                                           arrange(est_draft_year) %>% 
                                           pull(est_draft_year)), selected = "")
               ),
        column(3,
               selectInput('select_player', 'Player:', 
                           choices = qbs %>%
                               arrange(full_player_name) %>% 
                               pull(full_player_name), 
                           multiple = T)
               )
    ),
    
    plotOutput('plt'),
    DTOutput('tbl'),
    actionButton("enter_button", "Enter..."),
    plotOutput('comparison_plt')
)

server <- function(input, output, session) {
    
    observe({
        if(input$select_draft != "") {
            updateSelectInput(session, 
                              "select_player", 
                              "Player:",
                              choices = qbs %>%
                                  filter(est_draft_year == input$select_draft) %>%
                                  arrange(full_player_name) %>% 
                                  pull(full_player_name))
        }
    })

    data = reactive({
        validate(
            need(length(input$select_player) > 0, 'Select a player.')
        )
        
        selected_qb_df <- qbs %>%
            filter(full_player_name %in% input$select_player)
        
        bayes_df <- selected_qb_df %>%
            tidyr::crossing(x = seq(4, 10, 0.02)) %>%
            ungroup() %>%
            mutate(density = dnorm(x, eb_ypa, eb_ypa_sd),
                   prior = dnorm(x, prior_ypa_mu, prior_ypa_sd),
                   type = 'ypa'
            ) %>%
            bind_rows(
                selected_qb_df %>%
                    tidyr::crossing(x = seq(0, 0.1, 0.0002)) %>%
                    ungroup() %>%
                    mutate(density = dbeta(x, int_alpha1, int_beta1),
                           prior = dbeta(x, int_alpha0, int_beta0),
                           type = 'int.rate')
            ) %>%
            bind_rows(
                selected_qb_df %>%
                    tidyr::crossing(x = seq(0, 0.15, 0.0002)) %>%
                    ungroup() %>%
                    mutate(density = dbeta(x, sack_alpha1, sack_beta1),
                           prior = dbeta(x, sack_alpha0, sack_beta0),
                           type = 'sack.rate')
            ) %>%
            bind_rows(
                selected_qb_df %>%
                    tidyr::crossing(x = seq(1, 9, 0.02)) %>%
                    ungroup() %>%
                    mutate(density = dnorm(x, eb_ypc, eb_ypc_sd),
                           prior = dnorm(x, prior_rush_mu, prior_rush_sd),
                           type = 'ypc')
            )
    })
    
    output$plt <- renderPlot({
        asdf = data()
        ggplot(asdf, aes(group = full_player_name)) +
            geom_line(aes(x, density, color = team_color), size = 1.5, alpha = 0.9) +
            geom_ribbon(aes(x, ymin = 0, ymax = density, fill = team_color), alpha = 0.3) +
            geom_line(aes(x, prior), color = 'black', lty = 2) +
            scale_color_identity(aesthetics = c("color", "fill")) +
            facet_wrap(~ type, scales = "free") +
            theme_light() +
            theme(
                strip.text.x = element_text(size = 14),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                legend.position="bottom"
            )
    })
    
    rV <- reactiveValues(sel = "", selected_players = c())
    
    plot_data_tbl = reactive({
      validate(
        need(length(input$select_player) > 0, 'Select a player.')
      )
      
      df = qbs %>%
        filter(full_player_name %in% input$select_player) %>%
        select(full_player_name, team, attempts, eb_ypa, carries, eb_ypc, est_sack.rate, est_int.rate, team_color, sec_color) %>%
        rename(Player=full_player_name, last_team=team, est_ypa=eb_ypa, est_ypc=eb_ypc)
    })
    
    output$tbl = renderDT({
      DT::datatable(plot_data_tbl(), selection = list(mode = 'multiple', selected = rV$sel)) %>% 
        formatStyle('team_color', target = 'row', backgroundColor = JS("value")) %>%
        formatStyle('sec_color', target = 'row', color = JS("value"), fontWeight = 'bold') %>%
        formatRound(c('est_ypa', 'est_ypc'), 2) %>%
        formatRound(c('est_sack.rate', 'est_int.rate'), 3)
    })
    
    tableProxy <-  dataTableProxy('tbl')
    
    observeEvent(input$enter_button, {
      validate(
        need(!is.null(input$tbl_rows_selected), 'No rows selected.')
      )
      if (length(input$tbl_rows_selected) == 2) {
          rV$sel <- input$tbl_rows_selected
          rV$sel = rV$sel[!is.na(rV$sel)]
      }
      tableProxy %>% selectRows(NULL)
<<<<<<< HEAD
=======
      
>>>>>>> a5a2730c8444b25f1ca60e0fc724a3c2153d8cfe
      rV$selected_players = plot_data_tbl()[rV$sel,] %>% pull(Player)
    })
    
    output$comparison_plt = renderPlot({
      validate(
        need(length(rV$selected_players[!is.na(rV$selected_players)]) == 2, 'Need to select two players to compare.')
      )
      
      player1 = rV$selected_players[1]
      player2 = rV$selected_players[2]
      
      plotting_df = data.frame(
        stat = c(rep("YPA", 2),
                 rep("YPC", 2),
                 rep("INT%",2),
                 rep("SK%", 2)),
        player = c(rep(c(player1, player2), 4)),
        value = c(sim_norm(player1, player2, qbs, 'eb_ypa', 'eb_ypa_sd'), 1 - sim_norm(player1, player2, qbs, 'eb_ypa', 'eb_ypa_sd'),
                  sim_norm(player1, player2, qbs, 'eb_ypc', 'eb_ypc_sd'), 1 - sim_norm(player1, player2, qbs, 'eb_ypc', 'eb_ypc_sd'),
                  1 - sim_beta(player1, player2, qbs, 'int_alpha1', 'int_beta1'), sim_beta(player1, player2, qbs, 'int_alpha1', 'int_beta1'),
<<<<<<< HEAD
                  1 - sim_beta(player1, player2, qbs, 'sack_alpha1', 'sack_beta1'), sim_beta(player1, player2, qbs, 'sack_alpha1', 'sack_beta1'))) %>% 
          dplyr::mutate(player = as.character(player),
                        value = value * 100) %>%
          dplyr::left_join(., qbs %>% dplyr::select(full_player_name, team_color, sec_color), by = c("player"="full_player_name"))
      
      if(length(unique(plotting_df$team_color)) == 1) {
          player2_sec_color = unique(plotting_df$sec_color[plotting_df$player == player2])
          plotting_df$team_color[plotting_df$player == player2] = player2_sec_color
      }
      
      plotting_df$stat = factor(plotting_df$stat, levels = c("SK%", "INT%", "YPC", "YPA"))
=======
                  1 - sim_beta(player1, player2, qbs, 'sack_alpha1', 'sack_beta1'), sim_beta(player1, player2, qbs, 'sack_alpha1', 'sack_beta1'))
        ) %>% 
        dplyr::mutate(player = as.character(player),
                      value = value * 100) %>%
        dplyr::left_join(., qbs %>% dplyr::select(full_player_name, team_color, sec_color), by = c("player"="full_player_name"))
      
      if(length(unique(plotting_df$team_color)) == 1) {
        player2_sec_color = unique(plotting_df$sec_color[plotting_df$player == player2])
        plotting_df$team_color[plotting_df$player == player2] = player2_sec_color
      }
>>>>>>> a5a2730c8444b25f1ca60e0fc724a3c2153d8cfe
      
      ggplot(plotting_df, aes(x = stat, fill = team_color, weight = value)) +
        geom_bar(position="fill") +
        coord_flip() +
        scale_y_continuous(labels=percent) +
        scale_color_identity(aesthetics = "fill") +
        theme_minimal() +
        geom_label(stat = "fill_labels", color = 'white')
    })
    
}

shinyApp(ui = ui, server = server)
