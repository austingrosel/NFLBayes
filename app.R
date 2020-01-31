library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("NFL QB Comparisons"),
    
    selectInput('select_player', 'Player:', choices = qbs %>% 
                    pull(full_player_name), multiple = T),
    
    plotOutput('plt')
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
                   prior = dnorm(x, p_y_m, p_y_s),
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
            geom_line(aes(x, prior), color = 'black', lty = 2) +
            scale_color_identity(aesthetics = "color",) +
            facet_wrap(~ type, scales = "free") +
            theme_light() +
            theme(
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                legend.position="bottom"
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
