library(shiny)
library(DT)
library(scales)
library(JLutils)

ui <- fluidPage(theme = shinythemes::shinytheme("flatly"),
    titlePanel("NFL QB Bayesian Stat Comparisons"),
    tabsetPanel(
      tabPanel("Tool",
               helpText("Note: QBs below are grouped by the season they recorded their first offensive stat.
                        Data is from NFLScrapR that goes back to 2009."),
               fluidRow(
                 column(3,
                        selectInput("select_player", "Player:", 
                                    choices = c(""), multiple = T)
                 )
               ),
               plotOutput('plt'),
               DTOutput('tbl'),
               helpText(HTML("Select two rows in the table and click the button to compare two players against each other.
                        Note: <b>Rows are selected when the row turns light blue</b>, and only two rows can be selected for the plot below!
                        ")),
               fluidRow(
                 column(12, align = 'center',
                        actionButton("enter_button", "Compare Stats")
                 )
               ),
               br(),
               fluidRow(
                 column(2, align = 'center',
                        uiOutput('player1_text'),
                        br(),
                        fluidRow(
                          uiOutput('player1_img')
                        )
                 ),
                 column(8, align = 'center',
                        plotOutput('comparison_plt')
                 ),
                 column(2, align = 'center',
                        uiOutput('player2_text'),
                        br(),
                        fluidRow(
                          uiOutput('player2_img')
                        )
                 )
               )
      ),
      tabPanel("How this works",
               br(),
               uiOutput("how_this_works_text1"),
               img(src="graph.png"),
               uiOutput("how_this_works_text2"),
               img(src='comp.png'),
               br(),
               uiOutput("how_this_works_text3"),
               br()
      ),
      tabPanel("Acknowledgments",
               br(),
               uiOutput('ack_text')
      )
    )
)

server <- function(input, output, session) {
    
    observe({
      qb_list = qbs %>% 
        arrange(full_player_name) %>%
        split(., .[,'est_draft_year']) %>% 
        lapply(., function(x) x %>% pull('full_player_name'))
      qb_list = qb_list[order(names(qb_list), decreasing = T)]
      updateSelectInput(session,
                        "select_player", 
                        "Player:",
                        choices = qb_list)
    })

    data = reactive({
        validate(
            need(length(input$select_player) > 0, 'Select a player.')
        )
        
        selected_qb_df <- qbs %>%
            filter(full_player_name %in% input$select_player)
        
        bayes_df <- selected_qb_df %>%
            tidyr::crossing(x = seq(1, 10, 0.02)) %>%
            ungroup() %>%
            mutate(density = dnorm(x, eb_ypa, eb_ypa_sd),
                   prior = dnorm(x, prior_ypa_mu, prior_ypa_sd),
                   type = 'ypa'
            ) %>%
            bind_rows(
                selected_qb_df %>%
                    tidyr::crossing(x = seq(0, 0.15, 0.00002)) %>%
                    ungroup() %>%
                    mutate(density = dbeta(x, int_alpha1, int_beta1),
                           prior = dbeta(x, int_alpha0, int_beta0),
                           type = 'int.rate')
            ) %>%
            bind_rows(
                selected_qb_df %>%
                    tidyr::crossing(x = seq(0, 0.15, 0.00002)) %>%
                    ungroup() %>%
                    mutate(density = dbeta(x, sack_alpha1, sack_beta1),
                           prior = dbeta(x, sack_alpha0, sack_beta0),
                           type = 'sack.rate')
            ) %>%
            bind_rows(
                selected_qb_df %>%
                    tidyr::crossing(x = seq(1, 10, 0.02)) %>%
                    ungroup() %>%
                    mutate(density = dnorm(x, eb_ypc, eb_ypc_sd),
                           prior = dnorm(x, prior_rush_mu, prior_rush_sd),
                           type = 'ypc')
            )
    })
    
    output$plt <- renderPlot({
        tableProxy %>% selectRows(NULL)
        asdf = data()
        asdf$type_f = factor(asdf$type, levels = c("ypa", "sack.rate", "ypc", "int.rate" ))
        ggplot(asdf, aes(group = full_player_name)) +
            geom_line(aes(x, density, color = team_color), size = 1.5, alpha = 0.9) +
            geom_ribbon(aes(x, ymin = 0, ymax = density, fill = team_color), alpha = 0.3) +
            geom_line(aes(x, prior), color = 'black', lty = 2) +
            scale_color_identity(aesthetics = c("color", "fill")) +
            facet_wrap(~ type_f, scales = "free") +
            theme_light() +
            xlab("Stat Value") +
            theme(
                strip.text.x = element_text(size = 14),
                strip.background = element_rect(fill = 'black'),
                axis.text.x = element_text(size = 12),
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
        rename(Player=full_player_name, last_team=team, 
               est_ypa=eb_ypa, est_ypc=eb_ypc, est_sack.perc=est_sack.rate, est_int.perc=est_int.rate)
    })
    
    output$tbl = renderDT({
      cols_to_hide = which(grepl("color", colnames(plot_data_tbl())))
      DT::datatable(plot_data_tbl()
                    , options = list(paging = F, searching = F, columnDefs = list(list(visible=FALSE, targets=cols_to_hide)))
                    , selection = list(mode = 'multiple', selected = rV$sel)) %>% 
        formatStyle('team_color', target = 'row', backgroundColor = JS("value")) %>%
        formatStyle('sec_color', target = 'row', color = JS("value"), fontWeight = 'bold') %>%
        formatRound(c('est_ypa', 'est_ypc'), 2) %>%
        formatPercentage(c('est_sack.perc', 'est_int.perc'), 1)
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
      rV$selected_players = plot_data_tbl()[rV$sel,] %>% pull(Player)
    })
    
    comparison_data = reactive({
      validate(
        need(length(rV$selected_players[!is.na(rV$selected_players)]) == 2, 'Need to select two players and click the button to compare.')
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
                  1 - sim_beta(player1, player2, qbs, 'sack_alpha1', 'sack_beta1'), sim_beta(player1, player2, qbs, 'sack_alpha1', 'sack_beta1'))) %>% 
        dplyr::mutate(player = as.character(player),
                      value = value * 100) %>%
        dplyr::left_join(., qbs %>% dplyr::select(full_player_name, team_color, sec_color, team_logo), by = c("player"="full_player_name"))
      if(length(unique(plotting_df$team_color)) == 1) {
        player2_sec_color = unique(plotting_df$sec_color[plotting_df$player == player2])
        plotting_df$team_color[plotting_df$player == player2] = player2_sec_color
      }
      plotting_df$stat = factor(plotting_df$stat, levels = c("INT%", "SK%", "YPC", "YPA"))
      plotting_df
    })
    
    players_df = reactive({
      validate(
        need(length(rV$selected_players[!is.na(rV$selected_players)]) == 2, 'Need to select two players and click the button to compare.')
      )
      comparison_data() %>% distinct(player, .keep_all = T) %>% arrange(desc(team_color))
    })
    
    output$player1_text = renderUI({
      HTML(paste("<font color=\"", players_df()$team_color[1],"\"><b>", players_df()$player[1], "</b></font>"))
    })
    
    output$player1_img = renderUI({
      img(src=as.character(players_df()$team_logo[1]), align = "center", height = '100px', weight = '100px')
    })
    
    output$player2_text = renderText({
      HTML(paste("<font color=\"", players_df()$team_color[2],"\"><b>", players_df()$player[2], "</b></font>"))
    })
    
    output$player2_img = renderUI({
      img(src=as.character(players_df()$team_logo[2]), align = "center", height = '100px', weight = '100px')
    })
    
    output$comparison_plt = renderPlot({
      ggplot(comparison_data(), aes(x = stat, fill = team_color, weight = value)) +
        geom_bar(position="fill") +
        coord_flip() +
        scale_y_continuous(labels=percent) +
        scale_color_identity(aesthetics = "fill") +
        theme_minimal() +
        geom_label(stat = "fill_labels", color = 'white') +
        xlab("Stat") + ylab("Percent") +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12)
        )
    })
    
    output$how_this_works_text1 = renderText({  
      HTML(paste0("This tool was built to look at Bayesian concepts across commonly used quarterback statistics. We tend to evaluate these QB stats based on one number, usually the average. For example, a person may look at a boxscore after a season and see that QB A has a YPA (yards per attempt) of 7.8 and QB B has a YPA of 7.2; therefore, one may conclude QB A\'s \"true YPA\" is better than QB B. However, from a Bayesian stand point, we are missing a lot of information. Sample size, standard deviation, and prior information are also important to consider when looking at statistics.</br></br>
           Inspired by some of Kevin Cole (", a('@KevinColePFF', href = "https://twitter.com/KevinColePFF/"), ") and David Robinson\'s (", a('@drob', href = "https://twitter.com/drob/"), ") work, I decided to make a Shiny application that can view the posterior distributions of the most common quarterback stats we use nowadays: yards per attempt (YPA), yards per carry (YPC), sack rate (Sack.Perc/Sack.Rate), and interception rate (Int.Rate/Int.Perc). We can visualize how confident we are for every player\'s \"true\" value for each stat category. 
           The dotted line in the graph shows the distribution of the priors for every QB. For YPA and YPC, I used the normal distribution to calculate the priors and evenutally the posteriors; I used the beta distribution for Sack.Rate and Int.Rate.</br></br>"))
    })
    
    output$how_this_works_text2 = renderText({
      HTML(paste0("For each of these distributions, I assumed the priors were the same across all quarterbacks. If you read Kevin Cole\'s articles on Pro Football Focus (", a("https://www.pff.com/news/nfl-2018-draft-class-qbs-future", href="https://www.pff.com/news/nfl-2018-draft-class-qbs-future"),"), he uses different priors depending on where the QB was drafted. This is something that I may want to include in the future, but for now, I have assumed all quarterbacks come from the same distribution for each statistic.</br></br>
                  The table (and graph) shows the estimated \"true\" stat for each category based on the empirical priors (", a("http://varianceexplained.org/r/empirical_bayes_baseball/", href="http://varianceexplained.org/r/empirical_bayes_baseball/"),"), along with the quarterback's mean, standard deviation, and sample size for each statistic.</br></br>
                  Going back to the example above, another question we want to answer is how much better QB A is than QB B? Using Bayesian statistics, we can quantify this (", a("http://varianceexplained.org/r/bayesian_ab_baseball/", href="http://varianceexplained.org/r/bayesian_ab_baseball/"),"). 
                  When selecting two of the quarterbacks in the table and hitting the \"Compare Stats\" button, we can view how confident we are that QB A is better (or worse) than QB B in each statistcal category.
                  In the example below, we have selected Dwayne Haskins and Daniel Jones after their rookie seasons. We can see as of now, we're more confident that Daniel Jones's true stats are better than Dwayne Haskins except for YPA. 
                  Another way we can read this graph is to say there is a 57% chance that Dwayne Haskins has a better \"true\" YPA than Daniel Jones.</br></br>"))
    })
    
    output$how_this_works_text3 = renderText({
      HTML(paste0("One of the things I value in data analyses is the ability to make things reproducible. The code for this application can be found here: ", a('https://github.com/austingrosel/NFLBayes', href = "https://github.com/austingrosel/NFLBayes"), ". As of now, the application is not 100% reproducible because it assumes you have the same R and package versions as I do right now. Hopefully I\'ll be able to get a Dockerfile into the GitHub repository sooner than later.</br></br>
                  
                  <b>Next steps</b>:</br>
                  1. As I mentioned earlier, can we adjust each player\'s prior based on draft position and possibly college statistics? Does this help our framework?</br>
                  2. Can we use model more \"advanced\" statistics like EPA or QBR?</br></br>
                  
                  <b>Limitations</b>:</br>
                  1. My educational background comes from computer science, not statistics, and especially not Bayesian statistics. The things that I\'ve learned from Bayesian statistics I\'ve had to read and re-read over and over again. There is a significant possibility I\'ve messed some things up!</br>
                  2. I chose to evaluate QB rushing ability using YPC out of simplicity. I\'d presume rushing yards is probably a better indicator of a quarterback's rushing ability. However, I'd have to create a Poisson model to get rushing attempts, and I didn't want to get rid of the four panel graphs.
                  "))
    })
    
    output$ack_text = renderUI({
      HTML(paste0("
           <b>NFLScrapR Community</b>: ", a("https://github.com/maksimhorowitz/nflscrapR", href="https://github.com/maksimhorowitz/nflscrapR"), "; ", a("https://github.com/ryurko/nflscrapR-data", href="https://github.com/ryurko/nflscrapR-data")," </br>
           <b>Kevin Cole</b>: ", a("https://www.pff.com/news/nfl-2018-draft-class-qbs-future", href="https://www.pff.com/news/nfl-2018-draft-class-qbs-future"), "; ", a("https://predictivefootball.com/deshaun-watson-mitch-trubisky-and-the-importance-of-sample-size/", href="https://predictivefootball.com/deshaun-watson-mitch-trubisky-and-the-importance-of-sample-size/")," </br> 
           <b>Nan Xiao</b>: ", a("https://stephens999.github.io/fiveMinuteStats/shiny_normal_example.html", href="https://stephens999.github.io/fiveMinuteStats/shiny_normal_example.html")," </br>
           <b>David Robinson</b>: ", a("http://varianceexplained.org/r/empirical_bayes_baseball/", href="http://varianceexplained.org/r/empirical_bayes_baseball/"),"; ", a("http://varianceexplained.org/r/bayesian_ab_baseball/", href="http://varianceexplained.org/r/bayesian_ab_baseball/"),"; ", a("https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_R/2015-11-04-polarizing-technologies.Rmd", href="https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_R/2015-11-04-polarizing-technologies.Rmd"), " </br>
           <b>Lee Sharpe</b>: Twitter: ", a('@LeeSharpeNFL', href = "https://twitter.com/LeeSharpeNFL/"),"; Data for teams, colors and logs: ", a("https://github.com/leesharpe/nfldata", href="https://github.com/leesharpe/nfldata")
           ))
    })
    
}

shinyApp(ui = ui, server = server)
