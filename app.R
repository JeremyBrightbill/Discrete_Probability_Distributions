# Shiny app to show discrete probability distribution

# TO DO:
# Finish adding equations etc. on hypergeometric page
# Make sure all the slider settings are sensible

library(shiny)
library(tidyverse)

# Define UI -------------------------------------------------------

ui <- fluidPage(
  
  # Application title
  titlePanel("Discrete Probability Distributions"),
  
  # Separate tab for each distribution. Different sidebar and main panel within each. 
  tabsetPanel(
    
    tabPanel("Binomial", 
             
             sidebarPanel(
               
               radioButtons("type", 
                            "The probability of", 
                            choices = c("exactly" = "dbinom", 
                                        "less than or equal to" = "pbinom", 
                                        "more than" = "more")), 
               
               uiOutput("successes"), 
               
               sliderInput("n", 
                           HTML("in <i>n</i> independent trials"), 
                           min = 0, 
                           max = 100, 
                           value = 10, 
                           step = 1), 
               
               sliderInput("p", 
                           HTML("each with probability of success <i>p</i>"), 
                           min = 0, 
                           max = 1, 
                           value = 0.5), hr(), 
               
               HTML("<strong>Examples:</strong> Coin toss; rolling a given number on a die; employee turnover")
               
             ), # end sidebarPanel Binomial
             
             mainPanel(
               
               fluidRow(
                 
                 column(5, htmlOutput("equation")), 
                 
                 column(4, htmlOutput("equation_R_binom")), 
                 
                 column(2, h3(htmlOutput("output_prob")))
                 
               ),  # end fluidRow
               
               plotOutput("plot", height = "500px")
               
             ) # end mainPanel Binomial
             
    ), # end tabPanel "Binomial"
    
    tabPanel("Poisson", 
             
             sidebarPanel(
               
               radioButtons("type_pois", 
                            "The probability of", 
                            choices = c("exactly" = "exactly", 
                                        "less than or equal to" = "less_eq", 
                                        "more than" = "more")), 
               
               sliderInput("k",
                           HTML("<i>k</i> events occurring"),
                           min = 0,
                           max = 40,
                           value = 2), 
               
               HTML("in a given interval, if the average rate is"), br(), br(),
               
               sliderInput("lambda", # Would like to use MathJax
                           HTML("<b><i>lambda</i></b> per interval"), 
                           min = 1, 
                           max = 20, 
                           value = 4), hr(), 
               
               HTML("<strong>Examples:</strong> Calls received in a call center; cars passing on a highway")
               
             ), # end sidebarPanel "Poisson"
             
             mainPanel(
               
               fluidRow(
                 
                 column(5, htmlOutput("equation_pois")), 
                 
                 column(4, htmlOutput("equation_R_pois")), 
                 
                 column(2, h3(htmlOutput("prob_pois")))
                 
               ),  # end fluidRow
               
               plotOutput("plot_pois", height = "500px")
               
             ) # end mainPanel "Poisson"
             
    ), # end tabPanel "Poisson"
    
    tabPanel("Negative Binomial", 
             
             sidebarPanel(
               
               radioButtons("type_nb", 
                            "The probability of", 
                            choices = c("exactly" = "exactly", 
                                        "less than or equal to" = "less_eq", 
                                        "more than" = "more")), 
               
               sliderInput("x_nb",
                           HTML("<i>x</i> failures occurring"),
                           min = 0,
                           max = 50,
                           value = 2), 
               
               sliderInput("n_nb", 
                           HTML("before <i>n</i> successes"), 
                           min = 1, 
                           max = 50, 
                           value = 4), 
               
               HTML("in a series of independent trials, each with"), br(), br(),
               
               sliderInput("p_nb", 
                          HTML("probability of success <i>p</i>"), 
                          min = 0, 
                          max = 1, 
                          value = 0.5), 
              
              HTML("(<b>Note: </b>If <i>n</i> = 1 success, the distribution simplifies to <b><i>geometric</i></b>.)"), br(), hr(), 
               
              HTML("<strong>Examples: </strong>Coin tosses; business activity funnel (e.g. sales calls needed to get a meeting, meetings needed to close a deal")
              
             ), # end sidebar "Negative Binomial"
             
             mainPanel(
               
               fluidRow(
                 
                 column(5, htmlOutput("equation_nb")), 
                 
                 column(4, htmlOutput("equation_R_nb")), 
                 
                 column(2, h3(htmlOutput("prob_nb")))
                 
               ),  # end fluidRow
               
               plotOutput("plot_nb", height = "500px")
               
             ) # end mainPanel "Negative Binomial"
             
    ), # end tabPanel "Negative Binomial"
    
    tabPanel("Hypergeometric", 
             
             sidebarPanel(
               
               radioButtons("type_h", 
                            "The probability of getting", 
                            choices = c("exactly" = "exactly", 
                                        "less than or equal to" = "less_eq")), 
               
               uiOutput("options_x_h"), 
               
           #    HTML("from a population containing"), br(), br(), 
               
               sliderInput("M_h", 
                           HTML("from a population containing <i>M</i> objects of the desired class"), 
                           min = 1, 
                           max = 50, 
                           value = 20), 
               
               sliderInput("N_h", 
                           HTML("and <i>N</i> of the undesired class,"), 
                           min = 1, 
                           max = 50, 
                           value = 20), 
               
               uiOutput("options_k_h"), hr(), 
               
               HTML("<b>Examples:</b> Dealing from a deck of cards; drawing colored balls from a basket; sampling for opinion polls")
               
             ), 
             
             mainPanel(
               
               fluidRow(
                 
                 column(5, htmlOutput("equation_h")), 
                 
                 column(4, htmlOutput("equation_R_h")), 
                 
                 column(2, h3(htmlOutput("prob_h")))
                 
               ),  # end fluidRow
               
               plotOutput("plot_h", height = "500px")
               
             )
             
    )
    
  ) # end tabsetPanel
    
) # end fluidPage UI

# Server function ============================================

server <- function(input, output) {
  
  # Tab 1: Binomial ------------------------------------------
  
  output$successes <- renderUI({
    
    sliderInput("x",
                HTML("<i>x</i> successes"),
                min = 0,
                max = input$n,
                value = floor(input$n/3), 
                step = 1)
    
  })  
  
  output$equation <- renderUI({
    
    req(input$type)
    
    if (input$type == "dbinom") {
      
      withMathJax("$$
                  P(X=x) = \\binom{n}{x}p^{x}{(1 - p)}^{(n - x)}
                  $$")
      
    } else if (input$type == "pbinom") {
      
      withMathJax("$$
                  P(X\\le\\ x) = \\sum_{i=0}^{x}{\\binom{n}{i}p^{i}{(1 - p)}^{(n - i)}}
                  $$")
      
    } else {
      
      withMathJax("$$
P(X>x) = 1 - \\sum_{i=0}^{x}{\\binom{n}{i}p^{i}{(1 - p)}^{(n - i)}} 
                  $$")
      
    }
    
  })
  
  output$equation_R_binom <- renderText({
    
    if (input$type == "dbinom") {
      HTML("<h4>R syntax:</h4> dbinom(x, n, p)")
    } else if (input$type == "pbinom") {
      HTML("<h4>R syntax:</h4> pbinom(x, n, p)")
    } else {
      HTML("<h4>R syntax:</h4> pbinom(x, n, p, lower.tail = FALSE)")
    }
  })
  
  output$output_prob <- renderText({
    
    req(input$x)
    
    if (input$type == "dbinom") {
      paste("=", round(dbinom(input$x, input$n, input$p), 3)) 
    } else if (input$type == "pbinom") {
      paste("=", round(pbinom(input$x, input$n, input$p), 3))
    } else {
      paste("=", round(pbinom(input$x, input$n, input$p, lower.tail = FALSE), 3))
    }
    
  })
  
  output$plot <- renderPlot({
    
    req(input$x)
    
    table <- tibble(
      successes = 0:input$n, 
      prob = dbinom(successes, input$n, input$p), 
      dbinom = successes == input$x, 
      pbinom = successes <= input$x, 
      more = successes > input$x
    )
    
    if (input$type == "dbinom") {
      
      ggplot(table, aes(successes, prob, fill = dbinom)) + 
        geom_col(color = "gray40") + 
        scale_fill_manual(values = c("white", "darkblue"), 
                          guide = FALSE) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(), 
              panel.grid.minor.x = element_blank()) +
        labs(x = "Number of Successes", 
             y = "Probability")
      
    } else if (input$type == "pbinom") {
      
      ggplot(table, aes(successes, prob, fill = pbinom)) +
        geom_col(color = "gray40") + 
        scale_fill_manual(values = c("white", "darkblue"), 
                          guide = FALSE) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(), 
              panel.grid.minor.x = element_blank()) +
        labs(x = "Number of Successes", 
             y = "Probability")
      
    } else {
      
      ggplot(table, aes(successes, prob, fill = more)) +
        geom_col(color = "gray40") + 
        scale_fill_manual(values = c("white", "darkblue"), 
                          guide = FALSE) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(), 
              panel.grid.minor.x = element_blank()) +
        labs(x = "Number of Successes", 
             y = "Probability")
      
    }
    
  }) # End Tab 1: Binomial
  
  # Tab 2: Poisson --------------------------------------------------
  
  output$equation_pois <- renderUI({
    
    req(input$type_pois)
    
    if (input$type_pois == "exactly") {
      
      withMathJax("$$
                  P(X=k) = {e^{-\\lambda}\\lambda^{k}\\over k!}
                  $$")
      
    } else if (input$type_pois == "less_eq") {
      
      withMathJax("$$
                  P(X\\le\\ k) = \\sum_{i=0}^{k}{e^{-\\lambda}\\lambda^{k}\\over k!}
                  $$")
      
    } else {
      
      withMathJax("$$
                  P(X>k) = 1 -\\sum_{i=0}^{k}{e^{-\\lambda}\\lambda^{k}\\over k!}
                  $$")
      
    }
    
  })
  
  output$equation_R_pois <- renderUI({
    
    if (input$type_pois == "exactly") {
      HTML("<h4>R syntax:</h4> dpois(x, lambda)")
    } else if (input$type_pois == "less_eq") {
      HTML("<h4>R syntax:</h4> ppois(x, lambda)")
    } else {
      HTML("<h4>R syntax:</h4> ppois(x, lambda, lower.tail = FALSE)")
    }
  })
  
  output$prob_pois <- renderText({
    
    req(input$k)
    
    if (input$type_pois == "exactly") {
      paste("=", round(dpois(input$k, input$lambda), 3)) 
    } else if (input$type_pois == "less_eq") {
      paste("=", round(ppois(input$k, input$lambda), 3))
    } else if (input$type_pois == "more") {
      paste("=", round(ppois(input$k, input$lambda, lower.tail = FALSE), 3))
    }
    
  })
  
  output$plot_pois <- renderPlot({
    
    req(input$k)
    
    table_pois <- tibble(
      events = 0:(5*input$lambda), 
      prob = dpois(events, input$lambda), 
      exactly = events == input$k, 
      less_eq = events <= input$k, 
      more = events > input$k
    )
    
    if (input$type_pois == "exactly") {
      
      ggplot(table_pois, aes(events, prob, fill = exactly)) + 
        geom_col(color = "gray40") + 
        scale_fill_manual(values = c("white", "darkred"), 
                          guide = FALSE) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(), 
              panel.grid.minor.x = element_blank()) +
        labs(x = "Number of Events", 
             y = "Probability")
      
    } else if (input$type_pois == "less_eq") {
      
      ggplot(table_pois, aes(events, prob, fill = less_eq)) +
        geom_col(color = "gray40") + 
        scale_fill_manual(values = c("white", "darkred"), 
                          guide = FALSE) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(), 
              panel.grid.minor.x = element_blank()) +
        labs(x = "Number of Events", 
             y = "Probability")
      
    } else {
      
      ggplot(table_pois, aes(events, prob, fill = more)) +
        geom_col(color = "gray40") + 
        scale_fill_manual(values = c("white", "darkred"), 
                          guide = FALSE) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(), 
              panel.grid.minor.x = element_blank()) +
        labs(x = "Number of Events", 
             y = "Probability")
      
    }
    
  }) # end Tab 2: Poisson
  
  # Tab 3: Negative Binomial -------------------------------------------
  
  output$equation_nb <- renderUI({
    
    req(input$type_nb)
    
    if (input$type_nb == "exactly") {
      
      withMathJax("$$
                  P(X=x) = \\binom{x+n-1}{x}p^{n}{(1 - p)}^{(n - x)}
                  $$")
      
    } else if (input$type_nb == "less_eq") {
      
      withMathJax("$$
                  P(X\\le\\ x) = \\sum_{i=0}^{x}{\\binom{x+n-1}{x}p^{n}{(1 - p)}^{(n - x)}}
                  $$")
      
    } else {
      
      withMathJax("$$
                  P(X>x) = 1 -\\sum_{i=0}^{x}{\\binom{x+n-1}{x}p^{n}{(1 - p)}^{(n - x)}}
                  $$")
      
    }
    
  })
  
  output$equation_R_nb <- renderText({
    
    if (input$type_nb == "exactly") {
      HTML("<h4>R syntax:</h4> dnbinom(x, n, p)")
    } else if (input$type_nb == "less_eq") {
      HTML("<h4>R syntax:</h4> pnbinom(x, n, p)")
    } else {
      HTML("<h4>R syntax:</h4> pnbinom(x, n, p, lower.tail = FALSE)")
    }
  })
  
  output$prob_nb <- renderText({
    
    req(input$x_nb)
    
    if (input$type_nb == "exactly") {
      paste("=", round(dnbinom(input$x_nb, input$n_nb, input$p_nb), 3)) 
    } else if (input$type_nb == "less_eq") {
      paste("=", round(pnbinom(input$x_nb, input$n_nb, input$p_nb), 3))
    } else if (input$type_nb == "more") {
      paste("=", round(pnbinom(input$x_nb, input$n_nb, input$p_nb, lower.tail = FALSE), 3))
    }
    
  })
  
  output$plot_nb <- renderPlot({
    
    req(input$x_nb)
    
    table <- tibble(
      failures = 0:(5*input$n_nb), 
      prob = dnbinom(failures, input$n_nb, input$p_nb), 
      exactly = failures == input$x_nb, 
      less_eq = failures <= input$x_nb, 
      more = failures > input$x_nb
    )
    
    if (input$type_nb == "exactly") {
      
      ggplot(table, aes(failures, prob, fill = exactly)) + 
        geom_col(color = "gray40") + 
        scale_fill_manual(values = c("white", "purple4"), 
                          guide = FALSE) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(), 
              panel.grid.minor.x = element_blank()) +
        labs(x = "Number of Failures", 
             y = "Probability")
      
    } else if (input$type_nb == "less_eq") {
      
      ggplot(table, aes(failures, prob, fill = less_eq)) +
        geom_col(color = "gray40") + 
        scale_fill_manual(values = c("white", "purple4"), 
                          guide = FALSE) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(), 
              panel.grid.minor.x = element_blank()) +
        labs(x = "Number of Failures", 
             y = "Probability")
      
    } else {
      
      ggplot(table, aes(failures, prob, fill = more)) +
        geom_col(color = "gray40") + 
        scale_fill_manual(values = c("white", "purple4"), 
                          guide = FALSE) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(), 
              panel.grid.minor.x = element_blank()) +
        labs(x = "Number of Failures", 
             y = "Probability")
      
    }
    
  }) # End Tab 3: Negative Binomial
  
  # Tab 4: Hypergeometric --------------------------------------
  
  output$options_x_h <- renderUI({
    
    sliderInput("x_h",
                HTML("<i>x</i> objects of the desired class"),
                min = 0,
                max = input$M_h,
                value = 3, 
                step = 1)
    
  })  
  
  output$options_k_h <- renderUI({
    
    sliderInput("k_h",
                HTML("when sampling <i>k</i> objects from the population"),
                min = 1,
                max = input$M_h + input$N_h,
                value = floor((input$M_h + input$N_h)/4), 
                step = 1)
    
  })  
  
  output$plot_h <- renderPlot({
    
    req(input$x_h)
    req(input$k_h)
    
    table <- tibble(
      x = 0:input$k_h, 
      prob = dhyper(x, input$M_h, input$N_h, input$k_h), 
      exactly = x == input$x_h, 
      less_eq = x <= input$x_h
    )
    
    if (input$type_h == "exactly") {
      
      ggplot(table, aes(x, prob, fill = exactly)) + 
        geom_col(color = "gray40") + 
        scale_fill_manual(values = c("white", "darkgreen"), 
                          guide = FALSE) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(), 
              panel.grid.minor.x = element_blank()) +
        labs(x = "Number of Objects", 
             y = "Probability")
      
    } else if (input$type_h == "less_eq") {
      
      ggplot(table, aes(x, prob, fill = less_eq)) +
        geom_col(color = "gray40") + 
        scale_fill_manual(values = c("white", "darkgreen"), 
                          guide = FALSE) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(), 
              panel.grid.minor.x = element_blank()) +
        labs(x = "Number of Objects", 
             y = "Probability")
      
    } 
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

