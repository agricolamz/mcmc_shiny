# # generate mcmc for visualisation
# library(brms)
# get_prior(x~1, data = tibble(x = rnorm(100, 40, 30)))
# fit <- brm(x~1, data = tibble(x = rnorm(100, 50, 10)),
#            prior = prior(normal(0,10), class = Intercept) +
#              prior(cauchy(0,2), class = sigma))
# values <- fit[["fit"]]@sim[["samples"]][[1]][["b_Intercept"]]
# values <- plot(values[1:400])
# save(file = "mcmc.RData", values)

library(shiny)
suppressPackageStartupMessages(library(tidyverse))
library(cowplot)
theme_set(theme_bw()+theme(text = element_text(size = 20)))
library(ggtext)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("MCMC visualization"),
  p("Inspired by ",
    a("StataCorp LLC video", 
      href = "https://www.youtube.com/watch?v=OTO1DygELpY")),
  
  navbarPage("",
             tabPanel("Trace plot",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("trace_plot_t",
                                      "Number of samples:",
                                      min = 2,
                                      max = 400,
                                      value = 120,
                                      step = 10,
                                      animate = animationOptions(interval = 300,
                                                                 loop = FALSE))),
                        
                        mainPanel(
                          plotOutput("traceplot")
                        ))
             ),
             tabPanel("Random walk",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("markov_chain_t",
                                      "Number of samples:",
                                      min = 2,
                                      max = 400,
                                      value = 120,
                                      step = 10,
                                      animate = animationOptions(interval = 300,
                                                                 loop = FALSE))),
                        
                        mainPanel(
                          plotOutput("markov_chain_plot")
                        ))
                      
             ),
             tabPanel("MCMC",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("mcmc_t",
                                      "Number of samples:",
                                      min = 2,
                                      max = 400,
                                      value = 120,
                                      step = 10,
                                      animate = animationOptions(interval = 300,
                                                                 loop = FALSE))),
                        
                        mainPanel(
                          plotOutput("mcmc_plot")
                        ))
             )
  ),
  p("G. Moroz, ",
    a("see the code on GitHub.", 
      href = "https://github.com/agricolamz/mcmc_shiny"))
)

server <- function(input, output) {
  
  # first plot ---------------------------------------------------------------
  
  tibble(x = rnorm(400, mean = 50, sd = 10)) %>% 
    mutate(id = 1:n()) ->
    df1
  
  output$traceplot <- renderPlot({
    df1 %>% 
      slice(1:input$trace_plot_t)->
      df_for_plots
    last_point <- df_for_plots[input$trace_plot_t, ]$x
    
    df_for_plots %>% 
      ggplot(aes(x, id)) +
      geom_function(fun = function(x){1000*dnorm(x, mean = 50, sd = 10)+input$trace_plot_t})+
      geom_function(fun = function(x){input$trace_plot_t})+
      annotate(geom = "point", y = input$trace_plot_t, x = last_point, size = 7, color = "tomato")+
      annotate(geom = "richtext", y = input$trace_plot_t+90, x = max(df1$x), size = 7, label = "N(μ=50,σ=10)")+
      coord_flip()+
      geom_line(orientation = "y")+
      xlim(min(df1$x)-10, max(df1$x)+10) +
      ylim(0,550) +
      labs(y = "t", x = "")->
      p1
    
    df_for_plots %>%
      ggplot(aes(x))+
      geom_histogram(bins = 30)+
      coord_flip()+
      annotate(geom = "point", 
               y = 0, 
               x = last_point, 
               size = 7, color = "tomato")+
      scale_y_reverse()+
      scale_x_continuous(limits = c(min(df1$x)-10, max(df1$x)+10), position = "top")+
      labs(x = "", y = "") ->
      p2
    
    plot_grid(p2, p1, rel_widths = c(1, 3))
  })
  
  # second plot --------------------------------------------------------------
  values <- 50
  map_dbl(1:400, function(i){
    values[i+1] <<- rnorm(1, values[i], sd = 10)
  }) %>% 
    tibble(x = .) %>% 
    mutate(id = 1:n()) ->
    df2
  output$markov_chain_plot <- renderPlot({
    df2 %>% 
      slice(1:input$markov_chain_t)->
      df_for_plots
    last_point <- df_for_plots[input$markov_chain_t, ]$x
    
    df_for_plots %>% 
      ggplot(aes(x, id)) +
      geom_function(fun = function(x){500*dnorm(x, mean = last_point, sd = 10)+input$markov_chain_t})+
      geom_function(fun = function(x){input$markov_chain_t})+
      annotate(geom = "point", y = input$markov_chain_t, x = last_point, size = 7, color = "tomato")+
      annotate(geom = "richtext", y = input$markov_chain_t+90, x = max(df2$x)-10, size = 7, label = "N(μ=x<sup>t-1</sup>,σ=10)")+
      coord_flip()+
      geom_line(orientation = "y")+
      xlim(min(df2$x), max(df2$x)) +
      ylim(0,570) +
      labs(y = "t", x = "")->
      p1
    
    df_for_plots %>%
      ggplot(aes(x))+
      geom_histogram(bins = 30)+
      coord_flip()+
      annotate(geom = "point", 
               y = 0, 
               x = last_point, 
               size = 7, color = "tomato")+
      scale_y_reverse()+
      scale_x_continuous(limits = c(min(df2$x), max(df2$x)), position = "top")+
      labs(x = "", y = "") ->
      p2
    
    plot_grid(p2, p1, rel_widths = c(1, 3)) 
  })
  
  # third plot ---------------------------------------------------------------
  load("mcmc.RData")
  tibble(x = values) %>% 
    mutate(id = 1:n()) ->
    df3
  output$mcmc_plot <- renderPlot({
    df3 %>% 
      slice(1:input$mcmc_t)->
      df_for_plots
    last_point <- df_for_plots[input$mcmc_t, ]$x
    
    df_for_plots %>% 
      ggplot(aes(x, id)) +
      geom_function(fun = function(x){500*dnorm(x, mean = last_point, sd = 10)+input$mcmc_t})+
      geom_function(fun = function(x){input$mcmc_t})+
      annotate(geom = "point", y = input$mcmc_t, x = last_point, size = 7, color = "tomato")+
      annotate(geom = "richtext", y = input$mcmc_t+90, x = max(df3$x)-10, size = 7, label = "N(μ=x<sup>t-1</sup>,σ=10)")+
      coord_flip()+
      geom_line(orientation = "y")+
      xlim(min(df3$x), max(df3$x)+10) +
      ylim(0,570) +
      labs(y = "t", x = "")->
      p1
    
    df_for_plots %>%
      ggplot(aes(x))+
      geom_histogram(bins = 30)+
      coord_flip()+
      annotate(geom = "point", 
               y = 0, 
               x = last_point, 
               size = 7, color = "tomato")+
      scale_y_reverse()+
      scale_x_continuous(limits = c(min(df3$x), max(df3$x)), position = "top")+
      labs(x = "", y = "") ->
      p2
    
    plot_grid(p2, p1, rel_widths = c(1, 3))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
