library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(nycflights13)

# read in data
all_n <- readr::read_csv('3_summarize/out/oha_annual_scenarios.csv')
years_collapsed <- readr::read_csv('3_summarize/out/oha_by_lake_scenario.csv')
all_n <- arrange(all_n, print_name_unique)
named_labels <- unique(all_n$print_name_unique)
all_n$print_name_unique <- factor(all_n$print_name_unique, labels = unique(all_n$print_name_unique))
levels(all_n$print_name_unique)

metrics <- readr::read_csv('3_summarize/out/oha_metrics_by_lake.csv')
#ui
ui <- shinyUI(
  pageWithSidebar(
    headerPanel("Optical Habitat by Clarity Scenarios"),
    sidebarPanel(
      selectInput("select", "Select a lake", 
                  choices = named_labels)
      ),
    
    mainPanel(
      plotlyOutput("LakeDist",width="800",height = "400px"),
      plotlyOutput("LakeSensitivity",width="800",height = "400px"),
      plotlyOutput("AnnualCollapsed",width="800",height = "400px")
      
    )
  ))


#server
server <- function(input, output){
  
  output$LakeDist <- renderPlotly({
    df <- mutate(metrics, 
                 threshold = current_annual_perc_oh_mean >= 5,
                 target_lake = print_name_unique == input$select)
    p <- ggplot(df, aes(x = y2018_dist_optimal_mean, y = current_annual_percofmax_oh_mean)) +
      geom_point(aes(shape = threshold,
                     color = target_lake)) +
      theme_bw() +
      labs(x = 'Distance to optimal clarity (Secchi in m)', 
           y = 'Current OH as % of max OH',
           title = input$select)
    ggplotly(p)
  })
  
  output$LakeSensitivity <- renderPlotly({
    df <- mutate(metrics, 
                 threshold = current_annual_perc_oh_mean >= 5,
                 target_lake = print_name_unique == input$select)
    p <- ggplot(df, aes(x = y2018_slope_pp_per_interval_i_mean, y = y2018_slope_pp_per_interval_d_mean)) +
      geom_point(aes(shape = threshold,
                     color = target_lake)) +
      theme_bw() +
      labs(x = 'Current sensitivity to increase in secchi', 
           y = 'Current sensitivity to decrease in secchi',
           title = input$select)
    ggplotly(p)
  })
  
   output$AnnualCollapsed <- renderPlotly({
     df <- filter(years_collapsed, print_name_unique == input$select)
     df2 <- filter(metrics, print_name_unique == input$select)
     p <- ggplot(df, aes(x = secchi_scenario, y = avg_perc)) +
       geom_ribbon(aes(ymax = max, ymin = min), alpha = 0.6, fill = "gray70") +
       geom_point() +
       geom_line() +
       geom_hline(aes(yintercept = 5), col = 'red', linetype = 2) +
       geom_vline(data = df2, aes(xintercept = df2$optimal_secchi_mean), col = 'blue', linetype = 2) +
       geom_vline(data = df2, aes(xintercept = df2$y2018_secchi_mean), color = 'blue') +
       theme_bw() +
       labs(x = "Clarity scenario\n(Secchi depth, m)", 
            y = 'Percent of total benthic area',
            title = unique(df$print_name))
     ggplotly(p)
   })
  
}

shinyApp(ui = ui, server = server)
