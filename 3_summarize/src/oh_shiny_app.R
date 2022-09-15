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
meta <- readr::read_csv('1_fetch/out/lake_metadata.csv') %>%
  mutate(sad = area/depth)
metrics <- left_join(metrics, meta)

ggplot(years_collapsed, aes(x = secchi_scenario, y = avg_perc))+
  geom_line(aes(color = print_name %in% 'Grindstone Lake', group = site_id)) +
  scale_color_manual(values = c('gray', 'red'))
  

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
      geom_point(aes(shape = threshold), alpha = 0.5) +
      geom_point(data = filter(df, print_name_unique == input$select), 
                 aes(shape = threshold), color = 'green')+
      scale_shape_manual(values = c(1, 16))+
      theme_bw() +
      labs(x = 'Distance to optimal clarity (Secchi in m)', 
           y = 'Current OH as % of max OH',
           title = paste(input$select, 'highlighted in green.'),
           shape = 'Current OH >5%\nbenthic area')
    ggplotly(p)
  })
  
  output$LakeSensitivity <- renderPlotly({
    df <- mutate(metrics, 
                 threshold = current_annual_perc_oh_mean >= 5,
                 target_lake = print_name_unique == input$select)
    p <- ggplot(df, aes(x = y2018_slope_pp_per_interval_i_mean, y = y2018_slope_pp_per_interval_d_mean)) +
      geom_point(aes(shape = threshold), alpha = 0.5) +
      geom_point(data = filter(df, print_name_unique %in% input$select),
                 aes(shape = threshold), color = 'green') +
      scale_shape_manual(values = c(1,16)) +
      geom_abline(intercept = 0, slope = 1, color = 'darkgray', linetype = 2) +
      geom_hline(yintercept = 0, color = 'darkgray') +
      geom_vline(xintercept = 0, color = 'darkgray')+
      theme_bw() +
      labs(x = 'Sensitivity to increase in secchi\n(PP change in %BOH/+0.25m Secchi)', 
           y = 'Current sensitivity to decrease in secchi\n(PP change in %BOH/-0.25m Secchi)',
           title = paste(input$select, 'higlighted in green'))
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
