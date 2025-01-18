library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(DT)
library(plotly)

data <- read_csv('https://github.com/nkal22/combine_score/raw/main/cpoe_results.csv.gz') %>%
  mutate(
    shot_result = case_when(
      made_shot == 1 ~ "MAKE",
      made_shot == 0 ~ "MISS",
      TRUE ~ as.character(made_shot)  # Fallback for unexpected values
    ),
    cpoe = round(cpoe, 3)  # Round the cpoe column to 3 decimal places
  ) %>%
  select(
    Player = PLAYER1_NAME,
    offense_team,
    defending_team,
    Period = PERIOD,
    Seconds_remaining = total_secs_remaining,
    Score_Margin = SCOREMARGIN,
    Shot_Type = play_type,
    Distance,
    shot_result,
    cpoe
  )

# Define UI for application
ui <- fluidPage(
  titlePanel("Context-Adjusted Points Over Expected - 2023/24 Season"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("offense_team", "Select Offense Team:", 
                  choices = sort(c("All", unique(data$offense_team))), selected="All"),
      uiOutput("player_select"),
      selectInput("defending_team", "Select Defending Team:", 
                  choices = sort(c("All", unique(data$defending_team))), selected="All")
    ),
    
    mainPanel(
      h4("Player Rankings by Total CPOE for Selected Team"),
      DTOutput("player_rankings"),
      br(),  # Adds space between elements
      h4("Top 5 Shots by CPOE for Selected Player against Selected Defense"),
      DTOutput("top_shots"),
      br(),  # Adds space between elements
      h4("Bottom 5 Shots by CPOE for Selected Player against Selected Defense"),
      DTOutput("bottom_shots"),
      br(),  # Adds space between elements
      h4("Total CPOE for Selected Player against Selected Defense, Sorted by Play Type"),
      plotlyOutput("cpoe_plot", height = '800px', width = '1200px')  # Use plotlyOutput for interactive plots
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- data
    
    if (input$offense_team != "All") {
      df <- df %>% filter(offense_team == input$offense_team)
    }
    
    if (input$defending_team != "All") {
      df <- df %>% filter(defending_team == input$defending_team)
    }
    
    df
  })
  
  observe({
    players <- unique(filtered_data()$Player)
    updateSelectInput(session, "player_select", "Select Player:", choices = sort(c("All", players)), selected="All")
  })
  
  output$player_select <- renderUI({
    selectInput("player_select", "Select Player:", 
                choices = c("All", unique(filtered_data()$Player)))
  })
  
  output$player_rankings <- renderDT({
    df <- filtered_data()
    
    if (input$offense_team != "All") {
      df <- df %>% group_by(Player) %>% summarize(total_cpoe = sum(cpoe, na.rm = TRUE)) %>% arrange(desc(total_cpoe))
    } else {
      df <- df %>% group_by(Player) %>% summarize(total_cpoe = sum(cpoe, na.rm = TRUE)) %>% arrange(desc(total_cpoe))
    }
    
    datatable(df, 
              options = list(pageLength = 10),
              rownames = FALSE) %>%
      formatStyle('total_cpoe', 
                  backgroundColor = styleInterval(0, c("red", "blue")),
                  color = styleInterval(0, c("white", "white")))
  })
  
  output$top_shots <- renderDT({
    df <- filtered_data()
    
    if (input$player_select != "All") {
      df <- df %>% filter(Player == input$player_select)
    }
    
    datatable(df %>% arrange(desc(cpoe)), 
              options = list(pageLength = 5),
              rownames = FALSE) %>%
      formatStyle('cpoe', 
                  backgroundColor = styleInterval(0, c("red", "blue")),
                  color = styleInterval(0, c("white", "white")))
  })
  
  output$bottom_shots <- renderDT({
    df <- filtered_data()
    
    if (input$player_select != "All") {
      df <- df %>% filter(Player == input$player_select)
    }
    
    datatable(df %>% arrange(cpoe), 
              options = list(pageLength = 5),
              rownames = FALSE) %>%
      formatStyle('cpoe', 
                  backgroundColor = styleInterval(0, c("red", "blue")),
                  color = styleInterval(0, c("white", "white")))
  })
  
  output$cpoe_plot <- renderPlotly({
    df <- filtered_data()
    
    if (input$player_select != "All") {
      df <- df %>% filter(Player == input$player_select)
    }
    
    df_summary <- df %>% group_by(Shot_Type) %>% summarize(total_cpoe = sum(cpoe, na.rm = TRUE))
    
    plot_ly(df_summary, x = ~Shot_Type, y = ~total_cpoe, type = 'bar',
            text = ~paste('Play Type:', Shot_Type, '<br>Total CPOE:', total_cpoe),
            hoverinfo = 'text') %>%
      layout(title = 'Total CPOE by Play Type',
             xaxis = list(title = 'Play Type'),
             yaxis = list(title = 'Total CPOE'))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


