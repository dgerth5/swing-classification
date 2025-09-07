# Load libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(catboost)
library(tidyverse)
library(readr)
library(RColorBrewer)
library(DT)

# Load data
fg <- read_csv("fangraphs-leaderboards (70).csv")
batter_swing_metrics_summary2 <- read_csv("batter_swing_metrics_summary.csv")
model_df <- read_csv("swing_metrics.csv")   # only needed for count levels & sz params

# Factor name lookup
factor_name_df <- model_df %>%
  left_join(fg %>% select(MLBAMID, Name) %>%
              mutate(MLBAMID = as.numeric(MLBAMID)) %>%
              distinct(),
            by = c("batter" = "MLBAMID")) %>%
  filter(!is.na(Name)) %>%
  select(Name, batter) %>%
  rename(MLBAMID = batter) %>%
  distinct()

# Strike zone parameters by player
sz_params <- model_df %>%
  left_join(factor_name_df, by = c("batter" = "MLBAMID")) %>%
  group_by(Name) %>%
  summarise(top_sz = mean(sz_top, na.rm = TRUE),
            bot_sz = mean(sz_bot, na.rm = TRUE),
            .groups = "drop")

# Load model
cat_model <- readRDS("swing_class_cat_model.RDS")

# Custom CSS
custom_css <- "
  .content-wrapper, .right-side { background-color: #f8f9fa; }
  .main-header .logo { background-color: #1e3a5f !important; color: white !important; font-weight: bold; }
  .main-header .navbar { background-color: #1e3a5f !important; }
  .main-sidebar { background-color: #2c3e50 !important; }
  .sidebar-menu > li.active > a { background-color: #34495e !important; border-left: 3px solid #3498db !important; }
  .box { border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); border-top: 3px solid #3498db; }
  .box-header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border-radius: 8px 8px 0 0; }
  .box-header h3 { color: white !important; font-weight: 600; }
  .selectize-input { border-radius: 6px; border: 2px solid #e9ecef; transition: border-color 0.3s ease; }
  .selectize-input:focus { border-color: #3498db; box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25); }
"

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Predicted Swing Type App", titleWidth = 300),
  dashboardSidebar(width = 300,
                   sidebarMenu(menuItem("Swing Analysis", tabName = "analysis", icon = icon("chart-line")),
                               menuItem("About", tabName = "about", icon = icon("info-circle")))),
  dashboardBody(tags$head(tags$style(HTML(custom_css))),
                tabItems(
                  # Main Analysis Tab
                  tabItem(tabName = "analysis",
                          fluidRow(
                            # Player Selection
                            box(title = "Player Selection", status = "primary", solidHeader = TRUE, width = 4, height = "180px",
                                pickerInput("player", "Choose a Player:",
                                            choices = sort(unique(factor_name_df$Name)),
                                            selected = sort(unique(factor_name_df$Name))[1],
                                            options = pickerOptions(style = "btn-outline-primary", size = 10,
                                                                    liveSearch = TRUE, title = "Choose a player..."))),
                            # Player Metrics
                            box(title = "Swing Type Metrics By Player", status = "primary", solidHeader = TRUE, width = 8, height = "550px",
                                DT::dataTableOutput("playerMetricsTable"))),
                          fluidRow(
                            # Swing Heat Map
                            box(title = "Swing Type Heat Map by Count", status = "primary", solidHeader = TRUE, width = 12,
                                div(style = "text-align: center; padding: 20px;", plotOutput("swingPlot", height = "700px"))))),
                  # About Tab
                  tabItem(tabName = "about",
                          fluidRow(
                            box(title = "About", status = "primary", solidHeader = TRUE, width = 12,
                                h4("Predicted Swing Type Dashboard"),
                                p("This app allows the user to see what swing type a hitter is likely to use for different pitch locations."),
                                p("Swings were classified based on bat speed, swing length, attack angle, attack direction, and swing tilt using a Gaussian Mixture Model."),
                                p("Predicted swing type is estimated based on the location of the pitch, the count and the specific hitter."),
                                p("Full explanation can be found here: INSERT URL HERE"),
                                div(style = "text-align: center; color: #7f8c8d;",
                                    p("Created by David Gerth. Find me at @dgerth1305 on Twitter/X or https://dgerth5.github.io/"))))))))

# Server
server <- function(input, output) {
  
  # Metrics Table
  output$playerMetricsTable <- DT::renderDataTable({
    req(input$player)
    player_metrics <- batter_swing_metrics_summary2 %>%
      filter(Name == input$player) %>%
      select(pred_swing_type, tot_swings, swing_usage, whiff_rate, barrel_rate, wOBAcon) %>%
      arrange(pred_swing_type)
    DT::datatable(player_metrics,
                  options = list(pageLength = 15, scrollX = FALSE, dom = 't',
                                 columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                  colnames = c('Swing Type', 'Total Swings', 'Usage Rate', 'Whiff Rate', 'Barrel Rate', 'wOBAcon'),
                  rownames = FALSE,
                  caption = paste("Swing metrics for", input$player)) %>%
      DT::formatStyle(columns = 1:6, backgroundColor = "#f8f9fa", border = "1px solid #dee2e6") %>%
      DT::formatPercentage(c('swing_usage', 'whiff_rate', 'barrel_rate'), 1) %>%
      DT::formatRound('wOBAcon', 3)
  })
  
  # Swing Plot
  output$swingPlot <- renderPlot({
    req(input$player)
    
    # Lookup MLBAMID for selected player
    mlbamid <- factor_name_df$MLBAMID[factor_name_df$Name == input$player]
    
    # Strike zone params
    sz <- filter(sz_params, Name == input$player)
    
    # Explicit count order (3 rows × 4 cols layout)
    count_levels <- c("0-0","1-0","2-0","3-0",
                      "0-1","1-1","2-1","3-1",
                      "0-2","1-2","2-2","3-2")
    
    # Prediction grid
    grid <- expand.grid(plate_x = seq(-1, 1, length.out = 50),
                        plate_z = seq(0.5, 4.5, length.out = 50),
                        count   = count_levels)
    grid$count <- factor(grid$count, levels = count_levels)
    
    # Ensure MLBAMID is categorical with same levels as training
    grid$MLBAMID <- factor(mlbamid, levels = levels(as.factor(model_df$batter)))
    
    # Predict
    pred_data <- select(grid, count, plate_x, plate_z, MLBAMID)
    grid_pool <- catboost.load_pool(data = pred_data)
    grid$pred_swing_type <- catboost.predict(cat_model, grid_pool, prediction_type = "Class") + 1
    
    # Keep consistent factor levels
    all_levels <- as.character(1:10)
    grid$pred_swing_type <- factor(as.character(grid$pred_swing_type), levels = all_levels)
    
    # Dummy rows to preserve legend
    missing_levels <- setdiff(all_levels, unique(as.character(grid$pred_swing_type)))
    if (length(missing_levels) > 0) {
      dummy <- data.frame(plate_x = 10, plate_z = 10,
                          count = factor(count_levels[1], levels = count_levels),
                          MLBAMID = factor(mlbamid, levels = levels(as.factor(model_df$batter))),
                          pred_swing_type = factor(missing_levels, levels = all_levels))
      grid <- rbind(grid, dummy)
    }
    
    # Plot
    ggplot(grid, aes(x = plate_x, y = plate_z, fill = pred_swing_type)) +
      geom_tile(alpha = 0.85) +
      facet_wrap(~count, ncol = 4, labeller = labeller(count = function(x) paste("Count:", x))) +
      scale_fill_brewer(type = "qual", palette = "Paired",
                        name = "Predicted\nSwing Type",
                        guide = guide_legend(title.position = "top", title.hjust = 0.5, ncol = 1)) +
      geom_rect(data = sz,
                aes(xmin = -0.705, xmax = 0.705, ymin = bot_sz, ymax = top_sz),
                fill = NA, color = "#2c3e50", linewidth = 1.5, inherit.aes = FALSE) +
      labs(title = paste("Predicted Swing Types for", input$player),
           subtitle = "Season: 2024",
           x = "Horizontal Plate Position (feet)", 
           y = "Vertical Plate Position (feet)",
           caption = "Black rectangle indicates strike zone boundaries") +
      coord_cartesian(xlim = c(-1, 1), ylim = c(0.5, 4.5)) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#2c3e50"),
            plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#7f8c8d"),
            plot.caption = element_text(size = 10, color = "#95a5a6"),
            strip.text = element_text(size = 11, face = "bold", color = "#2c3e50"),
            strip.background = element_rect(fill = "#ecf0f1", color = "#bdc3c7"),
            legend.position = "right",
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 10),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "#ecf0f1", size = 0.5),
            axis.text = element_text(color = "#2c3e50"),
            axis.title = element_text(face = "bold", color = "#2c3e50"))
  })
}

# Run app
shinyApp(ui = ui, server = server)
