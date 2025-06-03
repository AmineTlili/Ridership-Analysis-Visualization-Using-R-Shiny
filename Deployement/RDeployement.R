library(readr)
library(dplyr)
library(shiny)
library(ggplot2)
library(leaflet)
library(stringi)
library(readxl)
library(sf) 
library(shinydashboard)
library(scales)
library(lubridate)
library(timeDate)

final_data1 <- read.csv("final_data.csv",encoding = "latin1")
merged_PROFIL_FER <- read.csv("merged_PROFIL_FER.csv",encoding = "latin1")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Ridership Patterns"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Station Statistics", tabName = "station_stats", icon = icon("map")),
      menuItem("Global Overview", tabName = "seasonality_trends", icon = icon("line-chart")),
      menuItem("Holiday Statistics", tabName = "holiday_stats", icon = icon("calendar-check")),
      menuItem("Most Used Station", tabName = "most_used", icon = icon("bar-chart")),
      menuItem("Station Comparison", tabName = "station_comparison", icon = icon("line-chart")),
      menuItem("Station Hourly Distribution", tabName = "hourly_distribution", icon = icon("clock"))
      
    )
  ),
  dashboardBody(
    tabItems(
      # Station Statistics Tab
      # Station Statistics Tab
      tabItem(
        tabName = "station_stats",
        fluidRow(
          box(
            leafletOutput("station_map"),  # Placeholder for the map
            width = 12
          )
          
        ),
        fluidRow(
          box(
            selectInput(
              "station_name",
              "Select Station:",
              choices = unique(final_data1$LIBELLE_ARRET),
              selected = "PERNETY"
            ),
            width = 12
          )
          
        ),
        
        fluidRow(
          box(
            tableOutput("station_stats_tab"), 
            width = 8
          ),
          box(
            plotOutput("station_pie_chart"),  # Placeholder for the pie chart
            width = 4
          )
        )
      ),
      tabItem(tabName = "seasonality_trends",
              fluidRow(
                box(
                  selectInput("time_frequency", "Select Time Frequency:", 
                              choices = c("Monthly", "Yearly", "Weekly", "Daily"), 
                              selected = "Monthly"),
                  width = 4
                ),
                box(
                  plotOutput("seasonality_trends_plot"),
                  width = 8
                )
              )
      ),
      
      
      # Holiday Statistics Tab
      tabItem(tabName = "holiday_stats",
              fluidRow(
                box(selectInput("station_selection", "Select Station or ALL:", 
                                choices = c("ALL", unique(final_data1$LIBELLE_ARRET))),
                    width = 12)
              ),
              
              fluidRow(
                box(plotOutput("mean_sum_nbdvald_plot"), width = 6),
                box(plotOutput("mean_sum_nbdvald_special_period_plot"), width = 6)
              )
      )
      ,
      # Most Used Station Tab
      tabItem(tabName = "most_used",
              fluidRow(
                box(dateRangeInput("date_range", "Select Date Range:",
                                   start = min(final_data1$JOUR, na.rm = TRUE),
                                   end = max(final_data1$JOUR, na.rm = TRUE)),
                    selectInput("station_count_filter", "Select Top Stations:",
                                choices = c(5, 10, 15, 20),
                                selected = 5),
                    selectInput("station_type_filter", "Select Station Type:",
                                choices = c("Both", "metroStation", "railStation"),
                                selected = "Both"),
                    width = 4),
                box(plotOutput("most_used_station_plot"), width = 8)
              ))
      ,
      tabItem(tabName = "station_comparison",
              fluidRow(
                box(
                  selectInput("station1", "Select Station 1:", choices = unique(final_data1$LIBELLE_ARRET), selected = unique(final_data1$LIBELLE_ARRET)[1]),
                  selectInput("station2", "Select Station 2:", choices = unique(final_data1$LIBELLE_ARRET), selected = unique(final_data1$LIBELLE_ARRET)[2]),
                  selectInput("columnSelect", "Select subscription category :", choices = c("AMETHYSTE", "AUTRE_TITRE", "FGT", "IMAGINE_R", "NAVIGO", "NAVIGO_JOUR", "NON_DEFINI", "TST")),
                  width = 4
                ),
                box(plotOutput("station_comparison_plot"), width = 8)
              )
      ),
      # Station Hourly Distribution Tab
      tabItem(
        tabName = "hourly_distribution",
        fluidRow(
          box(
            title = "Selection",
            selectInput("dist_station", "Select Station:", choices = NULL),
            selectInput("dist_profile", "Select Profile:", choices = NULL),
            width = 4
          ),
          box(
            title = "Hourly Distribution Bar Plot",
            plotOutput("bar_plot"),
            width = 8
          )
        ),
        fluidRow(
          box(
            title = "Hourly Distribution Pie Chart",
            plotOutput("pie_chart"),
            width = 12
          )
        )
      )
    )
  )
)
# Define server
server <- function(input, output, session) {
  
  selected_station_data <- reactive({
    final_data1 %>%
      filter(LIBELLE_ARRET == input$station_name)
  })
  
  # Filter data based on user inputs
  filtered_data_ref <- reactive({
    final_data1 %>%
      filter(JOUR >= input$start_date_ref, JOUR <= input$end_date_ref)
  })
  
  filtered_data_comp <- reactive({
    final_data1 %>%
      filter(JOUR >= input$start_date_comp, JOUR <= input$end_date_comp)
  })
  station_comparison_data <- reactive({
    selected_column <- input$columnSelect
    final_data1 %>%
      filter(LIBELLE_ARRET %in% c(input$station1, input$station2)) %>%
      group_by(JOUR, LIBELLE_ARRET) %>%
      summarise(Value = sum(!!as.name(selected_column), na.rm = TRUE)) %>%
      ungroup()
  })
  
  
  
  # Render the comparison plot
  output$station_comparison_plot <- renderPlot({
    ggplot(station_comparison_data(), aes(x = as.Date(JOUR), y = Value, color = LIBELLE_ARRET)) +
      geom_line() +
      labs(title = "Station Comparison Over Time (Across Years)",
           x = "Date",
           y = paste("Total", input$columnSelect, "Values"),  # Dynamic Y-axis label
           color = "Station") +
      theme_minimal() +
      scale_x_date(name = "Date", date_breaks = "1 year", date_labels = "%Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate X-axis labels for readability
  })
  # Reactive filtering based on the number of top stations
  filtered_data_most_used <- reactive({
    data <- final_data1 %>%
      filter(JOUR >= input$date_range[1] & JOUR <= input$date_range[2])
    
    if (input$station_type_filter != "Both") {
      data <- data %>% filter(ZdAType == input$station_type_filter)
    }
    
    # Convert input to numeric safely
    n_stations <- as.numeric(input$station_count_filter)
    if (is.na(n_stations)) n_stations <- 5  # Default to top 5 if conversion fails
    
    # Get top stations based on Total_NB_VALD
    top_stations <- data %>%
      group_by(LIBELLE_ARRET) %>%
      summarise(Total_Validations = sum(Total_NB_VALD, na.rm = TRUE)) %>%
      arrange(desc(Total_Validations)) %>%
      head(n = n_stations)  # Select top N stations
    
    data %>% filter(LIBELLE_ARRET %in% top_stations$LIBELLE_ARRET)
  })
  
  
  observeEvent(input$station_name, {
    selected_station <- final_data1 %>%
      filter(LIBELLE_ARRET == input$station_name) %>%
      select(LIBELLE_ARRET) %>%
      distinct()
    
    output$station_stats_tab <- renderTable({
      summarise_data <- final_data1 %>%
        filter(LIBELLE_ARRET %in% selected_station$LIBELLE_ARRET) %>%
        group_by(LIBELLE_ARRET, Weekday) %>%
        summarise(
          Total_Validations = sum(Total_NB_VALD, na.rm = TRUE),
          Avg_Validations = mean(Total_NB_VALD, na.rm = TRUE),
          Max_Validations = max(Total_NB_VALD, na.rm = TRUE)
        )
      return(summarise_data)
    })
    
    # Map rendering logic
    output$station_map <- renderLeaflet({
      arret_to_display <- input$station_name
      
      unique_station_data <- final_data1 %>%
        filter(LIBELLE_ARRET == arret_to_display) %>%
        distinct(LIBELLE_ARRET, ZdAXEpsg2154, ZdAYEpsg2154, .keep_all = TRUE) %>%
        slice(1)  # Use only the first row to optimize rendering
      
      if (nrow(unique_station_data) > 0) {
        # Convert to spatial object
        station_sf <- st_as_sf(
          unique_station_data,
          coords = c("ZdAXEpsg2154", "ZdAYEpsg2154"),
          crs = 2154
        )
        
        # Transform to EPSG 4326
        station_sf <- st_transform(station_sf, 4326)
        
        # Extract coordinates
        coords <- st_coordinates(station_sf)
        unique_station_data$X <- coords[1, "X"]
        unique_station_data$Y <- coords[1, "Y"]
        
        # Render Leaflet map
        leaflet() %>%
          addTiles() %>%
          setView(
            lng = unique_station_data$X,
            lat = unique_station_data$Y,
            zoom = 11
          ) %>%
          addMarkers(
            lng = unique_station_data$X,
            lat = unique_station_data$Y,
            popup = paste("LIBELLE_ARRET:", unique_station_data$LIBELLE_ARRET)
          )
      } else {
        # Default view if no data is available for the selected station
        leaflet() %>%
          addTiles() %>%
          setView(lng = 2.3522, lat = 48.8566, zoom = 10)  # Default: Paris view
      }
    })
    # Pie chart rendering logic (percentages in front of the Weekday label)
    output$station_pie_chart <- renderPlot({
      summarise_data <- final_data1 %>%
        filter(LIBELLE_ARRET %in% selected_station$LIBELLE_ARRET) %>%
        group_by(Weekday) %>%
        summarise(Total_Validations = sum(Total_NB_VALD, na.rm = TRUE))
      
      # Calculate percentages
      summarise_data <- summarise_data %>%
        mutate(Percentage = prop.table(Total_Validations) * 100)
      
      ggplot(summarise_data, aes(x = "", y = Percentage, fill = Weekday)) +
        geom_bar(stat = "identity", width = 1, color = "black") +
        coord_polar("y", start = 0) +
        geom_text(aes(y = Percentage, label = scales::percent(Percentage / 100, accuracy = 1)),
                  position = position_stack(vjust = 0.5), size = 4.5, hjust = 0.5) +
        theme_minimal() +
        labs(
          title = "Total Validations by Weekday (Percentage)",
          x = NULL,
          y = NULL,
          fill = "Weekday"
        ) +
        theme(legend.position = "right")
    })
    
    
  })
  
  # Add server logic for the "Holiday Statistics" tab
  observeEvent(input$station_selection, {
    
    # Plot the mean by weekday
    output$mean_sum_nbdvald_plot <- renderPlot({
      station_data <- final_data1 %>%
        filter(if (input$station_selection == "ALL") TRUE else LIBELLE_ARRET == input$station_selection) %>%
        group_by(Weekday, HolidayType) %>%
        summarise(Mean_Sum_NB_VALD = mean(Total_NB_VALD, na.rm = TRUE))
      
      ggplot(station_data, aes(x = Weekday, y = Mean_Sum_NB_VALD, color = HolidayType)) +
        geom_point(size = 3) +
        labs(title = "Average Number of validations Comparison by Station and Holiday days",
             x = "Day of the Week",
             y = "Mean Sum_NB_VALD") +
        theme_minimal()
    })
    
    # Plot the mean by weekday and special period
    output$mean_sum_nbdvald_special_period_plot <- renderPlot({
      special_period_data <- final_data1 %>%
        filter(if (input$station_selection == "ALL") TRUE else LIBELLE_ARRET == input$station_selection) %>%
        group_by(Weekday, StudentHolidays) %>%
        summarise(Mean_Sum_NB_VALD = mean(Total_NB_VALD, na.rm = TRUE))
      
      ggplot(special_period_data, aes(x = Weekday, y = Mean_Sum_NB_VALD, color = StudentHolidays)) +
        geom_point(size = 3) +
        scale_color_manual(values = c("No" = "blue", "Yes" = "orange")) +
        labs(title = "Average Number of validations Comparison by Station and School Breaks",
             x = "Day of the Week",
             y = "Mean Sum_NB_VALD",
             color = "StudentHolidays") +
        theme_minimal()
    })
  })
  
  output$most_used_station_plot <- renderPlot({
    data_to_plot <- filtered_data_most_used()
    
    ggplot(data_to_plot, aes(x = LIBELLE_ARRET, y = Total_NB_VALD, fill = ZdAType)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Total Validations by Metro and Rail Stations",
           x = "Station",
           y = "Total Validations") +
      theme_minimal() +
      theme(legend.position = "top") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis
  })
  
  observeEvent(input$time_frequency, {
    
    output$seasonality_trends_plot <- renderPlot({
      if (input$time_frequency == "Daily") {
        final_data1 <- final_data1 %>%
          mutate(JOUR = as.Date(JOUR))
        # Plot daily data
        final_data1 %>%
          ggplot(aes(x = JOUR, y = Total_NB_VALD)) +
          geom_line(color = "blue") +  # Line plot with color
          labs(title = "Daily Ridership Trends",
               x = "Date",
               y = "Total_NB_VALD") +
          scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
        
      }
      else if (input$time_frequency == "Monthly") {
        monthly_data <- final_data1 %>%
          mutate(YearMonth = format(as.Date(JOUR), "%Y-%m")) %>%  
          group_by(YearMonth) %>%
          summarize(Total_NB_VALD = mean(Total_NB_VALD, na.rm = TRUE))
        
        monthly_data %>%
          mutate(YearMonth = as.Date(paste0(YearMonth, "-01"))) %>%  # Convert YearMonth to Date format
          ggplot(aes(x = YearMonth, y = Total_NB_VALD)) +
          geom_line() +  # Line plot
          labs(title = "Total_NB_VALD per Month", x = "Month", y = "Total_NB_VALD") +
          scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      else if (input$time_frequency == "Yearly") {
        yearly_data <- final_data1 %>%
          mutate(Year = year(as.Date(JOUR))) %>%  # Convert to Year directly
          group_by(Year) %>%
          summarize(Total_NB_VALD = mean(Total_NB_VALD, na.rm = TRUE))
        if (nrow(yearly_data) > 0) {
          yearly_data %>%
            ggplot(aes(x = Year, y = Total_NB_VALD, fill = Total_NB_VALD)) +
            geom_bar(stat = "identity") +
            scale_fill_gradient(low = "#ADD8E6", high = "#00008B") +  # Reversed gradient
            labs(title = "Total_NB_VALD per Year", x = "Year", y = "Total_NB_VALD") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        } else {
          message("No data available for yearly visualization.")
        }
      } else if (input$time_frequency == "Weekly") {
        weekly_data <- final_data1 %>%
          mutate(Week = format(as.Date(JOUR), "%Y-%U")) %>%  # Weekly format (Year-WeekNumber)
          group_by(Week) %>%
          summarize(Total_NB_VALD = mean(Total_NB_VALD, na.rm = TRUE))
        
        weekly_data %>%
          mutate(Week = as.Date(paste0(Week, "-1"), format = "%Y-%U-%u")) %>%  # Convert Week to Date
          ggplot(aes(x = Week, y = Total_NB_VALD)) +
          geom_line(color = "purple") +
          labs(title = "Total_NB_VALD per Week", x = "Week", y = "Total_NB_VALD") +
          scale_x_date(date_labels = "%Y-W%U", date_breaks = "4 weeks") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    })
  })
  filtered_hourly_data <- reactive({
    req(input$dist_station, input$dist_profile)
    merged_PROFIL_FER %>%
      filter(LIBELLE_ARRET == input$dist_station, CAT_JOUR == input$dist_profile)
  })
  # Dynamically update select inputs for station and profile
  observe({
    updateSelectInput(session, "dist_station", 
                      choices = unique(merged_PROFIL_FER$LIBELLE_ARRET))
    updateSelectInput(session, "dist_profile", 
                      choices = unique(merged_PROFIL_FER$CAT_JOUR))
  })
  
  # Bar Plot
  output$bar_plot <- renderPlot({
    data <- filtered_hourly_data()
    req(nrow(data) > 0)  # Ensure data is available
    data$pourc_validations <- as.numeric(gsub(",", ".", data$pourc_validations))
    data$pourc_validations <- 100 * data$pourc_validations / sum(data$pourc_validations, na.rm = TRUE)
    
    time_order <- c(
      "0H-1H", "1H-2H", "2H-3H", "3H-4H", "4H-5H", "5H-6H", "6H-7H", "7H-8H",
      "8H-9H", "9H-10H", "10H-11H", "11H-12H", "12H-13H", "13H-14H", "14H-15H",
      "15H-16H", "16H-17H", "17H-18H", "18H-19H", "19H-20H", "20H-21H",
      "21H-22H", "22H-23H", "23H-0H"
    )
    data$TRNC_HORR_60 <- factor(data$TRNC_HORR_60, levels = time_order)
    
    ggplot(data, aes(x = TRNC_HORR_60, y = pourc_validations, fill = TRNC_HORR_60)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(
        title = paste("Bar Plot for Station:", input$dist_station, "Profile:", input$dist_profile),
        x = "Time Range",
        y = "Percentage Validations",
        fill = "Time Range"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Pie Chart
  output$pie_chart <- renderPlot({
    data <- filtered_hourly_data()
    req(nrow(data) > 0)  # Ensure data is available
    data$pourc_validations <- as.numeric(gsub(",", ".", data$pourc_validations))
    data$pourc_validations <- 100 * data$pourc_validations / sum(data$pourc_validations, na.rm = TRUE)
    
    data$Time_Category <- case_when(
      data$TRNC_HORR_60 %in% c("5H-6H", "6H-7H", "7H-8H", "8H-9H", "9H-10H") ~ "MATIN 5H-10H",
      data$TRNC_HORR_60 %in% c("10H-11H", "11H-12H", "12H-13H", "13H-14H", "14H-15H", "15H-16H", "16H-17H") ~ "MIDI 10H-17H",
      TRUE ~ "NUIT 17H-5H"
    )
    
    agg_data <- data %>%
      group_by(Time_Category) %>%
      summarise(pourc_validations = sum(pourc_validations))
    
    ggplot(agg_data, aes(x = "", y = pourc_validations, fill = Time_Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(
        title = paste("Time Category Distribution:", input$dist_station, "-", input$dist_profile),
        fill = "Time Categories"
      ) +
      scale_fill_manual(values = c("MATIN 5H-10H" = "#4E79A7", "MIDI 10H-17H" = "#F28E2B", "NUIT 17H-5H" = "#76B7B2"))
  })
  
}
# Run the application
shinyApp(ui, server)