# install and import necessary libraries
required_packages <- c("shiny", "shinydashboard", "DT", "dplyr", "ggplot2",
                       "countrycode", "fmsb", "fontawesome", "plotly", "tidymodels", "forecast")

if (!requireNamespace(required_packages, quietly = TRUE)) install.packages(required_packages)
lapply(required_packages, library, character.only = TRUE)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(countrycode)
library(fmsb)
library(fontawesome)
library(plotly)
library(tidymodels)
library(forecast)


# Import and clean data
data <- read.csv("world_data_2023.csv")
# str(data)
# Drop irrelevant columns
df_subset <- subset(data, select = -c(currency_code,abbreviation,largest_city,total_tax_rate_percent,
                                    armed_forces_size,calling_code,gasoline_price,minimum_wage,
                                    tax_revenue_percent,unemployment_rate))
# Drop rows with missing values
df <- na.omit(df_subset)
# 
# dim(df)
# Add region column
df <- df %>%
  mutate(region = countrycode(country, origin = "country.name", destination = "region"))
# Add iso code column
df <- df %>%
  mutate(iso_code = countryname(df$country, destination = 'iso3c'))
dim(df)
# write.csv(df,"findme.csv",row.names=FALSE)

# define function to properly display values
format_scientific <- function(x) {
  if(x == 0) return("0")
  exponent <- floor(log10(abs(x)))
  base <- signif(x / 10^exponent, digits = 3)
  paste0(base, "E", exponent)
}

# second dataset
data2 <- read.csv("renewable-share-energy.csv")
df_subset2 <- subset(data2, select = -c(Code))
df_subset2 <- df_subset2 %>%
  mutate(region = countrycode(Entity, origin = "country.name", destination = "region"))
df3 <- na.omit(df_subset2)
df3 <- df3 %>% select(-region)
df2 <- df3 %>% 
  filter(Year >= 2000 & Year <= 2022)
forecast_df <- data.frame(Entity = character(),
                          Year = integer(),
                          Renewables_percent = numeric(),
                          stringsAsFactors = FALSE)

entities <- unique(df2$Entity)
# conduct forecast
for (entity in entities) {
  entity_data <- df2 %>% filter(Entity == entity)
  ts_data <- ts(entity_data$Renewables_percent, start = min(entity_data$Year), end = max(entity_data$Year), frequency = 1)
  fit <- forecast::ets(ts_data)
  forecast_result <- forecast(fit, h = 5)
  forecast_values <- as.numeric(forecast_result$mean)
  forecast_entity_df <- data.frame(Entity = entity,
                                   Year = seq(max(entity_data$Year) + 1, max(entity_data$Year) + 5),
                                   Renewables_percent = forecast_values,
                                   stringsAsFactors = FALSE)
  forecast_df <- rbind(forecast_df, forecast_entity_df)
}
# bind the forecasted and past data
final_df <- rbind(df2, forecast_df)
final_df <- final_df %>%
  mutate(region = countrycode(Entity, origin = "country.name", destination = "region"))
df_renew <- na.omit(final_df)
df_renew <- df_renew %>% select(-region)


# UI
ui <- dashboardPage(
  
  title = "Sustainable City Tracker",
  # set overall colour
  skin = "purple",
  
  # Header is here
  dashboardHeader(
    title = "Group Five",
    dropdownMenu(type = "messages", badgeStatus = "success",
                 notificationItem(icon = icon("user",lib = "glyphicon"),
                                  status = "success",
                                  text = "Welcome to our Group Project!")
                 )
    ),
  
  
  # Sidebar is here
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sustainable City Tracker", tabName = "dashboardhome", icon= icon("seedling", lib="font-awesome")),
      menuItem("Country Overview", tabName = "dashboard1", icon = icon("earth-americas",lib = "font-awesome")),
      menuItem("CO2 Emission Tracker", tabName = "dashboard2", icon = icon("bar-chart")),
      menuItem("Healthcare Tracker", tabName = "dashboard3", icon = icon("bar-chart")),
      menuItem("Renewable Energy Tracker", tabName = "dashboard4", icon = icon("bar-chart")),
      menuItem("Dataset",tabName = "dataset", icon = icon("table"))
      )
    ),
  
  
  # Body is here
  dashboardBody(
    
    # Switch pages here
    tabItems(
      
      # tab home page
      tabItem(tabName = "dashboardhome",
              
              fluidRow(box(width = 12, solidHeader = TRUE, 
                           style = "display: flex; justify-content: center; align-items: center; height: 500px;
                           background-color: rgb(204,255,255)",
                           div(style = "text-align: center;",
                               tags$img(src = "homepage.jpg", style = "max-width: 100%; height: auto;")))),
              
              fluidRow(box(width=12, solidHeader = TRUE, status = "primary",
                           p(HTML("Welcome! Our sustainable city tracker is a multipurpose dashboard that provides insights on the overall quality and sustainability of the city. <br>The objective of this dashboard is to investigate the various key indicators that influence the quality of life and sustainability of the city. <br>As corporations grow aware of the importance in investing their businesses in a city that is both sustainable and progressive, this dashboard will provide detailed data visualizations to help with the decision making process."),
                             style = "font-size: 18px; text-align: center;"),
                           style = "background-color: rgb(204,255,255)"))
              
              ),
      
      # tab 1
      tabItem(tabName = "dashboard1",
              h2(HTML("<center><b>Country Overview</b></center>")),
              
              # tab row 1
              fluidRow(box(selectizeInput("ds1features","Search for a country",
                                          choices = unique(df$country),
                                          options = list(create = FALSE)),width = 12)),
              # tab row 2
              fluidRow(
                box(plotOutput("radar_chart"), width = 8,height = 477),
                box(valueBoxOutput("infobox1",width = 13),width = 4),
                box(valueBoxOutput("infobox2",width = 13),width = 4),
                box(valueBoxOutput("infobox3",width = 13),width = 4))
                
      ),
      
      # tab 2
      tabItem(tabName = "dashboard2",
              h2(HTML("<center><b>CO2 Emission Tracker</b></center>")),
              
              # tab row 1
              fluidRow(
                box(plotlyOutput("co2_map"),width = 12)),
              
              fluidRow(
                box(plotlyOutput("co2_regression"), width = 8),
                box(selectInput("SI_co2_regression",
                                "Select a variable:",
                                choices = c("urban_population", "gdp","forested_area_percent")), width = 4),
                box(valueBoxOutput("co2_pvalue",width = 13),width = 4),
                box(valueBoxOutput("co2_rsquared",width = 13),width = 4))
              
              ),
      
      # tab 3
      tabItem(tabName = "dashboard3",
              h2(HTML("<center><b>Healthcare Tracker</b></center>")),
              
              # tab row 1
              fluidRow(
                box(plotlyOutput("health_bar"), width = 12)),
              
              # tab row 2
              fluidRow(
                box(plotlyOutput("health_edu_regression"), width = 8),
                box(selectInput("SI_he_regression",
                                "Select a variable:",
                                choices = c("gross_tertiary_education_enrollment_percent", "gross_primary_education_enrollment_percent","physicians_per_thousand")), width = 4),
                box(valueBoxOutput("he_pvalue",width = 13),width = 4),
                box(valueBoxOutput("he_rsquared",width = 13),width = 4))
              
              ),
      
      # tab 4
      tabItem(
        tabName = "dashboard4",
        h2(HTML("<center><b>Renewable Energy Tracker</b></center>")),
        
        # tab row 1
        fluidRow(
          box(selectizeInput(
              "SI_renewable_share",
              "Search for a country",
              choices = unique(df_renew$Entity),
              options = list(create = FALSE)),width = 12)),
        
        # tab row 2
        fluidRow(
          box(plotlyOutput("renewable_prod"), 
            width = 12)),
        
        # tab row 3
        fluidRow(
          box(plotlyOutput("renewable_share"),width = 8),
          box(p(textOutput("renewable_share_value")),
            width = 4))),
      
      
      
      # tab 5
      tabItem(tabName = "dataset",
              h2(HTML("<center><b>Dataset</b></center>")),
              dataTableOutput("worldtable"))
    )
  )
)



# Server
server <- function(input, output) {
  

  # output for DT 
  output$worldtable <- renderDataTable({
    datatable(df, options = list(scrollX = TRUE))
  })
  

  
  # output for Radar Chart in Country Overview
  output$radar_chart <-renderPlot({
    # Combine max, min, and actual values into a single data frame
    temp2 <- df[df$country == input$ds1features, c('birth_rate','fertility_rate','gross_tertiary_education_enrollment_percent','gross_primary_education_enrollment_percent', 'life_expectancy')]
    rarr <- as.numeric(temp2)
    # Combine max, min, and actual values into a single data frame
    data_for_radar <- data.frame(rbind(c(100,100,100,100,100), rep(0, 5), rarr))
    colnames(data_for_radar) <- c('Birth Rate', 'Fertility Rate', 'Tertiary Education', 'Primary Education', 'Life Expectancy')
    radarchart(data_for_radar,
               seg = 6,
               title = paste("Key Indicators for", input$ds1features),
               pfcol = alpha("gray", 0.3),
               pcol = "blue",  
               plwd = 2,  
               cglcol = "grey",  
               cglty = 1,  
               axislabcol = "black",  
               vlcex = 0.8)
    },width=800,height=460)
  
    # output for Valuebox 1,2,3 in Country Overview
    temp3 <- reactive({
      df[df$country == input$ds1features, ]
    }) 
    output$infobox1 <- renderValueBox({
      valueBox(formatC(temp3()$population, format = "d", big.mark = ","),
              "Population",
              color = 'purple',
              # icon from glyphicon 
              icon = icon("child"))
    })
    output$infobox2 <- renderValueBox({
      valueBox(formatC(temp3()$land_area_km2, format = "d", big.mark = ","),
              "Land Area",
              color = 'purple',
              # icon from fontawesome
              icon= icon("location-dot", lib="font-awesome"))
    })
    output$infobox3 <- renderValueBox({
      valueBox(temp3()$capital,
               "Capital",
              color = 'purple',
              icon = icon("landmark"))
    })
    
    # output for plotly chart
    output$plotlychart <- renderPlotly({
      plot_ly(df,
              x = ~co2_emissions,
              y = ~forested_area_percent,
              type = "scatter",
              mode = "markers",
              hoverinfo = "text",
              text = paste("Forested Area: ", df$forested_area_percent, "%",
                           "<br>",
                           "CO2 Emission: ", df$co2_emissions, "tons"))
    })
    
   
    # output for health_bar in ds3
    health_df <- aggregate(cbind(life_expectancy, health_expenditure_oop) ~ region, data = df, mean, na.rm = TRUE)
  
    output$health_bar <- renderPlotly({
      plot_ly(data = health_df, x = ~region) %>%
        add_bars(y = ~health_expenditure_oop, name = "Health", marker = list(color = "blue")) %>%
        add_bars(y = ~life_expectancy, name = "Life", marker = list(color = "orange")) %>%
        layout(
          barmode = "group",
          title = "Health Expenditure and Life Expectancy by Region",
          xaxis = list(title = "Regions", showgrid = FALSE),
          yaxis = list(title = "Health Expenditure OOP (%)", showgrid = FALSE),
          yaxis2 = list(overlaying = "y", side = "right", title = "Life Expectancy (years)", showgrid = FALSE)) %>%
        add_trace(
          y = ~life_expectancy,
          name = "   ",
          marker = list(color = "rgba(255, 0, 0, 0)"),
          yaxis = "y2"
        )
    })
    
    # output for co2_map
    output$co2_map <- renderPlotly({
      plot_ly(df, 
              type='choropleth', 
              locations=df$iso_code, 
              z=df$co2_emissions, 
              text=df$country, 
              colorscale="Blues",
              reversescale = TRUE) %>%
        layout(
          title = "CO2 Emissions by Country",
          colorbar = list(title = "CO2 Emissions"),
          zmin = 0,  
          zmax = 1000000  
        )
    })
    
    # output for health_edu_regression
    output$health_edu_regression <- renderPlotly({
      lm_model <- linear_reg() %>% 
        set_engine('lm') %>% 
        set_mode('regression') %>%
        fit(as.formula(paste('life_expectancy ~', input$SI_he_regression)), data = df)
      # Prepare data for plotting
      x <- df[[input$SI_he_regression]]
      y <- df$life_expectancy
      x_range <- seq(min(x), max(x), length.out = 100)
      xdf <- data.frame(x_range)
      colnames(xdf) <- input$SI_he_regression
      ydf <- lm_model %>% predict(new_data = xdf)
      xy <- data.frame(xdf, ydf)

      # Create and return the plot
      plot_ly(df, x = ~get(input$SI_he_regression), y = ~life_expectancy, type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Data') %>%
        add_trace(data = xy, x = ~get(input$SI_he_regression), y = ~.pred, name = 'Regression Fit', mode = 'lines', alpha = 1)%>%
        layout(xaxis = list(title = input$SI_he_regression),
               yaxis = list(title = "Life Expectacy (years)"))
    })
    
    
    # output for co2_regression
    output$co2_regression <- renderPlotly({
      # Fit the linear model
      lm_model <- linear_reg() %>% 
        set_engine('lm') %>% 
        set_mode('regression') %>%
        fit(as.formula(paste('co2_emissions ~', input$SI_co2_regression)), data = df)
      # Prepare data for plotting
      x <- df[[input$SI_co2_regression]]
      y <- df$co2_emissions
      x_range <- seq(min(x), max(x), length.out = 100)
      xdf <- data.frame(x_range)
      colnames(xdf) <- input$SI_co2_regression
      ydf <- lm_model %>% predict(new_data = xdf)
      xy <- data.frame(xdf, ydf)

      # Create and return the plot
      plot_ly(df, x = ~get(input$SI_co2_regression), y = ~co2_emissions, type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Data') %>%
        add_trace(data = xy, x = ~get(input$SI_co2_regression), y = ~.pred, name = 'Regression Fit', mode = 'lines', alpha = 1) %>%
        layout(xaxis = list(title = input$SI_co2_regression),
               yaxis = list(title = "CO2 Emissions (tons)"))
    })
    
    # output for value boxes in co2
    output$co2_pvalue <- renderValueBox({
      # Fit the linear model
      lm_model <- linear_reg() %>% 
        set_engine('lm') %>% 
        set_mode('regression') %>%
        fit(as.formula(paste('co2_emissions ~', input$SI_co2_regression)), data = df)
      # Extract model summary
      model_summary <- summary(lm_model$fit)
      p_value <- coef(model_summary)[2, 4]
      if (p_value < 0.001){
        p_value2 <- format_scientific(p_value)
      } else {
        p_value2 <- round(p_value, 4)
      }
      valueBox(p_value2,
               "P Value",
               color = 'purple')
    })
    output$co2_rsquared <- renderValueBox({
      # Fit the linear model
      lm_model <- linear_reg() %>% 
        set_engine('lm') %>% 
        set_mode('regression') %>%
        fit(as.formula(paste('co2_emissions ~', input$SI_co2_regression)), data = df)
      # Extract model summary
      model_summary <- summary(lm_model$fit)
      r_squared <- model_summary$r.squared
      formatted_r_squared <- sprintf("%.4f", r_squared)
      valueBox(formatted_r_squared,
               "R Squared",
               color = 'purple')
    })
    
    # output for value boxes in health_edu
    output$he_pvalue <- renderValueBox({
      # Fit the linear model
      lm_model <- linear_reg() %>% 
        set_engine('lm') %>% 
        set_mode('regression') %>%
        fit(as.formula(paste('life_expectancy ~', input$SI_he_regression)), data = df)
      # Extract model summary
      model_summary <- summary(lm_model$fit)
      p_value <- coef(model_summary)[2, 4]
      if (p_value < 0.001){
        p_value2 <- format_scientific(p_value)
      } else {
        p_value2 <- round(p_value, 4)
      }
      valueBox(p_value2,
               "P Value",
               color = 'purple')
    })
    output$he_rsquared <- renderValueBox({
      # Fit the linear model
      lm_model <- linear_reg() %>% 
        set_engine('lm') %>% 
        set_mode('regression') %>%
        fit(as.formula(paste('life_expectancy ~', input$SI_he_regression)), data = df)
      # Extract model summary
      model_summary <- summary(lm_model$fit)
      r_squared <- model_summary$r.squared
      formatted_r_squared <- sprintf("%.4f", r_squared)
      valueBox(formatted_r_squared,
               "R Squared",
               color = 'purple')
    })
    
    # output for renewable_prod
    output$renewable_prod <- renderPlotly({
      req(input$SI_renewable_share) # to get the input from the local function
      
      data <- read.csv("modern-renewable-prod.csv")
      df_subset <- subset(data, select = -c(Code))
      df_subset <- subset(data, Year >= 2000 & Year <= 2022)
      df_subset <- df_subset %>%
        mutate(region = countrycode(Entity, origin = "country.name", destination = "region"))
      df <- na.omit(df_subset)
      filtered_df <- df[df$Entity == input$SI_renewable_share, ]
      # Aggregate the data for plotting
      new_df <- filtered_df %>%
        group_by(Year, Source) %>%
        summarize(Total_Electricity = sum(Electricity_Generation_TWh), .groups = 'drop')
      
      plot_ly(new_df, x = ~Year, y = ~Total_Electricity, color = ~Source, type = 'scatter', mode = 'lines') %>%
        layout(
          title = paste("Renewable energy generation for", input$SI_renewable_share, "by source, 2000-2022"),
          xaxis = list(title = "Year", showgrid = FALSE),
          yaxis = list(title = "Electric Generation (TWh)", showgrid = FALSE)
        )
    })
    
    # output for renewable_share
    output$renewable_share <- renderPlotly({
      req(input$SI_renewable_share) 
      
      # Filter data based on user selection
      filtered_data <- df_renew %>% filter(Entity == input$SI_renewable_share)
      
      plot_ly(filtered_data, x = ~Year, y = ~Renewables_percent, type = "scatter", mode = "lines", color = ~Entity) %>%
        layout(
          title = paste("Forecast of primary energy consumption (%) from renewable sources of",input$SI_renewable_share),
          xaxis = list(title = "Year", dtick = 1),
          yaxis = list(title = "Renewables Percentage"),
          shapes = list(
            list(
              type = "rect",
              fillcolor = "rgba(0,100,80,0.2)",
              layer = "below",
              line = list(width = 0),
              x0 = 2023,
              x1 = 2027,
              xref = "x",
              y0 = 0,
              y1 = 1,
              yref = "paper"
            )
          )
        )
    })
    
    # output for renewable_share_value
    output$renewable_share_value <- renderText({
      req(input$SI_renewable_share)
      filtered_data <- df_renew %>% filter(Entity == input$SI_renewable_share)
      paste("By 2027,",input$SI_renewable_share,"is expected to have", round((filtered_data[filtered_data$Year == 2027 & filtered_data$Entity == input$SI_renewable_share, "Renewables_percent"]),2),"% energy consumption from renewable sources.")
    })



}
# Run the dashboard
shinyApp(ui,server)


