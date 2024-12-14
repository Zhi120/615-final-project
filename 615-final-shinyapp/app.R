library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(dplyr)
library(WDI)
library(forecast)

# Data preparation
data <- WDI(
  country = c("CYP", "MLT"),
  indicator = c(
    "NY.GDP.MKTP.CD",     # GDP
    "SP.POP.TOTL",        # Population
    "ST.INT.ARVL",        # Visitors
    "FP.CPI.TOTL",        # Inflation Rate (Consumer Price Index)
    "NY.GDP.PCAP.CD"      # GDP per Capita
  ),
  start = 2000,
  end = 2022
) %>%
  rename(
    GDP = NY.GDP.MKTP.CD,
    Population = SP.POP.TOTL,
    Visitors = ST.INT.ARVL,
    InflationRate = FP.CPI.TOTL,
    IncomePerCapita = NY.GDP.PCAP.CD,
    Country = country,
    Year = year
  ) %>%
  mutate(
    Year = as.numeric(Year),
    GDP = as.numeric(GDP),
    Population = as.numeric(Population),
    Visitors = as.numeric(Visitors),
    InflationRate = as.numeric(InflationRate),
    IncomePerCapita = as.numeric(IncomePerCapita)
  ) %>%
  filter(!is.na(Year) & !is.na(GDP)) %>%
  distinct(Country, Year, .keep_all = TRUE)

# Extract data for Cyprus only
cyprus_data <- data %>%
  filter(Country == "Cyprus")

# UI section
ui <- fluidPage(
  tags$style(HTML("
    body {
      background-image: url('https://images.unsplash.com/photo-1534005647778-e5cfd1de5432?q=80&w=2564&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D');
      background-size: cover;
      background-attachment: fixed;
      background-repeat: no-repeat;
      background-position: center;
      font-family: 'Arial', sans-serif;
    }
    h1, h3 {
      color: #ffffff;
      text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.8);
    }
    .sidebar {
      background-color: rgba(255, 255, 255, 0.8);
      padding: 15px;
      border-radius: 10px;
    }
    .mainpanel {
      background-color: rgba(255, 255, 255, 0.8);
      padding: 20px;
      border-radius: 10px;
    }
  ")),
  
  titlePanel("Cyprus Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Analysis"),
      selectInput("variable", "Choose a Key Variable:",
                  choices = c("GDP" = "GDP",
                              "Population" = "Population",
                              "Visitors" = "Visitors",
                              "Inflation Rate" = "InflationRate",
                              "Income Per Capita" = "IncomePerCapita"),
                  selected = "GDP"),
      checkboxGroupInput("countries", "Select Countries for Comparison:",
                         choices = c("Cyprus", "Malta"),
                         selected = c("Cyprus", "Malta")),
      class = "sidebar"
    ),
    
    mainPanel(
      div(
        class = "mainpanel",
        tabsetPanel(
          tabPanel("General Description",
                   h3("About Cyprus"),
                   p("Cyprus is an island country located in the Eastern Mediterranean Sea, south of Turkey, west of Syria and Lebanon, northwest of Israel, and southeast of Greece."),
                   p("With a strategic location at the crossroads of Europe, Asia, and Africa, Cyprus has been a significant trade and cultural hub throughout history."),
                   
                   h4("Government"),
                   p("Cyprus is a presidential republic. The President of Cyprus serves as both the head of state and government. The island is divided into the internationally recognized Republic of Cyprus, which controls the southern two-thirds, and the Turkish Republic of Northern Cyprus, recognized only by Turkey, in the northern third."),
                   
                   h4("Economy"),
                   p("Cyprus has a developed, service-based economy. Tourism is a cornerstone of the economy, alongside shipping, financial services, and real estate. The country is a member of the European Union and uses the Euro as its currency."),
                   p("Cyprus is known for its low corporate tax rates, which attract international businesses, particularly in the finance and shipping industries."),
                   
                   h4("Population"),
                   p("The population of Cyprus is approximately 1.2 million people. The two main ethnic groups are Greek Cypriots, who make up about 77% of the population, and Turkish Cypriots, who make up around 18%. English is widely spoken, alongside Greek and Turkish."),
                   
                   h4("Natural Environment"),
                   p("Cyprus enjoys a Mediterranean climate with hot, dry summers and mild, wet winters. The island is known for its stunning beaches, mountain ranges such as the Troodos Mountains, and diverse flora and fauna."),
                   p("Endangered species such as the Mediterranean monk seal and green turtles nest along the island's coasts."),
                   
                   h4("History"),
                   p("Cyprus has a rich history spanning over 9,000 years, from the Neolithic period to modern times. It was influenced by various civilizations, including Mycenaean Greeks, Phoenicians, Assyrians, Egyptians, Persians, Romans, Byzantines, Crusaders, Venetians, Ottomans, and the British."),
                   p("The island gained independence from Britain in 1960, but tensions between Greek and Turkish Cypriots led to the division of the island in 1974. Efforts to reunify the island continue to this day."),
                   
                   h4("Cultural Heritage"),
                   p("Cyprus is home to numerous UNESCO World Heritage sites, such as the painted churches in the Troodos region and the archaeological remains of Paphos. The island's cuisine blends Greek, Middle Eastern, and Turkish influences, featuring dishes like halloumi cheese, souvlaki, and moussaka."),
                   
                   h4("Island Map"),
                   leafletOutput("islandMap", height = 400),
                   
                   h4("Global Location"),
                   leafletOutput("globalMap", height = 400)
          ),
          
          tabPanel("Trends of Key Variables",
                   h3("Key Variables for Cyprus"),
                   plotlyOutput("cyprusVariablePlot"),
                   uiOutput("trendsText")
                   
          ),
          
          tabPanel("Comparison with Other Islands",
                   h3("Comparison Analysis"),
                   plotlyOutput("comparisonPlot"),
                   uiOutput("comparisonText")
          ),
          
          tabPanel("Population Projection",
                   h3("Future Population of Cyprus"),
                   plotlyOutput("populationProjectionPlot"),
                   uiOutput("populationProjectionText") 
          ),
          
          tabPanel("SWOT Analysis",
                   h3("SWOT Analysis"),
                   h4("Strengths"),
                   tags$ul(
                     tags$li("Strategic location linking Europe, Asia, and Africa."),
                     tags$li("Thriving tourism industry."),
                     tags$li("Rich cultural heritage and historical sites.")
                   ),
                   h4("Weaknesses"),
                   tags$ul(
                     tags$li("Political division in the north."),
                     tags$li("Heavy reliance on tourism and services."),
                     tags$li("Limited natural resources.")
                   ),
                   h4("Opportunities"),
                   tags$ul(
                     tags$li("Renewable energy development, especially solar."),
                     tags$li("Enhanced economic cooperation within the EU."),
                     tags$li("Promotion of high-tech industries.")
                   ),
                   h4("Threats"),
                   tags$ul(
                     tags$li("Geopolitical tensions with neighboring countries."),
                     tags$li("Global economic downturn affecting tourism."),
                     tags$li("Climate change and rising sea levels.")
                   )
          ),
          
          tabPanel("Data Source",
                   h3("Data Source"),
                   p("The data used in this analysis was obtained from the World Bank's World Development Indicators (WDI) via the R package 'WDI'."),
                   p("More information about the WDI package can be found at this link: ",
                     a("https://cran.r-project.org/web/packages/WDI/WDI.pdf",
                       href = "https://cran.r-project.org/web/packages/WDI/WDI.pdf",
                       target = "_blank"))
          )
        )
      )
    )
  )
)

# Server section
server <- function(input, output) {
  
  # Map of Cyprus
  output$islandMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 33.3823, lat = 35.1264, zoom = 8) %>%
      addMarkers(lng = 33.3823, lat = 35.1264, popup = "Cyprus")
  })
  
  # Global location map
  output$globalMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 33.3823, lat = 35.1264, zoom = 2) %>%
      addMarkers(lng = 33.3823, lat = 35.1264, popup = "Cyprus")
  })
  
  # Cyprus Variable Plot
  output$cyprusVariablePlot <- renderPlotly({
    p <- ggplot(cyprus_data, aes(x = Year, y = get(input$variable))) +
      geom_line(color = "blue", size = 1) +
      labs(
        title = paste("Cyprus", input$variable, "Trends"),
        x = "Year",
        y = input$variable
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Trends Text Analysis
  output$trendsText <- renderUI({
    variable <- input$variable
    text <- switch(variable,
                   "GDP" = "Cyprus' GDP has shown consistent growth over the years, driven by strong performance in tourism, shipping, and financial services. The economy's resilience is also reflected in its recovery from global economic crises.",
                   "Population" = "Cyprus' population has remained stable, with slight growth over the years. This stability is due to a balanced birth rate and immigration trends, supporting the country's economic and social infrastructure.",
                   "Visitors" = "Tourism is a cornerstone of Cyprus' economy. The data shows a steady increase in visitors, with notable spikes during global travel booms, reflecting the island's popularity as a Mediterranean destination.",
                   "InflationRate" = "Cyprus' inflation rate has fluctuated in response to global economic conditions. Periods of higher inflation often coincide with rising energy prices and import costs, given the island's reliance on external supplies.",
                   "IncomePerCapita" = "Income per capita in Cyprus reflects steady economic development. Growth is largely supported by high-value industries such as financial services, tourism, and real estate.")
    HTML(paste("<div style='padding-top: 10px;'><strong>Analysis:</strong> ", text, "</div>"))
  })
  
  # Comparison Plot
  output$comparisonPlot <- renderPlotly({
    comparison_data <- data %>%
      filter(Country %in% input$countries) %>%
      filter(!is.na(get(input$variable))) %>%
      arrange(Year)
    p <- ggplot(comparison_data, aes(x = Year, y = get(input$variable), color = Country)) +
      geom_line(size = 1) +
      labs(
        title = paste("Comparison of", input$variable),
        x = "Year",
        y = input$variable,
        color = "Country"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Comparison Text Analysis
  output$comparisonText <- renderUI({
    variable <- input$variable
    text <- switch(variable,
                   "GDP" = "The GDP comparison reveals that both Cyprus and Malta have experienced significant growth over the years. Cyprus generally maintains a higher GDP, reflecting its larger population and a more diversified economy, which includes sectors like tourism, shipping, and financial services. Malta, on the other hand, has shown rapid growth in recent years due to its thriving technology and gaming industries, catching up steadily.",
                   "Population" = "Cyprus has a relatively larger and more stable population compared to Malta. While both countries have seen only modest changes in population over the years, Cyprus' larger geographic size and economic structure support a higher population. Malta's smaller land area and reliance on international migration have kept its population comparatively lower.",
                   "Visitors" = "Tourism is a cornerstone of both Cyprus and Malta's economies. Cyprus consistently attracts more visitors, leveraging its larger size, diverse attractions, and rich historical sites. Malta, while smaller, has carved a niche in luxury and heritage tourism, drawing a steady influx of high-value travelers.",
                   "InflationRate" = "Inflation rates in Cyprus and Malta show fluctuations over time, often in response to global economic conditions and local fiscal policies. Cyprus' inflation has historically been impacted by its dependence on imported goods and energy prices. Malta, with its smaller domestic market, has experienced periods of sharper inflation due to supply chain constraints.",
                   "IncomePerCapita" = "Income per capita highlights the economic transformations of both countries. Malta's rapid growth in the financial, technology, and iGaming sectors has led to significant increases in income per capita. Cyprus, with its larger population base, has maintained steady growth in per capita income, driven by its strong service and tourism sectors."
    )
    HTML(paste("<div style='padding-top: 10px;'><strong>Analysis:</strong> ", text, "</div>"))
  })
  
  # Population Projection
  output$populationProjectionPlot <- renderPlotly({
    population_ts <- ts(cyprus_data$Population, start = min(cyprus_data$Year), frequency = 1)
    population_forecast <- forecast(auto.arima(population_ts), h = 5)
    
    forecast_df <- data.frame(
      Year = seq(max(cyprus_data$Year) + 1, by = 1, length.out = 5),
      Population = as.numeric(population_forecast$mean)
    )
    
    combined_data <- rbind(
      cyprus_data %>% select(Year, Population),
      forecast_df
    )
    
    p <- ggplot(combined_data, aes(x = Year, y = Population)) +
      geom_line(color = "blue", size = 1) +
      geom_point(data = forecast_df, aes(x = Year, y = Population), color = "red") +
      labs(
        title = "Cyprus Population Projection",
        x = "Year",
        y = "Population"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$populationProjectionText <- renderUI({
    HTML(
      "<div style='padding-top: 10px;'>
      <strong>Analysis:</strong> 
      The population projection for Cyprus was conducted using an ARIMA (Auto-Regressive Integrated Moving Average) time series model. This method analyzes and forecasts data based on historical trends while automatically determining optimal parameters for the model. The data used in this projection spans from 2000 to 2022.
      <br><br>
      ARIMA has been particularly useful in identifying trends in historical population data, showing a consistent growth pattern in Cyprus. The projected values for the next five years suggest a steady increase in population, driven by positive natural growth rates where the birth rate has consistently exceeded the death rate. Socioeconomic stability has also contributed to this gradual rise.
      <br><br>
      While these projections provide valuable insights for resource allocation and policy planning, it is essential to note their limitations. The model assumes that no significant disruptions, such as economic crises or natural disasters, will occur in the near future. Hence, actual outcomes may vary based on unforeseen factors.
    </div>"
    )
  })
  
  
}

# Run Shiny App
shinyApp(ui = ui, server = server)


