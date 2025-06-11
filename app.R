# --- LIBRARIES ---
library(shiny)
library(plotly)
library(readr)
library(dplyr)
library(tidyr)
library(shinythemes)
library(zoo)

# --- LOAD ALL DATA ---
citycpi     <- read_csv("data/cpiPercentageChange.csv", skip = 1)
goodsserv   <- read_csv("data/cpiGoodsServices.csv", skip = 1)
groceries   <- read_csv("data/groceriesMovements.csv", skip = 1)
rent        <- read_csv("data/rentMovements.csv", skip = 1)
education   <- read_csv("data/educationMovements.csv", skip = 1)
services    <- read_csv("data/servicesAnnualMovement.csv", skip = 1)
weightedcpi <- read_csv("data/weightedCpi.csv", skip = 1)


# --- DATA PREPARATION ---
goods_timeseries <- goodsserv %>%
  select(Period = 1, Change = 2) %>%
  filter(!is.na(Change)) %>%
  mutate(Date = as.Date(as.yearqtr(Period, format = "%b-%y"), frac = 1)) %>%
  arrange(Date) %>%
  mutate(Period = factor(Period, levels = Period))

rent_timeseries <- rent %>%
  select(Period = 1, Change = 2) %>%
  filter(!is.na(Change)) %>%
  mutate(Date = as.Date(as.yearqtr(Period, format = "%b-%y"), frac = 1)) %>%
  arrange(Date) %>%
  mutate(Period = factor(Period, levels = Period))

education_timeseries <- education %>%
  select(Period = 1, Change = 2) %>%
  filter(!is.na(Change)) %>%
  mutate(Date = as.Date(as.yearqtr(Period, format = "%b-%y"), frac = 1)) %>%
  arrange(Date) %>%
  mutate(Period = factor(Period, levels = Period))

latest_city_cpi <- citycpi %>%
  head(1) %>% select(-1) %>%
  pivot_longer(everything(), names_to = "City", values_to = "Inflation") %>%
  mutate(Color = ifelse(City == "Weighted average of eight capital cities", "#d9534f", "#5bc0de")) %>%
  mutate(City = factor(City, levels = City[order(Inflation)]))

weightedcpi_long <- weightedcpi %>%
  rename(Year = `...1`) %>%
  pivot_longer(-Year, names_to = "Quarter_str", values_to = "CPI_Index") %>% filter(!is.na(CPI_Index)) %>%
  mutate(Quarter = sub(" no.", "", sub(" .*", "", Quarter_str))) %>%
  mutate(Label = paste(Year, Quarter)) %>%
  mutate(Label = factor(Label, levels = unique(Label[order(Year, match(Quarter, c("March", "June", "September", "December")))]), ordered = TRUE))

groceries_long <- groceries %>%
  rename(Category = `...1`) %>%
  pivot_longer(-Category, names_to = "Quarter_str", values_to = "PercentChange") %>%
  filter(!is.na(PercentChange)) %>%
  mutate(
    Quarter_clean = sub(" .*", "", Quarter_str),
    Date = as.Date(as.yearqtr(Quarter_clean, format = "%b-%y"))
  )


# --- UI ---
ui <- navbarPage(
  title = "Australia's Inflation Story",
  theme = shinytheme("cosmo"),
  
  # --- MODIFIED: Added footer style and adjusted main content height ---
  tags$head(
    tags$style(HTML("
      html, body {
        overflow: hidden;
        height: 100%;
        margin: 0;
        padding: 0;
      }
      .custom-sidebar {
        float: left;
        width: 30%;
        /* Adjusted height to make space for the footer */
        height: 85vh; 
        padding: 20px;
        box-sizing: border-box;
      }
      .custom-main {
        float: left;
        width: 70%;
        /* Adjusted height to make space for the footer */
        height: 85vh;
        padding: 10px;
        box-sizing: border-box;
      }
      /* NEW: CSS for the footer */
      .footer {
        position: fixed;
        bottom: 0;
        width: 100%;
        height: 40px; /* Adjust height as needed */
        line-height: 40px; /* Vertically center text */
        text-align: center;
        font-size: 0.9em;
        color: #6c757d;
        background-color: #f8f9fa;
        border-top: 1px solid #dee2e6;
        z-index: 1000; /* Ensure it's on top */
      }
    "))
  ),
  
  # --- TAB PANELS ---
  # (No changes to the content of the tab panels)
  tabPanel("The National Picture", div(class = "custom-sidebar", h3("The Big Picture"), p("This dashboard provides a snapshot of Australian inflation."), p("The bar chart shows the most recent annual inflation for each capital city, with the national average highlighted in red. The line chart tracks the long-term price level (CPI), showing the cumulative impact of inflation over time."), em("The Consumer Price Index (CPI) measures the average change in prices paid by households for a basket of goods and services.")), div(class = "custom-main", plotlyOutput("cityCpiBarPlot", height = "48%"), plotlyOutput("cumulativeCpiPlot", height = "48%"))),
  tabPanel("Driving Forces", div(class = "custom-sidebar", h3("What's Behind the Numbers?"), p("Inflation is driven by price changes in both goods and services. The top chart compares their most recent impact."), hr(), p("We feel inflation most in essential costs. Select a category below to see the latest annual price change."), selectInput("essentials_category", "Choose an essential category:", choices = c("Rent", "Education"))), div(class = "custom-main", plotlyOutput("goodsServicesBarPlot", height = "48%"), plotlyOutput("essentialsPlot", height = "48%"))),
  tabPanel("At The Checkout", div(class = "custom-sidebar", h3("The Grocery Bill"), p("No part of the household budget is more exposed to weekly price changes than groceries."), p("Select a food category to see how its price has changed over time compared to the overall cost of food."), selectInput("grocery_category", "Choose a food category:", choices = unique(groceries_long$Category), selected = "Food and non-alcoholic beverages")), div(class = "custom-main", plotlyOutput("groceryTimeSeriesPlot", height = "98%"))),
  
  # --- NEW: Footer UI element ---
  footer = div(
    class = "footer",
    "Data Source: ",
    a(href = "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release", 
      "Australian Bureau of Statistics (ABS)", 
      target = "_blank") # Opens the link in a new tab
  )
)

# --- SERVER ---
# (No changes to the server logic)
server <- function(input, output, session) {
  
  create_bar_layout <- function(p, y_title, x_title, show_legend = FALSE) {
    p %>% layout(
      yaxis = list(title = y_title, showgrid = FALSE, zeroline = FALSE, tickfont = list(size = 10)),
      xaxis = list(title = x_title, showgrid = TRUE, zeroline = FALSE),
      plot_bgcolor = 'rgba(245, 245, 245, 0.95)', paper_bgcolor = 'rgba(0,0,0,0)',
      margin = list(l = 100, t = 50, b = 50), showlegend = show_legend
    )
  }
  
  create_line_layout <- function(p, title, y_title) {
    p %>% layout(title = title, yaxis = list(title = y_title, showgrid = TRUE), xaxis = list(title = "Quarter", type = 'date', tickformat = "%b-%Y", showgrid = FALSE), plot_bgcolor = 'rgba(245, 245, 245, 0.95)', paper_bgcolor = 'rgba(0,0,0,0)', legend = list(orientation = 'h', y = -0.2, x = 0.5, xanchor = 'center'))
  }
  
  output$cityCpiBarPlot <- renderPlotly({
    plot_ly(data = latest_city_cpi, y = ~City, x = ~Inflation, type = 'bar', orientation = 'h', marker = list(color = ~Color)) %>%
      create_bar_layout(y_title = "", x_title = "Most Recent Annual Inflation (%)") %>% layout(title = "Inflation by Capital City")
  })
  
  output$cumulativeCpiPlot <- renderPlotly({
    plot_ly(data = weightedcpi_long, x = ~Label, y = ~CPI_Index, type = 'scatter', mode = 'lines', line = list(color = "#d9534f", width = 3)) %>%
      layout(title = "Long-Term Consumer Price Index (CPI)", yaxis = list(title = "CPI Index", showgrid = TRUE), xaxis = list(title = "", showgrid = FALSE, tickangle = -45, nticks = 15), plot_bgcolor = 'rgba(245, 245, 245, 0.95)', paper_bgcolor = 'rgba(0,0,0,0)', margin = list(t = 50))
  })
  
  output$goodsServicesBarPlot <- renderPlotly({
    plot_ly(data = goods_timeseries, y = ~Period, x = ~Change, type = 'bar', orientation = 'h', marker = list(color = "#5bc0de")) %>%
      create_bar_layout(y_title = "", x_title = "Annual Change (%)") %>%
      layout(title = "Goods vs. Services Inflation (Time Series)")
  })
  
  output$essentialsPlot <- renderPlotly({
    if (input$essentials_category == "Rent") {
      plot_ly(data = rent_timeseries, y = ~Period, x = ~Change, type = 'bar', orientation = 'h', marker = list(color = "#5bc0de")) %>%
        create_bar_layout(y_title = "", x_title = "Annual Change (%)") %>%
        layout(title = "Annual Change in Rent")
    } else { # Education
      plot_ly(data = education_timeseries, y = ~Period, x = ~Change, type = 'bar', orientation = 'h', marker = list(color = "#5bc0de")) %>%
        create_bar_layout(y_title = "", x_title = "Annual Change (%)") %>%
        layout(title = "Annual Change in Education Costs")
    }
  })
  
  output$groceryTimeSeriesPlot <- renderPlotly({
    req(input$grocery_category)
    plot_data <- groceries_long %>% filter(Category %in% c(input$grocery_category, "Food and non-alcoholic beverages"))
    plot_ly(data = plot_data, x = ~Date, y = ~PercentChange, color = ~Category, type = 'scatter', mode = 'lines+markers', colors = "Set1") %>%
      create_line_layout("Annual Change in Grocery Prices", "Annual Change (%)")
  })
}

# --- RUN APP ---
shinyApp(ui, server)