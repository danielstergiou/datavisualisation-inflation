library(shiny)
library(plotly)
library(readr)
library(dplyr)
library(tidyr)
library(shinythemes)
library(zoo)

citycpi <- read_csv("data/cpiPercentageChange.csv", skip = 1)
goodsserv <- read_csv("data/cpiGoodsServices.csv", skip = 1)
groceries <- read_csv("data/groceriesMovements.csv", skip = 1)
rent <- read_csv("data/rentMovements.csv", skip = 1)
education <- read_csv("data/educationMovements.csv", skip = 1)
weightedcpi <- read_csv("data/weightedCpi.csv", skip = 1)

prepare_timeseries_data <- function(data) {
  data %>%
    select(Period = 1, Change = 2) %>%
    filter(!is.na(Change)) %>%
    mutate(Date = as.Date(as.yearqtr(Period, format = "%b-%y"), frac = 1)) %>%
    arrange(Date) %>%
    mutate(Period = factor(Period, levels = unique(Period)))
}

groceries_long <- groceries %>%
  rename(Category = ...1) %>%
  pivot_longer(-Category, names_to = "Quarter_str", values_to = "PercentChange") %>%
  filter(!is.na(PercentChange)) %>%
  mutate(
    Date = as.Date(as.yearqtr(sub(" .*", "", Quarter_str), format = "%b-%y"))
  )

app_css <- "
  html, body { overflow: hidden; height: 100%; margin: 0; padding: 0; }
  .custom-sidebar { float: left; width: 30%; height: 85vh; padding: 20px; box-sizing: border-box; }
  .custom-main { float: left; width: 70%; height: 85vh; padding: 10px; box-sizing: border-box; }
  .footer {
    position: fixed; bottom: 0; width: 100%; height: 40px; line-height: 40px;
    text-align: center; font-size: 0.9em; color: #6c757d;
    background-color: #f8f9fa; border-top: 1px solid #dee2e6; z-index: 1000;
  }
"

ui <- navbarPage(
  title = "Australia's Inflation Story",
  theme = shinytheme("cosmo"),
  
  tags$head(tags$style(HTML(app_css))),
  
  footer = div(
    class = "footer",
    "Data Source: ",
    a(href = "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release", 
      "Australian Bureau of Statistics (ABS)", 
      target = "_blank")
  ),
  
  tabPanel("The National Picture",
           div(class = "custom-sidebar",
               h3("The Big Picture"),
               p("This dashboard provides a snapshot of Australian inflation."),
               p("The bar chart shows the most recent annual inflation for each capital city, with the national average highlighted in red. The line chart tracks the long-term price level (CPI), showing the cumulative impact of inflation over time."),
               p("The Consumer Price Index (CPI) is Australias key measure for inflation. The Australian Beauru of Statistics defines it as..."),
               hr(),
               em('"An important economic indicator that measures the change in the price of a "basket" of goods and services, which account for expenditure by households in capital cities."'),
               hr(),
               br(),
               p("It covers 11 categories of Goods and Services:"),
               tags$ul(
                 tags$li("Food and non-alcoholic beverages"),
                 tags$li("Alcohol and tobacco"),
                 tags$li("Clothing and footwear"),
                 tags$li("Housing"),
                 tags$li("Furnishings, household equipment and services"),
                 tags$li("Health"),
                 tags$li("Transport"),
                 tags$li("Communication"),
                 tags$li("Recreation and culture"),
                 tags$li("Education"),
                 tags$li("Insurance and financial services")
               )
           ),
           div(class = "custom-main",
               plotlyOutput("cityCpiBarPlot", height = "48%"),
               plotlyOutput("cumulativeCpiPlot", height = "48%")
           )
  ),
  
  tabPanel("Driving Forces",
           div(class = "custom-sidebar",
               h3("What's Behind the Numbers?"),
               p("Inflation is driven by price changes in both goods and services. The top chart shows a time-series of these changes."),
               hr(),
               p("We also feel inflation in essential costs. Select a category below to see its price trend."),
               selectInput("essentials_category", "Choose an essential category:", choices = c("Rent", "Education"))
           ),
           div(class = "custom-main",
               plotlyOutput("goodsServicesBarPlot", height = "48%"),
               plotlyOutput("essentialsPlot", height = "48%")
           )
  ),
  
  tabPanel("At The Checkout",
           div(class = "custom-sidebar",
               h3("The Grocery Bill"),
               p("No part of the household budget is more exposed to weekly price changes than groceries."),
               p("Its becoming harder to live on a standard budget and still get the food you once had gotten."),
               p("Although, Michael Harvey, a senior food retail analyst from Rabobank states:"),
               hr(),
               em('"We don’t see another wave of price increases coming through, but there’s also no sign of decreases; there’s not a lot of downward pressure,"'),
               br(),
               br(),
               em('"Cost of living pressure is not new, and it’s ongoing. Consumers will need to continue to respond to the high food prices in terms of their purchasing decisions."'),
               hr(),
               p("Select a food category to see how its price has changed over time compared to the overall cost of food."),
               selectInput("grocery_category", "Choose a food category:",
                           choices = unique(groceries_long$Category),
                           selected = "Food and non-alcoholic beverages")
           ),
           div(class = "custom-main",
               plotlyOutput("groceryTimeSeriesPlot", height = "98%")
           )
  )
)

server <- function(input, output, session) {
  create_layout <- function(p, plot_title, y_title, x_title = "", show_legend = FALSE) {
    p %>% layout(
      title = plot_title,
      yaxis = list(title = y_title, showgrid = FALSE, zeroline = FALSE, tickfont = list(size = 10)),
      xaxis = list(title = x_title, showgrid = TRUE, zeroline = FALSE),
      plot_bgcolor = 'rgba(245, 245, 245, 0.95)', 
      paper_bgcolor = 'rgba(0,0,0,0)',
      margin = list(l = 100, t = 50, b = 50), 
      showlegend = show_legend,
      legend = list(orientation = 'h', y = -0.2, x = 0.5, xanchor = 'center')
    )
  }
  output$cityCpiBarPlot <- renderPlotly({
    plot_data <- citycpi %>%
      head(1) %>%
      select(-1) %>%
      pivot_longer(everything(), names_to = "City", values_to = "Inflation") %>%
      mutate(Color = ifelse(City == "Weighted average of eight capital cities", "#d9534f", "#5bc0de")) %>%
      mutate(City = factor(City, levels = City[order(Inflation)]))
    
    plot_ly(data = plot_data, y = ~City, x = ~Inflation, type = 'bar', orientation = 'h', marker = list(color = ~Color)) %>%
      create_layout("Inflation by Capital City", y_title = "", x_title = "Most Recent Annual Inflation (%)")
  })
  output$cumulativeCpiPlot <- renderPlotly({
    plot_data <- weightedcpi %>%
      rename(Year = ...1) %>%
      pivot_longer(-Year, names_to = "Quarter_str", values_to = "CPI_Index") %>%
      filter(!is.na(CPI_Index)) %>%
      mutate(Quarter = sub(" .*", "", Quarter_str)) %>%
      mutate(Label = paste(Year, Quarter)) %>%
      mutate(Label = factor(Label, levels = unique(Label[order(Year, match(Quarter, c("March", "June", "September", "December")))]), ordered = TRUE))
    
    plot_ly(data = plot_data, x = ~Label, y = ~CPI_Index, type = 'scatter', mode = 'lines', line = list(color = "#d9534f", width = 3)) %>%
      create_layout("Long-Term Consumer Price Index (CPI)", y_title = "CPI Index") %>%
      layout(xaxis = list(showgrid = FALSE, tickangle = -45, nticks = 15))
  })
  output$goodsServicesBarPlot <- renderPlotly({
    plot_data <- prepare_timeseries_data(goodsserv)
    
    plot_ly(data = plot_data, y = ~Period, x = ~Change, type = 'bar', orientation = 'h', marker = list(color = "#5bc0de")) %>%
      create_layout("Goods Inflation (Time Series)", y_title = "", x_title = "Annual Change (%)")
  })
  output$essentialsPlot <- renderPlotly({
    if (input$essentials_category == "Rent") {
      data_to_plot <- prepare_timeseries_data(rent)
      plot_title <- "Annual Change in Rent"
    } else {
      data_to_plot <- prepare_timeseries_data(education)
      plot_title <- "Annual Change in Education Costs"
    }
    plot_ly(data = data_to_plot, y = ~Period, x = ~Change, type = 'bar', orientation = 'h', marker = list(color = "#5bc0de")) %>%
      create_layout(plot_title, y_title = "", x_title = "Annual Change (%)")
  })
  
  output$groceryTimeSeriesPlot <- renderPlotly({
    req(input$grocery_category)
    plot_data <- groceries_long %>%
      filter(Category %in% c(input$grocery_category, "Food and non-alcoholic beverages"))
    plot_ly(data = plot_data, x = ~Date, y = ~PercentChange, color = ~Category, type = 'scatter', mode = 'lines+markers', colors = "Set1") %>%
      create_layout("Annual Change in Grocery Prices", "Annual Change (%)", show_legend = TRUE) %>%
      layout(xaxis = list(type = 'date', tickformat = "%b-%Y", showgrid = FALSE))
  })
}

shinyApp(ui, server)