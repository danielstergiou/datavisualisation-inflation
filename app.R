library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinythemes)
library(DT)

# 2. Load CSV datasets
insider_trades  <- read_csv("data/insiderTrades.csv")
stock_prices    <- read_csv("data/stockPrices.csv")
congress_trades <- read_csv("data/congressTrades.csv")

# 3. Define UI
ui <- navbarPage(
  title = "Insider Trading Dashboard",
  theme = shinytheme("flatly"),
  
  # -----------------
  # Tab 1: TRENDS
  # -----------------
  tabPanel(
    "Trends",
    
    # Narrative (full width)
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("Story: Which Years & Sectors Led the Way?"),
          p(HTML(
            paste0(
              "From our sample data, <strong>2020</strong> saw a notable healthcare‐sector buy (JNJ), ",
              "but beginning in <strong>2021</strong>, technology dominates the insider‐trading landscape. ",
              "By <strong>2022</strong>, tech insiders (AAPL) were both buying and selling heavily, ",
              "and <strong>2023</strong> continued with a large MSFT buy. Technology clearly drives recent activity."
            )
          ))
        )
      )
    ),
    # Spacer
    div(style = "height: 20px;"),
    
    # Two side‐by‐side plots (each in a wellPanel)
    fluidRow(
      column(
        width = 6,
        wellPanel(
          h5("Total Insider Trades by Year"),
          plotlyOutput("bar_cases_by_year", height = "300px")
        )
      ),
      column(
        width = 6,
        wellPanel(
          h5("Insider Trades Over Time (by Sector)"),
          plotlyOutput("line_cases_by_sector", height = "300px")
        )
      )
    ),
    # Spacer before footer
    div(style = "height: 40px;"),
    # Footer text (not fixed; just below content)
    div(
      class = "footer-text",
      p(
        "Sources: insiderTrades.csv · SEC EDGAR",
        style = "font-size: 12px; color: #777; text-align: center; margin-bottom: 20px;"
      )
    )
  ),
  
  
  # -----------------
  # Tab 2: INSIDER ACTIVITY
  # -----------------
  tabPanel(
    "Insider Activity",
    
    # Narrative (full width)
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("Story: Reading Between Buy/Sell Lines"),
          p(HTML(
            paste0(
              "For example, <em>John Doe (CEO)</em> bought 5,000 AAPL shares on 2022-05-01 at $145.23—",
              "right before prices climbed to $150.75 when <em>Jane Smith (CFO)</em> sold 2,000 AAPL on 2022-05-15. ",
              "Tech insiders like <em>Bob Lee (VP)</em> bought MSFT in early 2023, while <em>Alice King (Director)</em> sold GOOG in late 2021. ",
              "We’ll track how these decisions aligned with stock prices below."
            )
          ))
        )
      )
    ),
    div(style = "height: 20px;"),
    
    # Table on left, Price chart on right
    fluidRow(
      column(
        width = 6,
        wellPanel(
          h5("Individual Insider Trades Table"),
          DTOutput("table_insider_trades")
        )
      ),
      column(
        width = 6,
        wellPanel(
          h5("Stock Price with Trade Markers"),
          plotlyOutput("price_chart", height = "350px")
        )
      )
    ),
    div(style = "height: 20px;"),
    
    # Buy vs Sell bar chart (full width)
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h5("Buy vs Sell Volume"),
          plotlyOutput("bar_buy_sell", height = "300px")
        )
      )
    ),
    div(style = "height: 40px;"),
    
    # Footer text
    div(
      class = "footer-text",
      p(
        "Sources: insiderTrades.csv · stockPrices.csv",
        style = "font-size: 12px; color: #777; text-align: center; margin-bottom: 20px;"
      )
    )
  ),
  
  
  # -----------------
  # Tab 3: CONGRESS TRADES
  # -----------------
  tabPanel(
    "Congress Trades",
    
    # Narrative (full width)
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("Story: Did Politicians Trade Ahead of Major Events?"),
          p(HTML(
            paste0(
              "In early 2020, before the COVID-19 market drop, <em>Nancy Pelosi</em> bought 100 AAPL shares (02-01-2020). ",
              "Soon after, <em>Richard Burr</em> sold 50 MSFT shares on 03-10-2020. ",
              "By mid-2021, <em>Josh Hawley</em> bought GOOG (06-15-2021), while <em>Maxine Waters</em> sold AMZN (09-30-2021). ",
              "In late 2022, <em>Pat Toomey</em> bought TSLA—suggesting ongoing political confidence in tech names."
            )
          ))
        )
      )
    ),
    div(style = "height: 20px;"),
    
    # Table and line chart side‐by‐side
    fluidRow(
      column(
        width = 6,
        wellPanel(
          h5("Congress Insider Trades Table"),
          DTOutput("table_congress_trades")
        )
      ),
      column(
        width = 6,
        wellPanel(
          h5("Congress Trades Per Year"),
          plotlyOutput("line_congress_trades_per_year", height = "300px")
        )
      )
    ),
    div(style = "height: 40px;"),
    
    # Footer text
    div(
      class = "footer-text",
      p(
        "Sources: congressTrades.csv · STOCK Act Database",
        style = "font-size: 12px; color: #777; text-align: center; margin-bottom: 20px;"
      )
    )
  )
) # end navbarPage


# 4. Define Server
server <- function(input, output, session) {
  
  # — Tab 1: Trends —
  output$bar_cases_by_year <- renderPlotly({
    df_year <- insider_trades %>%
      mutate(year = if_else(!is.na(Year), Year, year(as_date(Date)))) %>%
      count(year) %>%
      arrange(year)
    
    p <- ggplot(df_year, aes(x = factor(year), y = n)) +
      geom_col(fill = "#2C3E50") +
      labs(
        x = "Year",
        y = "Number of Trades"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11)
      )
    
    ggplotly(p) %>%
      layout(
        margin = list(l = 50, r = 20, t = 20, b = 50),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Trades")
      )
  })
  
  output$line_cases_by_sector <- renderPlotly({
    df_sector <- insider_trades %>%
      mutate(year = if_else(!is.na(Year), Year, year(as_date(Date)))) %>%
      group_by(year, Sector) %>%
      tally(name = "count") %>%
      ungroup()
    
    p <- ggplot(df_sector, aes(x = year, y = count, color = Sector)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        x = "Year",
        y = "Number of Trades"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12)
      )
    
    ggplotly(p) %>%
      layout(
        margin = list(l = 50, r = 20, t = 20, b = 50),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Trades")
      )
  })
  
  
  # — Tab 2: Insider Activity —
  output$table_insider_trades <- renderDT({
    insider_trades %>%
      select(Date, Ticker, Insider, Title, Trade_Type, Shares, Price, Sector, Year) %>%
      arrange(desc(Date))
  }, options = list(pageLength = 8, autoWidth = TRUE))
  
  output$price_chart <- renderPlotly({
    df_plot <- insider_trades %>%
      left_join(stock_prices, by = c("Date", "Ticker")) %>%
      arrange(Date)
    
    p <- ggplot(df_plot, aes(x = as_date(Date), y = Close)) +
      geom_line(color = "#7F8C8D") +
      geom_point(aes(color = Trade_Type), size = 2) +
      labs(
        x = "Date",
        y = "Close Price",
        color = "Trade Type"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
    
    ggplotly(p) %>%
      layout(
        margin = list(l = 50, r = 20, t = 20, b = 50),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Price")
      )
  })
  
  output$bar_buy_sell <- renderPlotly({
    df_bs <- insider_trades %>%
      filter(Trade_Type %in% c("Buy", "Sell")) %>%
      count(Trade_Type)
    
    p <- ggplot(df_bs, aes(x = Trade_Type, y = n, fill = Trade_Type)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("Buy" = "#27AE60", "Sell" = "#C0392B")) +
      labs(
        x = "",
        y = "Count"
      ) +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)
      )
    
    ggplotly(p) %>%
      layout(
        margin = list(l = 50, r = 20, t = 20, b = 50),
        xaxis = list(title = ""),
        yaxis = list(title = "Count")
      )
  })
  
  
  # — Tab 3: Congress Trades —
  output$table_congress_trades <- renderDT({
    congress_trades %>%
      select(Date, Member, Ticker, TransactionType, Shares, Value) %>%
      arrange(desc(Date))
  }, options = list(pageLength = 8, autoWidth = TRUE))
  
  output$line_congress_trades_per_year <- renderPlotly({
    df_ctyr <- congress_trades %>%
      mutate(year = year(as_date(Date))) %>%
      count(year) %>%
      arrange(year)
    
    p <- ggplot(df_ctyr, aes(x = year, y = n)) +
      geom_line(color = "#E74C3C", size = 1) +
      geom_point(color = "#E74C3C", size = 2) +
      labs(
        x = "Year",
        y = "Number of Trades"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12)
      )
    
    ggplotly(p) %>%
      layout(
        margin = list(l = 50, r = 20, t = 20, b = 50),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Trades")
      )
  })
}

# 5. Run the app
shinyApp(ui, server)


