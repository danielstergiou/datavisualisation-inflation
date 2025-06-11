library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(lubridate)
library(DT)

#–– DATA LOADING & PREP ––
# Congress trades
data_congress <- read_excel("data/congress-trading-all.xlsx") %>%
  mutate(
    Traded = as_date(Traded),
    excess_return = as.numeric(excess_return)
  )

# Average excess per trade day
daily_cong <- data_congress %>%
  group_by(Traded) %>%
  summarise(AvgExcess = mean(excess_return, na.rm = TRUE))

# S&P 500 historical
sp500 <- read_csv("data/sp500historicalData.csv", show_col_types = FALSE) %>%
  mutate(
    Date = mdy(Date)
  ) %>%
  arrange(Date) %>%
  mutate(
    DailyReturn = (Close / lag(Close) - 1) * 100
  )

# Build full time series: fill non-trade days, combine returns
full_ts <- sp500 %>%
  left_join(daily_cong, by = c("Date" = "Traded")) %>%
  replace_na(list(AvgExcess = 0)) %>%
  mutate(
    # Strategy daily = market + excess
    StrategyReturn = DailyReturn + AvgExcess,
    CumSP     = 100 * cumprod(1 + DailyReturn/100),
    CumCong   = 100 * cumprod(1 + StrategyReturn/100)
  )

#–– UI HELPERS ––
pageFooter <- function(txt) {
  div(class = "page-footer", tags$hr(), tags$em(txt))
}
pageHeader <- function(title, desc) {
  tagList(h2(title), p(desc), tags$hr())
}

#–– UI ––
ui <- navbarPage("Insider Trading Stories",
                 tabPanel("Congress Trades",
                          fluidPage(
                            pageHeader(
                              "Lawmakers’ Cumulative Returns vs Market",
                              "Strategy = S&P 500 return plus average excess return from congressional trades (start = 100)."
                            ),
                            plotOutput("plot_congress", height = "400px"),
                            DTOutput("table_congress"),
                            pageFooter("Data Source: QuiverQuant & Public Filings")
                          )
                 ),
                 tabPanel("Executive Timing", fluidPage(h4("Coming soon…"))),
                 tabPanel("Scandal Fallout", fluidPage(h4("Coming soon…")))
)

#–– SERVER ––
server <- function(input, output) {
  
  output$plot_congress <- renderPlot({
    ggplot(full_ts, aes(x = Date)) +
      geom_line(aes(y = CumCong, color = "Congress Strategy"), size = 1) +
      geom_line(aes(y = CumSP,   color = "S&P 500"),        size = 1) +
      scale_color_manual(values = c(
        "Congress Strategy" = "#2ca02c",
        "S&P 500"           = "#1f77b4"
      )) +
      labs(
        title = "Cumulative Returns: Congress vs S&P 500",
        x = NULL, y = "Index Value (Start = 100)",
        color = NULL
      ) +
      theme_minimal()
  })
  
  output$table_congress <- renderDT({
    data_congress %>% select(Traded, Member, excess_return)
  })
}

shinyApp(ui, server)




