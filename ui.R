
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dygraphs)
library(shinydashboard)

dbHeader <- dashboardHeader(title = "Risk Monitor")
# print(dbHeader$children)
# dbHeader$children[[2]]$children <-  tags$a(href='http://mycompanyishere.com', tags$span("test"))

dashboardPage(
  skin = "black",
  dbHeader,
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series", tabName = "select-time-series"),
      menuItem("Volatility", tabName = "volatility"),
      # menuItem("Risk", tabName = "risk"),
      hr(),
      textInput("ticker", "Symbol", value = "PETR4.SA"),
      div(
        style = "margin-left: 10px",
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "select-time-series",
        uiOutput("tickerDescription"),
        fluidRow(
          box(
            width = "12",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            column(
              5,
              radioButtons("timeSeries_DateRange_Std",
                label = "",
                choices = list(
                  "YTD" = 5, "5D" = 1, "1M" = 2, "6M" = 3,
                  "1Y" = 4, "2Y" = 7, "5Y" = 8, "MAX" = 6
                ),
                selected = 2, inline = TRUE
              )
            ),
            column(3, dateRangeInput("timeSeries_DateRange", ""))
          ),
        ),
        fluidRow(
          column(
            6,
            valueBoxOutput("price"),
            valueBoxOutput("min_price_1y"),
            valueBoxOutput("max_price_1y"),
            valueBoxOutput("period_return"),
            valueBoxOutput("return_1m"),
            valueBoxOutput("return_1y"),
          ),
          column(
            6,
            box(
              width = "12",
              solidHeader = TRUE,
              status = "primary",
              column(12, dygraphOutput("priceSeries", height = "220px"))
            )
          )
        ),
        fluidRow(
          box(
            title = "Table",
            width = "4",
            solidHeader = TRUE,
            collapsible = TRUE,
            tableOutput("returns")
          ),
          box(
            title = "Drawdown",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("drawdownPlot")
          )
        )
      ),
      tabItem(
        tabName = "volatility",
        fluidRow(
          column(
            5,
            radioButtons("volatility_DateRange_Std",
              label = "",
              choices = list(
                "YTD" = 5, "5D" = 1, "1M" = 2, "6M" = 3,
                "1Y" = 4, "2Y" = 7, "3Y" = 9, "5Y" = 8, "MAX" = 6
              ),
              selected = 9, inline = TRUE
            )
          ),
          column(3, dateRangeInput("volatility_DateRange", ""))
        ),
        fluidRow(
          box(
            title = "Volatility time series—annual volatility",
            width = "12",
            solidHeader = TRUE,
            collapsible = TRUE,
            dygraphOutput("volatility_timeSeries", height = "175px")
          )
        ),
        fluidRow(
          box(
            title = "Volatility tunnel—daily volatility",
            width = "12",
            solidHeader = TRUE,
            collapsible = TRUE,
            dygraphOutput("volatility_tunnel", height = "175px")
          )
        ),
        fluidRow(
          box(
            title = "Model parameters",
            width = 4,
            solidHeader = TRUE,
            collapsible = TRUE,
            htmlOutput("volatility_parameters")
          ),
          box(
            title = "Volatility",
            width = 4,
            solidHeader = TRUE,
            collapsible = TRUE,
            htmlOutput("volatility_summary")
          )
        )
      )
      # ,
      #
      # tabItem(
      #   tabName = "risk",
      #   fluidRow(
      #     column(2,
      #            numericInput("conf_interval", "Confidence interval (%)", value = 5),
      #            numericInput("data_window", "Window size", value = 252)
      #     )
      #   ),
      #   box(width = '100%', dygraphOutput("timeSeries", height = "175px"))
      # )
    )
  )
)