library(shiny)
library(shinydashboard)
library(stringr)
library(lubridate)
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)


# timeseries module ----
timeseries_ui <- function(id) {
  fluidRow(
    textInput(NS(id, "symbol"), "Symbol", value = "PETR4.SA"),
  )
}

timeseries_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      series <- try(
        getSymbols(input$symbol, auto.assign = FALSE),
        silent = TRUE
      )
      validate(
        need(series, "Please set a valid symbol")
      )
      attr(series, "symbol") <- input$symbol
      series
    })
  })
}

valid_series_reactive <- function(series) {
  reactive({
    x <- series()
    if (dim(x)[2] == 6) {
      x_ <- Ad(x)
    } else if (dim(x)[2] == 5) {
      x_ <- Cl(x)
    }
    colnames(x_) <- attr(x, "symbol")
    x_
  })
}

return_series_reactive <- function(series) {
  reactive({
    series() |> calculate_returns()
  })
}

calculate_returns <- function(series) {
  series |>
    CalculateReturns(method = "log") |>
    na.omit()
}

# daterange module ----
daterange_ui <- function(id) {
  fluidRow(
    column(
      6,
      radioButtons(NS(id, "choices"),
        label = "",
        choices = list(
          "YTD" = 5, "5D" = 1, "1M" = 2, "6M" = 3,
          "1Y" = 4, "2Y" = 7, "5Y" = 8, "MAX" = 6
        ),
        selected = 3, inline = TRUE
      )
    ),
    column(6, dateRangeInput(NS(id, "daterange"), ""))
  )
}

daterange_server <- function(id, timeseries) {
  moduleServer(id, function(input, output, session) {
    observe({
      dates <- selected_daterange()
      updateDateRangeInput(session, "daterange",
        start = format(dates[1]),
        end = format(dates[2])
      )
    })

    selected_daterange <- reactive({
      ix <- zoo::index(valSeries())
      lx <- tail(ix, 1) # last date
      dx <- switch(input$choices,
        "1" = range(tail(ix, 5)),                      # 5D
        "2" = c(as.Date(lx - dyears(1 / 12)), lx),     # 1M
        "3" = c(as.Date(lx - dyears(0.5)), lx),        # 6M
        "4" = c(as.Date(lx - dyears(1)), lx),          # 1Y
        "7" = c(as.Date(lx - dyears(2)), lx),          # 2Y
        "8" = c(as.Date(lx - dyears(5)), lx),          # 5Y
        "5" = c(as.Date(ISOdate(year(lx), 1, 1)), lx), # YTD
        "6" = range(ix)                                # MAX
      )
      dx
    })

    valSeries <- valid_series_reactive(timeseries)

    reactive({
      series <- valSeries()
      dr <- input$daterange |>
        format() |>
        paste(collapse = "/")
      series[dr]
    })
  })
}

# price plot ----
price_plot_ui <- function(id) {
  fluidRow(
    plotOutput(NS(id, "priceSeries"), height = "220px")
  )
}

price_plot_server <- function(id, series) {
  moduleServer(id, function(input, output, session) {
    output$priceSeries <- renderPlot({
      x <- series()
      x |>
        autoplot() +
        labs(
          x = NULL, y = NULL,
          title = str_glue("Prices - {colnames(x)}")
        ) +
        theme_bw()
    })
  })
}

# drawdown plot ----
drawdown_plot_ui <- function(id) {
  fluidRow(
    plotOutput(NS(id, "drawdownPlot"), height = "220px")
  )
}

drawdown_plot_server <- function(id, series) {
  moduleServer(id, function(input, output, session) {
    output$drawdownPlot <- renderPlot({
      rets <- returnSeries()
      chart.Drawdown(rets, plot.engine = "ggplot2") +
        labs(title = str_glue("Drawdown - {colnames(rets)}")) +
        scale_y_continuous(labels = scales::label_percent()) +
        theme_bw() +
        theme(legend.position = "none")
    })

    returnSeries <- return_series_reactive(series)
  })
}

# price stats ----
cagr_value_box <- function(series, n,
                           label = c("Y", "M"),
                           color = "blue") {
  label <- match.arg(label)
  percent <- scales::label_percent()
  p <- switch(label,
    "Y" = 1,
    "M" = 1 / 12
  )

  prices <- series()
  refdate <- index(prices) |> tail(1)
  rng <- c(as.Date(refdate - dyears(p * n)), refdate) |>
    format() |>
    paste(collapse = "/")

  rets <- calculate_returns(prices[rng])

  x <- rets |>
    na.omit() |>
    sum() |>
    as.numeric() |>
    percent()

  valueBox(
    x,
    str_glue("Return ({n}{label})"),
    color = color,
    icon = icon("percent")
  )
}

price_stats_ui <- function(id) {
  fluidRow(
    valueBoxOutput(NS(id, "price")),
    valueBoxOutput(NS(id, "min_price_1y")),
    valueBoxOutput(NS(id, "max_price_1y")),
    valueBoxOutput(NS(id, "period_return")),
    valueBoxOutput(NS(id, "return_1m")),
    valueBoxOutput(NS(id, "return_1y")),
  )
}

price_stats_server <- function(id, timeseries, selected_timeseries) {
  moduleServer(id, function(input, output, session) {
    validTimeSeries <- valid_series_reactive(timeseries)

    output$price <- renderValueBox({
      last_price <- selected_timeseries() |>
        tail(1) |>
        as.numeric()

      valueBox(
        last_price |> scales::label_dollar()(),
        "Current Price",
        color = "purple",
        icon = icon("dollar-sign")
      )
    })

    output$min_price_1y <- renderValueBox({
      x <- validTimeSeries() |>
        xts::last(252) |>
        min()

      valueBox(
        x |> scales::label_dollar()(),
        "Min. Price (1Y)",
        color = "yellow",
        icon = icon("dollar-sign")
      )
    })

    output$max_price_1y <- renderValueBox({
      x <- validTimeSeries() |>
        xts::last(252) |>
        max()

      valueBox(
        x |> scales::label_dollar()(),
        "Max. Price (1Y)",
        color = "yellow",
        icon = icon("dollar-sign")
      )
    })

    output$period_return <- renderValueBox({
      x <- calculate_returns(selected_timeseries())
      ret <- x |>
        sum() |>
        as.numeric()

      valueBox(
        ret |> scales::label_percent()(),
        "Period Return",
        color = "purple",
        icon = icon("percent")
      )
    })

    output$return_1m <- cagr_value_box(validTimeSeries, 1, "M") |>
      renderValueBox()

    output$return_1y <- cagr_value_box(validTimeSeries, 1) |>
      renderValueBox()
  })
}

# cagr stats ----
cagr_stats_ui <- function(id) {
  fluidRow(
    valueBoxOutput(NS(id, "return_5y")),
    valueBoxOutput(NS(id, "return_10y")),
    valueBoxOutput(NS(id, "return_15y")),
  )
}

cagr_stats_server <- function(id, timeseries) {
  moduleServer(id, function(input, output, session) {
    validTimeSeries <- valid_series_reactive(timeseries)

    returnSeries <- return_series_reactive(validTimeSeries)

    output$return_5y <- cagr_value_box(validTimeSeries, 5) |> renderValueBox()
    output$return_10y <- cagr_value_box(validTimeSeries, 10) |> renderValueBox()
    output$return_15y <- cagr_value_box(validTimeSeries, 15) |> renderValueBox()
  })
}

# App ----

stock_analysis_ui <- function() {
  dbHeader <- dashboardHeader(title = "Stock Analysis")

  dashboardPage(
    skin = "black",
    dbHeader,
    dashboardSidebar(
      sidebarMenu(
        menuItem("Returns Analysis", tabName = "returns-analysis"),
        menuItem("Volatility", tabName = "volatility"),
        # menuItem("Risk", tabName = "risk"),
        hr(),
        timeseries_ui("prices"),
        div(
          style = "margin-left: 10px",
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "returns-analysis",
          fluidRow(
            box(
              width = "12",
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              daterange_ui("prices")
            ),
          ),
          fluidRow(uiOutput("stock_info")),
          fluidRow(
            column(6, price_stats_ui("prices")),
            column(
              6,
              box(
                width = "12",
                solidHeader = TRUE,
                status = "primary",
                column(12, price_plot_ui("prices"))
              )
            )
          ),
          fluidRow(
            column(6, cagr_stats_ui("prices")),
            column(
              6,
              box(
                width = "12",
                solidHeader = TRUE,
                status = "primary",
                column(12, drawdown_plot_ui("prices"))
              )
            )
          )
        ),
        tabItem(
          tabName = "volatility",
          fluidRow(
            box(
              width = "12",
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              daterange_ui("volatility")
            ),
          ),
          fluidRow(
            box(
              title = "Volatility time series—annual volatility",
              width = "12",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("volatility_timeSeries", height = "175px")
            )
          ),
          fluidRow(
            box(
              title = "Volatility tunnel—daily volatility",
              width = "12",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("volatility_tunnel", height = "175px")
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
}

stock_analysis_app <- function() {
  ui <- stock_analysis_ui()

  server <- function(input, output, session) {
    timeseries <- timeseries_server("prices")

    output$stock_info <- renderUI({
      ts <- timeseries()
      refdate <- ts |>
        index() |>
        tail(1)
      symbol <- attr(ts, "symbol")
      tagList(
        tags$h1(strong(symbol), style = "margin-left: 50px"),
        tags$p(strong("Refdate: "), format(refdate),
          style = "margin-left: 50px"
        )
      )
    })
    selected_timeseries <- daterange_server("prices", timeseries)
    price_stats_server("prices", timeseries, selected_timeseries)
    price_plot_server("prices", selected_timeseries)
    drawdown_plot_server("prices", selected_timeseries)
    cagr_stats_server("prices", timeseries)
  }

  shinyApp(ui, server)
}

stock_analysis_app()