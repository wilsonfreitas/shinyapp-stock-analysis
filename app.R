library(shiny)
library(shinydashboard)
library(stringr)
library(lubridate)
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(rb3)


# functions ----
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

garch_fit_reactive <- function(series) {
  reactive({
    ret <- series()
    fGarch::garchFit(
      data = zoo::coredata(ret),
      include.mean = FALSE,
      trace = FALSE
    ) |> suppressWarnings()
  })
}

volatility_series_reactive <- function(series, garchFit) {
  reactive({
    ret <- series()
    fit <- garchFit()
    xts(fit@sigma.t, index(ret))
  })
}

# timeseries module ----
timeseries_ui <- function(id) {
  fluidRow(
    textInput(NS(id, "symbol"), "Symbol", value = "LREN3.SA"),
  )
}

timeseries_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      symbol <- str_to_upper(input$symbol)
      series <- try(
        getSymbols(symbol, auto.assign = FALSE),
        silent = TRUE
      )
      validate(
        need(series, "Please set a valid symbol")
      )
      attr(series, "symbol") <- symbol
      attr(series, "company_name") <- str_sub(symbol, 1, 4)
      series
    })
  })
}

# stock info ----
stock_info_ui <- function(id) {
  fluidRow(uiOutput(NS(id, "stock_info")), style = "margin-left: 5px")
}

stock_info_server <- function(id, timeseries) {
  moduleServer(id, function(input, output, session) {
    company_info <- reactive({
      ts <- timeseries()
      company_name <- attr(ts, "company_name")
      f <- download_marketdata(
        "GetListedSupplementCompany",
        company_name = company_name
      )
      x <- try(jsonlite::fromJSON(f, simplifyDataFrame = FALSE))
      validate(
        need(x, "no company info")
      )
      x[[1]]
    })

    company_details <- reactive({
      ci <- company_info()
      f <- download_marketdata("GetDetailsCompany", code_cvm = ci$codeCVM)
      x <- try(jsonlite::fromJSON(f, simplifyDataFrame = FALSE))
      validate(
        need(x, "no company details")
      )
      x
    })

    output$stock_info <- renderUI({
      ts <- timeseries()
      dates <- index(ts)
      lastdate <- tail(dates, 1)
      firstdate <- dates[1]
      symbol <- attr(ts, "symbol")
      ci <- company_info()
      cd <- company_details()
      tagList(
        tags$h3(cd$companyName),
        tags$p("Period:", format(firstdate), "/", format(lastdate)),
        tags$p("# ON:", ci$numberCommonShares),
        tags$p("# PN:", ci$numberPreferredShares),
        do.call(
          tags$ul,
          str_split(cd$industryClassification, "/")[[1]] |> lapply(tags$li)
        )
      )
    })
  })
}

# daterange module ----
daterange_ui <- function(id,
                         selected = 3,
                         choices = list(
                           "YTD" = 5, "5D" = 1, "1M" = 2, "6M" = 3,
                           "1Y" = 4, "2Y" = 7, "5Y" = 8, "MAX" = 6
                         )) {
  fluidRow(
    column(
      6,
      radioButtons(NS(id, "choices"),
        label = "",
        choices = choices,
        selected = selected,
        inline = TRUE
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
      ix <- zoo::index(timeseries())
      lx <- tail(ix, 1) # last date
      dx <- switch(input$choices,
        "1" = range(tail(ix, 5)),                      # 5D
        "2" = c(as.Date(lx - dyears(1 / 12)), lx),     # 1M
        "3" = c(as.Date(lx - dyears(0.5)), lx),        # 6M
        "4" = c(as.Date(lx - dyears(1)), lx),          # 1Y
        "5" = c(as.Date(ISOdate(year(lx), 1, 1)), lx), # YTD
        "6" = range(ix),                               # MAX
        "7" = c(as.Date(lx - dyears(2)), lx),          # 2Y
        "8" = c(as.Date(lx - dyears(5)), lx),          # 5Y
        "9" = c(as.Date(lx - dyears(3)), lx),          # 3Y
        "10" = c(as.Date(lx - dyears(4)), lx),         # 4Y
      )
      dx
    })

    reactive({
      series <- timeseries()
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

drawdown_plot_server <- function(id, returnSeries) {
  moduleServer(id, function(input, output, session) {
    output$drawdownPlot <- renderPlot({
      rets <- returnSeries()
      chart.Drawdown(rets, plot.engine = "ggplot2") +
        labs(title = str_glue("Drawdown - {colnames(rets)}")) +
        scale_y_continuous(labels = scales::label_percent()) +
        theme_bw() +
        theme(legend.position = "none")
    })
  })
}

# price stats ----
price_stats_ui <- function(id) {
  fluidRow(
    valueBoxOutput(NS(id, "price")),
    valueBoxOutput(NS(id, "min_price")),
    valueBoxOutput(NS(id, "max_price")),
    valueBoxOutput(NS(id, "period_return")),
  )
}

price_stats_server <- function(id, validTimeSeries, selectedTimeSeries) {
  moduleServer(id, function(input, output, session) {
    dollar <- scales::label_dollar()
    percent <- scales::label_percent()

    output$price <- renderValueBox({
      ts <- selectedTimeSeries()
      refdate <-  zoo::index(ts) |> tail(1)
      last_price <- ts |>
        tail(1) |>
        as.numeric()

      valueBox(
        dollar(last_price),
        str_glue("Last {format(refdate)}"),
        color = "purple",
        icon = icon("dollar-sign")
      )
    })

    output$min_price <- renderValueBox({
      sel_ser <- selectedTimeSeries()
      min_idx <- sel_ser |> which.min()
      x <- sel_ser[min_idx, ] |> as.numeric()
      min_date <- index(sel_ser[min_idx, ])

      valueBox(
        dollar(x),
        str_glue("Min {format(min_date)}"),
        color = "yellow",
        icon = icon("dollar-sign")
      )
    })

    output$max_price <- renderValueBox({
      sel_ser <- selectedTimeSeries()
      max_idx <- sel_ser |> which.max()
      x <- sel_ser[max_idx, ] |> as.numeric()
      max_date <- index(sel_ser[max_idx, ])

      valueBox(
        dollar(x),
        str_glue("Max {format(max_date)}"),
        color = "yellow",
        icon = icon("dollar-sign")
      )
    })

    output$period_return <- renderValueBox({
      x <- calculate_returns(selectedTimeSeries())
      ret <- x |>
        sum() |>
        as.numeric()

      valueBox(
        percent(ret),
        "Period Return",
        color = "purple",
        icon = icon("percent")
      )
    })
  })
}

# cagr stats ----
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

  x <- sum(rets, na.rm = TRUE)
  x <- exp(x) - 1

  valueBox(
    percent(x),
    str_glue("Return ({n}{label})"),
    color = color,
    icon = icon("percent")
  )
}

cagr_stats_ui <- function(id) {
  fluidRow(
    valueBoxOutput(NS(id, "return_1m")),
    valueBoxOutput(NS(id, "return_1y")),
    valueBoxOutput(NS(id, "return_5y")),
    valueBoxOutput(NS(id, "return_10y")),
    valueBoxOutput(NS(id, "return_15y")),
  )
}

cagr_stats_server <- function(id, validTimeSeries) {
  moduleServer(id, function(input, output, session) {
    output$return_1m <- cagr_value_box(validTimeSeries, 1, "M") |> renderValueBox()
    output$return_1y <- cagr_value_box(validTimeSeries, 1) |> renderValueBox()
    output$return_5y <- cagr_value_box(validTimeSeries, 5) |> renderValueBox()
    output$return_10y <- cagr_value_box(validTimeSeries, 10) |> renderValueBox()
    output$return_15y <- cagr_value_box(validTimeSeries, 15) |> renderValueBox()
  })
}

# volatility ----
volatility_series_ui <- function(id, height = "175px") {
  fluidRow(
    plotOutput(NS(id, "volatility_series"), height = height)
  )
}

volatility_series_server <- function(id, volatilityTimeSeries) {
  moduleServer(id, function(input, output, session) {
    output$volatility_series <- renderPlot({
      vol <- volatilityTimeSeries()
      colnames(vol) <- c("volatility")
      vol <- vol * sqrt(252) * 100

      vol |>
        autoplot() |>
        labs(y = "%")
    })
  })
}

volatility_parameters_ui <- function(id) {
  fluidRow(htmlOutput(NS(id, "volatility_parameters")))
}

volatility_parameters_server <- function(id, garchFit) {
  moduleServer(id, function(input, output, session) {
    output$volatility_parameters <- renderUI({
      fit <- garchFit()
      div(
        p("Model: GARCH(1,1)"),
        p("omega = ", format(fit@fit$par["omega"], digits = 8, nsmall = 8)),
        p("alpha1 = ", format(fit@fit$par["alpha1"], digits = 4, nsmall = 4)),
        p("beta1 = ", format(fit@fit$par["beta1"], digits = 4, nsmall = 4))
      )
    })
  })
}

volatility_summary_ui <- function(id) {
  fluidRow(htmlOutput(NS(id, "volatility_summary")))
}

volatility_summary_server <- function(id, returnTimeSeries, garchFit) {
  moduleServer(id, function(input, output, session) {
    instantVolatility <- reactive({
      fit <- garchFit()
      cf_ <- fit@fit$par
      vi <- sum(cf_ * c(1, tail(fit@data, 1)^2, tail(fit@h.t, 1)))
      sqrt(vi)
    })

    longTermVolatility <- reactive({
      fit <- garchFit()
      cf_ <- fit@fit$par
      vl <- cf_["omega"] / (1 - cf_["alpha1"] - cf_["beta1"])
      sqrt(vl)
    })

    halfLife <- reactive({
      fit <- garchFit()
      cf_ <- fit@fit$par
      log(0.5) / log(cf_["alpha1"] + cf_["beta1"])
    })

    output$volatility_summary <- renderUI({
      sd_ <- (var(returnTimeSeries()) * 252) |> sqrt()
      div(
        p(
          "Inst. vol. = ",
          format(instantVolatility() * 100, digits = 1, nsmall = 2), "%",
          "(", format(instantVolatility() * sqrt(252) * 100, digits = 1, nsmall = 1), "% yr", ")"
        ),
        p(
          "Long term vol. = ", format(longTermVolatility() * 100, digits = 1, nsmall = 2), "%",
          "(", format(longTermVolatility() * sqrt(252) * 100, digits = 1, nsmall = 1), "% yr", ")"
        ),
        p("Std. dev. = ", format(sd_ * 100, digits = 2, nsmall = 2), "% yr"),
        p("Half-life = ", format(halfLife(), digits = 2, nsmall = 2))
      )
    })
  })
}

# risk ----
risk_parameters_ui <- function(id) {
  fluidRow(
    column(6, numericInput(NS(id, "conf_interval"), "Confidence interval (%)", value = 5)),
    column(6, numericInput(NS(id, "data_window"), "Window size", value = 252))
  )
}

risk_summary_ui <- function(id) {
  fluidRow(htmlOutput(NS(id, "summary")))
}

risk_summary_server <- function(id, retSeries, volSeries) {
  moduleServer(id, function(input, output, session) {
    confInterval <- reactive({
      1 - (input$conf_interval / 100) / 2
    })

    output$summary <- renderUI({
      ret <- retSeries()
      vol <- volSeries()
      ret <- tail(retSeries(), input$data_window)
      vol <- tail(volSeries(), input$data_window)

      viol_n <- sum(abs(ret) > vol * qnorm(confInterval()))
      viol_p <- 1 - (viol_n / input$data_window)
      expec <- (input$conf_interval / 100) * input$data_window

      div(
        h4("V@R Backtesting"),
        p("Expected = ", format(expec, digits = 0)),
        p("Total = ", viol_n),
        p("Fraction = ", format(viol_p * 100, digits = 2, nsmall = 2), "%")
      )
    })
  })
}

volatility_tunnel_ui <- function(id, height = "175px") {
  fluidRow(plotOutput(NS(id, "volatility_tunnel"), height = height))
}

volatility_tunnel_server <- function(id, returnTimeSeries, volatilityTimeSeries) {
  moduleServer(id, function(input, output, session) {
    confInterval <- reactive({
      1 - (input$conf_interval / 100) / 2
    })

    output$volatility_tunnel <- renderPlot({
      ret <- returnTimeSeries()
      vol <- volatilityTimeSeries()
      ic <- confInterval()

      series <- merge.xts(
        returns = ret,
        top = vol * qnorm(ic),
        bottom = -vol * qnorm(ic)
      ) * 100

      colnames(series) <- c("returns", "top", "bottom")
      xdf <- as.data.frame(series)
      xdf$refdate <- as.Date(rownames(xdf))
      rownames(xdf) <- NULL
      percent <- scales::label_percent()

      xdf |> ggplot(aes(x = refdate, y = returns)) +
        geom_line(colour = "grey") +
        geom_line(aes(y = bottom), colour = "red", alpha = 0.5) +
        geom_line(aes(y = top), colour = "red", alpha = 0.5) +
        geom_point(
          data = subset(xdf, returns < bottom | returns > top),
          aes(x = refdate, y = returns),
          shape = 4
        ) +
        labs(
          x = NULL, y = "%",
          title = str_glue("GARCH {percent(ic)} Confidence Interval"),
          subtitle = "Cross-marks represent returns that exceed confidence interval"
        ) +
        theme_bw()
    })
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
        menuItem("Risk Analysis", tabName = "risk"),
        hr(),
        timeseries_ui("prices"),
        hr(),
        stock_info_ui("prices")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "returns-analysis",
          h1("Returns Analysis"),
          fluidRow(
            box(
              width = "12",
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              daterange_ui("prices")
            ),
          ),
          fluidRow(
            column(
              6,
              box(
                width = "12",
                solidHeader = TRUE,
                status = "primary",
                title = "Selected Period",
                column(12, price_stats_ui("prices"))
              )
            ),
            column(
              6,
              box(
                width = "12",
                solidHeader = TRUE,
                status = "primary",
                title = "Returns",
                column(12, cagr_stats_ui("prices"))
              )
            ),
          ),
          fluidRow(
            column(
              6,
              box(
                width = "12",
                solidHeader = TRUE,
                status = "primary",
                column(12, price_plot_ui("prices"))
              )
            ),
            column(
              6,
              box(
                width = "12",
                solidHeader = TRUE,
                status = "primary",
                column(12, drawdown_plot_ui("prices"))
              )
            ),
          )
        ),
        tabItem(
          tabName = "volatility",
          h1("Volatility Analysis"),
          fluidRow(
            box(
              width = "12",
              status = "primary",
              solidHeader = TRUE,
              daterange_ui("volatility",
                selected = 9,
                choices = list(
                  "1Y" = 4, "2Y" = 7, "3Y" = 9, "4Y" = 10, "5Y" = 8, "MAX" = 6
                )
              )
            ),
          ),
          fluidRow(
            box(
              width = "12",
              solidHeader = TRUE,
              column(12, volatility_series_ui("volatility", "200px"))
            )
          ),
          fluidRow(
            box(
              title = "Model parameters",
              width = 4,
              solidHeader = TRUE,
              column(12, volatility_parameters_ui("volatility"))
            ),
            box(
              title = "Volatility",
              width = 4,
              solidHeader = TRUE,
              column(12, volatility_summary_ui("volatility"))
            )
          )
        ),
        tabItem(
          tabName = "risk",
          h1("Risk Analysis"),
          fluidRow(
            box(
              width = "12",
              status = "primary",
              solidHeader = TRUE,
              daterange_ui("risk",
                selected = 9,
                choices = list(
                  "1Y" = 4, "2Y" = 7, "3Y" = 9, "4Y" = 10, "5Y" = 8, "MAX" = 6
                )
              )
            ),
          ),
          fluidRow(
            box(
              width = "12",
              solidHeader = TRUE,
              column(12, risk_parameters_ui("risk"))
            )
          ),
          fluidRow(
            box(
              width = "12",
              solidHeader = TRUE,
              column(12, volatility_tunnel_ui("risk", "200px"))
            )
          ),
          fluidRow(
            box(
              width = 4,
              solidHeader = TRUE,
              column(12, risk_summary_ui("risk"))
            )
          )
        )
      )
    )
  )
}

stock_analysis_app <- function() {
  ui <- stock_analysis_ui()

  server <- function(input, output, session) {
    timeseries <- timeseries_server("prices")
    validTimeSeries <- valid_series_reactive(timeseries)

    # stock info ----
    stock_info_server("prices", timeseries)

    # price returns ----
    selectedTimeSeries <- daterange_server("prices", validTimeSeries)
    selectedReturnSeries <- return_series_reactive(selectedTimeSeries)
    price_stats_server("prices", validTimeSeries, selectedTimeSeries)
    price_plot_server("prices", selectedTimeSeries)
    drawdown_plot_server("prices", selectedReturnSeries)
    cagr_stats_server("prices", validTimeSeries)

    # volatility ----
    selectedTimeSeries_vol <- daterange_server("volatility", validTimeSeries)
    selectedReturnSeries_vol <- return_series_reactive(selectedTimeSeries_vol)
    garchFit_vol <- garch_fit_reactive(selectedReturnSeries_vol)
    volatilityTimeSeries_vol <- volatility_series_reactive(selectedReturnSeries_vol, garchFit_vol)
    volatility_series_server("volatility", volatilityTimeSeries_vol)
    volatility_parameters_server("volatility", garchFit_vol)
    volatility_summary_server("volatility", selectedReturnSeries_vol, garchFit_vol)

    # risk ----
    selectedTimeSeries_risk <- daterange_server("risk", validTimeSeries)
    selectedReturnSeries_risk <- return_series_reactive(selectedTimeSeries_risk)
    garchFit_risk <- garch_fit_reactive(selectedReturnSeries_risk)
    volatilityTimeSeries_risk <- volatility_series_reactive(
      selectedReturnSeries_risk,
      garchFit_risk
    )
    volatility_tunnel_server("risk", selectedReturnSeries_risk, volatilityTimeSeries_risk)
    risk_summary_server("risk", selectedReturnSeries_risk, volatilityTimeSeries_risk)
  }

  shinyApp(ui, server)
}

stock_analysis_app()