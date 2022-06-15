
library(shiny)
library(dplyr)
library(fGarch)
library(lubridate)
library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)
library(ggplot2)
library(formattable)


shinyServer(function(input, output, session) {
  observe({
    validate(
      need(try(getData()), "Please set a valid ticker")
    )
    updateRadioButtons(session, "timeSeries_DateRange_Std",
      selected = "2"
    )
  })

  observe({
    # timeSeries_DateRange_Std()
    dates <- volatility_DateRange_Std()
    updateDateRangeInput(session, "volatility_DateRange",
      start = format(dates[1]),
      end = format(dates[2])
    )
  })

  observe({
    dates <- timeSeries_DateRange_Std()
    updateDateRangeInput(session, "timeSeries_DateRange",
      start = format(dates[1]),
      end = format(dates[2])
    )
  })

  timeSeries_DateRange <- reactive({
    as.Date(input$timeSeries_DateRange)
  })

  timeSeries_DateRange_Std <- reactive({
    ix <- zoo::index(valSeries())
    lx <- tail(ix, 1) # last date
    dx <- switch(input$timeSeries_DateRange_Std,
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

  volatility_DateRange <- reactive({
    as.Date(input$volatility_DateRange)
  })

  volatility_DateRange_Std <- reactive({
    ix <- zoo::index(retSeries())
    lx <- tail(ix, 1) # last date
    dx <- switch(input$volatility_DateRange_Std,
      "1" = range(tail(ix, 5)),                      # 5D
      "2" = c(as.Date(lx - dyears(1 / 12)), lx),     # 1M
      "3" = c(as.Date(lx - dyears(0.5)), lx),        # 6M
      "4" = c(as.Date(lx - dyears(1)), lx),          # 1Y
      "7" = c(as.Date(lx - dyears(2)), lx),          # 2Y
      "9" = c(as.Date(lx - dyears(3)), lx),          # 3Y
      "8" = c(as.Date(lx - dyears(5)), lx),          # 5Y
      "5" = c(as.Date(ISOdate(year(lx), 1, 1)), lx), # YTD
      "6" = range(ix)                                # MAX
    )
    dx
  })


  confInterval <- reactive({
    1 - (input$conf_interval / 100) / 2
  })


  getData <- reactive({
    validate(
      need(input$ticker != "", "Please set a ticker")
    )
    getSymbols(input$ticker, auto.assign = FALSE)
  })

  valSeries <- reactive({
    validate(
      need(try(getData()), "Please set a valid ticker")
    )
    ds <- getData()
    if (dim(ds)[2] == 6) {
      ds <- Ad(ds)
    } else if (dim(ds)[2] == 5) {
      ds <- Cl(ds)
    }
    colnames(ds) <- input$ticker
    ds
  })

  usedSeries <- reactive({
    series <- valSeries()
    dr <- timeSeries_DateRange() |>
      format() |>
      paste(collapse = "/")
    series[dr]
  })

  data_DateRange <- reactive({
    ix <- zoo::index(valSeries())
    range(ix)
  })

  retSeries <- reactive({
    PerformanceAnalytics::Return.calculate(valSeries(), method = "log") |>
      na.trim()
  })

  volatility_retSeries <- reactive({
    dr <- volatility_DateRange() |>
      format() |>
      paste(collapse = "/")
    retSeries()[dr]
  })

  garchFit <- reactive({
    ret <- volatility_retSeries()
    fGarch::garchFit(
      data = zoo::coredata(ret),
      include.mean = FALSE,
      trace = FALSE
    )
  })

  volSeries <- reactive({
    ret <- volatility_retSeries()
    fit <- garchFit()
    xts(fit@sigma.t, index(ret))
  })

  relRetSeries <- reactive({
    retSeries() / (volSeries() * qnorm(confInterval()))
  })

  output$returns <- renderTable(
    {
      ds <- valSeries()
      rs <- retSeries()
      last_date <- tail(zoo::index(ds), 1)

      start_date <- last_date - weeks(1)
      range_ <- paste(format(start_date, "%Y-%m-%d"), format(last_date, "%Y-%m-%d"), sep = "/")

      val <- ds[range_]
      ret <- rs[range_]

      data.frame(
        Date = as.character(index(ret)),
        "Value ($)" = as.numeric(val),
        "Daily returns (%)" = as.numeric(ret) * 100, check.names = FALSE
      )
    },
    striped = TRUE,
    spacing = "s",
    bordered = TRUE
  )

  output$volatility_summary <- renderUI({
    sd_ <- (var(volatility_retSeries()) * 252) |> sqrt()
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

  output$volatility_parameters <- renderUI({
    div(
      p("Model: GARCH(1,1)"),
      p("omega = ", format(coef(garchFit())["omega"], digits = 8, nsmall = 8)),
      p("alpha1 = ", format(coef(garchFit())["alpha1"], digits = 4, nsmall = 4)),
      p("beta1 = ", format(coef(garchFit())["beta1"], digits = 4, nsmall = 4))
    )
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
      h4("Volatility"),
      p(
        "Inst. vol. = ",
        format(instantVolatility() * 100, digits = 1, nsmall = 2), "%",
        "(", format(instantVolatility() * sqrt(252) * 100, digits = 1, nsmall = 1), "% yr", ")"
      ),
      p(
        "Long term vol. = ", format(longTermVolatility() * 100, digits = 1, nsmall = 2), "%",
        "(", format(longTermVolatility() * sqrt(252) * 100, digits = 1, nsmall = 1), "% yr", ")"
      ),
      p("Half-life = ", format(halfLife(), digits = 2, nsmall = 2)),
      h4("GARCH(1,1) Parameters"),
      p("omega = ", format(coef(garchFit())["omega"], digits = 8, nsmall = 8)),
      p("alpha1 = ", format(coef(garchFit())["alpha1"], digits = 4, nsmall = 4)),
      p("beta1 = ", format(coef(garchFit())["beta1"], digits = 4, nsmall = 4)),
      h4("V@R Backtesting"),
      p("Expected = ", format(expec, digits = 0)),
      p("Total = ", viol_n),
      p("Fraction = ", format(viol_p * 100, digits = 2, nsmall = 2), "%")
    )
  })

  output$timeSeries <- renderDygraph({
    ret <- volatility_retSeries()
    vol <- volSeries()

    ic <- confInterval()

    viol <- ret[abs(ret) > vol * qnorm(ic)]
    series <- merge.xts(
      returns = ret,
      top = vol * qnorm(ic), bottom = -vol * qnorm(ic), violation = viol
    ) * 100
    names(series) <- c("returns", "top", "bottom", "violation")

    dygraph(tail(series, input$data_window)) |>
      dyAxis("y", label = "%", drawGrid = TRUE) |>
      dySeries("returns", color = "black") |>
      dySeries("top", color = "grey", fillGraph = TRUE) |>
      dySeries("bottom", color = "grey", fillGraph = TRUE) |>
      dyLegend(show = "onmouseover") |>
      dySeries("violation", color = "red", drawPoints = TRUE, pointSize = 3, strokeWidth = 0) |>
      dyOptions(axisLineWidth = 1.5, drawGrid = FALSE)
  })

  output$volatility_tunnel <- renderDygraph({
    ret <- volatility_retSeries()
    vol <- volSeries()

    ic <- 0.99

    series <- merge.xts(returns = ret, top = vol * qnorm(ic), bottom = -vol * qnorm(ic)) * 100
    names(series) <- c("returns", "top", "bottom")

    dygraph(series) |>
      dyAxis("y", label = "%", drawGrid = TRUE) |>
      dySeries("returns", color = "black") |>
      dySeries("top", color = "grey", fillGraph = TRUE) |>
      dySeries("bottom", color = "grey", fillGraph = TRUE) |>
      dyLegend(show = "onmouseover") |>
      dyOptions(axisLineWidth = 1.5, drawGrid = FALSE)
  })

  output$volatility_timeSeries <- renderDygraph({
    vol <- volSeries()

    names(vol) <- c("volatility")

    dygraph(vol * sqrt(252) * 100) |>
      dyAxis("y", label = "%", drawGrid = TRUE) |>
      dySeries("volatility", color = "black") |>
      dyLegend(show = "onmouseover") |>
      dyOptions(axisLineWidth = 1.5, drawGrid = FALSE)
  })

  output$volatility_TermStructure <- renderPlot({
    fit <- garchFit()
    cf_ <- coef(fit)
    a_ <- log(1 / (cf_["alpha1"] + cf_["beta1"]))
    vl <- (longTermVolatility() ^ 2)
    vi <- (instantVolatility() ^ 2)
    vts <- function(t_, a_, vl, vi) {
      vl + ((1 - exp(-a_ * t_)) / (a_ * t_)) * (vi - vl)
    }
    x <- seq(round(halfLife() * 3))
    ds <- data.frame(Days = x, Volatility = sqrt(vts(x, a_, vl, vi) * 252) * 100)
    ggplot(ds, aes(x = Days, y = Volatility)) +
      geom_line(colour = "black") +
      geom_hline(aes(yintercept = sqrt(vl * 252) * 100), colour = "red") +
      labs(x = NULL, y = NULL) +
      theme_bw()
  })

  output$priceSeries <- renderDygraph({
    series <- valSeries()
    dr <- timeSeries_DateRange() |>
      format() |>
      paste(collapse = "/")
    validate(
      need(try(series[dr]), "Please set a valid date range")
    )

    dygraph(series[dr]) |>
      dyAxis("y", label = "$") |>
      dyLegend(show = "onmouseover") |>
      dyOptions(axisLineWidth = 1.5) |>
      dyOptions(stackedGraph = TRUE)
  })

  clean_ticker <- function(ticker) {
    if (str_ends(ticker, ".SA")) {
      str_replace(ticker, "\\.SA", "")
    } else {
      ticker
    }
  }

  output$drawdownPlot <- renderDygraph({
    series <- retSeries()
    dr <- timeSeries_DateRange() |>
      format() |>
      paste(collapse = "/")

    validate(
      need(try(series[dr]), "Please set a valid date range")
    )

    chart.Drawdown(series[dr], plot.engine = "ggplot2") +
      labs(title = str_glue("Drawdown - {clean_ticker(input$ticker)}")) +
      theme_bw() +
      theme(legend.position = "none")
  })

  output$price <- renderValueBox({
    series <- usedSeries()
    last_price <- series |>
      tail(1) |>
      as.numeric()

    valueBox(
      last_price |> accounting(digits = 2),
      "Current Price",
      color = "purple",
      icon = icon("dollar-sign")
    )
  })

  output$period_return <- renderValueBox({
    series <- usedSeries()
    ret <- log(series) |>
      diff() |>
      na.omit() |>
      sum() |>
      as.numeric()

    valueBox(
      ret |> percent(digits = 2),
      "Period Return",
      color = "purple",
      icon = icon("percent")
    )
  })

  output$min_price_1y <- renderValueBox({
    x <- valSeries() |>
      last(252) |>
      min()

    valueBox(
      x |> accounting(digits = 2),
      "Min. Price (1Y)",
      color = "yellow",
      icon = icon("dollar-sign")
    )
  })

  output$max_price_1y <- renderValueBox({
    x <- valSeries() |>
      last(252) |>
      max()

    valueBox(
      x |> accounting(digits = 2),
      "Max. Price (1Y)",
      color = "yellow",
      icon = icon("dollar-sign")
    )
  })

  output$min_price_1m <- renderValueBox({
    x <- valSeries() |>
      last(21) |>
      min()

    valueBox(
      x |> accounting(digits = 2),
      "Min. Price (1M)",
      color = "blue",
      icon = icon("dollar-sign")
    )
  })

  output$max_price_1m <- renderValueBox({
    x <- valSeries() |>
      last(21) |>
      max()

    valueBox(
      x |> accounting(digits = 2),
      "Max. Price (1M)",
      color = "blue",
      icon = icon("dollar-sign")
    )
  })

  output$return_1m <- renderValueBox({
    x <- valSeries() |>
      last(21) |>
      log() |>
      diff() |>
      na.omit() |>
      sum() |>
      as.numeric()

    valueBox(
      x |> percent(digits = 2),
      "Return (1M)",
      color = "blue",
      icon = icon("percent")
    )
  })

  output$return_1y <- renderValueBox({
    x <- valSeries() |>
      last(252) |>
      log() |>
      diff() |>
      na.omit() |>
      sum() |>
      as.numeric()

    valueBox(
      x |> percent(digits = 2),
      "Return (1Y)",
      color = "blue",
      icon = icon("percent")
    )
  })

  output$returnSeries <- renderDygraph({
    ret <- retSeries()
    dr <- timeSeries_DateRange() |>
      format() |>
      paste(collapse = "/")
    validate(
      need(try(ret[dr]), "Please set a valid date range")
    )

    dygraph(ret[dr]) |>
      dyAxis("y", label = "%") |>
      dyLegend(show = "onmouseover") |>
      dyOptions(axisLineWidth = 1.5, drawGrid = FALSE)
  })

  instantVolatility <- reactive({
    fit <- garchFit()
    cf_ <- coef(fit)
    vi <- sum(cf_ * c(1, tail(fit@data, 1)^2, tail(fit@h.t, 1)))
    sqrt(vi)
  })

  longTermVolatility <- reactive({
    fit <- garchFit()
    cf_ <- coef(fit)
    vl <- cf_["omega"] / (1 - cf_["alpha1"] - cf_["beta1"])
    sqrt(vl)
  })

  halfLife <- reactive({
    fit <- garchFit()
    cf_ <- coef(fit)
    log(0.5) / log(cf_["alpha1"] + cf_["beta1"])
  })
})