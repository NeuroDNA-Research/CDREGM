# CDR Explorer -- launched via CDREGM::cdr_explorer()
library(shiny)
library(CDREGM)

panel <- cdr_build_panel()
cs    <- CDREGM:::.cross_section(panel)
cs    <- cs[stats::complete.cases(cs[, c("g", "C_std", "D_std", "R_std",
                                         "N_std", "CDR")]), ]
model <- cdr_ols(panel)
b     <- stats::coef(model$fit)
countries <- stats::setNames(cs$iso2c, paste0(cs$country, " (", cs$iso2c, ")"))

comp_labels <- c("Capitalism (C)" = "C_std", "Democracy (D)" = "D_std",
                 "Rule of law (R)" = "R_std", "Natural resources (N)" = "N_std")

predict_g <- function(C, D, R, N) {
  b0 <- if ("(Intercept)" %in% names(b)) b[["(Intercept)"]] else 0
  b0 + b[["C_std"]] * C + b[["D_std"]] * D + b[["R_std"]] * R +
    b[["CDR"]] * C * D * R + b[["N_std"]] * N
}

ui <- fluidPage(
  titlePanel("CDR Explorer"),
  tabsetPanel(
    tabPanel("Scatter",
      sidebarLayout(
        sidebarPanel(
          selectInput("xvar", "X axis", comp_labels, selected = "C_std"),
          checkboxInput("lm", "Show linear fit", TRUE)
        ),
        mainPanel(plotOutput("scatter"))
      )
    ),
    tabPanel("CDR index",
      tableOutput("index")
    ),
    tabPanel("Country profile",
      selectInput("country", "Country", countries,
                  selected = unname(countries[1])),
      tableOutput("profile"),
      h4("Peer group"), tableOutput("peers")
    ),
    tabPanel("Counterfactual",
      sidebarLayout(
        sidebarPanel(
          selectInput("cf_country", "Country", countries,
                      selected = unname(countries[1])),
          sliderInput("dC", "Change in C", -0.5, 0.5, 0, 0.05),
          sliderInput("dD", "Change in D", -0.5, 0.5, 0, 0.05),
          sliderInput("dR", "Change in R", -0.5, 0.5, 0, 0.05)
        ),
        mainPanel(verbatimTextOutput("cf"))
      )
    )
  )
)

server <- function(input, output, session) {

  output$scatter <- renderPlot({
    x <- cs[[input$xvar]]
    plot(x, cs$g, pch = 19, col = "#3b6",
         xlab = names(comp_labels)[comp_labels == input$xvar],
         ylab = "growth rate g")
    if (isTRUE(input$lm)) abline(stats::lm(cs$g ~ x), col = "#c33", lwd = 2)
  })

  output$index <- renderTable({
    idx <- cdr_index(panel)
    idx$rank <- seq_len(nrow(idx))
    idx[, c("rank", "country", "C_std", "D_std", "R_std", "CDRs", "CDRp")]
  }, digits = 3)

  output$profile <- renderTable({
    r <- cs[cs$iso2c == input$country, ]
    data.frame(
      Metric = c("C", "D", "R", "CDR", "g"),
      Value  = c(r$C_std, r$D_std, r$R_std, r$CDR, r$g)
    )
  }, digits = 3)

  output$peers <- renderTable({
    as.data.frame(cdr_peer_group(input$country, k = 5, data = panel))
  }, digits = 3)

  output$cf <- renderPrint({
    r  <- cs[cs$iso2c == input$cf_country, ]
    C1 <- min(1, max(0, r$C_std + input$dC))
    D1 <- min(1, max(0, r$D_std + input$dD))
    R1 <- min(1, max(0, r$R_std + input$dR))
    g0 <- predict_g(r$C_std, r$D_std, r$R_std, r$N_std)
    g1 <- predict_g(C1, D1, R1, r$N_std)
    cat(sprintf("Country:            %s (%s)\n", r$country, r$iso2c))
    cat(sprintf("Baseline g:         %+.4f\n", g0))
    cat(sprintf("Counterfactual g:   %+.4f\n", g1))
    cat(sprintf("Change in g:        %+.4f\n", g1 - g0))
  })
}

shinyApp(ui, server)
