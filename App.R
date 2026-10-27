# Computer Shop Inventory Explorer (Shiny)
# Keep this file in the same folder as computer_shop_dataset.csv, then click Run App.

library(shiny)
library(DT)

shop <- read.csv("computer_shop_dataset.csv", stringsAsFactors = FALSE)
shop$Inventory_Value <- shop$Price_PHP * shop$Stock_Qty

peso <- function(x) paste0("\u20B1", formatC(x, format = "f", digits = 0, big.mark = ","))

css <- "
  body { background:#f4f6f9; font-family: 'Segoe UI', Helvetica, Arial, sans-serif; color:#1f2d3d; }
  .app-header { background:#16213e; color:#fff; padding:18px 28px; margin:-15px -15px 20px -15px; }
  .app-header h2 { margin:0; font-weight:600; font-size:24px; }
  .app-header p  { margin:4px 0 0; color:#b8c4e0; font-size:13px; }
  .well { background:#fff; border:1px solid #dde3ec; box-shadow:none; border-radius:8px; }
  .kpi { background:#fff; border:1px solid #dde3ec; border-radius:8px; padding:14px 16px; margin-bottom:15px; }
  .kpi .label { font-size:12px; text-transform:uppercase; letter-spacing:.05em; color:#6b7a90; }
  .kpi .value { font-size:24px; font-weight:600; color:#16213e; }
  .kpi .sub   { font-size:12px; color:#6b7a90; }
  .panel-card { background:#fff; border:1px solid #dde3ec; border-radius:8px; padding:16px; margin-bottom:15px; }
  .panel-card h4 { margin-top:0; font-size:16px; font-weight:600; }
  .highest { background:#fff7e0; border:1px solid #f0d98a; border-radius:8px; padding:10px 14px; margin-bottom:15px; font-weight:600; }
  table.dataTable thead th { background:#16213e; color:#fff; }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  div(class = "app-header",
      h2("Computer Shop Inventory Explorer"),
      p("Filter products by category, brand, price, and stock level")),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("category", "Category",
                  choices = c("All", sort(unique(shop$Category))), selected = "All"),
      selectInput("brand", "Brand",
                  choices = c("All", sort(unique(shop$Brand))), selected = "All"),
      sliderInput("price", "Price range (PHP)",
                  min = 0, max = ceiling(max(shop$Price_PHP) / 500) * 500,
                  value = c(0, ceiling(max(shop$Price_PHP) / 500) * 500),
                  step = 50, pre = "\u20B1", sep = ","),
      sliderInput("min_stock", "Minimum stock quantity",
                  min = 0, max = max(shop$Stock_Qty), value = 0, step = 1),
      radioButtons("sort_by", "Sort table by",
                   choices = c("Price (high to low)" = "price_desc",
                               "Price (low to high)" = "price_asc",
                               "Stock (low to high)" = "stock_asc",
                               "Inventory value" = "value_desc")),
      actionButton("reset", "Reset filters", width = "100%")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(3, uiOutput("kpi_items")),
        column(3, uiOutput("kpi_avg")),
        column(3, uiOutput("kpi_stock")),
        column(3, uiOutput("kpi_value"))
      ),
      uiOutput("highest_text"),
      fluidRow(
        column(6, div(class = "panel-card",
                      h4("Price by product"),
                      plotOutput("price_plot", height = "320px"))),
        column(6, div(class = "panel-card",
                      h4("Stock level by product"),
                      plotOutput("stock_plot", height = "320px")))
      ),
      div(class = "panel-card",
          h4("Matching products"),
          DTOutput("table"))
    )
  )
)

server <- function(input, output, session) {
  
  # Brands available depend on the chosen category
  observeEvent(input$category, {
    pool <- if (input$category == "All") shop else subset(shop, Category == input$category)
    updateSelectInput(session, "brand",
                      choices = c("All", sort(unique(pool$Brand))), selected = "All")
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "category", selected = "All")
    updateSliderInput(session, "price",
                      value = c(0, ceiling(max(shop$Price_PHP) / 500) * 500))
    updateSliderInput(session, "min_stock", value = 0)
    updateRadioButtons(session, "sort_by", selected = "price_desc")
  })
  
  filtered <- reactive({
    d <- shop
    if (input$category != "All") d <- subset(d, Category == input$category)
    if (input$brand != "All")    d <- subset(d, Brand == input$brand)
    d <- subset(d, Price_PHP >= input$price[1] & Price_PHP <= input$price[2] &
                  Stock_Qty >= input$min_stock)
    if (nrow(d) == 0) return(d)
    switch(input$sort_by,
           price_desc = d[order(-d$Price_PHP), ],
           price_asc  = d[order(d$Price_PHP), ],
           stock_asc  = d[order(d$Stock_Qty), ],
           value_desc = d[order(-d$Inventory_Value), ])
  })
  
  kpi <- function(label, value, sub = "") {
    div(class = "kpi", div(class = "label", label),
        div(class = "value", value), div(class = "sub", sub))
  }
  
  output$kpi_items <- renderUI({
    kpi("Products shown", nrow(filtered()), paste("of", nrow(shop), "in shop"))
  })
  output$kpi_avg <- renderUI({
    d <- filtered()
    kpi("Average price", if (nrow(d)) peso(mean(d$Price_PHP)) else "-")
  })
  output$kpi_stock <- renderUI({
    kpi("Units in stock", sum(filtered()$Stock_Qty))
  })
  output$kpi_value <- renderUI({
    kpi("Inventory value", peso(sum(filtered()$Inventory_Value)))
  })
  
  output$highest_text <- renderUI({
    d <- filtered()
    if (nrow(d) == 0) {
      div(class = "highest", "No products match the current filters.")
    } else {
      top <- d[which.max(d$Price_PHP), ]
      low <- d[which.min(d$Stock_Qty), ]
      div(class = "highest",
          paste0("Highest-priced: ", top$Product_Name, " (", top$Brand, ") - ",
                 peso(top$Price_PHP), "   |   Lowest stock: ", low$Product_Name,
                 " - ", low$Stock_Qty, " units"))
    }
  })
  
  output$price_plot <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, "No data to plot"))
    d <- d[order(d$Price_PHP), ]
    par(mar = c(4, 11, 1, 1))
    barplot(d$Price_PHP, names.arg = d$Product_Name, horiz = TRUE, las = 1,
            col = "#3b6fd4", border = NA, cex.names = 0.75,
            xlab = "Price (PHP)")
  })
  
  output$stock_plot <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, "No data to plot"))
    d <- d[order(d$Stock_Qty), ]
    cols <- ifelse(d$Stock_Qty < 10, "#d9534f", "#2e9e6b")
    par(mar = c(4, 11, 1, 1))
    barplot(d$Stock_Qty, names.arg = d$Product_Name, horiz = TRUE, las = 1,
            col = cols, border = NA, cex.names = 0.75,
            xlab = "Units in stock (red = under 10)")
  })
  
  output$table <- renderDT({
    d <- filtered()
    out <- data.frame(
      ID = d$Product_ID, Product = d$Product_Name, Category = d$Category,
      Brand = d$Brand, Price = d$Price_PHP, Stock = d$Stock_Qty,
      Value = d$Inventory_Value, check.names = FALSE
    )
    datatable(out, rownames = FALSE, options = list(pageLength = 10, dom = "ftip")) |>
      formatCurrency(c("Price", "Value"), currency = "\u20B1", digits = 0)
  })
}

shinyApp(ui, server)