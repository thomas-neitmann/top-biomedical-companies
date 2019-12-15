library(shiny)
library(dplyr)
library(ggplot2)

revenue <- read.csv("data/biomedical_companies_revenue.csv")
max_revenue <- max(revenue$revenue, na.rm = TRUE)
dollar <- function(x) scales::dollar(x, prefix = "$ ", suffix = " Bn.")

server <- function(input, output, session) {

  table_data <- reactive({
    revenue %>%
      filter(year == input$year) %>%
      arrange(desc(revenue)) %>%
      mutate(Rank = row_number(),
             revenue = dollar(revenue)) %>%
      filter(Rank <= input$rank) %>%
      rename(Company = company, Revenue = revenue) %>%
      select(Rank, Company, Revenue)
  })
  
  output$table <- function() {
    table_data() %>%
      kableExtra::kable(format = "html") %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
  }
  
  plot_data <- reactive({
    revenue %>%
      filter(company %in% input$company,
             year >= input$year_range[1L],
             year <= input$year_range[2L]) %>%
      mutate(tooltip = dollar(revenue))
  })
  
  years <- reactive({
    input$year_range[1L]:input$year_range[2L]
  })
  
  output$plot <- plotly::renderPlotly({
    
    p <- ggplot(plot_data()) +
      aes(year, revenue, fill = company, text = tooltip) +
      geom_col(width = 0.7, position = "dodge") +
      scale_x_continuous(name = "Year", breaks = years()) +
      scale_y_continuous(
        name = "Revenue",
        labels = scales::dollar_format(prefix = "$ ", suffix = " Bn."),
        limits = c(0, max_revenue)
      ) +
      scale_fill_manual(values = c("#0066CC", "#B1B3B3", "#000000")) +
      theme_minimal()
    
    plotly::ggplotly(p, tooltip = "text")
  })
}
