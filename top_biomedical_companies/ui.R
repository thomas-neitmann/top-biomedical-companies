library(shiny)
revenue <- read.csv("data/biomedical_companies_revenue.csv")
years <- unique(revenue$year)

ui <- fluidPage(
  titlePanel("Top Biomedical Companies by Revenue"),
  
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel(
        "input.tab == 'Table'",
        selectInput(
          inputId = "year",
          label = "Year",
          choices = years
        ),
        sliderInput(
          inputId = "rank",
          label = "Show Top N",
          ticks = FALSE,
          min = 1,
          max = 20,
          value = 10
        )
      ),
      
      conditionalPanel(
        "input.tab == 'Plot'",
        selectizeInput(
          inputId = "company",
          label = "Company",
          choices = unique(revenue$company),
          multiple = TRUE,
          selected = "Roche",
          options = list(maxItems = 3L)
        ),
        sliderInput(
          inputId = "year_range",
          label = "Year Range",
          ticks = FALSE,
          min = min(revenue$year),
          max = max(revenue$year),
          value = range(revenue$year),
          step = 1L,
          sep = ""
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tab",
        tabPanel("Table", tableOutput("table")),
        tabPanel("Plot", shinycssloaders::withSpinner(plotly::plotlyOutput("plot", height = "600px")))
      )
    )
  )
)
