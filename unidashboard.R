library(shiny)
library(DT) # For interactive tables
library(ggplot2)


# Load the dataset
data <- read.csv(file.choose())  # Replace "path/to/your/dataset.csv" with the actual path to your dataset
View(data)


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Retail Sales Dashboard"),
  
  # Sidebar layout with input parameters
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(data$Country)),
      numericInput("rows", "Number of Rows to Display:", value = 10, min = 1, max = nrow(data))
    ),
    
    # Main panel with DataTable output and plots
    mainPanel(
      DTOutput("table"),
      plotOutput("barplot"),
      plotOutput("piechart")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data based on selected country
  filtered_data <- reactive({
    subset(data, Country == input$country)
  })
  
  # Render the DataTable with the specified number of rows
  output$table <- renderDT({
    head(filtered_data(), input$rows)
  })
  
  # Plot for sales quantity by country
  output$barplot <- renderPlot({
    ggplot(filtered_data(), aes(x = Country, y = Quantity)) +
      geom_bar(stat = "sum", fill = "skyblue", color = "black") +
      labs(title = "Sales Quantity by Country", x = "Country", y = "Quantity")
  })
  
  # Pie chart for sales distribution by country
  output$piechart <- renderPlot({
    ggplot(filtered_data(), aes(x = "", fill = Country)) +
      geom_bar(width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Sales Distribution by Country", fill = "Country") +
      theme_void()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
