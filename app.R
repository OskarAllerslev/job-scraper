library(shiny)
library(DT)
library(dplyr)

# Define the UI
ui <- fluidPage(
  titlePanel("Job Search and Sorting"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("run_scraper", "Run Job Scraper"), # Button to run the scraper
      br(), br(),
      textInput("title", "Search Title", value = "", placeholder = "e.g., Manager, Engineer"),
      textInput("company", "Search Company", value = "", placeholder = "e.g., Google, NCC"),
      textInput("location", "Search Location", value = "", placeholder = "e.g., Copenhagen, Aarhus"),
      actionButton("reset", "Reset Filters"),
      hr(),
      p("Sort and search dynamically using the table below.")
    ),
    
    mainPanel(
      DTOutput("job_table")
    )
  )
)

# Define the Server
server <- function(input, output, session) {
  
  # Reactive variable to store data
  all_jobs <- reactiveVal(data.frame())
  
  # Function to load the CSV
  load_jobs <- function() {
    if (file.exists("all_jobs_with_companies_and_locations.csv")) {
      all_jobs(read.csv("all_jobs_with_companies_and_locations.csv", stringsAsFactors = FALSE))
    } else {
      all_jobs(data.frame(Title = character(), Company = character(), Location = character(), Link = character()))
    }
  }
  
  # Load data initially
  load_jobs()
  
  # Run the scraper when button is pressed
  observeEvent(input$run_scraper, {
    showModal(modalDialog("Running the scraper. Please wait..."))
    tryCatch({
      source("jobscraper.R") # Run the scraper script
      load_jobs()           # Reload the updated data
      removeModal()
      showModal(modalDialog("Scraper finished successfully!", easyClose = TRUE))
    }, error = function(e) {
      removeModal()
      showModal(modalDialog("Error running the scraper: ", e$message, easyClose = TRUE))
    })
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- all_jobs()
    
    if (input$title != "") {
      data <- data %>% filter(grepl(input$title, Title, ignore.case = TRUE))
    }
    if (input$company != "") {
      data <- data %>% filter(grepl(input$company, Company, ignore.case = TRUE))
    }
    if (input$location != "") {
      data <- data %>% filter(grepl(input$location, Location, ignore.case = TRUE))
    }
    
    return(data)
  })
  
  # Render the datatable
  output$job_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  # Reset filters
  observeEvent(input$reset, {
    updateTextInput(session, "title", value = "")
    updateTextInput(session, "company", value = "")
    updateTextInput(session, "location", value = "")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
