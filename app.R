library(shiny)
library(DT)
library(dplyr)
library(rvest)
library(stringr)

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

server <- function(input, output, session) {
  
  # Reactive variable to store data
  all_jobs <- reactiveVal(data.frame())
  
  # Function to load the CSV
  load_jobs <- function() {
    csv_file <- "all_jobs.csv"
    # Safely read if file is non-empty
    if (file.exists(csv_file) && file.size(csv_file) > 0) {
      all_jobs(read.csv(csv_file, stringsAsFactors = FALSE))
    } else {
      # Empty data frame if missing/empty
      all_jobs(data.frame(
        Title    = character(),
        Company  = character(),
        Location = character(),
        Link     = character(),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Load data initially
  load_jobs()
  
  # Run the scraper when button is pressed
  observeEvent(input$run_scraper, {
    showModal(modalDialog("Running the scraper. Please wait..."))
    
    # Use withProgress to create a progress bar
    withProgress(message = "Scraping jobs from JobIndex", value = 0, {
      base_url <- "https://www.jobindex.dk/jobsoegning?page="
      all_jobs_temp <- data.frame()
      total_pages <- 860
      
      for (i in seq_len(total_pages)) {
        # increment progress bar each iteration
        incProgress(1 / total_pages, detail = paste("Scraping page", i, "of", total_pages))
        
        url <- paste0(base_url, i)
        
        # Safely read the page
        page <- tryCatch(read_html(url), error = function(e) NULL)
        if (is.null(page)) {
          cat("Failed to scrape page", i, "\n")
          next
        }
        
        # Extract data
        job_titles <- page %>%
          html_nodes(".PaidJob-inner h4 a") %>%
          html_text() %>%
          str_trim()
        
        job_links <- page %>%
          html_nodes(".PaidJob-inner h4 a") %>%
          html_attr("href") %>%
          str_trim()
        
        job_companies <- page %>%
          html_nodes(".jix-toolbar-top__company") %>%
          html_text(trim = TRUE)
        
        job_locations <- page %>%
          html_nodes(".PaidJob-inner .jix_robotjob--area") %>%
          html_text() %>%
          str_trim()
        
        # Handle mismatched lengths
        max_length <- max(length(job_titles), length(job_companies),
                          length(job_locations), length(job_links))
        
        job_titles    <- c(job_titles,    rep(NA, max_length - length(job_titles)))
        job_companies <- c(job_companies, rep(NA, max_length - length(job_companies)))
        job_locations <- c(job_locations, rep(NA, max_length - length(job_locations)))
        job_links     <- c(job_links,     rep(NA, max_length - length(job_links)))
        
        # Combine all data into a data frame
        jobs <- data.frame(
          Title    = job_titles,
          Company  = job_companies,
          Location = job_locations,
          Link     = job_links,
          stringsAsFactors = FALSE
        )
        
        # Append to the main data frame
        all_jobs_temp <- bind_rows(all_jobs_temp, jobs)
      }
      
      # Write out the scraped data
      write.csv(all_jobs_temp, "all_jobs.csv", row.names = FALSE)
    })
    
    # Done scraping; reload jobs
    load_jobs()
    
    removeModal()
    showModal(modalDialog("Scraper finished successfully!", easyClose = TRUE))
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
