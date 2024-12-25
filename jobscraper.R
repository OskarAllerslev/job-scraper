library(rvest)
library(dplyr)
library(stringr)

# Base URL with pagination
base_url <- "https://www.jobindex.dk/jobsoegning?page="

# Initialize an empty data frame
all_jobs <- data.frame()

# Loop through pages (adjust range as needed)
for (i in 1:860) { 
  url <- paste0(base_url, i)
  
  # Read the page
  page <- tryCatch(read_html(url), error = function(e) NULL)
  
  # Skip if the page is invalid
  if (is.null(page)) {
    cat("Failed to scrape page", i, "\n")
    next
  }
  
  # Extract job titles
  job_titles <- page %>%
    html_nodes(".PaidJob-inner h4 a") %>%
    html_text() %>%
    str_trim()
  
  # Extract job links
  job_links <- page %>%
    html_nodes(".PaidJob-inner h4 a") %>%
    html_attr("href") %>%
    str_trim()
  
  
  job_companies <- page %>%
    html_nodes(".jix-toolbar-top__company") %>% # Corrected selector for the company name
    html_text(trim = TRUE)
  
 
  
  # Extract locations
  job_locations <- page %>%
    html_nodes(".PaidJob-inner .jix_robotjob--area") %>% # Updated selector for locations
    html_text() %>%
    str_trim()
  
  # Handle mismatched lengths
  max_length <- max(length(job_titles), length(job_companies), length(job_locations), length(job_links))
  
  job_titles <- c(job_titles, rep(NA, max_length - length(job_titles)))
  job_companies <- c(job_companies, rep(NA, max_length - length(job_companies)))
  job_locations <- c(job_locations, rep(NA, max_length - length(job_locations)))
  job_links <- c(job_links, rep(NA, max_length - length(job_links)))
  
  # Combine all data into a data frame
  jobs <- data.frame(
    Title = job_titles,
    Company = job_companies,
    Location = job_locations,
    Link = job_links,
    stringsAsFactors = FALSE
  )
  
  # Append to the main data frame
  all_jobs <- bind_rows(all_jobs, jobs)
  
  # Print progress
  cat("Scraped page", i, "\n")
}


write.csv(all_jobs, "all_jobs.csv", row.names = FALSE)
