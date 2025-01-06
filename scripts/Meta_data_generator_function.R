# Load necessary library
library(dplyr)


#### Meta Data Generator ####

# Function to create the sample meta data using input from the collaborator
generate_sample_meta_data <- function() {
  
  # Prompt user for inputs
  project_id <- readline("Enter Project ID: ")
  conditions <- strsplit(readline("Enter conditions (Space Separated): "), " ")[[1]]
  matrices <- strsplit(readline("Enter matrices (Space Separated): "), " ")[[1]]
  replicates <- as.numeric(readline("Enter number of replicates: "))
  num_subjects <- as.numeric(readline("Enter number of subjects in the study: "))
  

  full_data <- list()
  
  for (matrix in matrices) {
    matrix_data <- expand.grid(
      Project_id = project_id,
      Subject = 1:num_subjects,
      Condition = conditions,
      Matrix = matrix,
      Replicate = 1:replicates
    )
    
    full_data[[matrix]] <- matrix_data
  }
  
  
  temp_1 <- bind_rows(full_data) %>% 
    
    group_by(Matrix) %>% 
    arrange(Subject, .by_group = TRUE) %>% 
    mutate(Vial = paste(sprintf("%03d", row_number()), sep = "_")) %>% 
    ungroup()
  
  
  assign("temp_1", temp_1, envir = .GlobalEnv)
  
  write.csv(temp_1, "Sample_Meta_Data.csv", row.names = FALSE)
  cat("Data saved to 'Sample_Meta_Data.csv'.\n")
  
  
}  



# Run the function
generate_sample_meta_data()







