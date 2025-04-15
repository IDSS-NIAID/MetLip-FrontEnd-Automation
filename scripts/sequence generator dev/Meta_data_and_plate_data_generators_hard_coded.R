library(dplyr)

#### Meta Data Generator ####

# toy data, this example works. Increasing matrix types etc. will cause problems when generating plate data.
project_id <- "TEST-exp"
conditions <- c("Control", "Stim_a", "Stim_b")
matrices <- c("Plasma")
replicates <- 3
num_subjects <- 5

sample_volumes <- c(20, 10)
qc_volumes <- c(5, 5)
blank_volume <- 2





# Generate sample data for each matrix separately
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
  arrange(Subject, Condition, .by_group = TRUE) %>% 
  mutate(Vial = paste(sprintf("%03d", row_number()), sep = "_")) %>% 
  ungroup()





# the goal is to have the above chunk generate meta data from TAS sample list through a shiny app 
# the below chunk will use the meta data to develop a custom sequence



#### Plate Data Generator ####


# Initialize parameters for plate generation
plates <- 3
positions_per_plate <- 54
number_of_blanks <- 1
number_of_qc_samples <- 1




# Initialize counters and parameters
batch_number <- 1
plate_number <- 1
position <- 1


# Create a data frame to store the plate data
plate_data <- data.frame(Batch = integer(),
                         Plate = integer(),
                         Position = integer(),
                         Project_id = character(),
                         Vial = character(),
                         Matrix = character(),
                         stringsAsFactors = FALSE)




# use the sample list to calculate how many batches are required
plate_1_and_2_sample_position_max <- 54*2
plate_3_sample_position_max <- 54 - number_of_qc_samples - number_of_blanks




# break the data down by matrix and calculate how many batches are required per matrix type

temp_data_list <- list()

for (mtx in 1:length(matrices)) {
  
  a <- temp_1 %>% 
    filter(Matrix == matrices[mtx])
  
  assign(paste("temp_1", mtx, sep = "_"), a)
  
  total_batch_number <- ceiling(nrow(a)/(plate_1_and_2_sample_position_max + plate_3_sample_position_max))
  
  temp_data_list[[paste("temp_1", matrices[mtx])]] <- a
  
}


print(paste("This run will require", total_batch_number, "batches per matrix"))






# Initialize a tracker for the current row in temp_1
current_row <- 1

for (f in 1:length(temp_data_list)) {
  
  temp_2 <- temp_data_list[[f]]
  
  temp_2_matrix <- as.character(unique(temp_2$Matrix))
  
  number_of_samples <- nrow(temp_2)
  
  for (g in 1:total_batch_number) {
    
    
    # Check if the current row exceeds the number of samples
    if (current_row > nrow(temp_2) || is.na(temp_2$Vial[current_row])) {
      print("End of sample list reached. Stopping process.")
      break
    }
    
    
    # This loop fills positions for the first two plates.
    for (h in c(1:2)) {
      
      
      # Check if the current row exceeds the number of samples
      if (current_row == nrow(temp_2) || is.na(temp_2$Vial[current_row])) {
        print("End of sample list reached. Stopping process.")
        break
      }
      
      
      # Generate sample data for the first 108 samples which fills exactly two plates.
      for (i in current_row:min(current_row + 107, number_of_samples)) {
        
        if (plate_number < 3) {
          
          plate_data <- rbind(plate_data, data.frame(Batch = batch_number,
                                                     Plate = plate_number,
                                                     Position = position,
                                                     Project_id = project_id,
                                                     Vial = temp_2$Vial[i],
                                                     Matrix = temp_2_matrix))
          
          # Update position and plate/batch numbers
          position <- position + 1
          if (position > positions_per_plate) {
            position <- 1
            plate_number <- plate_number + 1
          }
        }
      }
      
      # Update the tracker for the current row
      current_row <- min(current_row + 108, number_of_samples)
    }
    
    
    
    
    
    # in this update, only one matrix is used within the for loop. So the 'number_of_qc_samples' was replaced with 1.
    plate_3_sample_position_max <- 54 - number_of_qc_samples - number_of_blanks
    
    
    plate_3_sample_number_max <- current_row + plate_3_sample_position_max - 1
    
    # Special treatment for plate 3, which holds the QCs and blank (this could be developed into an else block)
    if (plate_number == 3) {
      
      for (i in current_row:min(plate_3_sample_number_max, number_of_samples)) {
        
        plate_data <- rbind(plate_data, data.frame(Batch = batch_number,
                                                   Plate = plate_number,
                                                   Position = position,
                                                   Project_id = project_id,
                                                   Vial = temp_2$Vial[i],
                                                   Matrix = temp_2_matrix))
        # Update position
        position <- position + 1
      }
    }
    
    # Determine how many Matrix types there are in the current batch
    #batch_specific_matrix <- as.character(unique(plate_data$Matrix))
    
    # Add QC samples to the end of plate 3
    #for (j in 1:length(batch_specific_matrix)) {
    plate_data <- rbind(plate_data, data.frame(Batch = batch_number,
                                               Plate = 3,
                                               Position = plate_3_sample_position_max + 1,
                                               Project_id = project_id,
                                               Vial = paste("QC", temp_2_matrix, sep = "_"),
                                               Matrix = temp_2_matrix))
    #}
    
    # Add 1 blank to the last position of plate 3
    plate_data <- rbind(plate_data, data.frame(Batch = batch_number,
                                               Plate = 3,
                                               Position = plate_3_sample_position_max + 1 + 1,
                                               Project_id = project_id,
                                               Vial = "Blank",
                                               Matrix = "Blank")) 
    
    
    # Update the tracker for the current row
    current_row <- min(current_row + plate_3_sample_position_max, number_of_samples)
    
    # Reset for the next batch
    batch_number <- batch_number + 1
    plate_number <- 1
    position <- 1
  }
}

