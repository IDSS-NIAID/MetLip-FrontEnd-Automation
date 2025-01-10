##### Required libraries ####
library(tidyverse) 

#### Read in data ####
# This script depends on the Plate_Metadata and Acquired_Samples_Data data frames.
# Plate_Metadata is generated from MetLipAutomation::generate_plate_meta_data() 
# Acquired_Samples_Data is currently made manually using TAS project submission details.

plate_data <- read.csv("Plate_Metadata.csv")  # Load plate metadata
aquired_samples <- read.csv("Acquired_Samples_Data.csv")  # Load acquired sample data


#### Data Pre-processing ####

# Remove rows with missing values and exclude QC and BLANK samples
# Missing values correspond to empty positions in the plate
temp_2 <- plate_data %>% 
  drop_na() %>%  # Remove rows with missing values
  filter(!str_detect(Submitted_name, "QC") & !str_detect(Submitted_name, "BLANK"))  # Exclude QC and BLANK samples

# Extract QC and BLANK sample data
qc_plate_data <- plate_data %>% 
  filter(str_detect(Submitted_name, "QC"))  # Filter QC samples

blank_plate_data <- plate_data %>% 
  filter(str_detect(Submitted_name, "BLANK"))  # Filter BLANK samples

# Extract unique project identifier
project_id <- unique(temp_2$Project_id)


#### Sequence Generator ####

# Loop through each unique MS method
for (method in unique(aquired_samples$MS_method)) {
  
  # Filter data for the current MS method and join with plate metadata
  select_method <- aquired_samples %>% 
    filter(MS_method == method) %>%  # Select MS method iteratively 
    left_join(plate_data, by = "Submitted_name") %>%  # Merge with plate data
    mutate(Randomization = NA) %>%  # Initialize randomization column
    group_by(Subject_id) %>%  # Group by subject
    mutate(Randomization = ifelse(is.na(Randomization), runif(n()), Randomization)) %>%  # Assign random values for randomization
    arrange(Randomization, .by_group = TRUE) %>%  # Arrange in randomized order
    ungroup() %>% 
    select(Project_id, Submitted_name, Vial, MS_method, Batch, Plate, Position, Injection_vol, Matrix, Randomization)  # Select relevant columns
  
  # Extract unique MS method name iteratively
  ms_method <- unique(select_method$MS_method)
  
  # Initialize lists to store results
  my_list <- list()
  my_list_holder <- list()
  
  # Loop through each unique matrix type
  for (mtx in unique(select_method$Matrix)) {
    
    # Filter data for the current matrix type iteratively
    isl_temp <- select_method %>% filter(Matrix == mtx)
    
    # Produce QC and BLANK sample entries
    new_qc_row <- data.frame(Project_id = project_id, 
                             MS_method = ms_method,
                             Vial = "QC-0", 
                             Condition = paste("QC", mtx, sep = "-"), 
                             Matrix = mtx, 
                             Batch = qc_plate_data$Batch, 
                             Plate = qc_plate_data$Plate,
                             Position = qc_plate_data$Position,
                             Injection_vol = qc_plate_data$Injection_vol)
    
    new_blank_row <- data.frame(Project_id = project_id, 
                                MS_method = ms_method,
                                Vial = "Blank-0", 
                                Condition = paste("Blank", mtx, sep = "-"), 
                                Matrix = mtx, 
                                Batch = blank_plate_data$Batch, 
                                Plate = blank_plate_data$Plate,
                                Position = blank_plate_data$Position,
                                Injection_vol = blank_plate_data$Injection_vol)
    
    # Add QC and BLANK rows to dataset
    isl_temp <- bind_rows(new_qc_row, new_blank_row, isl_temp)
    
    # Counter to track insertions
    insert_counter <- 0
    
    # Loop through rows to insert additional QC/BLANK samples periodically
    for (i in 1:nrow(isl_temp)) {
      
      qc_blanks <- isl_temp[i, ]  # Select current row
      
      # Every 10 samples, insert a new QC and BLANK sample
      if(i %% 10 == 0){
        insert_counter <- insert_counter + 1
        
        new_qc_row <- data.frame(Project_id = project_id,
                                 MS_method = ms_method,
                                 Vial = paste("QC", insert_counter, sep = "-"), 
                                 Condition = paste("QC", mtx, sep = "-"), 
                                 Matrix = mtx,
                                 Batch = qc_plate_data$Batch, 
                                 Plate = qc_plate_data$Plate,
                                 Position = qc_plate_data$Position,
                                 Injection_vol = qc_plate_data$Injection_vol)
        
        new_blank_row <- data.frame(Project_id = project_id,
                                    MS_method = ms_method,
                                    Vial = paste("Blank", insert_counter, sep = "-"), 
                                    Condition = paste("Blank", mtx, sep = "-"), 
                                    Matrix = mtx,
                                    Batch = blank_plate_data$Batch, 
                                    Plate = blank_plate_data$Plate,
                                    Position = blank_plate_data$Position,
                                    Injection_vol = blank_plate_data$Injection_vol)
        
        # Add the new QC and BLANK samples
        qc_blanks <- bind_rows(qc_blanks, new_qc_row, new_blank_row)
      }
      
      # Store results in list
      my_list[[i]] <- qc_blanks
    }
    
    # Insert final QC and BLANK samples
    last_insert_counter <- insert_counter + 1
    
    new_qc_row <- data.frame(Project_id = project_id, 
                             MS_method = ms_method,
                             Vial = paste("QC-last", last_insert_counter, sep = "-"), 
                             Condition = paste("QC", mtx, sep = "-"), 
                             Matrix = mtx,
                             Batch = qc_plate_data$Batch, 
                             Plate = qc_plate_data$Plate,
                             Position = qc_plate_data$Position,
                             Injection_vol = qc_plate_data$Injection_vol)
    
    new_blank_row <- data.frame(Project_id = project_id, 
                                MS_method = ms_method,
                                Vial = paste("Blank-last", last_insert_counter, sep = "-"), 
                                Condition = paste("Blank", mtx, sep = "-"), 
                                Matrix = mtx,
                                Batch = blank_plate_data$Batch, 
                                Plate = blank_plate_data$Plate,
                                Position = blank_plate_data$Position,
                                Injection_vol = blank_plate_data$Injection_vol)
    
    # Store final QC entry
    my_list[[mtx]] <- new_qc_row
    my_list_holder[[mtx]] <- my_list
  } 
  
  # Combine results into final dataset and export
  out_isl <- bind_rows(my_list_holder) %>% 
    mutate(Run_number = paste("Run", row_number(), sep = "-"),
           Data_file = paste(Project_id, Matrix, Vial, Run_number, sep = "_")) %>% 
    select(Vial, MS_method, Plate, Position, Injection_vol, Matrix, Data_file) %>% 
    rename(`Sample Name` = Vial, 
           `MS method` = MS_method,
           `Plate Position` = Plate,
           `Vial Position` = Position,
           `Injection Volume` = Injection_vol,
           `Sample Type` = Matrix,
           `Data File` = Data_file)
  
  # The exported sequence can be directly imported to SciexOS
  write.csv(out_isl, paste(Sys.Date(), project_id, ms_method, "Sequence.csv", sep = "_"), row.names = F)
}
