library(dplyr)

# this function is to be run after temp_1 and plate_data objects have been generated

#### Plate Data Randomization ####

randomize_plate_data <- function(plate_data) {
  
  meta <- temp_1 %>% 
    select(Vial, Subject, Condition, Replicate)
  
  meta_names <- names(meta[ ,-1])
  
  set.seed(3.14159)
  
  randomized_plate_data <- plate_data %>%
    filter(Vial != "Blank") %>% 
    filter(Vial != "QC_Plasma") %>% 
    mutate(Randomization = NA) %>% 
    left_join(meta, by = "Vial") %>% 
    group_by(Subject) %>% 
    mutate(Randomization = ifelse(is.na(Randomization), runif(n()), Randomization)) %>%
    arrange(Randomization, .by_group = TRUE) %>% 
    ungroup() %>% 
    select(-all_of(meta_names), -Randomization, -Matrix, -Project_id) 
  
  
  assign("randomized_plate_data", randomized_plate_data, envir = .GlobalEnv)
  
  write.csv(randomized_plate_data, "Randomized_Plate_Data.csv", row.names = FALSE)
  cat("Data saved to 'Plate_Data.csv'.\n")
  
}

randomize_plate_data(plate_data)

