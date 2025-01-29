library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(writexl)
library(purrr)



# function to generate plate meta data. Alternative would be to source this function from a separate script.

generate_plate_meta_data <- function(sample_meta_data,
                                     num_wells = 54, num_blanks = 1, num_qc = 1, plates_per_batch = 3,
                                     path = 'Plate_Metadata.csv',
                                     randomize = FALSE, seed = 394875)
{
  # get rid of those pesky "no visible binding" errors
  if(FALSE)
    Matrix <- Batch <- Plate <- Position <- Project_id <- Vial <- merge_id <- reserve_blank <- reserve_qc <- NULL
  
  # sample_meta_data <- MetLipAutomation::generate_sample_meta_data('abc123', c('stim', 'control'), c('plasma', 'kidney', 'heart'), 5, 40, path = NULL)
  
  # assuming `sample_meta_data$Matrix` is a factor (fix if not)
  if(!is.factor(sample_meta_data$Matrix))
    sample_meta_data$Matrix <- as.factor(sample_meta_data$Matrix)
  
  
  # determine sample stats
  num_matrices <- levels(sample_meta_data$Matrix) |> length()
  num_samples <- nrow(sample_meta_data)
  samples_per_matrix <- num_samples / num_matrices
  
  
  # determine batch sizes and assign batch numbers for each row of `sample_meta_data`
  # we will assign `merge_id` to each row of `sample_meta_data` to match with `retval` below
  if(plates_per_batch * num_wells - (num_blanks + num_qc*num_matrices) > num_samples)  # do they all fit in one batch?
  {
    # one batch
    batches <- 1
    
    # one batch containing samples from all matrices
    batch_matrix <- unique(sample_meta_data$Matrix) |>
      as.numeric() |>
      list()
    
    # number of QC samples per batch
    num_qc_per_batch <- num_qc * num_matrices
    
    # add `merge_id` and `Batch` to `sample_meta_data`
    sample_meta_data <- sample_meta_data |>
      mutate(merge_id = row_number(),
             Batch = 1)
    
  }else if(plates_per_batch * num_wells - (num_blanks + num_qc) > samples_per_matrix){ # do they all fit in one batch per matrix?
    
    # num_matrices batches
    batches <- 1:num_matrices
    
    # one entry per batch listing the matrix for that batch
    batch_matrix <- unique(sample_meta_data$Matrix) |>
      as.numeric() |>
      as.list()
    
    # number of QC samples per batch
    num_qc_per_batch <- num_qc
    
    # add `merge_id` and `Batch` to `sample_meta_data`
    sample_meta_data <- sample_meta_data |>
      group_by(Matrix) |>
      mutate(merge_id = row_number(),
             Batch = as.integer(Matrix)) |>
      ungroup()
    
  }else{                                                                               # if neither, split into batches by matrix
    
    batches_per_matrix <- ceiling(samples_per_matrix / (plates_per_batch * num_wells - (num_blanks + num_qc)))
    
    # list of batches for each matrix
    batches <- map(1:num_matrices, ~ 1:batches_per_matrix + batches_per_matrix*(.x-1))
    
    # list of matrices for each batch
    batch_matrix <- map(unlist(batches), ~ ceiling(.x / batches_per_matrix))
    
    # number of QC samples and maximum number of samples per batch
    num_qc_per_batch <- num_qc
    max_samples_per_batch <- plates_per_batch * num_wells - (num_blanks + num_qc_per_batch)
    
    # assign samples to batches
    sample_order <- map(batches, ~ rep(.x, each = max_samples_per_batch))
    
    if(randomize) # randomize the order of samples
    {
      if(!is.null(seed))
        set.seed(seed)
      
      sample_order <- map(sample_order, ~ sample(.x))
    }
    
    # add `merge_id` and `Batch` to `sample_meta_data`
    sample_meta_data <- sample_meta_data |>
      group_by(Matrix) |>
      mutate(        #/---- grab batches for this Matrix ----\  /-- pick first n() --\
        Batch = sample_order[[unique(as.integer(Matrix))]][        1:n()         ]) |>
      ungroup() |>
      group_by(Matrix, Batch) |>
      mutate(merge_id = row_number()) |>
      ungroup()
    
  }
  
  
  # expand list of parameters into data.frame for all batches
  retval <- expand.grid(Batch = unlist(batches) |> unique(),
                        Plate = 1:plates_per_batch,
                        Position = 1:num_wells,
                        Project_id = unique(sample_meta_data$Project_id)) |>
    
    arrange(Batch, Plate, Position) |>
    
    # set up for merging sample data
    mutate(reserve_blank = Plate == plates_per_batch & Position > num_wells - num_blanks,
           reserve_qc    = Plate == plates_per_batch & Position > num_wells - num_blanks - num_qc_per_batch & !reserve_blank,
           merge_id = ifelse(reserve_blank | reserve_qc, NA, 1)) # need a `cumsum` by `Batch` that will ignore `NA` values -- take care of that below
  
  
  # calculate merge_id for `retval`, ignoring `NA` values
  for(i in unique(retval$Batch))
  {
    index <- which(retval$Batch == i & !is.na(retval$merge_id))
    retval$merge_id[index] <- cumsum(retval$merge_id[index])
  }
  
  
  # merge sample data
  retval <- left_join(retval, sample_meta_data, by = join_by(Batch, Project_id, merge_id))
  
  for(i in unique(retval$Batch))
  {
    retval$Vial[retval$Batch == i & retval$reserve_blank] <- 'BLANK'
    
    index <- retval$Batch == i & retval$reserve_qc
    retval$Vial[index] <- paste('QC', batch_matrix[[i]], sep = '')
    retval$Matrix[index] <- levels(sample_meta_data$Matrix)[batch_matrix[[i]]]
  }
  
  retval <- retval |>
    select(Batch, Plate, Position, Project_id, Vial, Matrix) |>
    filter(!is.na(Vial))
  
  
  # write csv if desired
  if(!is.null(path))
    write.csv(retval, path, row.names = FALSE)
  
  invisible(retval)
}


# define the UI (User Interface)
ui <- dashboardPage(
  dashboardHeader(title = "Meta Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sample Meta Data", tabName = "sample_meta", icon = icon("search")),
      menuItem("Plate Meta Data", tabName = "plate_meta", icon = icon("vial"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "sample_meta",
              fluidRow(
                box(title = "Upload Sample Meta Data", status = "primary", solidHeader = TRUE, 
                    fileInput("sample_file", "Choose Excel File", accept = c(".xlsx", ".xls")),
                    DTOutput("sample_table")
                )
              )
      ),
      tabItem(tabName = "plate_meta",
              fluidRow(
                box(title = "Generate Plate Meta Data", status = "primary", solidHeader = TRUE, 
                    actionButton("generate", "Generate Data"),
                    downloadButton("download_plate", "Download CSV"),
                    downloadButton("download_plate_excel", "Download Excel"),
                    DTOutput("plate_table")
                )
              )
      )
    )
  )
)

# write the server logic. The server function controls what happens when the users interact with the dashboard.
server <- function(input, output, session) {
  sample_data <- reactiveVal()
  plate_data <- reactiveVal()
  
  observeEvent(input$sample_file, {
    req(input$sample_file)
    df <- read_excel(input$sample_file$datapath)
    sample_data(df)
  })
  
  output$sample_table <- renderDT({
    req(sample_data())
    datatable(sample_data())
  })
  
  observeEvent(input$generate, {
    req(sample_data())
    plate_meta_data <- generate_plate_meta_data(sample_data()) # calls on our previously defined function
    plate_data(plate_meta_data)
  })
  
  output$plate_table <- renderDT({
    req(plate_data())
    datatable(plate_data())
  })
  
  output$download_plate <- downloadHandler(
    filename = function() { "plate_meta_data.csv" },
    content = function(file) {
      req(plate_data())
      write.csv(plate_data(), file, row.names = FALSE)
    }
  )
  
  output$download_plate_excel <- downloadHandler(
    filename = function() { "plate_meta_data.xlsx" },
    content = function(file) {
      req(plate_data())
      write_xlsx(plate_data(), file)
    }
  )
}

shinyApp(ui, server)
