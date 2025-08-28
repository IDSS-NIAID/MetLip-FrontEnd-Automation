# PD_filter.R
# Filter PD output for peptides with sufficiently complete data for each condition
# chowdhuryih-20250626-PI1

#############
# PD Filter #
#############

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# packages for filtering
library(readxl)
library(openxlsx)
library(readr)

#root <- here::here()
root <- "C:/Users/lacroixis/OneDrive - National Institutes of Health/01 Collaborators/zz_TEMP/Kevin"
# Load the correct version of MSfmtR
# MSfmtR::load_MSfmtR(file.path(root, 'lib'),
#                     commit = "...")
# devtools::load_all(file.path(root, "MSfmtR"))

#BiocManager::install("MSstats")

# install with 
#devtools::install_github("IDSS-NIAID/MSfmtR")

library(MSfmtR) 


### Configuration ###

request <- 'test'

config <- list(input_dir = file.path(root, 'data', 'PD', request),
               output_dir = file.path(root, 'data', 'PD', request),
               in_file = 'test.xlsx',
               in_sheet = c('Proteins'), # can be multiple sheets
               out_xlsx = paste0(request, '_filtered-test.xlsx'),
               
               #groups = ,
               filter_cols_prefix = c('Abundance:', 'Found in Sample:'),
               prop_good = 0.5,
               lloq = 1e6,
               valid_chr = 'High',
               max_ratio = 100,
               
               protein_alignment = parse(text = 'grep("Abundance:", names(proteins), value = TRUE, fixed = TRUE)[1]'),
               peptide_alignment = parse(text = 'grep("Abundance:", names(peptides), value = TRUE, fixed = TRUE)[1]'),
               protein_pid = 'Accession',
               peptide_pid = 'Protein Accessions')


#############
# PD Filter #
#############

filtered <- list()

for(j in seq_along(config$in_sheet)){
  
  ### Import Data ###
  
  dat <- read_PD(config, file = "C:/Users/lacroixis/OneDrive - National Institutes of Health/01 Collaborators/zz_TEMP/Kevin/data/PD/test/test.xlsx", in_sheet = config$in_sheet[j])
  proteins <- dat$proteins
  peptides <- dat$peptides
  
  # get all conditions
  conditions <- names(proteins)[starts_with(match = 'Abundance:', vars = names(proteins))] |>
    str_split(pattern = 'BR\\d+, ') |>
    map_chr(~ .x[2]) |>
    str_trim() |>
    unique()
  
  names(conditions) <- conditions
  
  # for non-standard conditions 
  # conditions <- c(G1 = 'G1', UF = 'Unfed')
  
  # flag filters
  for(i in seq_along(conditions))
  {
    # get the column names
    cols <- map(config$filter_cols_prefix, ~ 
                  tidyselect::matches(paste0(.x, ".*?", names(conditions)[i]), vars = names(proteins))) |>
      unlist()
    
    # check missingness for `cond`
    proteins[[paste0('drop_', conditions[i])]] <- check_missingness(proteins,
                                                                    cols,
                                                                    prop_good = config$prop_good,
                                                                    llod = config$lloq, # --- check_missingness parameter is in fact llod
                                                                    valid_chr = config$valid_chr)
  }
  
  
  ### Flag data for filtering ###
  
  # condition name conflicts can cause `filter_completeness` to fail
  # for example, use "(MIC)" when looking for MIC to avoid matches to "1/2 MIC"
  conditions_special <- conditions
  # conditions_special[conditions == 'MIC'] <- '(MIC)'
  
  proteins <- proteins |>
    
    filter_completeness(cols = tidyselect::starts_with('Abundance Ratio (log2):', vars = names(proteins)),
                        filter_cols = tidyselect::starts_with('drop_', vars = names(proteins)),
                        conditions = conditions_special,
                        ratio = TRUE, max_ratio = log2(config$max_ratio)) |>
    
    filter_completeness(cols = tidyselect::starts_with('Abundance Ratio:', vars = names(proteins)),
                        filter_cols = tidyselect::starts_with('drop_', vars = names(proteins)),
                        conditions = conditions_special,
                        ratio = TRUE, max_ratio = config$max_ratio) |>
    
    filter_completeness(cols = tidyselect::starts_with('Abundance Ratio Adj. P-Value:', vars = names(proteins)),
                        filter_cols = tidyselect::starts_with('drop_', vars = names(proteins)),
                        conditions = conditions_special,
                        ratio = TRUE)
  
  
  ### Apply filters ###
  
  # UPDATED
  # keep these proteins in the filtered set
  drop_mat <- dplyr::select(proteins, dplyr::starts_with("drop_")) |> as.matrix()
  keep <- !apply(drop_mat, 1, any)
  
  # all columns minus drop flags and redundant columns
  # (drop every column after the first drop_... column)
  export_cols <- 1:(starts_with('drop_', vars = names(proteins))[1] - 1)
  
  # filtered protein set
  proteins <- proteins[keep,export_cols]
  
  
  ### Remove Peak Found results ###
  
  cond_pairs <- combn(conditions, 2)
  for(p in 1:(dim(cond_pairs)[2]))
  {
    # check which direction the ratio goes
    ar_col  <- tidyselect::matches(paste0("Abundance Ratio:", ".*?",
                                          cond_pairs[1,p], ".*?", cond_pairs[2,p]),
                                   vars = names(proteins))
    lar_col <- tidyselect::matches(paste0("Abundance Ratio \\(log2\\):", ".*?",
                                          cond_pairs[1,p], ".*?", cond_pairs[2,p]),
                                   vars = names(proteins))
    
    if(length(ar_col) == 1 & length(lar_col) == 1)
    {
      ratio_direction <- 'a/b'
    }else{
      ar_col  <- tidyselect::matches(paste0("Abundance Ratio:", ".*?",
                                            cond_pairs[2,p], ".*?", cond_pairs[1,p]),
                                     vars = names(proteins))
      lar_col <- tidyselect::matches(paste0("Abundance Ratio \\(log2\\):", ".*?",
                                            cond_pairs[2,p], ".*?", cond_pairs[1,p]),
                                     vars = names(proteins))
      
      if(length(ar_col) == 1 & length(lar_col) == 1)
      {
        ratio_direction <- 'b/a'
      }else{
        paste('Ratio of', cond_pairs[1,p], 'and', cond_pairs[2,p], 'not found. Skipping.') |>
          warning()
        next
      }
    }
    
    # find columns for each condition in the current pair
    grp_abund_a  <- tidyselect::matches(paste0('Group Abundance', ".*?", cond_pairs[1,p]),
                                        vars = names(proteins), ignore.case = FALSE)
    grp_abund_b  <- tidyselect::matches(paste0('Group Abundance', ".*?", cond_pairs[1,p]),
                                        vars = names(proteins), ignore.case = FALSE)
    
    found_cols_a <- tidyselect::matches(paste0('Found in Sample', ".*?", cond_pairs[1,p]),
                                        vars = names(proteins), ignore.case = FALSE)
    found_cols_b <- tidyselect::matches(paste0('Found in Sample', ".*?", cond_pairs[2,p]),
                                        vars = names(proteins), ignore.case = FALSE)
    
    normd_cols_a <- tidyselect::matches(paste0('Abundances \\(Normalized\\)', ".*?", cond_pairs[1,p]),
                                        vars = names(proteins), ignore.case = FALSE)
    normd_cols_b <- tidyselect::matches(paste0('Abundances \\(Normalized\\)', ".*?", cond_pairs[2,p]),
                                        vars = names(proteins), ignore.case = FALSE)
    
    # go through and fix all `Peak Found` columns
    # (this might be redundant, but the code is easier to read)
    for(k in 1:dim(proteins)[1])
    {
      drop <- which(proteins[k,c(found_cols_a, found_cols_b)] == 'Peak Found')
      
      if(any(!is.na(proteins[k,c(normd_cols_a, normd_cols_b)[drop]])))
      {
        # drop `Peak Found` results
        proteins[k,c(normd_cols_a, normd_cols_b)[drop]] <- NA
        
        # update group abundances
        a <- median(unlist(proteins[k,normd_cols_a]), na.rm = TRUE)
        b <- median(unlist(proteins[k,normd_cols_b]), na.rm = TRUE)
        
        if(length(grp_abund_a) == 1 & length(grp_abund_b) == 1) # these aren't always included
        {
          proteins[[grp_abund_a]][k] <- a
          proteins[[grp_abund_b]][k] <- b
        }
        
        # ratio update
        ratio_updt <- case_when(is.na(a) & is.na(b) ~ NA, # shouldn't find any of these, but...
                                is.na(a) ~ 0.01,
                                is.na(b) ~ 100,
                                TRUE     ~ a / b) |>
          max(config$max_ratio) |>
          min(1 / config$max_ratio)
        
        # flip the ratio if it is going in the other direction
        if(ratio_direction == 'b/a')
          ratio_updt <- 1/ratio_updt
        
        proteins[[ ar_col]][k] <- round(     ratio_updt , 2)
        proteins[[lar_col]][k] <- round(log2(ratio_updt), 2)
      }
    }
  }
  
  # lastly, save the results for this sheet
  filtered[[ config$in_sheet[j] ]] <- list(dat = dat,
                                           proteins = proteins,
                                           peptides = peptides)
}


### format the workbook ###

if(!file.exists(file.path(config$output_dir, config$out_xlsx)))
{
  for(j in seq_along(filtered))
  {
    if(j == 1)
      wb <- NULL
    
    # filtered sheet
    wb <- process_wb(filtered[[j]]$proteins, filtered[[j]]$peptides, config,
                     save_intermediate = FALSE, sort_cols = FALSE,
                     protein_alignment = starts_with('Abundance:', vars = names(proteins))[1],
                     peptide_alignment = starts_with('Abundance:', vars = names(peptides))[1],
                     protein_pid = 'Accession', peptide_pid = 'Protein Accessions',
                     wb = wb, sheet = paste(names(filtered)[j], 'Filtered'),
                     wb_checkpoint = 'ignore') # this get around a bug in how `updt_config` creates wb_checkpoint when in_sheet has multiple values
    
    wb <- process_wb(filtered[[j]]$dat$proteins, filtered[[j]]$peptides, config,
                     save_intermediate = FALSE, sort_cols = FALSE,
                     protein_alignment = starts_with('Abundance:', vars = names(proteins))[1],
                     peptide_alignment = starts_with('Abundance:', vars = names(peptides))[1],
                     protein_pid = 'Accession', peptide_pid = 'Protein Accessions',
                     wb = wb, sheet = paste(names(filtered)[j], 'Unfiltered'),
                     wb_checkpoint = 'ignore') # this get around a bug in how `updt_config` creates wb_checkpoint when in_sheet has multiple values
  }
  
  saveWorkbook(wb, file = file.path(config$output_dir, config$out_xlsx))
}

