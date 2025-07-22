
#### Install Packages As Needed ####
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Biostrings")


#### Load Required Libraries ####
library(Biostrings)
library(tidyverse)
library(stringr)


#### Read in data ####

# Proteins of Interest List
POI <- read.csv("Proteins_of_Interest.csv")

# Human protein FASTA database
fasta_file <- "sprot_human_032022_1.fasta"
protein_seqs <- readAAStringSet(fasta_file)


#### Coercion ####

# Extract headers and sequences
protein_df <- data.frame(
  header = names(protein_seqs),
  sequence = as.character(protein_seqs),
  stringsAsFactors = FALSE
)

# Parse the headers into helpful columns
protein_df$uniprot_id <- str_extract(protein_df$header, "\\|[^|]+\\|") %>%  str_remove_all("\\|")
protein_df$gene_name <- str_extract(protein_df$header, "GN=\\w+") %>% str_remove("GN=")
protein_df$organism  <- str_extract(protein_df$header, "OS=[^=]+") %>%  str_remove("OS=") |> str_trim()

# Reset the row ids
rownames(protein_df) <- NULL

# Filter the data frame for proteins of interest
filtered_df <- protein_df %>% 
  filter(uniprot_id %in% POI$uniprot_id)



#### Export filtered data frame ####
# Save as .csv
write.csv(filtered_df, row.names = F, file = "filtered_proteins.csv")


#### Convert the filtered data frame back to FASTA format ####

# Create named character vector
seqs_named <- setNames(filtered_df$sequence, filtered_df$header)

# Convert to AAStringSet
fasta_out <- AAStringSet(seqs_named)



##### Export as a FASTA file #####
writeXStringSet(fasta_out, filepath = "filtered_proteins.fasta")

