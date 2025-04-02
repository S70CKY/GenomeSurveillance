# Load required libraries
library(data.table)
library(readxl)
library(writexl)  # For writing to Excel

# Set the main directory where the barcode folders are located
main_directory <- "/Users/s70cky/Desktop/Isolate_EGGNOG_XZ"

# Get the paths to all .xlsx files in the barcodeXX folders
file_list1 <- list.files(path = "/Users/s70cky/Desktop/Neuer Ordner", 
                         pattern = "*.xlsx", full.names = TRUE, recursive = TRUE)

# Output file for processed data (Excel)
output_file <- "/Users/s70cky/Desktop/processed_data.xlsx"

# Function to process each file
process_file <- function(file) {
  # Attempt to read the file and catch errors
  df <- tryCatch({
    read_excel(file, .name_repair = "unique")
  }, error = function(e) {
    message(paste("Error reading file:", file, "-", e$message))
    return(NULL)
  })
  
  # If the file was read successfully, check if necessary columns are present
  if (!is.null(df)) {
    if (!("query" %in% names(df)) || !("KEGG_ko" %in% names(df))) {
      warning(paste("Skipping file:", file, "- Missing required columns"))
      return(NULL)
    }
    
    # Convert to data.table for efficient processing
    setDT(df)  # Ensure df is a data.table object
    
    # Select the relevant columns and process data
    df <- df[, .(query, KEGG_ko)]  # Select only 'query' and 'KEGG_ko' columns
    df <- df[!is.na(KEGG_ko)]  # Remove rows with missing 'KEGG_ko' values
    df <- df[, .(query, KEGG_ko = unlist(strsplit(KEGG_ko, ","))), by = query]  # Split 'KEGG_ko' entries
    df[, barcode := basename(dirname(file))]  # Extract barcode (folder name as 'barcode')
  }
  
  return(df)
}

# Print total number of files to be processed
message(paste("Total files to process:", length(file_list1)))

# Create a list to store all data
all_data <- list()

# Loop through each file and process it
for (file in file_list1) {
  message(paste("Processing file:", file))  # Log which file is being processed
  
  df <- process_file(file)
  
  # Check if the dataframe is not empty
  if (!is.null(df) && nrow(df) > 0) {
    message(paste("Rows processed for", file, ":", nrow(df)))  # Log number of rows processed
    
    # Append the data to the list
    all_data[[length(all_data) + 1]] <- df
  } else {
    message(paste("Skipping file:", file, "- No valid data"))  # Log if no data to write
  }
  
  # Free memory and perform garbage collection
  rm(df)
  gc()
}

# Combine all data into one data.table
final_data <- rbindlist(all_data)

# Write the final data to an Excel file
write_xlsx(final_data, output_file)

message("Processing completed and data saved to Excel!")
