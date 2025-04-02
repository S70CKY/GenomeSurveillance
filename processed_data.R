# get all the data out of folder

library(data.table)
library(readxl)
library(writexl)

file_list1 <- list.files(path = "/Users/s70cky/Desktop/Neuer Ordner", 
                         pattern = "*.xlsx", full.names = TRUE, recursive = TRUE)

output_file <- "/Users/s70cky/Desktop/processed_data.xlsx"

process_file <- function(file) {

  df <- tryCatch({
    read_excel(file, .name_repair = "unique")
  }, error = function(e) {
    message(paste("Error reading file:", file, "-", e$message))
    return(NULL)
  })
  
  if (!is.null(df)) {
    if (!("query" %in% names(df)) || !("KEGG_ko" %in% names(df))) {
      warning(paste("Skipping file:", file, "- Missing required columns"))
      return(NULL)
    }
    
    setDT(df)
    
    df <- df[, .(query, KEGG_ko)] 
    df <- df[!is.na(KEGG_ko)] 
    df <- df[, .(query, KEGG_ko = unlist(strsplit(KEGG_ko, ","))), by = query]
    df[, barcode := basename(dirname(file))]
  }
  
  return(df)
}

message(paste("Thymmäh:", length(file_list1)))

all_data <- list()

for (file in file_list1) {
  message(paste("Processing file:", file))  
  
  df <- process_file(file)
  
  if (!is.null(df) && nrow(df) > 0) {
    message(paste("My names Jeff", file, ":", nrow(df)))
    
    all_data[[length(all_data) + 1]] <- df
  } else {
    message(paste("Skipping file:", file, "- No valid data")) 
  }
  
  rm(df)
  gc()
}

final_data <- rbindlist(all_data)

rm(list = setdiff(ls(), c("final_data")))
# write_xlsx(final_data, output_file)

