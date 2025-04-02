# Load necessary libraries
library(readxl)
library(writexl)

# Define the file paths
input_file <- "/Users/s70cky/Desktop/processed_data.xlsx"  # Path to your input Excel file

# Read the Excel file
df <- read_excel(input_file)

# Check if the first two columns are identical and remove them if so
if (all(df[[1]] == df[[2]])) {
  df <- df[, -c(1, 2)]  # Remove the first and second columns
}

# Remove rows where 'KEGG_ko' contains empty values
df <- df[!grepl("^\\s*$", df$KEGG_ko), ]

# Alternatively, if you want to check for specific empty entries like "-" (not just whitespace):
df <- df[df$KEGG_ko != "-", ]

# Remove 'ko:' prefix from the 'KEGG_ko' column
df$KEGG_ko <- gsub("^ko:", "", df$KEGG_ko)

output_file <- "/Users/s70cky/Desktop/df_all_barcodes.xlsx"
write_xlsx(df, output_file)


# Load necessary libraries
library(dplyr)
library(tidyr)

# Assuming 'df' already contains the processed data

# Create a binary matrix of KO vs. barcode (0 for absent, 1 for present)
matrix_data <- df %>%
  # Group by KEGG_ko and barcode and count occurrences
  group_by(KEGG_ko, barcode) %>%
  summarise(count = n(), .groups = 'drop') %>%
  # Pivot the data to create a matrix (KEGG_ko vs barcode)
  pivot_wider(names_from = barcode, values_from = count, values_fill = list(count = 0)) %>%
  # Replace any non-zero values with 1 (indicating presence)
  mutate(across(where(is.numeric), ~ ifelse(. > 0, 1, 0)))

# Print the resulting matrix
print(matrix_data)

# Optionally, save it to an Excel file
#write_xlsx(matrix_data, "/Users/s70cky/Desktop/KO_barcode_matrix.xlsx")

library(ggplot2)
library(reshape2)

# Assuming 'matrix_data' is already your binary matrix data (from previous code)

# Melt the matrix into a long format suitable for ggplot2
heatmap_data <- matrix_data %>%
  pivot_longer(cols = -KEGG_ko, names_to = "barcode", values_to = "presence") 

# Create the heatmap using ggplot2
ggplot(heatmap_data, aes(x = barcode, y = KEGG_ko, fill = factor(presence))) +
  geom_tile(color = "white") + # Tile with white borders
  scale_fill_manual(values = c("0" = "red", "1" = "blue")) +  # Colors for 0 and 1
  theme_minimal() +  # Minimal theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels
        axis.title = element_blank(),  
        panel.grid = element_blank())



# Define the selected Citrobacter koseri barcodes
selected_barcodes <- c("barcode33", "barcode45", "barcode80")

# Check which barcodes exist in the dataset
existing_barcodes <- selected_barcodes[selected_barcodes %in% colnames(matrix_data)]

# Subset the matrix to include only existing barcodes
citrobacter_koseri <- matrix_data %>%
  select(KEGG_ko, all_of(existing_barcodes))

# Print to check if all selected barcodes are present
print(colnames(citrobacter_koseri))

# Find rows where all selected barcodes have '1' (KO present in all barcodes)
shared_kos <- citrobacter_koseri %>%
  filter(rowSums(select(., -KEGG_ko)) == length(existing_barcodes))

# Print the shared KO list
print(shared_kos)


staphylococcaceae_barcodes <- c(
  "barcode05", "barcode07", "barcode13", "barcode14", "barcode15",
  "barcode16", "barcode17", "barcode18", "barcode19", "barcode20",
  "barcode22", "barcode23", "barcode24", "barcode30", "barcode34",
  "barcode35", "barcode36", "barcode37", "barcode38", "barcode39",
  "barcode41", "barcode42", "barcode04"
)

# Check which barcodes exist in the dataset
existing_staph_barcodes <- staphylococcaceae_barcodes[staphylococcaceae_barcodes %in% colnames(matrix_data)]

# Subset the matrix to include only existing Staphylococcaceae barcodes
staphylococcaceae_df <- matrix_data %>%
  select(KEGG_ko, all_of(existing_staph_barcodes))

# Find rows where all selected barcodes have '1' (KO present in all barcodes)
shared_staph_kos <- staphylococcaceae_df %>%
  filter(rowSums(select(., -KEGG_ko)) == length(existing_staph_barcodes))



library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

# Define barcode groups
barcode_groups <- list(
  Corynebacteriaceae = c("barcode27", "barcode26", "barcode33", "barcode31", "barcode25", "barcode21", "barcode12", "barcode10", "barcode09"),
  Dermacoccaceae = c("barcode28", "barcode29"),
  Enterobacteriaceae = c("barcode08", "barcode81", "barcode80", "barcode79", "barcode78", "barcode76", "barcode73", "barcode72", "barcode71", "barcode70", "barcode69", "barcode68", "barcode67", "barcode62", "barcode61", "barcode52", "barcode51", "barcode50", "barcode49", "barcode48", "barcode47", "barcode46", "barcode45"),
  Enterococcaceae = c("barcode32", "barcode40", "barcode43", "barcode82", "barcode83", "barcode84", "barcode85", "barcode86", "barcode87", "barcode88", "barcode89", "barcode90", "barcode91", "barcode93", "barcode94", "barcode95", "barcode96", "barcode01"),
  Morganellaceae = c("barcode74"),
  Staphylococcaceae = c("barcode05", "barcode07", "barcode13", "barcode14", "barcode15", "barcode16", "barcode17", "barcode18", "barcode19", "barcode20", "barcode22", "barcode23", "barcode24", "barcode30", "barcode34", "barcode35", "barcode36", "barcode37", "barcode38", "barcode39", "barcode41", "barcode42", "barcode04"),
  Streptococcaceae = c("barcode92")
)

# Function to filter KOs shared across all barcodes in a group
find_shared_kos <- function(group_name, barcodes, data) {
  existing_barcodes <- barcodes[barcodes %in% colnames(data)]
  if (length(existing_barcodes) == 0) return(NULL)
  
  filtered_df <- data %>% select(KEGG_ko, all_of(existing_barcodes))
  shared_kos <- filtered_df %>% filter(rowSums(select(., -KEGG_ko)) == length(existing_barcodes))
  
  return(shared_kos)
}

# Process each family separately and save in different dataframes
for (group in names(barcode_groups)) {
  assign(group, find_shared_kos(group, barcode_groups[[group]], matrix_data))
}
