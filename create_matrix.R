# create matrix with  0 and 1
library(readxl)
library(writexl)
library(ggplot2)
library(reshape2)

input_file <- "/Users/s70cky/Desktop/processed_data.xlsx"  

df <- input_file

if (all(df[[1]] == df[[2]])) {
  df <- df[, -c(1, 2)]  
}


df <- df[!grepl("^\\s*$", df$KEGG_ko), ]

df <- df[df$KEGG_ko != "-", ]

df$KEGG_ko <- gsub("^ko:", "", df$KEGG_ko)

output_file <- "/Users/s70cky/Desktop/df_all_barcodes.xlsx"
# write_xlsx(df, output_file)

matrix_data <- df %>%
  group_by(KEGG_ko, barcode) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = barcode, values_from = count, values_fill = list(count = 0)) %>%
  mutate(across(where(is.numeric), ~ ifelse(. > 0, 1, 0)))

print(matrix_data)
KO_barcode_matrix <- matrix_data
#write_xlsx(matrix_data, "/Users/s70cky/Desktop/KO_barcode_matrix.xlsx")

# heatmap, just for show
heatmap_data <- matrix_data %>%
  pivot_longer(cols = -KEGG_ko, names_to = "barcode", values_to = "presence") 


ggplot(heatmap_data, aes(x = barcode, y = KEGG_ko, fill = factor(presence))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "red", "1" = "blue")) +  
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  
        axis.title = element_blank(),  
        panel.grid = element_blank())
