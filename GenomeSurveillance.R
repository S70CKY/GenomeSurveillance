# hirarchy in terms of similarities barcode -> family

# (1) barcode -> strain
# (2) strain -> species
# (3) species -> genus
# (4) genus -> family

library(readxl)
library(tidyr)
library(dplyr)

# df for single analysis
df_strain_header_barcode <- read_excel("/Users/s70cky/Desktop/isolate_metamaps.xlsx", sheet = "strains")[, -1]
df_species_header_barcode <- read_excel("/Users/s70cky/Desktop/isolate_metamaps.xlsx", sheet = "spices")[, -1]
df_genus_header_barcode <- read_excel("/Users/s70cky/Desktop/isolate_metamaps.xlsx", sheet = "genus")[, -1]
df_family_header_barcode <- read_excel("/Users/s70cky/Desktop/isolate_metamaps.xlsx", sheet = "family")[, -1]

# import matrix (KO's, barcode(i))
df_matrix_T_F <- read_excel("/Users/s70cky/Desktop/KO_barcode_matrix.xlsx")

# (1)
df_long <- df_strain_header_barcode %>%
  pivot_longer(cols = everything(), names_to = "Strain", values_to = "Barcode") %>%
  drop_na()

df_long_strain <- df_long %>%
  filter(Barcode %in% colnames(df_matrix_T_F))

strain_list <- list()

for (sp in unique(df_long_strain$Strain)) {
  barcodes <- df_long_strain %>% filter(Strain == sp) %>% pull(Barcode)
  df_filtered <- df_matrix_T_F[,c("KEGG_ko", barcodes), drop = FALSE]
  colnames(df_filtered)[1] <- sp
  strain_list[[sp]] <-df_filtered
}

for (sp in names(strain_list)) {
  strain_list[[sp]] <- strain_list[[sp]] %>%
    filter(rowSums(.[,-1] == 1) == ncol(.[,-1]))
}

for (strain in names(strain_list)) {
  colnames(strain_list[[strain]])[1] <- "KO"
}

# (2)
df_long_species <- df_species_header_barcode %>%
  pivot_longer(cols = everything(), names_to = "Species", values_to = "Barcode") %>%
  drop_na()

df_long_species <- df_long_species %>%
  filter(Barcode %in% colnames(df_matrix_T_F))

species_list <- list()

for (species in unique(df_long_species$Species)) {
  barcodes <- df_long_species %>% filter(Species == species) %>% pull(Barcode)
  valid_barcodes <- barcodes[barcodes %in% colnames(df_matrix_T_F)]  # Only keep existing barcodes
  
  if (length(valid_barcodes) > 0) {
    species_df <- df_matrix_T_F[, c("KEGG_ko", valid_barcodes), drop = FALSE]
    colnames(species_df)[1] <- "KO"
    
    species_df <- species_df %>%
      filter(rowSums(.[, -1] == 1) == ncol(.[, -1]))  # Keep only KO’s common in all barcodes
    
    if (nrow(species_df) > 0) {
      species_list[[species]] <- species_df
    }
  }
}

# (3)
df_long_genus <- df_genus_header_barcode %>%
  pivot_longer(cols = everything(), names_to = "Genus", values_to = "Barcode") %>%
  drop_na()

df_long_genus <- df_long_genus %>%
  filter(Barcode %in% colnames(df_matrix_T_F))

genus_list <- list()

for (genus in unique(df_long_genus$Genus)) {
  barcodes <- df_long_genus %>% filter(Genus == genus) %>% pull(Barcode)
  valid_barcodes <- barcodes[barcodes %in% colnames(df_matrix_T_F)]  # Only keep existing barcodes
  
  if (length(valid_barcodes) > 0) {
    genus_df <- df_matrix_T_F[, c("KEGG_ko", valid_barcodes), drop = FALSE]
    colnames(genus_df)[1] <- "KO"
    
    genus_df <- genus_df %>%
      filter(rowSums(.[, -1] == 1) == ncol(.[, -1]))  # Keep only KO’s common in all barcodes
    
    if (nrow(genus_df) > 0) {
      genus_list[[genus]] <- genus_df
    }
  }
}

# (4)
df_long_family <- df_family_header_barcode %>%
  pivot_longer(cols = everything(), names_to = "Family", values_to = "Barcode") %>%
  drop_na()

df_long_family <- df_long_family %>%
  filter(Barcode %in% colnames(df_matrix_T_F))

family_list <- list()

for (family in unique(df_long_family$Family)) {
  barcodes <- df_long_family %>% filter(Family == family) %>% pull(Barcode)
  valid_barcodes <- barcodes[barcodes %in% colnames(df_matrix_T_F)] 
  
  if (length(valid_barcodes) > 0) {
    family_df <- df_matrix_T_F[, c("KEGG_ko", valid_barcodes), drop = FALSE]
    colnames(family_df)[1] <- "KO"
    
    family_df <- family_df %>%
      filter(rowSums(.[, -1] == 1) == ncol(.[, -1]))
    
    if (nrow(family_df) > 0) {
      family_list[[family]] <- family_df
    }
  }
}

# 
rm(list = setdiff(ls(), c("species_list", "genus_list", "family_list", "strain_list")))

# extract families
for (name in names(family_list)) {
  assign(name, family_list[[name]])
}

