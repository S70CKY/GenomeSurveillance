# hirarchy in terms of similarities barcode -> family

rm(list = ls())
gc()

# (1) barcode -> strain
# (2) strain -> species
# (3) species -> genus
# (4) genus -> family
# (5) diff in families

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
  valid_barcodes <- barcodes[barcodes %in% colnames(df_matrix_T_F)]  
  
  if (length(valid_barcodes) > 0) {
    species_df <- df_matrix_T_F[, c("KEGG_ko", valid_barcodes), drop = FALSE]
    colnames(species_df)[1] <- "KO"
    
    species_df <- species_df %>%
      filter(rowSums(.[, -1] == 1) == ncol(.[, -1]))
    
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
  valid_barcodes <- barcodes[barcodes %in% colnames(df_matrix_T_F)] 
  
  if (length(valid_barcodes) > 0) {
    genus_df <- df_matrix_T_F[, c("KEGG_ko", valid_barcodes), drop = FALSE]
    colnames(genus_df)[1] <- "KO"
    
    genus_df <- genus_df %>%
      filter(rowSums(.[, -1] == 1) == ncol(.[, -1]))
    
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


rm(list = setdiff(ls(), c("species_list", "genus_list", "family_list", "strain_list")))

for (name in names(family_list)) {
  assign(name, family_list[[name]])
}

# (5)
family_ko_list <- lapply(family_list, function(df) df$KO)

ko_counts <- table(unlist(family_ko_list))

family_unique_list <- list()

for (family in names(family_list)) {
  ko_set <- family_ko_list[[family]]
  unique_kos <- ko_set[ko_counts[ko_set] == 1]
  
  if (length(unique_kos) > 0) {
    family_unique_list[[family]] <- unique_kos
  }
}

max_length <- max(sapply(family_unique_list, length))
family_unique_matrix <- sapply(family_unique_list, function(x) c(x, rep(NA, max_length - length(x))))
family_unique_df <- as.data.frame(family_unique_matrix)

rm(list = setdiff(ls(), c("species_list", "genus_list", "family_list", "strain_list", "family_unique_list", "family_unique_df", "family_ko_list")))

# AFTER 02.04.25 MEETING
# (6) families between each other unique
# (7) send to ezra ko unique
# (8) run with NCBI 
# (9) group pathogenes (morg, enterobacterus, enteroc (in and pout try both)) to non pathog -> for unique 
# (10) aerocaccasea urinea (pathogene) 97% completion is cut (also streptococcus acalactrea if i find one)


# (11) modififed bases (comon barcodes add them up)
# (12) resistance sequence (NCBI)


# (6)
family_diff_list <- list()

for (family1 in names(family_list)) {
  for (family2 in names(family_list)) {
    if (family1 != family2) { 
      ko_set1 <- family_ko_list[[family1]]
      ko_set2 <- family_ko_list[[family2]]
      
      unique_to_family1 <- setdiff(ko_set1, ko_set2)  
      unique_to_family2 <- setdiff(ko_set2, ko_set1)  
      
      family_diff_list[[paste(family1, family2, sep = "_vs_")]] <- data.frame(
        Family1 = family1,
        Family2 = family2,
        Unique_to_Family1 = paste(unique_to_family1, collapse = ";"),
        Unique_to_Family2 = paste(unique_to_family2, collapse = ";")
      )
    }
  }
}

# Convert list to dataframe
family_diff_df <- do.call(rbind, family_diff_list)
rm(list = setdiff(ls(), c("family_diff_df", "species_list", "genus_list", "family_list", "strain_list", "family_unique_list", "family_unique_df", "family_ko_list")))

# (7)

# (8)

# (9)

# (10)

# (11)

# (12)
# MEETING 23.4 3 p.m.
