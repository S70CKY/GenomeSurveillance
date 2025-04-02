# GenomeSurveillance
**Code**
## processed_data.R
Extract all the data from the barcode folders.

## create_matrix.R
Create the matrix for every barcode with the KO's as 0 = does not and 1 = has it.

## GenomeSurveillance.R
(02.04.25)
Creation of same KO's from strain -> species -> genus -> family (1-4).
Unique KOs comparing all families to each other (5).

(date onwards)
Unique KOs by comparing single families to each other (6)




**Files**
## isolate_metamaps.xlsx
Which barcode belongs to which strain -> species -> genus -> family.

## processed_data.xlsx
Barcodes with KOs.

## KO_barcode_matrix
Matrix derived from create_matrix.R

## family_unique_kos.csv
(5)

## family_differences.csv
(6)
