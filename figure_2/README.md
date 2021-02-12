# figure_2

## brachy_goslims_space.txt

- a space-delimited association file with B. distachyon gene IDs and GOSLIM terms

## wheat_goslims_space.txt

- a space-delimited association file with T. aestivum gene IDs and GOSLIM terms

## /GOSLIM_1.5_files

- This folder contains 3 folders, one for each plant genotype (Bd21-3, Sr9b, and W2691)

- These contain files with titles formatted like: "filtered_DE_[genotype]_#dpi_0.05_1.5_[up/down]_names_GOSLIM_fixed2.csv"

-- These files are association files between gene ID and GO term again, but only the differentially expressed genes (log2fc >= 1.5 and p-value < 0.05) are listed.

## Figure_2.R

- This script takes the wheat/brachy_goslims_space.txt files and creates the brachy/wheat GO universes

- Then, all the files in the Bd21-3, W2691, and Sr9b folders are read into a list of files

- TopGO is run on these with metadata being attached

- TopGO has to run each ontology (MF, BP, CC) separately so some of the code is repeated for the different ontologies

- Finally, data is combined and plotted