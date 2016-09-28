setwd("/Users/afr/Desktop/Elisabetta/")
#### import the data ####
gn <- read.delim("mc.txt", header = F, sep = ",", stringsAsFactors = F)
colnames(gn) <- "Sp_names"
library(readxl)
taxonomy <- read_excel("IOC_6.3_vs_other_lists.xlsx")
## transform the data from excel class to dataframe
taxonomy_df  <- as.data.frame(taxonomy)

#### the function to find the gn names in taxonomy ####
find.names <- function(gn, taxonomy_df){
  sp_names_matched <- as.data.frame(matrix(nrow=0, ncol=4))
  i <- 1
  for(sp_name in seq_along(gn$Sp_names)){
    found <- 0
    for(column in seq_along(colnames(taxonomy))){
      if ((sum(taxonomy_df[,column] == gn$Sp_names[sp_name], na.rm = T) >= 1) & found < 1){
        row <- which(taxonomy_df[,column] == gn$Sp_names[sp_name])
        sp_names_matched[i,] <- c(gn$Sp_names[sp_name],taxonomy_df[row,2], row, column)
        found <- 1
        i <- i + 1
      }
    }
  }
  sp_names_matched
}
#### to run the function with the data ####
the_names_file <-  find.names(gn, taxonomy_df = taxonomy_df)
