library(colorout)
library(dplyr)
library(stringr)
library(readr)
library(data.table)

###


setwd("/Users/tubuliferous/Desktop/gtex_analysis/")

bed_format_variants <- function(dt){
# For now treat indels as SNPs, i.e. 1-bp variants 
  setnames(dt, "#CHROM", "CHROM")
  dt <- genotypes %>% mutate(START = (POS - 1), END = POS)
  del_indx <- genotypes$ID %>% str_detect(".+:D")
  ins_indx <- genotypes$ID %>% str_detect(".+:I")
  dt$CLASS <- "SNP"
  dt$CLASS[del_indx] <- "DEL"
  dt$CLASS[ins_indx] <- "INS"
  # dt$end[del_indx] <- dt$start + nchar(dt$ALT)
  dt %>% select(CHROM, START, END, ID, REF, ALT, CLASS) %>% return
}

genotypes <- fread("reference/GTEx_genot_imputed_variants_info4_maf05_CR95_CHR_POSb37_ID_REF_ALT.txt")
bed_genotypes <- bed_format_variants(genotypes)
dim(bed_genotypes)
# Generate chr start/end positions for genotypes in gtex manifest 
fread("/Users/tubuliferous/Desktop/gtex_analysis/reference/GTEx_genot_imputed_variants_info4_maf05_CR95_CHR_POSb37_ID_REF_ALT.txt")


bedify_gtex <- function(dt){
  dt %>% mutate(start = SNP_Pos)
}





setwd("/Users/tubuliferous/Desktop/gtex_analysis/")

blood <- fread("/Users/tubuliferous/Desktop/gtex_analysis/download_data/GTEx_Analysis_V4_eQTLs/Whole_Blood.portal.eqtl")

blood %>% arrange(P_Val) %>% filter(1:1000)  


capture <- blood %>% bedify_gtex