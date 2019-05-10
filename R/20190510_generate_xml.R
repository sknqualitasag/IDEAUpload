#' ---
#' title: Generate XML-Format from Imputing-Files
#' output: html_notebook()
#' ---


#' Read the example csv-file in data
s_imputing_vms_csv_file <- file.path("../data/VMS_SumUpLOG.1019.csv")
f_imputing_vms <- read.csv(file=s_imputing_vms_csv_file, header = TRUE, sep = ";", stringsAsFactors=FALSE)

#' Choose only the column of interest and the breeds
names(f_imputing_vms)
df_vms <- f_imputing_vms[,c(3,4,8:15,20:22)]
colnames(df_vms)[1] <- "id"
df_vms$MultiMUTTERmatch <- as.character(df_vms$MultiMUTTERmatch)
df_vms$VVsuspekt <- as.character(df_vms$VVsuspekt)
df_vms$ExterneSNP <- as.character(df_vms$ExterneSNP)
df_vms <- df_vms[df_vms$ImputationsRasse %in% c("LIM","AAN"),]

#' Check correctness of parentage
str(df_vms)
test <-subset(df_vms,
              df_vms$MultiVATERmatch == "" 
#              & df_vms$MultiMUTTERmatch == "" 
              & df_vms$OhneVATERmatch == "" 
              & df_vms$OhneMUTTERmatch == ""
              & df_vms$VaterPedigree == ""
              & df_vms$VaterSNP == ""
              & df_vms$MutterPedigree == ""
              & df_vms$MutterSNP == ""
#              & df_vms$VVsuspekt == ""
              & df_vms$MVsuspekt == ""
#              & df_vms$ExterneSNP == ""
)

