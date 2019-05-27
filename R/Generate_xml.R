#' ---
#' title: Generate XML-Format from Imputing-Files
#' output: html_notebook()
#' ---

#' Command to execute via Console
args<-commandArgs(trailingOnly = TRUE)
#if(length(args)!=3) stop("didn't receive 3 arguments")
if(length(args)!=2) stop("didn't receive 2 arguments")
s_imputing_vms_csv_file <-args[1]
#s_imputing_si70_csv_file <-args[2]
#s_imputing_xml_file <-args[3]
s_imputing_xml_file <-args[2]

if(!file.exists(s_imputing_vms_csv_file)) stop("1st argument isn't an existing file")
#if(!file.exists(s_imputing_si70_csv_file)) stop("2nd argument isn't an existing file")

#' Load requiered packages
 suppressPackageStartupMessages(if(! require("readr")) {
install.packages("readr", repos="https://stat.ethz.ch/CRAN/")
require("readr")
})

suppressPackageStartupMessages(if(! require("dplyr")) {
  install.packages("dplyr", repos="https://stat.ethz.ch/CRAN/")
  require("dplyr")
})
suppressPackageStartupMessages(if(! require("XML")) {
  install.packages("XML", repos="https://stat.ethz.ch/CRAN/")
  require("XML")
})

##Delete global environment
#rm(list=ls())
#
#' Read the example csv-file in data
#' vignette("readr") -> Rectangular parsers oder Available column specifications
#' option guess_max say until which line we should check before give a type like chr, logical
#s_imputing_vms_csv_file <- file.path("inst/extdata/test.csv")
#s_imputing_si70_csv_file <- file.path("inst/extdata/testsi.csv")
tbl_imputing_vms <- read_delim(file=s_imputing_vms_csv_file, delim = ";", guess_max = 5000)
#tbl_imputing_si70 <- readr::read_delim(file=s_imputing_si70_csv_file, delim = ";", guess_max = 5000)

#' Both files in one
#tbl_imputing <- rbind(tbl_imputing_vms, tbl_imputing_si70)
tbl_imputing <- tbl_imputing_vms

#' Choose only the columns and the breeds of interest
tbl_df_vms <- tbl_imputing %>% select(c(3,4,8:15,20:22))
colnames(tbl_df_vms)[1] <- "id"
tbl_df_vms <- tbl_df_vms %>% filter(ImputationsRasse == "LIM" | ImputationsRasse == "AAN" | ImputationsRasse == "SIM")

###' Check correctness of parentage
###' Following columns should be empty, otherwise the parentage is not correct
###' # https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e
tbl_correctParentage_vms <- tbl_df_vms %>% filter(is.na(MultiVATERmatch) 
                          & is.na(MultiMUTTERmatch)
                          & is.na(OhneVATERmatch)
                          & is.na(OhneMUTTERmatch)
                          & is.na(VaterPedigree)
                          & is.na(VaterSNP)
                          & is.na(MutterPedigree)
                          & is.na(MutterSNP)
                          & is.na(VVsuspekt)
                          & is.na(MVsuspekt)
                          & is.na(ExterneSNP))

##' Add the status in new column according to https://qualitasag.atlassian.net/wiki/spaces/ZWS/pages/323322046/IDEA+-+Upload+Genotypen+Informationen
##' for Swiss population: pop = CHE
##' for genotyped animals that parentage correct are: genotyped = Y
##' for correct parentage: share = Y
##' for tissue of the sample : tissue = U
tbl_correctParentage_vms$pop <- "CHE"
tbl_correctParentage_vms$genotyped <- "Y"
tbl_correctParentage_vms$share <- "Y"
tbl_correctParentage_vms$tissue <- "U"

table(tbl_correctParentage_vms$ImputationsRasse)
#Write CSV of genotypes with correct Parentage which would be upload
readr::write_delim(tbl_correctParentage_vms, delim =";",path = paste(Sys.Date(),sep = "_","GenotypForUploadingOnIDEA.csv"))

rm(tbl_df_vms);rm(tbl_imputing);rm(tbl_imputing_vms);rm(s_imputing_vms_csv_file)
#rm(tbl_imputing_si70);rm(s_imputing_si70_csv_file)
df <- tbl_correctParentage_vms %>% select(c(1,14:17))

#' Create of XML 
genoanim <- newXMLNode("interbull", attrs = c(type = "animinfo", version ="1.0"))
animals <- newXMLNode("animals", parent = genoanim)
for (i in 1:nrow(df)) {
  a <- newXMLNode("a", attrs = df[i,"id"],
                  parent = animals)
  
  appNames <- names(df)[names(df) != "id"]
  genobeef <- mapply(function(w, x, y, z)
    newXMLNode("GENO_BEEF", attrs = c(pop = w, genotyped = x, share = y, tissue = z), parent = a),
    df[i,appNames[1]], df[i,appNames[2]], df[i,appNames[3]], df[i,appNames[4]])
  
}
saveXML(genoanim, file = s_imputing_xml_file)
