#' Command to execute via Console
args<-commandArgs(trailingOnly = TRUE)
print(args)
if(length(args)!=4) stop("didn't receive 4 arguments")
s_imputing_old_csv_file <-args[1]
s_imputing_vms_csv_file <-args[2]
s_imputing_si70_csv_file <-args[3]
s_imputing_xml_file <-args[4]
if(!file.exists(s_imputing_old_csv_file)) stop("1st argument isn't an existing file")
if(!file.exists(s_imputing_vms_csv_file)) stop("2nd argument isn't an existing file")
if(!file.exists(s_imputing_si70_csv_file)) stop("3rd argument isn't an existing file")

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


#' Read the "old" csv-file in data
#s_imputing_old_csv_file <- file.path("inst/extdata/test.csv")
tbl_imputing_old <- readr::read_delim(file=s_imputing_old_csv_file, delim = ";", guess_max = 5000)

#' Read the "new" csv-files in data
#s_imputing_vms_csv_file <- file.path("inst/extdata/test1.csv")
tbl_imputing_vms <- readr::read_delim(file=s_imputing_vms_csv_file, delim = ";", guess_max = 5000)
#s_imputing_si70_csv_file <- file.path("inst/extdata/testsi.csv")
tbl_imputing_si70 <- readr::read_delim(file=s_imputing_si70_csv_file, delim = ";", guess_max = 5000)

#' Both new files in one
tbl_imputing <- rbind(tbl_imputing_vms, tbl_imputing_si70)


#' Differenciate first and second file
tbl_first_file <- tbl_imputing_old  %>% select(ITBID)
tbl_second_file <- tbl_imputing  %>% select(ITBID)
#' New ids in a file
tbl_diff_file <- anti_join(tbl_second_file,tbl_first_file, by = c("ITBID"))
#' Similar ids in both files
tbl_to_update_file <- inner_join(tbl_first_file, tbl_second_file, by = c("ITBID"))

#' Check correctness of parentage for new ids
tbl_diff_file <- left_join(tbl_diff_file,tbl_imputing,by = c("ITBID"))
tbl_diff_file <- tbl_diff_file %>% select(c(1,4,8:15,20:22))
colnames(tbl_diff_file)[1] <- "id"
tbl_new_vms <- tbl_diff_file %>% filter(ImputationsRasse == "LIM" | ImputationsRasse == "AAN" | ImputationsRasse == "SIM")
tbl_correctParentage_vms <- tbl_new_vms %>% filter(is.na(MultiVATERmatch) 
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
cat("---new genotypes infos: ")
print(table(tbl_correctParentage_vms$ImputationsRasse))
tbl_correctParentage_vms$share <- "Y"
rm(tbl_new_vms);rm(tbl_diff_file)

#' Check correctness of parentage for already uploaded ids and update the share informations
tbl_to_update_file <- left_join(tbl_to_update_file,tbl_imputing,by = c("ITBID"))
tbl_to_update_file <- tbl_to_update_file %>% select(c(1,4,8:15,20:22))
colnames(tbl_to_update_file)[1] <- "id"
tbl_upadte_vms <- tbl_to_update_file %>% filter(ImputationsRasse == "LIM" | ImputationsRasse == "AAN" | ImputationsRasse == "SIM")
#' Parentage of already uploaded info are still i.O. -> no need to upload again this info
tbl_stillOKParentage_vms <- tbl_upadte_vms %>% filter(is.na(MultiVATERmatch) 
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
#' Parentage of already uploaded info are not anymore i.O -> share = N -> need to upload this info
tbl_ChangeShareParentage_vms <- anti_join(tbl_upadte_vms,tbl_stillOKParentage_vms, by = c("id"))
cat("---old genotypes infos, where share set to N due to change in the parentage: ")
print(table(tbl_ChangeShareParentage_vms$ImputationsRasse))

tbl_ChangeShareParentage_vms$share <- "N"
rm(tbl_first_file);rm(tbl_imputing_old);rm(tbl_imputing);rm(tbl_second_file);rm(tbl_stillOKParentage_vms);rm(tbl_to_update_file);rm(tbl_upadte_vms)

#' Pool data together to upload of new genotypes or incorrect parentage
tbl_IDEA <- rbind(tbl_correctParentage_vms,tbl_ChangeShareParentage_vms)

#' Add the status in new column according to https://qualitasag.atlassian.net/wiki/spaces/ZWS/pages/323322046/IDEA+-+Upload+Genotypen+Informationen
#' for Swiss population: pop = CHE
#' for genotyped animals that parentage correct are: genotyped = Y
#' for tissue of the sample : tissue = U
tbl_IDEA$pop <- "CHE"
tbl_IDEA$genotyped <- "Y"
tbl_IDEA$tissue <- "U"

#' Write CSV of genotypes with correct Parentage which would be upload
readr::write_delim(tbl_IDEA, delim =";",path = paste(Sys.Date(),sep = "_","GenotypForUploadingOnIDEA.csv"))

cat("---Genotypes Info to upload of IDEA: ", dim(tbl_IDEA)[1])
df <- tbl_IDEA %>% select(c(1,14:17))

#' Create of XML 
genoanim <- newXMLNode("interbull", attrs = c(type = "animinfo", version ="1.0"))
animals <- newXMLNode("animals", parent = genoanim)
for (i in 1:nrow(df)) {
  a <- newXMLNode("a", attrs = df[i,"id"],
                  parent = animals)
  
  appNames <- names(df)[names(df) != "id"]
  genobeef <- mapply(function(w, x, y, z)
    newXMLNode("GENO_BEEF", attrs = c(pop = w, genotyped = x, share = y, tissue = z), parent = a),
    df[i,appNames[2]],df[i,appNames[3]],df[i,appNames[1]],df[i,appNames[4]])
  
}
saveXML(genoanim, file = s_imputing_xml_file)

