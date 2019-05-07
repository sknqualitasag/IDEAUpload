#' ---
#' title: Generate XML-Format
#' output: html_notebook()
#' ---
#' 

#' Read the example csv-file in inst/extdata
s_csv_file <- file.path("inst/extdata/genotyped_animal.csv")
df <- read.csv(file=s_csv_file, header = TRUE, sep = ";")

#' Create of XML 
#' https://stackoverflow.com/questions/39246686/how-to-create-xml-from-csv-properly
#library(XML)
#xml <- xmlTree("animals")
#for (i in 1:nrow(df)) {
#  xml$addNode("a", attrs = c(id = df[i,"id"]), close = FALSE)
##  appNames <- names(df)[names(df) != "id"]
##  for (j in appNames) {
##    xml$addNode(j, df[i, j])
##  }
#  xml$closeNode()
#}
#xml$closeNode()
#saveXML(xml$doc(), "animinfo.xml")
#xmlParse("animinfo.xml")

vNode = newXMLNode("GENO_BEEF",
                   attrs = c(type = "integer", length = 3))
