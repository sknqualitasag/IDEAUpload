#' ---
#' title: Generate XML-Format
#' output: html_notebook()
#' ---
#' 

#' Read the example csv-file in inst/extdata
s_csv_file <- file.path("inst/extdata/genotyped_animal.csv")
df <- read.csv(file=s_csv_file, header = TRUE, sep = ";", stringsAsFactors=FALSE)

#' Create of XML 
library(XML)

genoanim <- newXMLNode("interbull", attrs = c(type = "animinfo", version ="1.0"))
animals <- newXMLNode("animals", parent = genoanim)
for (i in 1:nrow(df)) {
  a <- newXMLNode("a", attrs = c( id =df[i,"id"]),
                  parent = animals)
  
  appNames <- names(df)[names(df) != "id"]
  genobeef <- mapply(function(w, x, y, z)
    newXMLNode("GENO_BEEF", attrs = c(pop = w, genotyped = x, share = y, tissue = z), parent = a),
    df[i,appNames[1]], df[i,appNames[2]], df[i,appNames[3]], df[i,appNames[4]])
  
}
print(genoanim)
saveXML(genoanim, file="IDEAUploadFile.xml")

