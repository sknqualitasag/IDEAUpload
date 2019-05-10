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

#' 1. Versuch
#' Loop funktionniert nicht, es überschreibt die vorherige Tiere
for (i in 1:nrow(df)) {
  genoanim <- newXMLNode("interbull", attrs = c(type = "animinfo", version ="1.0"))
  animals <- newXMLNode("animals", parent = genoanim)
  a <- newXMLNode("a", attrs = c( id =df[i,"id"]),
                  parent = animals)
  
  appNames <- names(df)[names(df) != "id"]
  genobeef <- mapply(function(w, x, y, z)
    newXMLNode("GENO_BEEF", attrs = c(pop = w, genotyped = x, share = y, tissue = z), parent = a),
    df[i,appNames[1]], df[i,appNames[2]], df[i,appNames[3]], df[i,appNames[4]])
  
}
genoanim


#' 2. Versuch
#' Ab dem Node animals ist i.O., jedoch Interbull node fehlt
xmlt_anim <- xmlTree("animals")
 for (i in 1:nrow(df)) {
    xmlt_anim$addNode("a", attrs = c(id = df[i,"id"]), close = FALSE)
     appNames <- names(df)[names(df) != "id"]
     mapply(function(w, x, y, z)
       xmlt_anim$addNode("GENO_BEEF", attrs = c(pop = w, genotyped = x, share = y, tissue = z), close = FALSE),
       df[i,appNames[1]], df[i,appNames[2]], df[i,appNames[3]], df[i,appNames[4]])
     xmlt_anim$closeNode()
    xmlt_anim$closeNode()
 }
xmlt_anim$doc()  
  