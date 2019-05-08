#' ---
#' title: Generate XML-Format
#' output: html_notebook()
#' ---
#' 

#' Read the example csv-file in inst/extdata
s_csv_file <- file.path("inst/extdata/genotyped_animal.csv")
df <- read.csv(file=s_csv_file, header = TRUE, sep = ";")

#' Create of XML 
library(XML)
genoanim <- newXMLNode("interbull", attrs = c(type = "animinfo", version ="1.0"))
animals <- newXMLNode("animals", parent = genoanim)
a <- newXMLNode("a", attrs = c( id = "LIMIRLM12314567890"),
           parent = animals)
genobeef <- newXMLNode("GENO_BEEF", attrs = c(pop = "IRL", genotyped = "Y", share = "Y", tissue ="E"),
                       parent = a)
genoanim


#Alternative
newXMLNode("interbull",
           attrs = c(type = "animinfo", version ="1.0"), newXMLNode("animals",  newXMLNode("a",
                      attrs = c(id = "LIMIRLM12314567890")), newXMLNode("GENO_BEEF",
                                                                        attrs = c(pop = "IRL", genotyped = "Y", share = "Y", tissue ="E")))
)

#Anwendung Beispiele mit unsere Daten
b <- mapply(function(x)
  newXMLNode("a",
             attrs = c(id = x)),
            df[, 1])
grp <- newXMLNode("animals", .children = b)
subTree <- newXMLNode("grp","a", "GENO_BEEF")

doc = newXMLDoc()
newXMLNode("root", parent = doc)

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
