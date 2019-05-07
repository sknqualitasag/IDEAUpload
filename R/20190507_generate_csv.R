#' ---
#' title: Generate CSV-Format
#' output: html_notebook()
#' ---
#' 

#' Read the example xml-file in inst/extdata
s_xml_file <- file.path(here::here(), "inst/extdata/genotyped_animal.xml")

#' try to read the content with xml2::read_xml()
# (xml_content <- xml2::read_xml(s_xml_file))

#' try with XML::xmlParse
doc = XML::xmlParse(s_xml_file)
geno_list <- XML::xmlToList(doc, addAttributes = TRUE)

#' extract animals from complete list
animal_list <- geno_list$animals


#' initialise result tibble
tbl_geno_result <- NULL
#' loop over animals and extract info
#'
for (lidx in seq_along(animal_list)){ # 1:length(animal_list), if animal_list empty ==> 1:0 
  # lidx <- 1
  #' current animal
  cur_animal <- animal_list[[lidx]]
  
  tbl_cura_id <- dplyr::bind_rows(cur_animal$.attrs)
  tbl_cura_genoinfo <- dplyr::bind_rows(cur_animal$GENO_BEEF)
  tbl_current_animal <- dplyr::bind_cols(tbl_cura_id, tbl_cura_genoinfo)
                                        
  if (is.null(tbl_geno_result)){
    tbl_geno_result <- tbl_current_animal
  } else {
    tbl_geno_result <- dplyr::bind_rows(tbl_geno_result, tbl_current_animal)
  }
  
} 

#' write to csv
#' 
s_csv_geno_file <- "genotyped_animal.csv"
readr::write_csv2(tbl_geno_result, path = s_csv_geno_file)
                  
                  