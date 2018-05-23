library(xml2)

read.tlx <- function(tilia_file) {
  tilia_xml <- read_xml(tilia_file)
  
  tilia_spreadsheetbook <- xml_child(tilia_xml, "SpreadSheetBook")
  
  tilia_spreadsheet <- xml_child(tilia_spreadsheetbook, "SpreadSheet")
  
  columns <- xml2::xml_attrs(xml2::xml_find_all(tilia_spreadsheet, "//Col"))
  cols <- sapply(columns, function(x)as.numeric(x["ID"]))
  
  rows <- xml2::xml_attrs(xml2::xml_find_all(tilia_spreadsheet, "//cell"))
  rows <- sapply(rows, function(x)as.numeric(x["row"]))
  
  data_table <- matrix(nrow = max(rows), ncol = max(cols))
  
  n_xml_col_childs <- xml_length(tilia_spreadsheet)
  
  for (col_xml in xml_children(tilia_spreadsheet)) {
    this_col_id <- as.numeric(xml_attr(col_xml,"ID"))
    for (this_cell_xml in xml_children(col_xml)) {
      this_row_id <- as.numeric(xml_attr(this_cell_xml,"row"))
      data_table[this_row_id, this_col_id] <- xml2::xml_text(this_cell_xml)
    }
  }
  
  count_cols <- regexpr('#', data_table[1,]) < 0 & !is.na(data_table[1,])
  
  count_rows <- regexpr('#', data_table[,1]) < 0 & !is.na(data_table[,1])
  
  taxon.name <- data_table[count_rows,2]
  
  counts <- t(matrix(as.numeric(data_table[count_rows,count_cols]), nrow = length(taxon.name)))
  
  colnames(counts) <- taxon.name
  
  variable.element <- data_table[count_rows,3]
  
  variable.units <- data_table[count_rows,4]
  
  variable.context <- data_table[count_rows,5]
  
  variable.taphonomy <- data_table[count_rows,6]
  
  ecological.group <- data_table[count_rows,7]
  
  rva <- list(
    counts = counts,
    
    taxon.name = taxon.name,
    
    variable.element = variable.element,
    
    variable.units = variable.units,
    
    variable.context = variable.context,
    
    variable.taphonomy = variable.taphonomy,
    
    ecological.group= ecological.group
  )
  return(rva)
}