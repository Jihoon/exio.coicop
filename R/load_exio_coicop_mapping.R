#' Load pre-made EXIO-COICOP mappings
#'
#' @return map_cty list containing mappings for three countries ('IN', 'BR', 'ZA'), map_def for default mapping
#' @export
#'
#' @examples
load_exio_coicop_mapping <- function() {
  # Read in EXIO-COICOP mapping
  mapping_path = here::here("..", "Bridging", "EXIO-COICOP")
  list.files(mapping_path)
  coicop_names = readxl::read_xlsx(paste0(mapping_path, '/ICP_EXIO_Qual_UN_Edited.xlsx'), range = cell_cols("A"))

  # Derive default mapping based on equal shares
  map_qual = readxl::read_xlsx(paste0(mapping_path, '/ICP_EXIO_Qual_UN_Edited.xlsx'), range = cell_cols(c("B", NA)))
  q_sum = rowSums(map_qual)
  map_def <<- diag(1/q_sum) %*% as.matrix(map_qual)
  map_def[is.nan(map_def)] <<- 0

  # Country-specific mappings from our three-country study
  map_cty <<- list()
  map_cty$IN <<- readxl::read_xlsx(paste0(mapping_path, '/IND.bridge.EXIO-COICOP.xlsx'), range = cell_cols(c("B", NA)))
  map_cty$ZA <<- readxl::read_xlsx(paste0(mapping_path, '/ZAF.bridge.EXIO-COICOP.xlsx'), range = cell_cols(c("B", NA)))
  map_cty$BR <<- readxl::read_xlsx(paste0(mapping_path, '/BRA.bridge.EXIO-COICOP.xlsx'), range = cell_cols(c("B", NA)))
}
