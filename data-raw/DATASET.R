# Country labels for EXIOBASE3
exio_ctys <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
               "FR", "GR", 
               "HR", # Added new at EXIO3
               "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", 
               "PL", "PT", "RO", "SE", "SI", "SK", "GB", "US", "JP", "CN", 
               "CA", "KR", "BR", "IN", "MX", "RU", "AU", "CH", "TR", "TW", 
               "NO", "ID", "ZA", "WA", "WL", "WE", "WF", "WM")


load_exio_coicop_mapping <- function() {
  # Read in EXIO-COICOP mapping
  mapping_path = here::here("data-raw", "mapping")
  list.files(mapping_path)
  coicop_names = readxl::read_xlsx(paste0(mapping_path, '/ICP_EXIO_Qual_UN_Edited.xlsx'), range = readxl::cell_cols("A"))
  
  # Derive default mapping based on equal shares
  map_qual = readxl::read_xlsx(paste0(mapping_path, '/ICP_EXIO_Qual_UN_Edited.xlsx'), range = readxl::cell_cols(c("B", NA)))
  q_sum = rowSums(map_qual)
  map_def <<- diag(1/q_sum) %*% as.matrix(map_qual)
  map_def[is.nan(map_def)] <<- 0
  
  # Country-specific mappings from our three-country study
  map_cty <<- list()
  map_cty$IN <<- readxl::read_xlsx(paste0(mapping_path, '/IND.bridge.EXIO-COICOP.xlsx'), range = readxl::cell_cols(c("B", NA)))
  map_cty$ZA <<- readxl::read_xlsx(paste0(mapping_path, '/ZAF.bridge.EXIO-COICOP.xlsx'), range = readxl::cell_cols(c("B", NA)))
  map_cty$BR <<- readxl::read_xlsx(paste0(mapping_path, '/BRA.bridge.EXIO-COICOP.xlsx'), range = readxl::cell_cols(c("B", NA)))
}


load_EXIOBASE3 <- function() {
  L=vroom::vroom(here::here("data-raw", "exio", "L_2015.csv"), col_names = FALSE)
  S=vroom::vroom(here::here("data-raw", "exio", "S_2015.csv"), col_names = FALSE)
  Y=vroom::vroom(here::here("data-raw", "exio", "Y_2015.csv"), col_names = FALSE)
  
  # Lables from Extension
  label.S <- vroom::vroom(here::here("data-raw", "exio", "labs_S_2011.csv"), col_names = FALSE)[,1:2] %>% rename(name = X1, unit = X2) # slice(1413:1707)
  
  S_sparse <<- Matrix::Matrix(as.matrix(S))
  L_sparse <<- Matrix::Matrix(as.matrix(L))
  
  # Parse the final energy extensions
  idx.FE.NENE <<- grep("NENE", label.S$name)
  idx.FE.NTRA <<- grep("NTRA", label.S$name)
  idx.FE.TAVI <<- grep("TAVI", label.S$name)
  idx.FE.TMAR <<- grep("TMAR", label.S$name)
  idx.FE.TOTH <<- grep("TOTH", label.S$name)
  idx.FE.TRAI <<- grep("TRAI", label.S$name)
  idx.FE.TROA <<- grep("TROA", label.S$name)
  idx.FE.LOSS <<- grep("LOSS", label.S$name)
  idx.FE <<- c(idx.FE.NENE, idx.FE.NTRA, idx.FE.TAVI, idx.FE.TMAR, idx.FE.TOTH, idx.FE.TRAI, idx.FE.TROA)
  
  # Derive the stressor intensity matrix
  S_FE <<- S_sparse[idx.FE,]
  M <<- S_FE %*% as.matrix(L_sparse)
  
  # Select only the household FD
  Y_hh <<- Y[, seq(1, 343, 7)]
}


# Generate/Load base data
load_exio_coicop_mapping()
load_EXIOBASE3()

FE_carriers <- label.S[idx.FE,]

# Add entries to the package
usethis::use_data(M, internal = FALSE, overwrite = TRUE)
usethis::use_data(FE_carriers, internal = FALSE, overwrite = TRUE)
usethis::use_data(exio_ctys, map_def, map_cty, Y_hh, internal = TRUE, overwrite = TRUE)
