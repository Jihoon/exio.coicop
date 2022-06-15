#' Load EXIOBASE3 data
#'
#' @return S, L, M, Y matrices as global variables
#' @export
#'
#' @examples load_EXIOBASE3()

load_EXIOBASE3 <- function() {
  L=vroom::vroom(here::here("..", "coicop_FE", "IOT_2015_pxp", "L_2015.csv"), col_names = FALSE)
  S=vroom::vroom(here::here("..", "coicop_FE", "IOT_2015_pxp", "S_2015.csv"), col_names = FALSE)
  Y=vroom::vroom(here::here("..", "coicop_FE", "IOT_2015_pxp", "Y_2015.csv"), col_names = FALSE)

  # Lables from Extension
  label.S <- readxl::read_xls(here::here("..", "coicop_FE", "IOT_2015_pxp", "labs_S_2011.xls"), col_names = FALSE)[,1:2] %>% rename(name = ...1, unit = ...2) # slice(1413:1707)

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
