#' Final Energy Intensity Matrix for EXIO FD Vector
#'
#' A dataset containing total final energy intensities \code{M=S⋅L}
#'
#' @format A sparse matrix of the size 222x9800. Each row is for a specific carrier for a specific accounting category (NTRA, NENE, etc.).
#' @source Derived from EXIOBASE3 (provided by Arkaitz in Jan 2019)
#' @examples final_cons_coicop = rep(1, 164)  # 164 categories
#' @examples fd = map_to_exio(final_cons_coicop, 'IN')
#' @examples footprint_dom = M %*% fd$dom
"M"

#' Information on Final Energy Carriers included in the M Matrix 
#'
#' A dataset containing names and units of energy carriers
#'
#' @format A sparse matrix of the size 222x9800. Each row is for a specific carrier for a specific accounting category (NTRA, NENE, etc.).
#' \describe{
#'   \item{name}{Contains self-explanatory codes other than NENE (non-energy) & NTRA (non-transport)}
#'   \item{unit}{All in TJ}
#' }
#' @source Extracted from EXIOBASE3 (provided by Arkaitz in Jan 2019)
"FE_carriers"