
normalize_FD <- function(vec_exio_fd) {
  # vec_exio_fd: Get 9800x1 EXIO FD vector, given by pull(Y_hh, n)
  fd_mat = matrix(vec_exio_fd, ncol = 49)
  fd_sum = rowSums(fd_mat)
  
  # Get shares in each row
  f = diag(1/fd_sum) %*% fd_mat
  f[is.nan(f)] <- 0
  
  # Return 200x49 for an easier multiplication in map_to_exio()
  return(f)
}


#' Convert COICOP consumption vector to EXIOBASE final demand vectors
#'
#' @param vec_coicop A consumption vector in COICOP. It is a numerical vector with 164 entries.
#' @param country 2-letter ISO country code
#'
#' @return A list with 'dom' and 'imp' entries, which are respectively for domestic and imported consumption vectors in EXIO product classification. Each vector has a length of 9800 (200 products x 49 countries).
#' @export
#'
#' @examples final_cons_coicop = rep(1, 164)  # A dummy consumption vector for 164 categories
#' @examples fd = map_to_exio(final_cons_coicop, 'IN')  # For India
#' @examples footprint_dom = M %*% fd_query$dom
#' @examples footprint_imp = M %*% fd_query$imp

map_to_exio <- function(vec_coicop, country='AT') {
  # Convert 164 COICOP vector to 200 EXIO vector
  # vec_coicop: COICOP consumption profile 164x1
  
  if (!(country %in% c('BR', 'IN', 'ZA'))) {
    map_mtx = map_def
  }
  else {
    map_mtx = map_cty[[country]]
  }
  
  mat_exio = diag(vec_coicop) %*% as.matrix(map_mtx)
  vec_exio = colSums(mat_exio) # length = 200
  
  # Estimate import demand based on EXIO FD
  country_index = which(exio_ctys==country)
  if (length(country_index)==0) {
    stop(paste("Country", country, "doesn't belong to EXIOBASE."))
    
  }
  mat_exio = diag(vec_exio) %*% normalize_FD(pull(Y_hh, country_index)) # 200x49
  
  dom_id = seq((country_index-1)*200+1, country_index*200)
  imp_id = setdiff(1:9800, dom_id)
  
  vec_dom = vec_imp = matrix(mat_exio, ncol=1)
  vec_dom[imp_id] = 0
  vec_imp[dom_id] = 0
  
  # Return import and domestic separately
  vec = list(dom = vec_dom, imp = vec_imp)
  return(vec)
}