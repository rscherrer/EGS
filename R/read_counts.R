#' Read local census
#'
#' @param filename Path to the folder
#' @param habitat What habitat?
#' @param ecotype What ecotype?
#'
#' @export

read_counts <- function(folder, habitat, ecotype) {

  read_data(folder, paste0("count", habitat, ecotype))

}
