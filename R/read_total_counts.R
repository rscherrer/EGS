#' Read total census
#'
#' @param filename Path to the folder
#'
#' @export

read_total_counts <- function(folder) {

  n00 <- read_counts(folder, habitat = 0, ecotype = 0)
  n01 <- read_counts(folder, habitat = 0, ecotype = 1)
  n10 <- read_counts(folder, habitat = 1, ecotype = 0)
  n11 <- read_counts(folder, habitat = 1, ecotype = 1)

  mapply(sum, n00, n01, n10, n11)

}
