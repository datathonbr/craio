#' craio
#'
#' A Caio's random last name generator. 
#' @param l numeric. The parameter to generate a random index
#'
#' @return The generated full name. 
#' @examples{
#' \donttest{
#'   craio(5)
#'}
#'}
#' @export

craio <- function(l){
  d <- lexiconPT::oplexicon_v2.1
  pal <- stringr::str_detect(d$term, "ente$")
  names <- d[pal, ]$term
  ind <- rpois(n = 1, lambda = l)*3 
  
  if(ind <= length(names)){
    moanr::moanr(l, l)
    full_name <- stringr::str_c("Caio ", stringr::str_to_title(names[ind]))
    print(full_name)
  } 
}

