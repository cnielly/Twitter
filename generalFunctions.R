### The functions used by the other files


#' To treat the tweets
#'
#' @param x a string
#'
#' @return a string
#' @export
#'
#' @examples
#' \dontrun{
#' treatTweet("The new iPhone sucks")
#' }
treatTweet <- function(x) {tolower(str_replace_all(x,"[^[:graph:]]", " "))}

