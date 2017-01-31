#' @title Finds TypDB datasets 
#'
#' @description Finds all the datasets in TypDB for with data on given languages
#' 
#' @param language.code A characer vector of language codes (of any type)
#'
#' @param result Controls what is returned
#'
#' @return If \code{result} is \code{"data"}, a list of with appropriate data
#' frames. If \code{result} is \code{"names"}, a character vectors of dataset
#' names. 
#
#' @export
typdb.locate.dataset <- function(language.code, result = c("data", "names")) {
  # parse the options
  result = match.arg(result)
  
  # get the dataset lists
  dataset_names <- data(package = "TypDB")$results[, "Item"]
  
  # check which datasets match
  matches <- sapply(dataset_names, function(name) {
    data <- get(name)
    codeinfo = attr(data, "LanguageID")
    if(is.null(codeinfo)) return(FALSE)
      
    return(any(language.code %in% data[[codeinfo$var]]))
  })
  
  dataset_names <- dataset_names[matches]
  
  # return the correct result
  if(result == 'data') {
    return(lapply(dataset_names, get))
  } 
  if(result == 'names') {
    return(dataset_names)
  } 
  NULL
}
