extr <- function(o, var) {
  if(is.null(o[[var]])) NA else o[[var]]
}


#' @title Retrieve and merge TypDB datasets
#'
#' @description \code{typdb.data.merge} merges data from different TypDB
#' datasets based on the overlapping language codes (see \link{language_ids}). 
#  
#' @param ... Two or more TypDB dataset names
#' @param result An option controlling what to return (see details) 
#' 
#' @return A data frame that is either the merged datasets, the language code
#' mapping table or the join table indicating which rows will be joined
#' (depending on \code{result} parameter)
#' 
#' @details The \code{result} parameter controls what the function will return.
#' If the value of this parameter is \code{"data"} (default), the merged data
#' frame is returned. If \code{result} is \code{"mapping"}, the language
#' mappings required to perform the merge are returned instead (this is a
#' relevant subset of \link{language_ids} data frame). Finally, if \code{result}
#' is \code{"index"}, the result is a merged data frame consisting of row
#' numbers of the datasets that are merged together. This is useful in trackign
#' down any language code mismatches.
#'
#' The function will produce a warning if any language codes are duplicated,
#' missing, or if there is an ambigues match. Furthermore, the column names of
#' the resulting data frame will be disambiguated if nessesary.
#'
#' @export
typdb.data.merge <- function(..., result = c('data', 'mapping', 'index')) {
  # read in the parameters
  datasets <- list(...)
  result   <- match.arg(result)
  
  # early bailout if nothing to merge
  if(length(datasets)==1) return(datasets[[1]])
    
  # collect all the nessesary information in a table
  datasets <- data.frame(
    name =  make.names(sapply(substitute(list(...))[-1], deparse)),
    id_col = sapply(lapply(datasets, attr, "LanguageID"), extr, "var"),
    id_type = sapply(lapply(datasets, attr, "LanguageID"), extr, "standard"),
    data = I(datasets),
    # prevent R from creating factors
    stringsAsFactors=FALSE
  )  
  datasets$index.name <- paste0(datasets$name, ".row")
  
  # validate the language codes
  datasets$data <- mapply(
    datasets$name, datasets$data, datasets$id_col, datasets$id_type, FUN = function(name, data, var, typ) {
    
    # every language must have valid id annotation
    if(is.na(var) || is.na(typ) || !(typ %in% names(language_ids)) || !(var %in% names(data))) {
      stop(name, ": invalid or missing language id annotation!")
    }
    
    ids <- data[[var]]
    
    if(!all(ids %in% language_ids[[typ]])) {
      warning(name, ": language identifiers absent in the mapping table", call.=FALSE)    
    }
    
    # remove all is that are empty or unknown
    if(any(is.na(ids) | ids %in% "")) {
      warning(name, ": ids missing, removing affected rows!", call.=FALSE)
    }
    
    data[!(is.na(ids) | ids %in% ""), ,drop=F]
  }, SIMPLIFY=FALSE)
  
  # build a mapping table consisting only of language ids 
  # that occur in the tables we have to merge
  mapping_table <- with(datasets, {
    # get only the rows of language id table that have at least one relevant occuring id
    include_row <- Reduce(
      `|`,
      mapply(data, id_col, id_type, FUN = function(d, var, typ) {language_ids[[typ]] %in% d[[var]]})
    )
    # and only the relevant columns
    include_col <- names(language_ids) %in% datasets$id_type
    
    mappings <- unique(language_ids[include_row, include_col, drop=F]) 
    mappings <- mappings[complete.cases(mappings), ,drop=F]
  })
  rownames(mapping_table) <- NULL
  
  if(result == 'mapping') return(mapping_table)
  
  # check for ambigous mappings
  if(any(apply(mapping_table, 2, function(x) any(duplicated(x))))) {
    call <- match.call()
    call$result = "mapping"
    warning("Ambiguous language code mapping, run ", deparse(as.call(call)), " to inspect the mapping table", call.=F)
  }
  
  # build an join index by mergig meta-tables that include dataset rows
  index_join <- with(datasets, {
    # build index tables for individual datasets
    index_tables <- mapply(
      index.name, data, id_col, id_type,  FUN = function(name, data, var, typ) {
      index <- data.frame(1L:nrow(data), data[[var]])
      names(index) <- c(name, typ)
      merge(index, mapping_table, by = typ, all.x=TRUE, all.y=FALSE)
    }, SIMPLIFY=FALSE)
    
    # and join these tables together
    Reduce(function(a, b) merge(a, b, all=T), index_tables)
  })
  rownames(index_join) <- NULL

  if(result == 'index') return(index_join)
 
  # now do the actual join!
  # but first disambiguate data frame columns
  datasets$data <- local({
    # find ambigous names across the datasets
    dup_names <- unlist(lapply(datasets$data, names))
    dup_names <- dup_names[duplicated(dup_names)]
    
    mapply(datasets$name, datasets$data, FUN=function(name, data) {
      names(data) <- ifelse(names(data) %in% dup_names, paste(names(data), name, sep='.'), names(data))
      data
    }, SIMPLIFY=FALSE)
  })
  
  join_data <- mapply(datasets$index.name, datasets$data, FUN=function(index, data) {
    data[index_join[[index]], ,drop=F]
  }, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  
  join_data <- do.call(data.frame, c(join_data, list(check.names=F)))
  rownames(join_data) <- NULL
    
  join_data
}    
