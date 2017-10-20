library(dplyr)
library(purrr)

# utility functions

fnum <- function(n) {
  format(n, digit = 5, big.mark = ",") 
}

# return data_frame with kind, name, description
get_factor_details <- function(md, m) {
  nn = names(summary(md[ , m]))
  this_name = colnames(md)[m]
  nf = first(nn)
  kind = "unknown"
  process_type = "unknown"

  # need names separates by commas?
  if(nf == "TRUE" || nf == "FALSE") {
    kind = "boolean"
    process_type = ifelse(nf == "TRUE", 
                          "TRUE, then FALSE", "FALSE, then TRUE")
    description = paste(this_name, 
                        ": Factor with two levels: True/False", sep="" )
  }

  # let's arbitrarily split it at mean
  # need to do this when we load it so it's the reactive value
  if(nf == "Min." ) {
    #get min max median
    kind = "numeric"
    process_type = "split at mean"
    vals = md[ , m]
    new_column = paste0("factor_",this_name)
    midpoint = mean(vals)
    orig_names = names(md)
    md[ , new_column ] = md[ , m ] > midpoint
       
    description = paste(this_name, 
                 ": Numeric: Min: ", fnum(min(vals)), 
                 " Max: ", fnum(max(vals)), 
                 " Mean:", fnum(mean(vals)), sep = "" )
  }

  data_frame(kind = kind, name = this_name, 
             description = description,
             process_type = process_type )
}


# maybe make this a data_frame
get_option_text <- function(md, m) {
  nn = names(summary(md[ , m]))
  this_name = colnames(md)[m]
  nf = first(nn)

  if(nf == "TRUE"  || nf == "FALSE") {
     return(paste(this_name, "Factor with two levels: True/False", sep=": " ))
  }

  # arbitrarily split at mean
  # need to do this when we load it so it's reactive value
  if(nf == "Min." ) {
    #get min max median
    vals = md[ , m]
    new_column = paste0("factor_",this_name)
    midpoint = mean(vals)
    
    orig_names = names(md)
    md[ , new_column ] = md[ , m ] > midpoint
       
    return(paste(this_name, 
                 "Numeric: Min: ", fnum(min(vals)), 
                 "Max: ", fnum(max(vals)), 
                 "Mean:", fnum(mean(vals)) ))
  }
  this_name
}

all_factor_details <- function(md) {
  if(is.null(md)) { return(NULL) }
  details = map(1:ncol(md), ~ get_factor_details(md, .))
  bind_rows(details)
}

new_factor_details <- function(md) {
  mtypes <- apply(md, 2, class)
  uniqvals <- apply(md, 2, function(x) { length(unique(x)) } )
  uniqvalues <- apply(md, 2, function(x) { 
         vv <- unique(x)
         if(length(vv) < 10) { 
           str_c(vv, collapse= ", ") 
         } else { 
           str_c("num unique values: ", length(unique(x))) 
         }} )

  df <- data_frame(type = mtypes, 
                   uniqvals = uniqvals,
                   uniqvalues = uniqvalues,
                   name = colnames(md), 
                   description = colnames(md)) %>% 
          mutate(methodapplied = "none",
                 ready = ifelse(uniqvals != 2, FALSE, TRUE),
                 description = ifelse(ready, name, str_c(name, sep=" "))) 
  
  df
}
