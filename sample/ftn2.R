apply_factorization <- function(d) {
  split_point <- quantile(d, probs = c(0, 0.5, 1))[2]
  newdata <- map(d,
          ~ { if (. <= split_point) {
                TRUE
            } else {
              FALSE
            }}) %>%
         unlist
  return(as.factor(newdata))
}

factorize_data <- function(md) {
  if(is.null(md)) {
    print("Factorize data , NULL")
    return(NULL)
  }

  ff <- all_factor_details(md)
  mdnew <- md
  labels <- vector(mode="character", length = ncol(md))
  for(i in 1:ncol(md)) {
    fdtl <- get_factor_details(md, i)
    labels[i] = fdtl$description[1]
    if(fdtl[1] != "boolean") {
      mdnew[, i] <- apply_factorization(md[, i])
    }
  }
  list(metadata = mdnew, labels = labels, factornames = colnames(md))
}

