# take a list of data and metadata
# look at the data,
#   figure out the factors
#   process the factors
#   create factorized data
# return list with this new information

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

factorize_data <- function(lst) {
  data <- lst[["mvdata"]]
  md <- lst[["metadata"]]

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

  ffdata <- mdnew
  newlist <- list("mvdata" = lst[["mvdata"]],
                  "origmetadata" = lst[["metadata"]],
                  labels = labels, #ff,
                  "metadata" = ffdata)
}

# myData 
myData <- reactive({
})

myMetaData <- reactive({
})

# depend upon data and metadata
factorizedData <- reactive({
  td <- list("mvdata" = myData(),
             "metadata" = myMetaData())
  factorize_data(td)
})

# depend upon selected factor, threshold, and factorize data
#
#the old base data
processedData <- reactive({

   lst <- factorizedData()

   otutdata <- lst[["mvdata"]]
   md <- lst[["metadata"]]

   input$selectedFactor
   factorData <- md[, input$selectedFactor]

   #base data
   suppressWarnings(
      helper.data_by_auc(theOtut(), 
                         theSelectedFactor(), 
                         input$adj_method))
   #now filter it

})

