library(rlist) # for list.parse

list_attributes <- function( la ){
  ln <- list()

  list_recursive <- function (lb){
    class_name = class(lb)
    lb <- if(is.environment(lb) ) as.list(lb) else lb
    lb <- if(is.expression(lb)) {
      expr_str <- list("expression" = as.character(lb))
      attributes(expr_str) <- attributes(lb)
      expr_str
    } else {
      lb
    }

    if( is.list(lb) ) {
      list(
        attrib = lapply(
          attributes(lb)
          ,function(lattrib){
            if(is.environment(lattrib)){
              capture.output(str(lattrib))
            } else{
              lattrib
            }
          }
        )
        , class = class_name
        , children = lapply( lb, list_recursive )
      )
    } else if( inherits(lb, c("zoo", "xts", "data.frame") ) ) {
      ldf <- list.parse( data.frame(lb) )
      list( attrib = attributes(lb), class = class_name, data = ldf)
    } else {
      list( class = class_name, value = lb)
    }
  }

  ln[[deparse(substitute(la))]] <- list_recursive(la)
  return(ln)
}

library(xts)

data(sample_matrix)
sample_xts <- as.xts(sample_matrix)
p <- plot( sample_xts[,"Close"])

# devtools::install_github( "timelyportfolio/listviewer" )
library( listviewer )
jsonedit( list_attributes(p) )


xts:::current.xts_chob()
structure(xts:::current.xts_chob(),class=c("replot_xts","environment"))
