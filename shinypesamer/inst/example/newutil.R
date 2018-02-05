library(dplyr)
library(stringr)
library(purrr)

#
identify_factors <- function(df) {

  num_uniq <- map_int(names(df), function(x) {
      df %>% dplyr::pull(x) %>% unique %>% length })

  uniq <- map(names(df), function(x) {
      df %>% dplyr::pull(x) %>% unique })

  names(num_uniq) <- names(df)
  names(uniq) <- names(df)

  uniq_vals <- map_chr(names(df),
    function(x) {
      if(num_uniq[[x]] < 10) {
        s <- str_c(paste(uniq[[x]]), collapse = ",")
        if (nchar(s) < 20) {
          s
        } else {
          str_c("num unique values: ", num_uniq[x])
        }
      } else {
        str_c("num unique values: ", num_uniq[x])
      }
    })

  #names(uniq_vals) <- names(df)
  types <- map_chr(names(df), function(x) { class( uniq[[x]] ) })
  names(types) <- names(df)

  descriptions <- map_chr(names(df), function(x) {
    if(types[[x]] == "numeric") {
            vals <- df[[x]]
      sprintf("Numeric: min %4.3f max %4.3f median %4.3f mean %4.3f",
              min(vals, na.rm = TRUE),
              max(vals, na.rm = TRUE),
              median(vals, na.rm = TRUE),
              mean(vals, na.rm = TRUE))
    } else {
     sprintf("character, num values %d", num_uniq[[x]])
    }

    })

  # need labels
  new_df <- data_frame(name = colnames(df),
                       num_unique = num_uniq,
                       unique_values = uniq_vals,
                       method_applied = "none",
                       ready = (num_unique == 2),
                       type = types,
                       description = descriptions,
                       labels = "FALSE,TRUE",
                       true_labels = "TRUE",
                       idnum = 1:length(colnames(df)))
}
