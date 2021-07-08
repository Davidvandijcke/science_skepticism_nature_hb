#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}






## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
## END



# function to lag variables within a panel #
lagPanel <- function( idvar, timevar, value, suffix = '',lags=1 ){
  df <- data.frame( idvar, timevar, value)
  last.time <- df %>% dplyr::filter(!is.na(timevar)) %>%
    dplyr::mutate(timevar = timevar + lags, lagged_value = value, value = NULL)
  out <- as.matrix(df %>% dplyr::left_join(last.time, by = c("idvar", "timevar")))[,4]
  colnames( out ) <- NULL
  return( out )
}
# end of lag panel #




# function to lead variables within a panel #
leadPanel <- function( idvar, timevar, value, suffix = '',lags=1 ){
  df <- data.frame( idvar, timevar, value)
  last.time <- df %>% dplyr::filter(!is.na(timevar)) %>%
    mutate(timevar = timevar - lags, lagged_value = value, value = NULL)
  out <- as.matrix(df %>% left_join(last.time, by = c("idvar", "timevar")))[,4]
  colnames( out ) <- NULL
  return( out )
}
# end of lead panel #