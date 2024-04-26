#' Categorize based on space or time variable
#'
#' @param x
#' @param breaks
#' @param type can be either "numeric" or "date". Defaults to "numeric"
#'
#' @return a categorical variable
#' @export
#'
#' @examples
cut_spacetime=function(x,breaks,type="numeric",prefix="R"){
  if(breaks==""){breaks=c()}else{
    breaks=stringr::str_split(breaks,";")[[1]]
    if(type=="numeric"){as.numeric(breaks)}
    if(type=="date"){breaks=lubridate::ymd(breaks)}
  }
  breaks=c(min(x,na.rm=TRUE),
           breaks,
           max(x,na.rm=TRUE))
  xcut=cut(x,
           breaks,
           labels=paste0(prefix,1:(length(breaks)-1)),
           include.lowest=TRUE)
  return(xcut)
}
