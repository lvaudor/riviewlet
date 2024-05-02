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
#' data_raw=readr::read_csv("data-raw/data_ganga.csv",show_col_types=FALSE)
#'data_summary=aggregate_data(data_raw, time_acc="year", space_acc=1)
#'cut_space(data_summary$data_aggregated$ID,breaks="50;100")
cut_space=function(x, breaks, prefix="R"){
  if(breaks[1]==""){breaks=c()}else{
    breaks=as.numeric(stringr::str_split(breaks,";")[[1]])
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

#' Categorize based on space or time variable
#'
#' @param x
#' @param breaks
#'
#' @return a categorical variable
#' @export
#'
#' @examples
#' data_raw=readr::read_csv("data-raw/data_ganga.csv",show_col_types=FALSE)
#'data_summary=aggregate_data(data_raw, time_acc="year", space_acc=1)
#'cut_time(data_summary$data_aggregated$DATE,breaks="2000;2005")
cut_time=function(x,breaks,prefix="P"){
  if(breaks[1]==""){breaks=c()}else{
    breaks=stringr::str_split(breaks,";")[[1]]
    # if breaks is passed as a year, then we need to convert it to a date
    if(stringr::str_detect(breaks[1],"\\d{4}$")){breaks=paste0(breaks,"-07-01")}
    breaks=lubridate::ymd(breaks)
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

