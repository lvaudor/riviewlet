#' If corresponding input does not exist, return default value
#' @param input list of input elements
#' @param name name of input
#' @param value default value
#' @return the default value if input does not exist or the input value if it does.
#' @export
#'
#' @examples
#' data=readr::read_csv("data-raw/data_Lhassa.csv")
#' dat=aggregate_data(data, time_acc="year")

get_default=function(input,name,value){
  result=ifelse(is.null(input[[name]]),
                        value,
                        input[[name]])
  return(result)
}
