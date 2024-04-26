#' Get and format raw data from datapath
#' @param datapath
#' @param vartime
#' @param varspace
#' @param vary
#' @return a tibble containing variables x, metric (name) and value, and possibly xcat
#' @export
#'
#' @examples
#'data=get_data("data/Lhassa_6_04_2024.csv",vary="VEGETATION_POLYGONS_p40")
#'data=get_data("data/Sabarmati_19_02_2024.csv",vary="VEGETATION_POLYGONS_p40")
get_data=function(datapath,vartime="DATE",varspace="Unnamed: 0",vary){
  data0=readr::read_csv(datapath)
  data=data0 %>%
    dplyr::mutate(time=data0[[vartime]],
                  ID=data0[[varspace]]) %>%
    dplyr::mutate(y_n=rep(vary,dplyr::n()),
                  y=data0[[vary]])%>%
    dplyr::select(ID,time,y_n,y)
  return(data)
}
