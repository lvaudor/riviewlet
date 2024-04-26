
# interpolation des données
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
replace_NA=function(x){
  x=zoo::na.approx(x,na.rm=FALSE)
  xnew=x
  ind=which(is.na(xnew))
  xnew[ind]=mean(x,na.rm=TRUE)
  return(xnew)
}

# détection des ruptures: packagepoint, algo PELT
#' Title
#'
#' @param x
#' @param segcrit
#' @param pen.value
#'
#' @return
#' @export
#'
#' @examples
find_breaks=function(x,segcrit="mean", pen.value="2*log(n)"){
  if(segcrit=="mean"){fseg=changepoint::cpt.mean}
  if(segcrit=="var"){fseg=changepoint::cpt.var}
  if(segcrit=="meanvar"){fseg=changepoint::cpt.meanvar}
  seg=fseg(x,
           method="PELT",
           minseglen=5,
           penalty="Manual",
           pen.value=pen.value)@cpts
  seg=seg[-length(seg)]
  return(seg)
}
# based on break points, attribute a segment number to each observation
#' Title
#'
#' @param x
#' @param segcrit
#' @param pen.value
#'
#' @return
#' @export
#'
#' @examples
define_segments=function(x,segcrit="mean",pen.value="2*log(n)"){
  cpts=find_breaks(x,
                   segcrit=segcrit,
                   pen.value=pen.value)
  series=rep(0,length(x))
  series[cpts]=1
  series=cumsum(series)+1
  return(series)
}

# calculate table with raw series + segment identifier + mean value per segment
#' Title
#'
#' @param data
#' @param segcrit
#' @param pen.value
#'
#' @return
#' @export
#'
#' @examples
calc_table=function(data,segcrit="mean",pen.value="log(n)"){
  tib=data %>%
    dplyr::mutate(measured=dplyr::case_when(is.na(y)~FALSE,
                              !is.na(y)~TRUE)) %>%
    dplyr::mutate(y=replace_NA(y)) %>%
    dplyr::mutate(seg=define_segments(y,
                               segcrit=segcrit,
                               pen.value=pen.value)) %>%
    dplyr::group_by(seg) %>%
    dplyr::mutate(ymean=mean(y),
           ysd=sd(y)) %>%
    dplyr::mutate(yup=ymean+2*ysd,
           ylo=ymean-2*ysd) %>%
    dplyr::ungroup()
  return(tib)
}
