
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
#' @param z the variable based on which to segment
#' @param segcrit the criterion used for segmenting (can be "mean","var", or "meanvar"). Defaults to "mean".
#' @param pen.value
#' @return
#' @export
#' @examples
#' data(data_ganga)
#' data_metric=get_metric(data_ganga,metric="ACw_mean")
#' data_slopes=get_slopes(data_metric,y_trans="log10")
#' calc_table(data_slopes,z="slope")
calc_table=function(data,
                    z,
                    segcrit="mean",pen.value="log(n)"){
  z=rlang::sym(z)
  data=data %>%
    dplyr::mutate(zseg={{z}})
  z=rlang::sym(z)
  data=data %>%
    dplyr::mutate(z={{z}}) %>%
    dplyr::mutate(measured=dplyr::case_when(is.na(z)~FALSE,
                              !is.na(z)~TRUE)) %>%
    dplyr::mutate(z=replace_NA(z)) %>%
    dplyr::mutate(seg=define_segments(z,
                                      segcrit=segcrit,
                                      pen.value=pen.value)) %>%
    dplyr::group_by(seg) %>%
    dplyr::mutate(zmean=mean(z),
           zsd=sd(z)) %>%
    dplyr::mutate(zup=zmean+2*zsd,
           zlo=zmean-2*zsd) %>%
    dplyr::ungroup() %>%
    dplyr::select(-z)
  return(data)
}
