str_min <- function(x){
  str_x <- stringr::str_extract_all(x,"\\d\\d|\\d",simplify = T) %>% as.numeric
  str_min <- min(str_x)

  if(is.infinite(str_min)){
    NA
  } else{
    str_min
  }
}

str_add <- function(x){
  str_x <- stringr::str_split(x,";",simplify = T) %>% as.numeric()
  str_addition <- sum(str_x)
  as.character(str_addition)
}

str_divide <- function(x){
  str_x <- stringr::str_split(x,"/",simplify = T) %>% as.numeric()
  str_division <- sum(str_x)/length(str_x)
  as.character(str_division)
}

str_average <- function(x){
  str_x <- stringr::str_split(x,"-",simplify = T) %>% as.numeric()

  if(length(str_x)<=1){
    return(as.character(str_x))
  }

  str_avg <- (min(str_x) + max(str_x))/2
  as.character(str_avg)
}

str_range <- function(x){
  str_x <- stringr::str_remove_all(x, "\\[|\\]")
  str_x <- stringr::str_split(str_x, "-", simplify = T) %>% as.numeric()
  if(str_x[2]>13 & !is.na(str_x[2])){
    str_x[2] <- 13
  }
  str_range <- abs(str_x[1]-str_x[2])
  ifelse(is.na(str_range),NA,paste0(str_range,";"))
}

ext_disrupt_period <- function(x,period = NULL){
  if(x == "[]"){
    return(0)
  }
  onset_strings <- stringr::str_remove_all(x, "^\\[|\\]$") %>% stringr::str_split("\\]\\[", simplify = T) %>% dplyr::as_data_frame()

  onset_data <- onset_strings %>%
    tidyr::gather() %>%
    tidyr::separate(value,c("onset","end"),"-") %>%
    dplyr::mutate_at(vars(2,3),funs(as.numeric)) %>%
    dplyr::mutate(
      end = ifelse(end>13,13,end),
      early = dplyr::case_when(
        onset <  5 & end <= 5 ~ end - onset,
        onset <  5 & end >  5 ~   5 - onset,
        onset >= 5 ~ 0
      ),
      late  = dplyr::case_when(
        onset >= 5 & end >  5  ~ end - onset,
        onset <  5 & end >  5  ~ end - 5,
        onset >= 5 & end >  13 ~ 13 - onset,
        onset <  5 & end >  13 ~ 13 - 5
      ))

  if(is.null(period)){
    onset_data
  }else if(period == "early"){
    onset_data %>% dplyr::summarize(early = sum(early,na.rm=T)) %>% dplyr::pull()
  } else if(period == "late"){
    onset_data %>% dplyr::summarize(late = sum(late,na.rm=T)) %>% dplyr::pull()
  }
}
