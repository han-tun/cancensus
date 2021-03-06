# Internal functions that do useful things frequently required in other functions

cancensus_base_url <- function(){
  url <- getOption("cancensus.base_url")
  if (is.null(url)) url <- "https://censusmapper.ca"
  url
}

clean_vector_list <- function(vector_list,dataset=NULL){
  if (!("data.frame") %in% class(vector_list)) {
    if (class(vector_list)=="character") {
      if (is.null(dataset))  dataset <- dataset_from_vector_list(vector_list)
      vector_list = list_census_vectors(dataset) %>%
        dplyr::filter(vector %in% vector_list)
    } else
      stop(paste0("Don't know how to parse vector list: ",vector_list))
  }
  vector_list
}

dataset_from_vector_list <- function(vector_list){
  dataset <- attr(vector_list,'dataset')
  if (is.null(dataset)) {
    vectors = ifelse(class(vector_list)=="character",vector_list,vector_list$vector)
    dataset <- vectors %>%
      as.character() %>%
      lapply(function(d)unlist(strsplit(d,"_"))[2]) %>%
      unlist() %>%
      unique()
    if (length(dataset)!=1) stop("Unable to determine dataset")
  }
  dataset
}

cancensus_na_strings <- c("x", "F", "...", "..", "-","N","*","**")

as.num = function(x, na.strings = cancensus_na_strings) {
  stopifnot(is.character(x))
  na = x %in% na.strings
  x[na] = 0
  x = as.numeric(x)
  x[na] = NA_real_
  x
}

as.int = function(x, na.strings = cancensus_na_strings) {
  stopifnot(is.character(x))
  na = x %in% na.strings
  x[na] = 0
  x = as.integer(x)
  x[na] = NA_integer_
  x
}

# List of eligible datasets
# VALID_DATASETS <- c("CA1996","CA01","CA06","CA11","CA16",
#                     "CA01xSD", "CA06xSD", "CA11xSD", "CA16xSD",
#                     "TX2000", "TX2001", "TX2002", "TX2003", "TX2004",
#                     "TX2005", "TX2006", "TX2007", "TX2008", "TX2009",
#                     "TX2010", "TX2011", "TX2012", "TX2013", "TX2014", "TX2015", "TX2016", "TX2017")


