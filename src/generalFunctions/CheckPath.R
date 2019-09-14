# Function to check if a folder path have "/" at the end.

# Can be used for other suffixes, like "=" at the end of url queries.

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-08-17


CheckPath <- function(link, suffix = "/") {
  if(!grepl(paste0(suffix, "$"), link)) {
    return(paste0(link, suffix))
  }
}
  

