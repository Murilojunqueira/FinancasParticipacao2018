# Unzip file and store in temp file.

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-08-17


unzipTemp <- function(DirOutputRaw) {
  
  # Temp dir and file to store Unziped file
  tempdir <- dirname(tempfile())
  
  # Unzip file
  UnzipedFile <- unzip(zipfile = paste0(DirOutputRaw),
                       exdir = tempdir)
  
  return(UnzipedFile)
  
}
  

