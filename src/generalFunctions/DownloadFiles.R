# Download a list of files

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-08-29

# urls = links to be download
# fileName = name of the files. If NULL, the urls will be displayed

DownloadFiles <- function(urls, fileNames = NULL, DirOutput = NULL, Override = FALSE) {
  
  # In case of NULL fileNames
  if (is.null(fileNames)) {
    fileNamesMessage <- urls
    fileNames <- paste0(FileDownload, seq_along(urls))
  } else {
    fileNamesMessage <- fileNames
  }
  
  for (i in seq_along(urls)) {
    
    message("Downloading file ", fileNamesMessage[i])
    
    if(isTRUE(Override) | !file.exists(paste0(DirOutput, fileNames[i]))) {
      # Download File
      download.file(url = urls[i], 
                    destfile = paste0(DirOutput, fileNames[i]),
                    mode='wb')
    } else {
      message("File already downloaded.")
    }
  }
}


# End