library(curl)
library(XML)

listFiles <- function(username, password, relPath = "/", 
                      dav = "https://beehub.nl") {
  uri <- URLencode(paste(dav, relPath, sep=""))
  
  # fetch directory listing via curl and parse XML response
  h <- new_handle()
  handle_setopt(h, customrequest = "PROPFIND")
  handle_setopt(h, username = username)
  handle_setopt(h, password = password)
  response <- curl_fetch_memory(uri, h)
  text <- rawToChar(response$content)
  doc <- xmlParse(text, asText=TRUE)
  
  # calculate relative paths
  base <- paste(paste("/", strsplit(uri, "/")[[1]][-1:-3], sep="", collapse=""), "/", sep="")
  result <- unlist(
    xpathApply(doc, "//d:response/d:href", function(node) {
      sub(base, "", URLdecode(xmlValue(node)), fixed=TRUE)
    })
  )
  result[result != ""]
}

