library(RCurl)
library(XML)
library(stringi)

base.url <- "http://www.museothyssen.org/thyssen/ficha_obra/"

res <- lapply(1:2000, function(item){
  
  url <- paste0(base.url, item)
  kk <- getURL(url, .encoding = "UTF-8")

  if (kk == " "){
    print(paste(item, "vacio", sep = "\t"))
    return(list())
  }
  
  print(item)

  kkk <- htmlParse(kk)
  
  #titulo <- xpathSApply(kkk, "//div[@id='colCentralContenidosInt']//dd[@class='dd_titulo']", xmlValue)
  #fecha  <- xpathSApply(kkk, "//div[@id='colCentralContenidosInt']//dd[@class='dd_fecha']", xmlValue)
  
  ficha <- xpathSApply(kkk, "//div[@id='colCentralContenidosInt']//dl[@class='datosAutor']/dd", xmlValue)
  ficha <- stri_trim(ficha)
  
  texto <- stri_trim(xpathSApply(kkk, "//span[@id='contReader2']", xmlValue))
  
  list(ficha = ficha, texto = texto)

})

thyssen <- Filter(function(x) length(x) > 1, res)

save(thyssen, file = "thyssen.Rdata")
