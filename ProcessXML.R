convertXml2Xlsx = function() { # Pass the path

  path = "C:/Users/Sebastián/OneDrive/Documentos/BusinessIntelligence/AprendeR/Entrenamientos/copia/"
  fileName = "20161106_090642.tcx"
  
  setwd(path) 
  xmlfile=xmlParse(fileName,getDTD=FALSE)
  class(xmlfile) 
  xmltop = xmlRoot(xmlfile)
  
  xmltrack = xmltop[['Activities']][['Activity']][['Lap']][['Track']]
  
  #read xml file
  xmldataframe <- xmlToDataFrame(xmltrack)

  #process to separate latitude and longitude
  longitud = sapply(xmldataframe[,2], FUN=getLongitude) 
  xmldataframe = cbind(xmldataframe,as.numeric(longitud))
  latitud = sapply(xmldataframe[,2], FUN=getLatitude) 
  xmldataframe[,2] = as.numeric(latitud)
  
  #process to separate date and time
  time = substr(xmldataframe[,1],start = 12, stop = 19)
  xmldataframe = cbind(xmldataframe,time)
  xmldataframe[,1] = substr(xmldataframe[,1],start = 1, stop = 10)

  names(xmldataframe) = c("Date","Latitude","Distance","Altitude","Longitude","Time")
  xmldataframe[,3] = as.numeric(xmldataframe[,3])
  xmldataframe[,4] = as.numeric(xmldataframe[,4])

  library("xlsx")
  write.xlsx(xmldataframe,paste(path,"20161106_090642.xlsx",sep=""))
}


#########################################
##       Get Longitude
#########################################
getLongitude = function(position) {
  end = nchar(position)
  pos = regexpr("-",position)
  longitude = substr(position,start=pos,stop=end)
  longitude
}

#########################################
##       Get Latitude
#########################################
getLatitude = function(position) {
  end = nchar(position)
  pos = regexpr("-",position)
  latitude = substr(position,start=1,stop=pos-1)
  latitude
}
