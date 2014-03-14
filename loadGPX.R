# Read in xml files
library(XML)
library(spacetime)
library(trajectories)

#function for loading GPX 
loadGPX = function(filename){
  
  #parse gpx file
  xDoc=xmlTreeParse(filename,useInternalNodes=TRUE)
  xRoot = xmlRoot(xDoc)
  xTrk = xRoot[["trk"]]
  
  #extract name and description of track
  name = xmlValue(xTrk[["name"]])
  description = xmlValue(xTrk[["desc"]])
  
  #create List of trackpoints
  trkptList = xmlToList(xTrk[["trkseg"]])
  
  #create data frame
  tkpDf = do.call(rbind,lapply(trkptList, function(x) data.frame(as.POSIXct(as.character(x$time), format="%Y-%m-%dT%H:%M:%OSZ"),as.numeric(x$.attrs["lat"]),as.numeric(x$.attrs["lon"]),as.numeric(x$ele))))
  names(tkpDf)=c("time","lat","lon","elevation")
  coordinates(tkpDf)<-~lon+lat
  proj4string(tkpDf)=CRS("+init=epsg:4326")
  stidf = STIDF(geometry(tkpDf), tkpDf$time, tkpDf@data)
  
  #check for redundant timestamps and remove them!
  redundant = which(diff(as.numeric(index(stidf@time)))==0)
  if(length(redundant)!=0){
    stidf = stidf[-redundant,]
  }
  track = Track(stidf)
  attr(df,"name") = name
  attr(df,"description") = description
  return(track)
}

#test
filename1="C:/Users/staschc/Dropbox/R/projects/trackAnalysis/data/track_2.gpx"
filename2="C:/Users/staschc/Dropbox/R/projects/trackAnalysis/data/track.gpx"

track1=loadGPX(filename1)

track2=loadGPX(filename2)

tracks = Tracks(list(track1,track2))
tracksCol = TracksCollection(list(tracks))
stplot(tracksCol,col=2,axes=TRUE)

