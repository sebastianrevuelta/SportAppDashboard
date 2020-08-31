# paint the height during the race --------
paintHeight = function(path,fileName) {

  print(paste("Reading: ", path))
  training <- getTraining(path,paste(path,fileName,sep=""),FALSE)
  
  distance = training$Distance
  altitude = training$Altitude
  
  #max altitude
  alt_max = altitude[!is.na(altitude)]
  pos = which(alt_max == max(alt_max))
  dist_max = round(distance[pos],2)
  dist_min = distance[2]
  alt_min = altitude[2]
  uphill = round((max(alt_max)-alt_min)/(dist_max*1000-dist_min*1000),4)*100
  
  print(paste("Max Altitude: ",max(alt_max)))
  print(paste("Min Altitude: ",alt_min))
  title = print(paste("La pendiente de los primeros",dist_max,"kilometros es del",uphill,"%"))
  
  qplot(distance,altitude,xlim = c(0,10), ylim = c(700,900),colour='endomondo tracks',main = title)

}

# Read trainings from strava
readFromStrava = function() {
    
    files = list.files(str_glue(getwd(),"/stravaInfo/"),pattern = ".json")
    trainings <- data.frame(matrix(ncol = 14))
    colnames(trainings) <- c("ID","Date","Time","Distance","Elevation","Year","Month",
                             "Day","Hour","Weekday","Pace","Sport","Calories","Notes")

    ##TODO: Mutate to create each column from the list

   counter <- 0
   for(i in 1:length(files)) {
        
        fileName <- str_glue(getwd(),"/stravaInfo/",files[i])
        print(paste("Reading:",fileName))
        data <- rjson::fromJSON(file=fileName) 
        
        for(j in 1:length(data)) {
            counter <- counter + 1    
            trainings[counter,]$ID       = counter
            trainings[counter,]$Notes    = data[[j]]$name
            trainings[counter,]$Date     = data[[j]]$start_date_local  ## TimeStamp
            trainings[counter,]$Time     = as.numeric(data[[j]]$moving_time) ## Time
            trainings[counter,]$Distance = as.numeric(data[[j]]$distance) ## Distance
            trainings[counter,]$Elevation = as.numeric(data[[j]]$total_elevation_gain) ## Elevation
            trainings[counter,]$Calories = 0 ## Calories
            trainings[counter,]$Sport    = data[[j]]$type
        }
    }
    
    # Process trainings    
    trainings %>%
        processTraining()

}

readFromStravaPurr = function() {
    
    files = list.files(str_glue(getwd(),"/stravaInfo/"),pattern = ".json")
    trainings <- data.frame(matrix(ncol = 13))
    colnames(trainings) <- c("ID","Date","Time","Distance","Year","Month",
                             "Day","Hour","Weekday","Pace","Sport","Calories","Notes")
    
    files_tbl <- files %>% 
        as.data.frame(stringsAsFactors = FALSE) %>%
        set_names("file")
    
    files_data_tbl <- files_tbl %>% 
        select(file) %>%
        mutate(data = file %>% map(readJSONTraining)) %>%
        unnest() %>%
        mutate(list = data %>% map(unlist)) %>%
        mutate(Distance = data %>% map(separateItems(5))) %>%
        mutate(Notes = data %>% map(separateItems(4))) %>%
        mutate(Time = data %>% map(separateItems(6))) %>%
        mutate(Calories = 0) %>%
        mutate(Sport = data %>% map(separateItems(9)))

    # Process trainings    
    #trainings %>%
    #    processTraining()
    
}

separateItems <- function(data, pos) {
    unlist(data)[[5]]
}

readJSONTraining <- function(file) {
    fileName = str_glue(getwd(),"/stravaInfo/",file)
    rjson::fromJSON(file = fileName)
}

# Process trainings ----
processTraining <- function(trainings) {
    
    trainings <- trainings %>%
        
        mutate("Time"    = Time) %>%
        mutate("Distance"= round(digits=2,Distance/1000)) %>%
        mutate("Date"    = ymd_hms(Date)) %>%
        mutate("Year"    = year(Date)) %>%
        mutate("Month"   = month(Date, label = TRUE, abbr = FALSE)) %>%
        mutate("Day"     = day(Date)) %>%
        mutate("Weekday" = wday(Date, label = TRUE, abbr = FALSE)) %>%
        mutate("Hour"    = hour(Date)) %>%
        mutate("Pace"    = getTrainingPace(Time/60,Distance)) %>%
        mutate("Pace Description" = dplyr::ntile(Pace,5)) 
    
    trainings[which(trainings$"Pace Description"==1),]$"Pace Description" <- "Very Fast"
    trainings[which(trainings$"Pace Description"==2),]$"Pace Description" <- "Fast"
    trainings[which(trainings$"Pace Description"==3),]$"Pace Description" <- "Normal"
    trainings[which(trainings$"Pace Description"==4),]$"Pace Description" <- "Low"
    trainings[which(trainings$"Pace Description"==5),]$"Pace Description" <- "Very Low"
    
    trainings    
    
}

# create data frame for the trainings --------
time4Training = function(path) {

  files = list.files(path,pattern = ".tcx")
  trainings <- data.frame(matrix(ncol = 13, nrow = length(files)))
  colnames(trainings) <- c("ID","Date","Time","Distance","Year","Month",
                           "Day","Hour","Weekday","Pace","Sport","Calories","Notes")
  
  total_track <- data.frame(matrix(ncol = 5))
  colnames(total_track) <- c("ID","Date","Latitude","Longitude","Altitude")

  for(i in 1:length(files)) {
    
    fileName <- paste(path,files[i],sep="")
    print(paste("Reading: ", fileName))
    xmlfile=xmlParse(fileName,getDTD=FALSE)
    xmltop = xmlRoot(xmlfile)
    
    header <- getHeaderTraining(xmltop)
    trainings[i,]$ID = i
    trainings[i,]$Date = as.character(header$text[1])  ## TimeStamp
    trainings[i,]$Time = as.numeric(header$TotalTimeSeconds[2]) ## Time
    trainings[i,]$Distance = as.numeric(header$DistanceMeters[2]) ## Distance
    trainings[i,]$Calories = as.numeric(header$Calories[2]) ## Calories
    trainings[i,]$Sport = getSport(xmltop) ## Distance
    
    ## erite the track with the current ID
    if (trainings[i,]$Sport == "Running") {
        track <- getTraining(xmltop,i)    
        if (!is.null(track)) {
          total_track <- rbind(total_track,track)
          ##write_rds(track,str_glue("dfTrack",{i},".rds"))
        }
    }
    
  }
  ## writ all the trainings
  write_rds(total_track,"dfTracks.rds")

  
  trainings$Time <- round(digits=3,trainings$Time) #Time
  trainings$Distance <- round(digits=3,trainings$Distance) #Distance

  # Process trainings    
  trainings %>%
      processTraining()
  
}

### paint line evolution ----
plot_line_evolution <- function(df,var1,var2,unit,ncolor) {

    df %>%
        
        ##Canvas
        ggplot(aes(x = df[[var1]], y = df[[var2]])) +
        
        ##Geometry
        geom_line(size=1.5, color = viridisLite::viridis(n = 20)[ncolor]) +
        geom_point(size=1) +
        geom_smooth(method = "lm", se = FALSE) +
        #geom_text(aes(label = df[[var1]]),vjust=-0.5) +
        geom_label(label = paste0(df[[var2]]," (",unit,")"),
                   color = "white",
                   fill  = viridisLite::viridis(n = 20)[10],
                   fontface = "bold",
                   size = 4,
                   vjust = -1) +
        ##Format
        theme_light() +
        expand_limits(y = max(df[[var2]])) +
        scale_y_continuous() +
        labs(
            title = paste(var2,"Evolution by",var1),
            subtitle = "",
            x = var1,
            y = var2
        )
    
}

##paint bar/column graph ----
plot_col_evolution <- function(df,var1,var2,unit,ncolor) {
    
    df %>%
        
        ##Canvas
        ggplot(aes(x = df[[var1]], y = df[[var2]])) +
        
        ##Geometry
        geom_col(fill = viridisLite::viridis(n = 20)[10]) +
        coord_flip() +
        geom_label(label = paste0(df[[var2]]," (",unit,")"),
                   color = "white", 
                   fill  = viridisLite::viridis(n = 20)[ncolor],
                   fontface = "bold",
                   size = 4) +
        ##Format
        theme_light() +
        labs(
            title = paste(var2,"Evolution by",var1),
            subtitle = "",
            x = var1,
            y = var2
        )
    
}

plot_col_evolution_no_flip <- function(df,var1,var2,unit,ncolor) {
    
    df %>%
        
        ##Canvas
        ggplot(aes(x = df[[var1]], y = df[[var2]])) +
        
        ##Geometry
        geom_col(fill = viridisLite::viridis(n = 20)[10]) +
        geom_label(label = paste0(df[[var2]]," (",unit,")"),
                   color = "white", 
                   fill  = viridisLite::viridis(n = 20)[ncolor],
                   fontface = "bold",
                   size = 4) +
        ##Format
        theme_light() +
        labs(
            title = paste(var2,"Evolution by",var1),
            subtitle = "",
            x = var1,
            y = var2
        )
    
}


### print trainings ----
print.entrenos = function(trainings) {
  nTotal <- nrow(trainings)
  print(paste("Total number of trainings: ", nTotal))
  
}

# getPace ----
getPace = function(trainings,year) {
  x = (sum(trainings[,2])/60)/((sum(trainings[,3]))/1000)
  y = (x-trunc(x))*60/100
  x = round(digits=2,trunc(x) + y)
  
  print(paste("In",year,"the average pace is:",x))
  
  if (x == "NaN") { x <- 0 }
   
  x
  
}
###############################################
#getTrainingPace
###############################################
getTrainingPace = function(time,distance) {
    x = (time)/(distance)
    y = (x-trunc(x))*60/100
    x = round(digits=2,trunc(x) + y)
    if (x == "NaN") { x <- 0 }
    as.numeric(x)
}

# paintGraphFromTable ----
paintGraphFromTable = function(table,title,low,medium,high,order,label) {
  if (order == TRUE) {
  cols <- ifelse(table<=low, "yellow", 
                 ifelse(table<=medium, "orange", "red"))
  }
  else {
    cols <- ifelse(table<=low, "red", 
                   ifelse(table<=medium, "orange", "yellow")) 
  }
  if (label == "Na") {
    barplot(table,main=title,col=cols,ylim = c(0,high))
  }
  else {
    barplot(table,main=title,col=cols,ylim = c(0,high),names.arg=label)
  }
}

# getOcurrences ----
getOcurrences = function(mapTime,hours) {
  ocurrences <- c()
  for(i in 1:length(hours)) {
    nTimes = mapTime[[hours[i]]]
    if (is.null(nTimes)) { nTimes <- 0 }
    ocurrences[i] <- nTimes
  }
  ocurrences
}


#######################################################################
# frequentTrainings ----
# Basically, it detects your frequent trainings taking as input the 
# tcx (xml files) of a endomondo training the it compares training by 
# training (the GPS positions) in order to detect similar trainings
# finally it keeps in a excel file all the trainings with the times
# it believes you have repeated
#######################################################################
frequentTrainings = function(path, threshold) {
  library("xlsx")
  files = list.files(path,pattern = ".tcx")
  trainings <- list()
  
  for(i in 1:length(files)) {
    print(paste("Reading: ", files[i]))
    training <- getTraining(path,paste(path,files[i],sep=""),FALSE)
    
    trainings[[i]] <- training
    
    ##a data frame with all the trainings
    if (i == 1) {
      alltrainings = training
    }
    else {
      alltrainings = rbind(alltrainings,training)
    }
  }

  #compare maps
  map = createMapSimilitudeTrainings(trainings,files, threshold)

  #update trainings
  alltrainings = updateTrainings(alltrainings, map)

  colNames <- names(alltrainings) 
  colNames[7] <- "nTimes"
  names(alltrainings) <- colNames
  
  excelPath = paste(path,paste(files[i],"alltrainings.xlsx",sep=""))
  
  write.xlsx(alltrainings,excelPath)
  
}

##############################################################
# This function gets the hills from a running training
# As Input it takes the tcx (xml file) of the training
# and basically process for each GPS position the distance 
# and the attitude, if it detects a slope of x% (where X is a parameter) 
# then it marks the position as a hill 
##############################################################
calculateHills = function(data, hillPercentage) {

  for(i in 1:nrow(data)) {
    
     training <- data[1,]
      
    #getHills
    training = getHills(training, hillPercentage)
    colNames <- names(training) 
    colNames[7] <- "Hills"
    names(training) <- colNames
    
    excelPath = paste(path,paste(files[i],"Hills.xlsx",sep=""))
    
    write.xlsx(training,excelPath)
  
  }
}

######################################################################
#
# getHills
# it receives a trianing (data frame) and the percentage to consider a 
# a hill as a hill
# In the column 7th it puts TRUE/FALSE in order to consider it as a hill
######################################################################
getHills = function(training, hillPercentage) {
  
  #no hill by default      
  training[,7] <- FALSE
  nrow = nrow(training) 
  for(j in 1:nrow) {
    dist = training[j,]$Distance
    difference = 0
    k = j
    #it considers a distance of 100 meters and then takes the attitude 
    while(difference < 100) {
        k = k + 1
        if (k < nrow) {
          difference = (training[k,]$Distance - dist)
        }
        else {
          break;
        }
    }
    if (difference >= 100) {
      #get the altitude
      att1 = training[k,]$Altitude
      att2 = training[j,]$Altitude
      height =  att1 - att2
      #check if you go up or down, a certain %
      if (!is.na(height)) {
        if (abs(height) >= hillPercentage) {
          print(paste("Found hill in the kilometer",training[k,]$Distance))
          for(p in j:k) {
            training[p,7] <- TRUE
          }
        }
      }
    }
  }
  training
}
      

######################################################################
#
# updateTrainings
#
######################################################################
updateTrainings = function(alltrainings,map) {
  for(i in 1:nrow(alltrainings)) { 
    date = alltrainings[i,1]
    nTimes = map[[date]]
    if (is.null(nTimes)) {
      alltrainings[i,7] = 1
    }
    else {
      alltrainings[i,7] = nTimes
    }
  }
  alltrainings
}

######################################################################
#
# createMapSimilitudeTrainings
# #TODO: files is not neccesary, used dim trainings
######################################################################
createMapSimilitudeTrainings = function(trainings, files, threshold) {
  
  #Todo: Crear lista de similitudes.
  #2016-01-04 ->  2016-01-07,  2016-01-11,  2016-01-18,  2016-02-24, 2016-03-17
  #2016-03-22 ->  2016-03-31
  #2016-08-23 ->  2016-08-26
  map <- new.env(hash=T, parent=emptyenv())
  for(i in 1:length(files)) {
    for(j in i:length(files)) {
      if (i != j) {
        tr1 = trainings[[i]]
        tr2 = trainings[[j]]
        similitude = compareTrainings(tr1,tr2)
        if (similitude > threshold) {
          print(paste("The similitude between trainings",tr1[1,]$Date,"and",tr2[1,]$Date,"is",similitude))
          key = tr1[1,]$Date
          value = map[[key]]
          if (is.null(value)) {
            map[[key]] <- 2
          }
          else {
            map[[key]] <- value + 1 
          }
        }
      }
    }
  }
  map
}

######################################################################
# getTraining
# parse the xml file (tcx file)
# ir returns a dataframe
######################################################################
getTraining = function(xmltop,ID) {
  
  xmltrack <- xmltop[['Activities']][['Activity']][['Lap']][['Track']]
  #read xml file
  track <- xmlToDataFrame(xmltrack)
  if ( c("AltitudeMeters") %in% colnames(track)) {
      track <- track %>%
          rename("Altitude" = "AltitudeMeters")
  }
  else {
      track$Altitude <- 0
  }
  if ( c("Position") %in% colnames(track)) {
      track$ID <- ID
      track <- track %>%
          mutate(Latitude  = substr(Position,0,9)) %>%
          mutate(Longitude = substr(Position,10,19)) %>%
          mutate(Date     = ymd_hms(Time)) %>%
          select("ID","Date","Latitude","Longitude","Altitude")
  }
  else {
      track <- NULL
  }


  track
}
###########################################################
# getHeaderTraining
###########################################################
getHeaderTraining = function(xmltop) { # Pass the path
  
  xmltrack = xmltop[['Activities']][['Activity']]
  xmlToDataFrame(xmltrack)
}

###########################################################
# getSport
###########################################################
getSport = function(xmltop) { # Pass the path
    a <- xmlToList(xmltop)
    a$Activities$Activity$.attrs[1]
}

#########################################
##       Get Hour
#########################################
getHour = function(time) {
  pos = regexpr(":",time)
  hour = substr(time,start=pos-2,stop=pos-1)
  hour
}



#########################################
##       compareTrainings
#########################################
compareTrainings = function(train1, train2) {

  nrow1 = nrow(train1)
  nrow2 = nrow(train2)
  
  nrow = min(nrow1,nrow2)
  
  latitudeDiff = abs(train1[2:nrow-2,]$Latitude) - abs(train2[2:nrow-2,]$Latitude)
  longitudeDiff = abs(train2[2:nrow-2,]$Longitude) - abs(train2[2:nrow-2,]$Longitude)

  similitude = ((100-sum(abs(latitudeDiff)))+100-sum(abs(longitudeDiff)))/2
  
  similitude
}