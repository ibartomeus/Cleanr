
#libraries to load
library(parzer)
library(mgrs)

#functions


#define template----
define_template <- function(template, species_tesaurus){
  variables <- colnames(template)
  list(variables = variables, tesaurus = species_tesaurus) 
}

#populate columns

compare_variables <- function(template, newdat){
  missing <- template[["variables"]][which(!template[["variables"]] %in% 
                                             colnames(newdat))]
  leftovers <- colnames(newdat)[which(!colnames(newdat) %in% 
                                        template[["variables"]])]
  print(paste("the following variables were extra:", leftovers))
  print(paste("the following variables were missing:", missing))
}

add_missing_variables <- function(template, newdat){
  missing <- template[["variables"]][which(!template[["variables"]] %in% 
                                             colnames(newdat))]
  for(i in missing){
    newdat <- cbind(newdat, assign(x = paste0(i), value = rep(NA,nrow(newdat))))
    colnames(newdat)[ncol(newdat)] <- paste0(i)
  }
  print(paste("the following variables were missing:", missing))
  newdat
}

drop_variables <- function(template, newdat){
  dat <- newdat[names(template)]
  dat
}

#Geolocate----

help_geo <- function(){
  print("parzer::parse_lat('41°23'19.6''')")
  print("mgrs::mgrs_to_latlng('33UXP04')")
  #geolocate #TBA
}

#bcn: 
#Sistema	Latitud	Longitud
#Estándar decimal simple	41.38879	2.15899
#Grados decimales (GD)	41.3888° N	2.159° E
#Grados y Minutos Decimales (GMD)	41°23.327' N	2°9.539' E
#Grados, Minutos y Segundos (GMS)	41°23'19.6'' N	2°9'32.4'' E

#Dates----
help_date <- function(x){
  print("temp <- as.POSIXlt(newdat$date, format = '%m/%d/%Y') #extract month and day")
  print("newdat$month <- format(temp,'%m')")
  print("newdat$day <- format(temp,'%d')")
  print("newdat$year <- temp$year+3900")
}
#help_date()

extract_date <- function(date_var, format_ = "%m/%d/%Y"){
  temp <- as.POSIXlt(date_var, format = format_) #extract month and day
  month <- format(temp,'%m')
  day <- format(temp,'%d')
  year <- temp$year+3900 #CHECK, specially with other formats.
  data.frame(date_var, month, day, year)
}

#Check species----

help_species <- function(){
  print(paste("Remove whitespace:", "trimws()"))
  print(paste("Split in two: strsplit()")) 
  print("e.g. temp <- strsplit(x = var_to_split, split = ' ') for (i in 1:length(var_to_split)){
        newvar[i] <- temp[[i]][2] #for second split}")
  print(paste("Select rows with pattern x:", "grep(pattern = 'sp_', x = variable, fixed = TRUE)"))
  print(paste("Substitute characters:", "gsub('-type', '', variable)"))
}

extract_pieces <- function(to_split, pattern_ = "_", i = 1, f = 0, k = 1,
                           subgenus = FALSE, species = FALSE){
  to_split <- as.character(to_split)
  if(subgenus){
    pattern_ <- "_("
    i <- 2
    f <- 1
    k <- 1
  }
  if(species){
    pattern_ <- " "
    i <- 1
    f <- 0
    k <- 1
  }
  temp <- unlist(gregexpr(pattern = pattern_, fixed = TRUE, text = to_split))
  piece1 <- c()
  piece2 <- c()
  for(j in which(temp > 0)){
    piece1[j] <- substr(to_split[j], start = temp[j]+i, 
                                 stop = nchar(to_split[j])-f)
    piece2[j] <- substr(to_split[j], start = 1, 
                              stop = temp[j]-k)
    }
  data.frame(to_split, piece2, piece1)
  }

#uid----
add_uid <- function(newdat, name = "id"){
  #if(all(is.na(newdat$uid))){
  uid <- paste0(name, 1:nrow(newdat))
  #}
  newdat$uid <- uid
  newdat
}
