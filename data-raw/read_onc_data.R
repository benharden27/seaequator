library(sea)
library(tidyverse)
master_folder <- "~/data/SEA/equatorial"
subfolders <- list.files(master_folder, pattern = "S[0-9]{3}")

for (i in 1:length(subfolders)) {

  print(i)
  # set route folder
  root_folder <- file.path(master_folder,subfolders[i])

  ctd_fold <- file.path(root_folder,"CTD","Cnv")
  if(length(list.files(ctd_fold, "\\.cnv")) == 0) {
    ctd_fold <- file.path(root_folder,"cnv")
    if(length(list.files(ctd_fold, "\\.cnv")) == 0) {
      ctd <- NULL
    } else {
      ctd <- sea::read_ctd_fold(ctd_fold)
    }
  } else {
    ctd <- sea::read_ctd_fold(ctd_fold)
  }

  a<-list(ctd=ctd)

  # a$adcp$u <- ifelse(a$adcp$u>1,NA,a$adcp$u)
  # a$adcp$v <- ifelse(a$adcp$v>1,NA,a$adcp$v)
  for (j in 1:length(a$ctd)) {
    # find possible oxygen values
    ii <- stringr::str_which(names(a$ctd[[j]]@data),"oxygen")
    if(length(ii) > 1) {
      mean_o2 <- colMeans(sapply(a$ctd[[j]]@data[ii], unlist),na.rm = T)
    } else {
      mean_o2 <- mean(a$ctd[[j]]@data[ii], na.rm = T)
    }
    oval <- ii[which(mean_o2 < 10)][1]
    if(is.na(oval)) {
      oval <- ii[1]
      a$ctd[[j]]@data[[oval]] <- NA
    }

    if(is.null(a$ctd[[j]]@data$par)) {
      par = NA
    } else {
      par <- a$ctd[[j]]@data$par
    }


    station <- paste0(subfolders[i],"_",stringr::str_pad(a$ctd[[j]]@metadata$station,3,pad = "0"))

    if(is.null(a$ctd[[j]]@data$theta)) {
      a$ctd[[j]]@data$theta <- oce::swTheta(a$ctd[[j]]@data$salinity,a$ctd[[j]]@data$temperature,
                                            a$ctd[[j]]@data$pressure)
    }

    if(is.null(a$ctd[[j]]@data$fluorescence)) {
      a$ctd[[j]]@data$fluorescence <- NA
    }

    if(is.null(a$ctd[[j]]@data$oxygen)) {
      a$ctd[[j]]@data$oxygen <- NA
    }
    ctd_add <- tibble::tibble(dep = a$ctd[[j]]@data$depth,
                      temp = a$ctd[[j]]@data$temperature,
                      theta = a$ctd[[j]]@data$theta,
                      sigtheta = a$ctd[[j]]@data$sigmaTheta,
                      sal = a$ctd[[j]]@data$salinity,
                      fluor = a$ctd[[j]]@data$fluorescence,
                      par = par,
                      oxygen = a$ctd[[j]]@data[[oval]],
                      lon = a$ctd[[j]]@metadata$longitude,
                      lat = a$ctd[[j]]@metadata$latitude,
                      station = station,
                      cruise = subfolders[i])
    if (j == 1){
      ctd <- ctd_add
    } else {
      ctd <- dplyr::bind_rows(ctd,ctd_add)
    }
  }

  a$ctd2 <- ctd


  # then get datasheets
  files <- list.files(file.path(root_folder,"SHIPDATA"),pattern = "\\.xls")

  # hourly work
  hourly_file <- find_datasheet(files,"(H|h)ourlywork")
  if (length(hourly_file) == 1) {
    hourly <- try(read_hourly(file.path(root_folder,"SHIPDATA",hourly_file)))
    if(inherits(hourly,"try-error")) {
      warning("Hourly file cannot be opened. Returning NULL dataset.")
      hourly <- NULL
    }
  } else {
    hourly <- NULL
  }

  if (i == 8) {
    ii <- which.min(hourly$lat)
    hourly$lat[1:ii] <- -hourly$lat[1:ii]
  }

  if(!is.null(hourly)) {
    a$hourly <- hourly
  } else {
    a$hourly <- NULL
  }

  assign(subfolders[i],a)

}

library(usethis)
for (name in subfolders)
  do.call("use_data", list(as.name(name), overwrite = TRUE))
