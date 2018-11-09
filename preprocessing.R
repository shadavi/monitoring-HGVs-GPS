#------------------------summary function-------------------------------------

f_vna <- function(v){
  if(length(v) == 0)
    return(NA)
  return(v)
}

dist_to_first_stop <- function(dist, timestamp, sel_stop){
  fst_t <- head(timestamp[sel_stop],1)
  sum(dist[timestamp <= fst_t])
}

avg_dist_between_stops <- function(dist, timestamp, sel_stop){
  ival <- findInterval(timestamp, timestamp[sel_stop]) # e.g. [0,0,1,1,1,1,1,2,2,2] for stops at '3' and '8'
  mean(by(dist, ival, sum))
}

# THIS IS A FUNCTION
truck_summary <- function(data){
  ddply(data, .(id), summarize,
        id = head(id,1),
        countrycod = head(countrycode,1),
        eurovalue = head(eurovalue,1),
       # we are not using mtm
        # mtm = head(mtm,1), we are not 
        
        obs_time = sum(time)/3600,
        bxl_time = sum(time[in_bxl])/3600,
        penta_time=sum(time[in_bxl & in_penta])/3600,
        obs_time_driven = sum(time[time < 300])/3600,
        bxl_time_driven = sum(time[in_bxl & time<300])/3600,
        penta_time_driven = sum(time[in_bxl& in_penta & time<300])/3600,
        
        obs_dist = sum(dist)/1000,
        bxl_dist = sum(dist[in_bxl])/1000,
        penta_dist = sum(dist[in_bxl & in_penta])/1000,
        obs_dist_driven = sum(dist[time < 300])/1000,
        bxl_dist_driven = sum(dist[in_bxl & time < 300])/1000,
        penta_dist_driven = sum(dist[in_bxl & in_penta & time < 300])/1000,
        chull_area= areapl(cbind(X[chull(X,Y)],Y[chull(X,Y)]))*10000000000,
        chull_area_bxl= areapl(cbind(X[in_bxl][chull(X[in_bxl],Y[in_bxl])],Y[in_bxl][chull(X[in_bxl],Y[in_bxl])]))*10000000000,
        
       
        npoints = length(timestamp),
        npoints_bxl = sum(in_bxl),
        npoints_ring = sum(!is.na(on_ring)),
        npoints_penta = sum(in_penta),
        
        # WARNING: start/stop may be artificial if around 02:00:00
        start_t = head(timestamp,1),
        start_X = head(X,1),
        start_Y = head(Y,1),
        start_inbxl = head(in_bxl,1),
        
        stop_t = tail(timestamp,1),
        stop_X = tail(X,1),
        stop_Y = tail(Y,1),
        stop_inbxl = tail(in_bxl,1),
        
        cnt_entry = sum(entry),
        entry_fst_t = f_vna(head(timestamp[entry],1)),
        entry_fst_x = f_vna(head(X[entry],1)),
        entry_fst_y = f_vna(head(Y[entry],1)),
        
        cnt_exit = sum(exit),
        exit_lst_t = f_vna(tail(timestamp[exit],1)),
        exit_lst_x = f_vna(tail(X[exit],1)),
        exit_lst_y = f_vna(tail(Y[exit],1)),
        
        # all the following now IN_BXL only
        bxl_stops_5m = sum(in_bxl & time > 300),
        bxl_stops_5m_5v = sum(in_bxl & time > 300 & velocity < 5),
        bxl_stops_6m_50d = sum(in_bxl & time > 360 & dist < 50),
        bxl_stops_0v = sum(in_bxl & velocity == 0),
        bxl_stops_0s = sum(in_bxl & speed == 0),
        
        # all the following now IN_BXL and in_penta
        penta_stops_5m = sum(in_bxl & in_penta & time > 300),
        penta_stops_5m_5v = sum(in_bxl & in_penta& time > 300 & velocity < 5),
        penta_stops_6m_50d = sum(in_bxl& in_penta & time > 360 & dist < 50),
        penta_stops_0v = sum(in_bxl & in_penta& velocity == 0),
        penta_stops_0s = sum(in_bxl & in_penta& speed == 0),
        
        # last argument is 'stop' condition selector
        # WARNING: it may have left+entered bxl before its first stop!!
        bxl_first_stop_dist_to = dist_to_first_stop( dist, timestamp, (in_bxl & time > 300 & (velocity == 0 | dist < 50)) )/1000,
        bxl_first_stop_dist_inbxl = dist_to_first_stop( dist[in_bxl], timestamp[in_bxl], (in_bxl & time > 300 & (velocity == 0 | dist < 50)) )/1000,
        bxl_stops_avg_dist = avg_dist_between_stops( dist, timestamp, (in_bxl & time > 300 & (velocity == 0 | dist < 50)) )/1000,
        
        
       distance_toFirststop = distm(c(f_vna(head((Y[time > 360 & (velocity == 0 | dist < 50)]),1)),f_vna(head((X[time > 360 & (velocity == 0 | dist < 50)]),1))), c( head(Y,1),head(X,1)), fun = distHaversine)/1000 ,
       distance_toFirststop_from_firstBXL = distm(c(f_vna(head((Y[time > 360 & (velocity == 0 | dist < 50)]),1)),f_vna(head((X[time > 360 & (velocity == 0 | dist < 50)]),1))), c( head(Y[in_bxl],1),head(X[in_bxl],1)), fun = distHaversine)/1000
      
  )}

# same as above with three lines removed
truck_summary_hourly <- function(data){
  ddply(data, .(id), summarize,
        id = head(id,1),
        countrycod = head(countrycode,1),
        eurovalue = head(eurovalue,1),
        # we are not using mtm
        # mtm = head(mtm,1), we are not 
        
        obs_time = sum(time)/3600,
        bxl_time = sum(time[in_bxl])/3600,
        penta_time=sum(time[in_bxl & in_penta])/3600,
        obs_time_driven = sum(time[time < 300])/3600,
        bxl_time_driven = sum(time[in_bxl & time<300])/3600,
        penta_time_driven = sum(time[in_bxl& in_penta & time<300])/3600,
        
        obs_dist = sum(dist)/1000,
        bxl_dist = sum(dist[in_bxl])/1000,
        penta_dist = sum(dist[in_bxl & in_penta])/1000,
        obs_dist_driven = sum(dist[time < 300])/1000,
        bxl_dist_driven = sum(dist[in_bxl & time < 300])/1000,
        penta_dist_driven = sum(dist[in_bxl & in_penta & time < 300])/1000,
        chull_area= areapl(cbind(X[chull(X,Y)],Y[chull(X,Y)]))*10000000000,
      #  chull_area_bxl= areapl(cbind(X[in_bxl][chull(X[in_bxl],Y[in_bxl])],Y[in_bxl][chull(X[in_bxl],Y[in_bxl])]))*10000000000,
        
        
        npoints = length(timestamp),
        npoints_bxl = sum(in_bxl),
        npoints_ring = sum(!is.na(on_ring)),
        npoints_penta = sum(in_penta),
        
        # WARNING: start/stop may be artificial if around 02:00:00
        start_t = head(timestamp,1),
        start_X = head(X,1),
        start_Y = head(Y,1),
        start_inbxl = head(in_bxl,1),
        
        stop_t = tail(timestamp,1),
        stop_X = tail(X,1),
        stop_Y = tail(Y,1),
        stop_inbxl = tail(in_bxl,1),
        
        cnt_entry = sum(entry),
        entry_fst_t = f_vna(head(timestamp[entry],1)),
        entry_fst_x = f_vna(head(X[entry],1)),
        entry_fst_y = f_vna(head(Y[entry],1)),
        
        cnt_exit = sum(exit),
        exit_lst_t = f_vna(tail(timestamp[exit],1)),
        exit_lst_x = f_vna(tail(X[exit],1)),
        exit_lst_y = f_vna(tail(Y[exit],1)),
        
        # all the following now IN_BXL only
        bxl_stops_5m = sum(in_bxl & time > 300),
        bxl_stops_5m_5v = sum(in_bxl & time > 300 & velocity < 5),
        bxl_stops_6m_50d = sum(in_bxl & time > 360 & dist < 50),
        bxl_stops_0v = sum(in_bxl & velocity == 0),
        bxl_stops_0s = sum(in_bxl & speed == 0),
        
        # all the following now IN_BXL and in_penta
        penta_stops_5m = sum(in_bxl & in_penta & time > 300),
        penta_stops_5m_5v = sum(in_bxl & in_penta& time > 300 & velocity < 5),
        penta_stops_6m_50d = sum(in_bxl& in_penta & time > 360 & dist < 50),
        penta_stops_0v = sum(in_bxl & in_penta& velocity == 0),
        penta_stops_0s = sum(in_bxl & in_penta& speed == 0),
        
        # last argument is 'stop' condition selector
        # WARNING: it may have left+entered bxl before its first stop!!
        bxl_first_stop_dist_to = dist_to_first_stop( dist, timestamp, (in_bxl & time > 300 & (velocity == 0 | dist < 50)) )/1000,
        bxl_first_stop_dist_inbxl = dist_to_first_stop( dist[in_bxl], timestamp[in_bxl], (in_bxl & time > 300 & (velocity == 0 | dist < 50)) )/1000,
        bxl_stops_avg_dist = avg_dist_between_stops( dist, timestamp, (in_bxl & time > 300 & (velocity == 0 | dist < 50)) )/1000
        
        
      #  distance_toFirststop = distm(c(f_vna(head((Y[time > 360 & (velocity == 0 | dist < 50)]),1)),f_vna(head((X[time > 360 & (velocity == 0 | dist < 50)]),1))), c( head(Y,1),head(X,1)), fun = distHaversine)/1000 ,
       # distance_toFirststop_from_firstBXL = distm(c(f_vna(head((Y[time > 360 & (velocity == 0 | dist < 50)]),1)),f_vna(head((X[time > 360 & (velocity == 0 | dist < 50)]),1))), c( head(Y[in_bxl],1),head(X[in_bxl],1)), fun = distHaversine)/1000
        
  )}
#------------------------perhour function-------------------------------------

perhour<-  function(data,day){
  by(data, data$hour, function(data){
    tr_summary <- truck_summary_hourly(data)
    hour <- head(data$hour,1)
    write.csv(tr_summary, paste("summ_hour","_",day,"_",hour, sep=""),row.names = FALSE)
  })
}

#------------------------obu_results function-------------------------------------

obu_results <- function(file_ids) {
    #read file
  Sys.setlocale("LC_ALL", "C") # in case of encoding errors
  obu<-read_csv(paste0("locations_",file_ids,".csv")) #adjust file name accordingly
  
  # remove NAs and print removed lines
    if (  nrow(obu[is.na(obu$gid),])>0){
    print("These lines will be removed, as they have na values:")
    print(obu[is.na(obu$gid),])
    print(obu[is.na(obu$pid),])  
  }
  
  obu<-obu[!is.na(obu$gid),]
  obu<-obu[!is.na(obu$pid),]
  obu <- subset(obu, grepl("POINT", obu$geom))
  
  # order based on ID and time
  setorderv(obu, c("id", "timestamp")) # reference-based sorting, faster and no copy needed 

  # get X and Y from geom string
  regex <-str_match(obu$geom, "POINT \\(([-]*[0-9]+[.]*[0-9]*+) ([-]*[0-9]+[.]*[0-9]*+)\\)")
  obu$X<-as.numeric(regex[,2])
  obu$Y<-as.numeric(regex[,3])
  # Example quality check
  if (sum(is.na(obu$X)) + sum(is.na(obu$Y)) != 0)
    print("Error, some obu$X or obu$Y have a NA value!")
  
  # import spatially 
  
  obu_sp <- SpatialPointsDataFrame(data=obu[,c("id","timestamp","velocity","countrycode","eurovalue","X","Y")], coords=obu[,c("X","Y")],proj4string=CRS(proj4string(bxl)))

  rm(obu)
  #------------------------ adding a column for in_bxl--------------
  #-------------------------input: obu_sp, output:obu_sp-----------------------------
  
  obu_sp$in_bxl <- !is.na(over(obu_sp,bxl)[[1]])
  
  #--------------------- keep data of trucks that have data in_bxl ------------------------------------
  #-------------------------input: obu_sp, output:obu_bxl-----------------------------
  
  truckid_bxl <- unique(obu_sp[obu_sp$in_bxl,]$id)
  obu_bxl <- obu_sp[is.element(obu_sp$id, truckid_bxl),]
  counts<-data.frame("total","in_bxl","in_bxl_in_ring")
  counts[1]<-length(unique(obu_sp$id))
  # WARNING, obu_sp still in memory and may take up gigabytes... consider obu_sp <- obu_sp[...
  rm(obu_sp)
  counts[2]<-length(unique(obu_bxl$id))
  
  #------------------- adding a column for in_penta, on_ring--------------
  #-------------------------input: obu_bxl, output:obu_bxl-----------------------------
  obu_bxl$in_penta <- !is.na(over(obu_bxl,penta)[[1]])
  obu_bxl$on_ring <- as.numeric(over(obu_bxl,ring)[["id"]])

  #--------------------- keep data of trucks that have data in_bxl and not on_ring------------------------------------
  #---------------------------------input: obu_sp, output:obu_bxl-----------------------------
  obu_bxl$in_bxl<- obu_bxl$in_bxl & is.na(obu_bxl$on_ring)
  truckid_bxl <- unique(obu_bxl[obu_bxl$in_bxl,]$id)
  obu_bxl <- obu_bxl[is.element(obu_bxl$id, truckid_bxl),]
  
  counts[3]<-length(unique(obu_bxl[obu_bxl$in_bxl,]$id))
  
  #--------------------- convert timestamp to native format -----------------------------
  obu_bxl$timestamp<- anytime(obu_bxl$timestamp)
  
  # per truck calculations, first compute first and last match so they can be excluded
  sel_first_id <- obu_bxl$id != c(F, head(obu_bxl$id, -1)) # [1,2,3,4] != [1,1,2,3]
  sel_last_id <- obu_bxl$id != c(tail(obu_bxl$id, -1), F) # [1,2,3,4] != [2,3,4,4]
  
  #------------adding a column to "obu_sp" for "time" from previous point (seconds) -----------
  obu_bxl$time<- obu_bxl$timestamp - c(head(obu_bxl$timestamp, 1), head(obu_bxl$timestamp, -1)) # time to previous point
  obu_bxl[sel_first_id, "time"] <- as.difftime(0, unit="secs") # 0 for first row of each id
  
  #---------------------------- adding a column to "obu_sp" for distance from previous point (meters) ---------------------------------
  
  obu_bxl$dist <- c(0,distGeo(head(obu_bxl,-1),tail(obu_bxl,-1))) # [0, dist([1,2,3], [2,3,4])]
  obu_bxl[sel_first_id, "dist"] <- 0 # 0 for first row of each id
  
  #-----------------------adding a column to "obu_sp" for speed (km/h)-------------------------------
  
  obu_bxl$speed <- (obu_bxl$dist/1000)/as.numeric(obu_bxl$time, units="hours")
  obu_bxl[is.na(obu_bxl$speed),"speed"] <- 0 # division by zero (incl. first row)
  
  #-----------------------adding a column for flag of going in and going out-------------------------------
  obu_bxl$entry <- !sel_first_id & obu_bxl$in_bxl & c(F, head(!obu_bxl$in_bxl, -1)) # all but firsts: in bxl, previous not
  obu_bxl$exit <- !sel_last_id & obu_bxl$in_bxl &c(tail(!obu_bxl$in_bxl, -1), F) # all but lasts: in bxl, next not
  
  tr_summary <- truck_summary(obu_bxl@data)
  
  
  
  #--------------------------------------interesting points----------------
  # For maximum analysis options, save entire DataFrame of relevant points (cleaning at VUB)
  
  # entries in bxl
  all_entries <- obu_bxl[obu_bxl$entry,]
  all_exits <- obu_bxl[obu_bxl$exit,]
  
  # stops > 5 minutes (can do more stringent cleaning at VUB)
  bxl_stops5m <- obu_bxl[obu_bxl$in_bxl & obu_bxl$time > 300,]
  
  # speed of 0
  bxl_speed0 <- obu_bxl[obu_bxl$in_bxl & obu_bxl$velocity == 0,]
  
  # parked
  bxl_parked <- obu_bxl[obu_bxl$in_bxl & (sel_first_id | sel_last_id),]
  
  day <- as.numeric(format(head(obu_bxl,1)$timestamp, "%d"))
  
  #-----------------------write to file-------------------------------
  write.csv(tr_summary,paste("summary","_",file_ids, sep=""),row.names = FALSE)
  write.csv(all_entries,paste("all_entries","_",file_ids, sep=""),row.names = FALSE)
  write.csv(all_exits,paste("all_exits","_",file_ids, sep=""),row.names = FALSE)
  write.csv(bxl_stops5m,paste("bxl_stops5m","_",file_ids, sep=""),row.names = FALSE)
  write.csv(bxl_speed0,paste("bxl_speed0","_",file_ids, sep=""),row.names = FALSE)
  write.csv(bxl_parked,paste("bxl_parked","_",file_ids, sep=""),row.names = FALSE)
  write.csv(counts,paste("counts","_",file_ids, sep=""),row.names = FALSE)
  
  #----------------------- per hour stuff -------------------------------
 
  # add hour as column
  obu_bxl$hour <- as.numeric(format(obu_bxl$timestamp, "%H"))

  perhour(obu_bxl@data,file_ids)
  
}

#===============================================================================
#===============================================================================

