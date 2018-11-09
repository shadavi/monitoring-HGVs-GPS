# setwd but with multiple possible options
setwd_exists <- function(dirs) {
  # check if dir exists, if so setwd to it.
  lapply(dirs, function(d) {
    if (file.exists(d))
      setwd(d)
  })
}

# return first existing directory, avoids need to setwd all the time (and hence break inclusions)
first_existing <- function(dirs) {
  dir_exists <- file_test("-d", dirs)
  head(dirs[dir_exists],1) # first one
}

# get file id and truck ids to be removed
# condition at this moment is: obs_time< 0.03  | obs_dist_driven<0.2 | chull_area<5000 

get_removable_ids <- function(dirs, file_names) {
  setwd_exists(paste0(dirs))
  ldply(file_names, function(fid) {
    if (file.exists(fid)) {
      summary<-read_csv(fid,
                      col_types = cols_only(id = col_character(), obs_time=col_double(),obs_dist_driven = col_double(), chull_area=col_double() ))# faster: only read needed cols
    data.frame(fid=fid, ids=summary[(summary$obs_time< 0.03  | summary$obs_dist_driven<0.2 | summary$chull_area<5000) ,]$id)}
  })
}


# get ID of trucks that start before 4:20 or end after 3:40
get_ids_idSwitch<- function(dirs,file_names) {
  setwd_exists(paste0(dirs))
  ldply(file_names, function(fid) {
    summary<-read_csv(fid,
                      col_types = cols_only(id = col_character(), start_t=col_datetime(format = ""),stop_t=col_datetime(format = "") )) # faster: only read needed cols
    data.frame(fid=fid, ids=summary[(((hour(summary$start_t)==4) &  (minute(summary$start_t)<20)) | ((hour(summary$stop_t)==1) &  (minute(summary$stop_t)>40))) ,]$id)
  })
}

# plot quantiles (0.05,0.25,0.5,0.75,0.95)
plot_quantiles <- function (plotdata, plottitle, yaxistitle) {
  theme_set(theme_gray(base_size = 14))
  p <- ggplot(plotdata) +
    geom_vline(xintercept = c(6,12,18), color="grey")+
    geom_ribbon(aes(ymin=lower, ymax=upper, x=hour, fill = "5 -- 95%\nQuantile"), alpha = 0.9, linetype=2)+
    geom_ribbon(aes(ymin=q25, ymax=q75, x=hour, fill = "25 -- 75%\nQuantile"), alpha = 0.9, linetype=2 )+
    geom_line(aes(y=median, x=hour, colour = "Median")) +
    scale_colour_manual("",values="Black")+
#   scale_fill_manual("",values=c("#66CCFF","#CCFFCC"))+
    scale_fill_manual("",values=c("azure4","azure3"))+
        ggtitle(plottitle)+
    scale_x_continuous(name="Hour of the day", breaks=function(v) seq(v[1],v[2],by=2), expand=c(0,0))+
    scale_y_continuous(name=yaxistitle)+
    theme(legend.position="bottom",
          legend.text=element_text(size=22),
          plot.title = element_text(size = 25),
          axis.title.x = element_text( size=24),
          axis.text.x= element_text(size=22, color="black"),
          axis.title.y = element_text( size=24),
          axis.text.y= element_text(size=22, color="black"))
  # hide yaxis title?
  if (yaxistitle == "") {
    p <- p + theme(axis.title.y=element_blank())
  }
  p # return
}

# purify contours: only keep those that do not contain multiple pure ones
# WARNING: this function changes the output quite significantly, and not sure what is best...
pure_contours <- function(polydf) {
  nivs <- unique(polydf$level)
  flatdf <- polydf[polydf$level == nivs[length(nivs-7)],]
  
  lapply(seq(length(nivs)-1,1,-1), function(i) {
    # contains: rows = current level, cols = previous flat
    gc <- gContains(polydf[polydf$level == nivs[i],], flatdf, byid=T)
    # remove previous that are part of one bigger one
    cnt_row <- data.frame(sum=rowSums(gc))
    flatdf <<- flatdf[!(cnt_row$sum == 1),]
    cnt_col <- data.frame(sum=colSums(gc))
    flatdf <<- rbind(flatdf, polydf[polydf$level == nivs[i],][cnt_col$sum <= 1,])
  })
  flatdf
}

# ------------------------points to line function
points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

# ------------------------points to line function over split over IDS


points_to_line_split_id<-function(data){
  return(do.call(rbind,lapply(split(data,data$id),function(x){
    temp<-as.data.frame(x)
    colnames(x)<-c("id","X","Y")
    l<-points_to_line(x,"X","Y")
    l
  })))
}
