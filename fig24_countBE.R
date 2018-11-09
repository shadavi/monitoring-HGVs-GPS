library(plyr) # ddply
library(readr)  # read_csv
library(ggplot2)
library(lubridate) #hour


source("obu_functions.R") # common OBU functions, run before setwd'ing somewhere

dir_data<-c("/home/sheida/Documents/obu/p2016/",
            "/home/tias/people/sheida/OBU/data/oct2016/Summaries/")


file_ids<-c(20161001:20161008,20161010:20161025)
weekdays_exclude <- c("Saturday", "Sunday", "Samstag","Sonntag")
file_ids_week<-c(20161003:20161007,20161010:20161014,20161017:20161021,20161024,20161025)
summary_names<-paste0("summary_",file_ids_week) #adjust file name accordingly


setwd_exists(paste0(dir_data))


# get day id and truck ids to be removed
# obs_time< 2min or obs_distance< 200m or chull_area>500m2

removable_ids <- get_removable_ids(dir_data, summary_names)


# Create a cnt_driving timeseries (can have missing values!)
# overall obs_dist_driven > 1km, hourly bxl_dist_driven > 0.01km

setwd_exists(paste0(dir_data))


cnt_driving <- ldply(file_ids, function(fid) {
  print(fid)
  sub_files <- paste("summ_hour_",fid,"_",0:23,sep="")
  ldply(sub_files, function(fname, fid) {
    if (file.exists(fname)) {
      obu<-read_csv(fname,
                    col_types = cols_only(id = col_character(), start_t = col_datetime(),obs_dist_driven=col_double())) # faster: only read needed cols
      # remove ids with less then 1 km
      remove_ids<-removable_ids[removable_ids$fid==paste0("summary_",fid),"ids"]
      obu <- obu[!(obu$id %in% remove_ids),] # WARNING, this includes outside bxl...
      # fix time zone 
      attributes(obu$start_t)$tzone <- "Europe/Brussels"
      # ASSUME each file only one hour; return time,count
      data.frame(time=trunc(obu$start_t[1], "hour"), count=nrow(obu))
    }
  }, fid)
})


#3 make a chart

# remove Saturdays and Sundays
cnt_driving_week<-cnt_driving[!(weekdays(cnt_driving$time) %in% weekdays_exclude), ]

# run quantile function over splits of data per hour
cnt_driving_week$hour <- hour(cnt_driving_week$time)
plotdata <- ddply(cnt_driving_week, .(hour), function(x) quantile(x$count, c(0.05,0.25,0.5,0.75,0.95)))

colnames(plotdata)	<-	c("hour","lower","q25","median","q75","upper") 
# plot with quantiles
plot_quantiles(plotdata, "","Count")
dev.copy2pdf(file='/home/sheida/Workplace/papers/obu_paper/images/24.pdf',out.type="cairo", width=10, height=4)

