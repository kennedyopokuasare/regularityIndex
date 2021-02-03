
is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}

normalize<-function(data,range=c(0,1)){
    if(min(data)==max(data)){
        print("Min Max scaling failed - min=max")
        return (rep(range[2],length(data)))
    }
  
    a=range[1]
    b=range[2]
    x_prime= a+((data-min(data,na.rm=T))*(b-a))/(max(data,na.rm=T)-min(data,na.rm=T))
  
   return (x_prime)
}

#to ensure that there are hourly values for each hour 0 to 23
preprocess <-function(data,range=c(-0.5,0.5),default_hourly=0){
    require(dplyr)
    # produce 24 hour data 
    all_hours=data.frame(local_hour=c(0:23))
    data_24=left_join(all_hours,data,by="local_hour")
    #fill in missing hours
    data_24[is.na(data_24)]<-default_hourly
    data_24[!is.finite.data.frame(data_24)]<-default_hourly
    
    # normalise data
    data_24=data_24 %>% mutate(value=normalize(value,range=range))
    return (data_24)
}
regularityIndex<-function(day_date,day_data,all_days_data){
    day_data=preprocess(day_data)
    #print("preprocessed day data")
    #print(day_data)
    ri=c()
    dates=unique(all_days_data$local_date)
    dates=dates[dates!=day_date]
    for(entry in dates){
        data=all_days_data %>%filter(local_date==entry) %>% select(local_hour,value)
        data=preprocess(data)
        entry_ri=sum(day_data$value*data$value)/24
        ri=c(ri,entry_ri)
    }
    #print("daily regularity index")
    #print(day_date)
    #print(ri)
    return (mean(ri))
}

