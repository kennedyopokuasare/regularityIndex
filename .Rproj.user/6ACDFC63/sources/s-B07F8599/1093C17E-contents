
library("dplyr")
source('regularityIndex.R')

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

compute_screen_hourly<-function(screen_data){
    screen_data<-screen_data %>% 
                        mutate( screen_status =case_when( .$screenOn=="false" ~ -1, .$screenOn=="true" ~ 1, TRUE~-1 )) %>% 
                        group_by(local_date,local_hour) %>% 
                        summarise( value=mean(Modes(screen_status)) )%>%
                        ungroup() %>%
                        arrange(local_date,local_hour)
     return (screen_data)       

}


sample_date="./sample_screen_data.csv"
screen_data <- read.csv(sample_date, stringsAsFactors = F)

screen_hourly=compute_screen_hourly(screen_data)


ri_feature=screen_data%>%
            mutate(screen_status =case_when(
                        .$screenOn=="false" ~ 0,
                        .$screenOn=="true" ~ 1,
                        TRUE~-1
                    ))%>%
              filter(screen_status>-1)%>%
              mutate(local_date=as.Date(local_date))%>%
              group_by(local_date)%>%
              summarise(
                  ri=regularityIndex(
                        first(local_date), #current date
                        screen_hourly%>%filter(local_date==first(local_date)), #hourly screen values for the current datte
                        screen_hourly #hourly screen values for everyday
                        )
              )


write.table(ri_feature,"ri_features.csv",quote=F,col.names=T,row.names=F,sep=",")

