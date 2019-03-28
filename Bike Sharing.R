library(ggplot2)
stations = read.csv('C:/Users/rosha/Downloads/Ch5_bike_station_locations.csv')
summary(stations)
ggplot(stations,aes(x=latitude))+geom_histogram(binwidth=0.005,color='white')+theme_bw()
ggplot(stations,aes(x=longitude))+geom_histogram(binwidth=0.005,color='white')+theme_bw()
ggplot(stations,aes(x=longitude,y=latitude))+geom_point()+theme_bw()  

k2=kmeans(stations,2)
k2
k3=kmeans(stations,3)
k3
dfcluster=cbind(stations,clus2=k2$cluster,clus3=k3$cluster) 
dfcluster$clus2=as.factor(dfcluster$clus2)
dfcluster$clus3=as.factor(dfcluster$clus3)
ggplot(dfcluster,aes(x=longitude,y=latitude,color=k2$cluster))+geom_point()+theme_bw()
ggplot(dfcluster,aes(x=longitude,y=latitude,color=k3$cluster))+geom_point()+theme_bw()
ggplot(dfcluster,aes(x=longitude,y=latitude,color=clus3,shape=clus2))+geom_point()+theme_bw()+
  geom_point(aes(x=k2$centers[1,2],y=k2$centers[1,1]),color='black',shape=11,size=3)+
  geom_point(aes(x=k2$centers[2,2],y=k2$centers[2,1]),color='black',shape=11,size=3)+
  geom_point(aes(x=k3$centers[1,2],y=k3$centers[1,1]),color='red',shape=11,size=3)+
  geom_point(aes(x=k3$centers[2,2],y=k3$centers[2,1]),color='red',shape=11,size=3)+
  geom_point(aes(x=k3$centers[3,2],y=k3$centers[3,1]),color='red',shape=11,size=3)
  
library(geosphere)
miles2=distm(dfcluster[,c('latitude','longitude')],k2$centers,fun=distHaversine)/1609  #here 1609 is divided to convert metres in miles
dfcluster[c('station21','station22')]=miles2
dfcluster$avg2=apply(dfcluster[c('station21','station22')],1,FUN = mean) 
dfcluster$pred2=ifelse(dfcluster$clus2==1,dfcluster$station21,dfcluster$station22)
nrow(subset(dfcluster,avg2!=pred2))
