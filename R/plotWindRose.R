
windData<-HarmsWay

plot.Windrose <- function(scan_site, DayBgn, DayEnd, speed_bins= 10, dir_bins = 36)
{
    require(ggplot2)
    windData <- grabNRCS.data(network = "SCAN", site_id = scan_site, timescale = "hourly", DayBgn = DayBgn, DayEnd = DayEnd)
    dirIndx<- grep(x=colnames(windData), pattern = "direction", ignore.case = T)
    speedIndx <- grep(x=colnames(windData), pattern = "speed average", ignore.case = T)
    if(length(speedIndx)==0){stop("No wind data at selected SCAN data - try another site")}
    direct<-windData[,dirIndx]
    speed<-windData[, speedIndx]
    all<-as.data.frame(cbind("Dir"=as.numeric(direct), "Speed"=as.numeric(speed)))

    all_binned<-cbind(all, "Speed.cut"= cut(all$Speed, breaks = speed_bins), "Dir.cut"= cut(all$Dir, breaks = dir_bins))



    ggplot2::ggplot(data = all_binned, aes(x=Dir.cut, fill=Speed.cut, colors=factor(Speed.cut)))+geom_bar()+coord_polar(theta = "x", start = 0)+xlab("")

}
