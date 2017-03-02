
# Possible reported variables:
# ---------------------------
# Snow-water equivalent (SWE): WTEQ::value
# Change in Snow-water equivalent (delta-SWE): WTEQ::delta
# Snow Depth: SNWD::value
# Change in Snow depth: SNWD::delta
# Precipitation Accumulation:
#
#
#Hourly
#https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/hourly/1062:AK:SNTL/-23,0/WTEQ::value,SNWD::value,PREC::value,TOBS::value,TMAX::value,TMIN::value,TAVG::value




library(reshape)



SNOTEL<-function(site_id, timescale, DayBgn, DayEnd){

    options(stringsAsFactors = F)
    ctrlTimeScale<-c("hourly", "daily", "monthly")
     if (!timescale %in% ctrlTimeScale){stop("Please enter one of the following for timescale: 'hourly', 'daily', 'monthly'")}

    meta<-grabNRCS.meta(ntwrks = "SNTL")
    site_state<-as.character(meta[[1]]$state[grep(pattern = statID, x=meta[[1]]$site_id)])

    baseURL <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/"

    fullURL <- paste0(baseURL, timescale,"/", site_id, ":", site_state, ':SNTL/POR_BEGIN,POR_END/SNWD::value')

    rawData<- readLines(fullURL)
    onlyData<-rawData[-grep("^[#]", rawData)]
    outDF <- data.frame(do.call('rbind', strsplit(as.character(onlyData),',',fixed=TRUE)))

}

SCAN<- function(site_id, timescale,){

    #https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/2197:CO:SCAN|id=%22%22|name/POR_BEGIN,POR_END/stationId,TMAX::value,TMIN::value,PRCP::value,RHUM::value:hourly%20MEAN,WSPDX::value:hourly%20MAX,WSPDV::value:hourly%20MEAN,SRADV::value:hourly%20MEAN,LRADT::value,PVPV::value:hourly%20MEAN,SVPV::value:hourly%20MEAN,SMS:-2:value:hourly%20MEAN,SMS:-4:value:hourly%20MEAN,SMS:-8:value:hourly%20MEAN,SMS:-20:value:hourly%20MEAN,SMS:-40:value:hourly%20MEAN,STO:-2:value:hourly%20MEAN,STO:-4:value:hourly%20MEAN,STO:-8:value:hourly%20MEAN,STO:-20:value:hourly%20MEAN,STO:-40:value:hourly%20MEAN


    ctrlTimeScale<-c("hourly", "daily", "monthly")
    if (!timescale %in% ctrlTimeScale){stop("Please enter one of the following for timescale: 'hourly', 'daily', 'monthly'")}

    meta<-grabNRCS.meta(ntwrks = "SCAN")

    site_state<-as.character(meta[[1]]$state[grep(pattern = site_id, x=meta[[1]]$site_id)])

    baseURL <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/"

    fullURL <- paste0(baseURL, timescale,"/", site_id, ":", site_state, ':SCAN|id=%22%22|name/POR_BEGIN,POR_END/SNWD::value,WTEQ::value')

    rawData<- readLines(fullURL)
    onlyData<-rawData[-grep("^[#]", rawData)]
    onlyData[which(nchar(onlyData)<=11)]<-paste0(onlyData[which(nchar(onlyData)<=11)],"NA")
    outDF <- data.frame(do.call('rbind', strsplit(as.character(onlyData),',',fixed=TRUE)))

}

BOR <- function(site_id, timescale, dayBgn, dayEnd){
    ctrlTimeScale<-c("daily", "monthly")
    if (!timescale %in% ctrlTimeScale){stop("Please enter one of the following for timescale: 'daily', 'monthly'")}

    meta<-grabNRCS.meta(ntwrks = "BOR")

    site_state<-as.character(meta[[1]]$state[grep(pattern = site_id, x=meta[[1]]$site_id)])

    baseURL <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/"

    fullURL <- paste0(baseURL, timescale,"/", site_id, ":", site_state, ':BOR/POR_BEGIN,POR_END/RESC::value')

    rawData<- readLines(fullURL)
    onlyData<-rawData[-grep("^[#]", rawData)]
    onlyData[which(nchar(onlyData)<=11)]<-paste0(onlyData[which(nchar(onlyData)<=11)],"NA")
    outDF <- data.frame(do.call('rbind', strsplit(as.character(onlyData),',',fixed=TRUE)))

    }
# all current reporting sites in Co:
#    "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultipleStationReport/daily/network=%22SNTLT%22,%22SNTL%22%20AND%20element=%22SNWD%22|name/0,0/name,stationId,state"


# data<-readLines("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customGroupByMonthReport/daily/816:WY:SNTL|id=%22%22|name/POR_BEGIN,POR_END/SNWD::value")
# onlyCSV<-data[-grep("^[#]", data)]
# foo <- data.frame(do.call('rbind', strsplit(as.character(onlyCSV),',',fixed=TRUE)))
# colnames(foo)<-paste(unlist(foo[1,]), "-", unlist(foo[2,]))
# colnames(foo[,1:2])<-c("Water Year", "Day")
# NRCSraw<<-foo[-(1:2),]
# names<-foo[1,]
# onlyData<-foo[-(1:2),]
#
# #onlyData[,1]<-paste0(onlyData[,1],",",onlyData[,2])
# #onlyData<-onlyData[-2]
# colnames(onlyData)<-c("waterYear, Day", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
# meltData<-data.table::melt(data = onlyData, id.vars = c("waterYear, Day"))
#
# waterYearData<-colsplit(meltData$`waterYear|Day`, split = ",", names = c("waterYear", "day", "month", "PRECIPval"))
#
#
# zoo::as.Date.yearmon(as.numeric(paste0(onlyData[,1],onlyData[,2])))
