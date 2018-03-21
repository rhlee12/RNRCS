##############################################################################################
#' @title plotSCAN.windrose

#' @author Robert Lee \email{rhlee@colorado.edu}

#' @description A wind rose plotting function for SCAN sites. For a given SCAN site ID, and requested period, a wind rose is output.

#' @param scan_site The numeric ID for a valid SCAN site.\cr
#' @param DayBgn Specify the beginning date (as YYYY-MM-DD ) for the returned data.\cr
#' @param DayEnd Specify the end date (as YYYY-MM-DD ) for the returned data.\cr
#' @param speed_bins Optional. The number of bins to return wind speeds in. Defaults to 10.\cr
#' @param dir_bins Optional. The number of bins to return wind directions. Defaults to 36 bins (10 degree increments). \cr
#' @param units Optional. The format speed data are returned in (either "metric" or "imperial").
#'
#' @return Returns a wind rose for the requested parameters as a ggplot2 object.\cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, weather, wind, wind rose\cr

#' @references Downloads <https://wcc.sc.egov.usda.gov/reportGenerator>
#'
#' @examples
#' #Returns a wind rose for Nunn #1, for data for the period between Jan 1, 2017 and Feb 1, 2017.
#' \dontrun{
#' plotSCAN.windrose(scan_site = 2017, DayBgn = "2017-01-01", DayEnd = "2017-02-01")
#' }

#' @seealso grabNRCS.data()

#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-06)
#     Original creation
##############################################################################################

plotSCAN.windrose <- function(scan_site, DayBgn, DayEnd, speed_bins= 10, dir_bins = 15, units=c("metric", "imperial"))
{
    if(missing(units)){units="metric"}

    validUnits<-c("metric", "imperial")

    if(!units %in% validUnits){
        message("Invalid 'units' argument, only 'metric' or 'imperial' can be input.")
        message("Defaulting to metric units (meters per second).")
        units<-"metric"}
    #get some data
    windData <- grabNRCS.data(network = "SCAN", site_id = scan_site, timescale = "hourly", DayBgn = DayBgn, DayEnd = DayEnd)

    if(missing(speed_bins)){speed_bins=10}
    if(missing(dir_bins)){dir_bins=36}

    #find good data
    dirIndx<- grep(x=colnames(windData), pattern = "wind.direction", ignore.case = T)
    speedIndx <- grep(x=colnames(windData), pattern = "wind.speed.average", ignore.case = T)

    # Stop if we didn't get data
    if(length(speedIndx)==0){stop("No wind data at selected SCAN data - try another site")}

    #Store the good data
    direct<-windData[,dirIndx]
    speed<-windData[, speedIndx]

    all<-as.data.frame(cbind("Dir"=as.numeric(direct), "Speed"=as.numeric(speed)))
    all<-all[all$Dir>=0,]

    #bin up th data
    degreeSteps<-360/dir_bins
    dir.bins<-seq(0, 360, 360/dir_bins)
    SpeedCut= cut(all$Speed, breaks = speed_bins)
    DirCut= cut(all$Dir, breaks = dir.bins)
    all_binned<-cbind(all, SpeedCut, DirCut)
    all_binned<-stats::na.omit(all_binned)

    #Label making for the plot
    bgnLabels<- unique((dir.bins-(degreeSteps/2))%%360)
    endLabels<- unique((dir.bins+(degreeSteps/2))%%360)

    dirLabels<-paste0(bgnLabels, "-", endLabels)
    titleString<-paste0("SCAN:", scan_site, "  -  ", DayBgn, " to ", DayEnd)

    # Report correct unit information
    if(units=="metric"){
        speed<-round(speed*0.44704, digits = 2) # Convert from reported MPH to m/s.
        legendUnits <-"Wind Speed (m/s)"
    }else{
        legendUnits <-"Wind Speed (MPH)"
    }

    #make the plot
    plot<-ggplot2::ggplot(data = all_binned, ggplot2::aes(x=DirCut, fill=SpeedCut, colors=as.factor(SpeedCut)))+
        ggplot2::geom_bar(width = .95, show.legend = T)+
        ggplot2::theme_linedraw()+
        ggplot2::coord_polar(theta = "x", start = 0)+
        ggplot2::xlab("")+
        ggplot2::ylab("Count")+
        ggplot2::labs(title=titleString)+
        ggplot2::scale_x_discrete(labels=dirLabels)+
        ggplot2::scale_fill_discrete(h = c(0, 240), l=65, c=100, name=legendUnits)

    return(plot)
}
