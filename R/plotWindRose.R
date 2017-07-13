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

    if(!units %in% validUnits){stop("Please enter either 'metric' or 'imperial' for the units argument.")}

    windData <- grabNRCS.data(network = "SCAN", site_id = scan_site, timescale = "hourly", DayBgn = DayBgn, DayEnd = DayEnd)
    if(missing(speed_bins)){speed_bins=10}
    if(missing(dir_bins)){dir_bins=36}

    dirIndx<- grep(x=colnames(windData), pattern = "wind.direction", ignore.case = T)
    speedIndx <- grep(x=colnames(windData), pattern = "wind.speed.average", ignore.case = T)
    if(length(speedIndx)==0){stop("No wind data at selected SCAN data - try another site")}
    direct<-windData[,dirIndx]
    speed<-windData[, speedIndx]

    # Report correct unit information
    if(units=="metric"){
        speed<-round(speed*0.44704, digits = 2) # Convert from reported MPH to m/s.
        titleUnits <-"Wind rose (m/s)"
    }else{
        titleUnits <-"Wind rose (MPH)"
    }

    all<-as.data.frame(cbind("Dir"=as.numeric(direct), "Speed"=as.numeric(speed)))
    all<-all[all$Dir>=0,]

    degreeSteps<-360/dir_bins
    dir.bins<-seq(0, 360, 360/dir_bins)
    all_binned<-cbind(all, "Wind Speed"= cut(all$Speed, breaks = speed_bins), "DirCut"= cut(all$Dir, breaks = dir.bins))
    all_binned<-stats::na.omit(all_binned)

    bgnLabels<- unique((dir.bins-(degreeSteps/2))%%360)
    endLabels<- unique((dir.bins+(degreeSteps/2))%%360)

    dirLabels<-paste0(bgnLabels, "-", endLabels)
    titleString<-paste0("SCAN:", scan_site, " ", titleUnits, ", ", DayBgn, " to ", DayEnd)

    plot<-ggplot2::ggplot(data = all_binned, ggplot2::aes_string(x="DirCut", fill="Wind Speed", colors=factor("Wind Speed")))+
        ggplot2::geom_bar(width = .95, show.legend = T)+
        ggplot2::theme_linedraw()+
        ggplot2::coord_polar(theta = "x", start = 0)+
        ggplot2::xlab("")+
        ggplot2::ylab("Count")+
        ggplot2::labs(title=titleString)+
        ggplot2::scale_x_discrete(labels=dirLabels)

    return(plot)
}
