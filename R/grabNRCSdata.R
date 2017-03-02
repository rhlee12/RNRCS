##############################################################################################
#' @title grabNRCS.data

#' @author Robert Lee \email{rhlee@colorado.edu}

#' @description A data downloading tool for NRCS networks.

#' @param network The network of the NRCS/AWDB site of interest. Currently only works for options: 'SCAN','SNTL','SNTLT', and 'OTHER'.\cr
#' @param site_id The NRCS site ID. Use grabNRCS.meta to retrieve a list of available sites in a specified network. \cr
#' @param timescale Specify the desired timescale of the data. Typically 'hourly', 'daily', or 'monthly'\cr
#' @param DayBgn Optional. Specify the beginning date for the returned date, otherwise the beginning of the period of record is returned.\cr
#' @param DayEnd Optional. Specify the end date for the returned date, otherwise the end of the period of record is returned.\cr
#'
#' @return Returns a data frame of requested data. If the output is not specified, the data frame is returned as 'NRCS.df' to the global environment.\cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, weather\cr

#' @references A list networks and their identifiers can be found here: https://www.wcc.nrcs.usda.gov/report_generator/AWDB_Network_Codes.pdf

#' @examples
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2017-03-03)
#     original creation
##############################################################################################

grabNRCS.data<-function(network, site_id, timescale, DayBgn, DayEnd){

    #load element data - MUST FIX
    eCodes<-readRDS(file=paste0(getwd(),"/data/NRCS_eCodes.rds"))

    #Default to period of record for station if dates not specified.
    if(missing(DayBgn)){DayBgn<-"POR_BEGIN"}
    if(missing(DayEnd)){DayEnd<-"POR_END"}

    #factors suck
    options(stringsAsFactors = F)

    #Stick to the appropriate timescales
    ctrlTimeScale<-c("hourly", "daily", "monthly")
    if (!timescale %in% ctrlTimeScale){stop("Please enter one of the following for timescale: 'hourly', 'daily', 'monthly'")}

    #Stick to the networks- need to add "USGS:, "OTHER", "COOP", "SNOW", "BOR" once those are sorted out
    ctrlNetworks<-c("SCAN","SNTL","SNTLT","SNOW","MPRC","OTHER","ALL")
    if (!timescale %in% ctrlTimeScale){stop("Please enter one of the following for network: 'SCAN', 'SNTL', 'SNTLT', 'SNOW', 'MPRC', 'OTHER', 'ALL'")}

    #Station metadata
    meta<-grabNRCS.meta(ntwrks = network)
    site_state<-as.character(meta[[1]]$state[grep(pattern = site_id, x=meta[[1]]$site_id)])

    #Get station elements, translate into element codes
    comboCode<-paste(network, site_id, sep=":")
    siteElmnt<-grabNRCS.elements(comboCode)
    siteEnames<-trimws(siteElmnt[[1]]$element, which = "both")
    eCodeIndx <- c()
    for(i in 1:length(siteEnames)){
        eCodeIndx<-append(eCodeIndx, grep(tolower(siteEnames[i]), tolower(eCodes$ElementName)))
    }
    siteEcodes<- eCodes$ElementCode[eCodeIndx] #which(match(tolower(siteEnames), tolower(eCodes$ElementName)))
    eCodeString<-do.call(paste, c(as.list(siteEcodes), sep = "::value,"))
    eCodeString<-paste0(eCodeString, "::value")

    #Build data URL
    baseURL <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/"

    fullURL <- paste0(baseURL, timescale, "/", site_id, ":", site_state, ':', network, '/', DayBgn, ",", DayEnd, '/', eCodeString)

    rawData<- readLines(fullURL)
    onlyData<-rawData[-grep("^[#]", rawData)]
    topline<-onlyData[1]


    #Add NAs in for blank entries, need to do twice to catch all
    onlyData<-paste0(onlyData[grep(",$", onlyData)], "NA")
    onlyData<-gsub(pattern = ",,", replacement = ",NA,", onlyData)
    onlyData<-gsub(pattern = ",,", replacement = ",NA,", onlyData)

    onlyData<-append(topline, onlyData)
    dfNRCS <- data.frame(do.call('rbind', strsplit(as.character(onlyData),',',fixed=TRUE)))
    colnames(dfNRCS)<-dfNRCS[1,]
    NRCS.df<<-dfNRCS[-1,]

}



