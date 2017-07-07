##############################################################################################
#' @title grabBOR.data

#' @author Robert Lee \email{rhlee@colorado.edu}

#' @description A data downloading tool for reservoirs managed by the United States Bureau of Reclamation. Data retrieval is limited by the speed of the connection, if timeout errors persist break requested time periods down into smaller chunks, or use a faster connection.

#' @param site_id The BOR site ID. Use grabNRCS.meta to retrieve a list of available sites in a specified network. Consider using the package 'metScanR' to locate sites.\cr
#' @param timescale Specify the desired timescale of the data. Typically 'hourly', 'daily', or 'monthly'\cr
#' @param DayBgn Specify the beginning date (as YYYY-MM-DD ) for the returned data.\cr
#' @param DayEnd Specify the end date (as YYYY-MM-DD ) for the returned data.\cr
#'
#' @return Returns a data frame of requested data and a list of varaibles with no data.\cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, weather, reservoir, Bureau of Reclamation\cr

#' @references Downloads <https://wcc.sc.egov.usda.gov/reportGenerator>
#'
#' @examples
#' \dontrun{
#' JacksonLake<- grabBOR.data(site_id = 13010500, timescale = "monthly",
#' DayBgn="2016-01-01", DayEnd="2017-01-01")
#' }
#' #Return monthly summaries for the period of record at a Jackson Lake, WY.

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2017-04-10)
#     original creation
##############################################################################################

grabBOR.data<-function(site_id, timescale, DayBgn, DayEnd){
    #some options to make life easier
    options(timeout = 400)
    options(stringsAsFactors = F)

    #Default to period of record for station if dates not specified.
    # if(missing(DayBgn)){DayBgn<-"POR_BEGIN"}
    # if(missing(DayEnd)){DayEnd<-"POR_END"}

    #Stick to the appropriate timescales
    ctrl_timescale<-c("daily", "monthly")
    if (!timescale %in% ctrl_timescale){stop("Please enter one of the following for timescale: 'hourly', 'daily', 'monthly'")}
    network <- "BOR"

    #Station metadata
    meta<-RNRCS::grabNRCS.meta(ntwrks = network)
    site_state<-as.character(meta[[1]]$state[grep(pattern = site_id, x=meta[[1]]$site_id)])

    #Get station elements, translate into element codes
    comboCode<-paste(network, site_id, sep=":")
    #siteElmnt<-RNRCS::grabNRCS.elements(comboCode)
    #siteEnames<-trimws(siteElmnt[[1]]$element, which = "both")
    # eCodeIndx <- c()
    # for(i in 1:length(siteEnames)){
    #     eCodeIndx<-append(eCodeIndx, grep(tolower(siteEnames[i]), tolower(eCodes$ElementName)))
    # }


    eCodeString<-"REST::value,RESC::value,RESA::value"

    #Build data URL
    baseURL <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/"

    fullURL <- paste0(baseURL, timescale, "/", site_id, ":", site_state, ':', network, '|id=""|name/', DayBgn, ",", DayEnd, '/', eCodeString)

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
    dfNRCS[dfNRCS=="NA"]=NA

    # Remove all NA columns, and report which data varaiables they were
    tempDF<-as.data.frame(dfNRCS[-1,])
    missingVars<-names(which(colSums(is.na(tempDF))==nrow(tempDF)))
    missingVars<-missingVars

    # Output the final results to the global environment
    BOR.df<-tempDF[,-which(colSums(is.na(tempDF))==nrow(tempDF))]
    return(BOR.df)
}



