##############################################################################################
#' @title grabNRCS.data

#' @author Robert Lee \email{rhlee@colorado.edu}

#' @description A data downloading tool for NRCS networks. Data retrieval is limited by the speed of the connection, if timeout errors persist break requested time periods down into smaller chunks, or use a faster connection.

#' @param network The network of the NRCS/AWDB site of interest. Currently only works for options: 'SCAN','SNTL','SNTLT', and 'OTHER'.\cr
#' @param site_id The NRCS site ID. Use grabNRCS.meta to retrieve a list of available sites in a specified network. Consider using the package 'metScanR' to locate sites.\cr
#' @param timescale Specify the desired timescale of the data. Typically 'hourly', 'daily', or 'monthly'\cr
#' @param DayBgn Specify the beginning date (as YYYY-MM-DD ) for the returned data.\cr
#' @param DayEnd Specify the end date (as YYYY-MM-DD ) for the returned data.\cr
#'
#' @return Returns a data frame of requested data. Only elements with at least one data will be returned in the data frame.\cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, weather\cr

#' @references Downloads <https://wcc.sc.egov.usda.gov/reportGenerator>
#'
#' @examples
#' grabNRCS.data(network="SNTLT", site_id=1198, timescale="monthly",
#' DayBgn = '2017-01-01', DayEnd = '2017-05-01')
#' #Return monthly summaries between January and May 2017 of record at a SNOLITE site.

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2017-04-03)
#     Original creation
#   Robert Lee (2017-07-06)
#    Removed period of record calls, soil elements,
#    and swithced to read.csv for data return
##############################################################################################

grabNRCS.data<-function(network, site_id, timescale, DayBgn, DayEnd){
    #some options to make life easier
    options(timeout = 400)
    options(stringsAsFactors = F)

    #load element info
    eCodes<-RNRCS::elementCodes

    #Stick to the appropriate timescales
    ctrlTimeScale<-c("hourly", "daily", "monthly")
    if (!timescale %in% ctrlTimeScale){stop("Please enter one of the following for timescale: 'hourly', 'daily', 'monthly'")}

    #Stick to the networks- need to add "USGS:, "OTHER", "COOP", "SNOW", "BOR" once those are sorted out
    ctrlNetworks<-c("SCAN","SNTL","SNTLT")
    if (!network %in% ctrlNetworks){stop("Please enter one of the following for network: 'SCAN', 'SNTL', 'SNTLT'")}

    #Station metadata
    meta<-grabNRCS.meta(ntwrks = network)
    site_state<-as.character(meta[[1]]$state[grep(pattern = site_id, x=meta[[1]]$site_id)])

    #Get station elements, translate into element codes
    comboCode<-paste(network, site_id, sep=":")
    siteElmnt<-RNRCS::grabNRCS.elements(comboCode)
    siteEnames<-trimws(siteElmnt[[1]]$element, which = "both")
    eCodeIndx <- c()
    for(i in 1:length(siteEnames)){
        eCodeIndx<-append(eCodeIndx, grep(tolower(siteEnames[i]), tolower(eCodes$ElementName)))
    }

    siteEcodes<- eCodes$ElementCode[eCodeIndx]
    eCodeString<-do.call(paste, c(as.list(siteEcodes), sep = "::value,"))
    eCodeString<-paste0(eCodeString, "::value")

#     Soil temperature and moisture are currently broken, will implement again later
#     stoString<-"STO:-2:value:hourly MEAN,STO:-4:value:hourly MEAN,STO:-8:value:hourly MEAN,STO:-20:value:hourly MEAN,STO:-40:value:hourly MEAN,"
#     smsString<-"SMS:-2:value:hourly MEAN,SMS:-4:value:hourly MEAN,SMS:-8:value:hourly MEAN,SMS:-20:value:hourly MEAN,SMS:-40:value:hourly MEAN,"
#
#
#      eCodeString<-gsub(pattern="STO::value", replacement = stoString, eCodeString)
#      eCodeString<-gsub(pattern="SMS::value", replacement = stoString, eCodeString)

     eCodeString<-gsub(pattern="STO::value,", replacement = "", eCodeString)
     eCodeString<-gsub(pattern="SMS::value,", replacement = "", eCodeString)
    # #Build data URL
    baseURL <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/"

    fullURL <- paste0(baseURL, timescale, "/", site_id, ":", site_state, ':', network, '|id=""|name/', DayBgn, ",", DayEnd, '/', eCodeString)

    # rawData<- readLines(fullURL)
    # onlyData<-rawData[-grep("^[#]", rawData)]
    # topline<-onlyData[1]
    #
    # <-read.csv(fullURL, header = T, comment.char = "#")
    #
    # #Add NAs in for blank entries, need to do twice to catch all
    # onlyData<-paste0(onlyData[grep(",$", onlyData)], "NA")
    # onlyData<-gsub(pattern = ",,", replacement = ",NA,", onlyData)
    # onlyData<-gsub(pattern = ",,", replacement = ",NA,", onlyData)
    #
    # onlyData<-append(topline, onlyData)
    # dfNRCS <- data.frame(do.call('rbind', strsplit(as.character(onlyData),',',fixed=TRUE)))
    # colnames(dfNRCS)<-dfNRCS[1,]
    # dfNRCS[dfNRCS=="NA"]=NA
    tempDF<-utils::read.csv(fullURL, comment.char = "#", quote = "")
    if(ncol(tempDF)==1){stop("The NRCS API is currently unavailable, please try again later.")}
    # Remove all NA columns, output the final results to the global environment
    NRCS.df<-tempDF[,-which(colSums(is.na(tempDF))==nrow(tempDF))]

    return(NRCS.df)
}



