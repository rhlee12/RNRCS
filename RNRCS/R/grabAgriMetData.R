##############################################################################################
#' @title grabAgriMet.data

#' @author Robert Lee \email{rhlee@colorado.edu}

#' @description A data downloading tool for AgriMet networks. Data retrieval is limited by the speed of the connection, if timeout errors persist break requested time periods down into smaller chunks, or use a faster connection.

#' @param site_id An AgriMet station code (4 upper-case characters). A station map with codes is located here: \url{https://www.usbr.gov/pn/agrimet/agrimetmap/agrimap.html}.\cr
#' @param timescale Specify the desired timescale of the data. Valid entries are 'subhourly', hourly', or 'daily'.\cr
#' @param DayBgn Specify the beginning date (as YYYY-MM-DD ) for the returned data.\cr
#' @param DayEnd Specify the end date (as YYYY-MM-DD ) for the returned data.\cr
#' @param pCodes A character vector of AgriMet parameter codes you'd like returned. Codes and more information can be found on the AgriMet website: \url{https://www.usbr.gov/pn/agrimet/aginfo/station_params.html}
#'
#' @return Returns a data frame of requested data. Only elements with at least one data will be returned in the data frame.\cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, weather, AgriMet\cr

#'
#' @examples
#' \dontrun{
#' # Return wind data at the Nampa Station, for Jan 2020
#' getAgriMet.data(site_id="NMPI", timescale="subhourly", DayBgn = "2020-01-01", DayEnd="2020-02-01", pCodes=c("WD", "WG", "WS"))
#'}

#' @seealso Currently none
#' @importFrom magrittr %>%
#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2020-07-06)
#     Original creation
##############################################################################################

getAgriMet.data=function(site_id, timescale, DayBgn, DayEnd, pCodes){

    #Error handling - only accept valid timescales
    if(timescale %in% c("subhourly", "hourly", "daily")){
        # build Parameter stings
        PCODES=paste0(pCodes, collapse = "&pcode=")

        # Parse out componens of entered dates
        BGN_YEAR=lubridate::year(DayBgn)
        BGN_MONTH=lubridate::month(DayBgn)
        BGN_DAY=lubridate::day(DayBgn)

        END_YEAR=lubridate::year(DayEnd)
        END_MONTH=lubridate::month(DayEnd)
        END_DAY=lubridate::day(DayEnd)

        # URL to use is conditional on the timescale
        if(timescale=="subhourly"){
            baseURL="https://www.usbr.gov/pn-bin/instant.pl?station=STATIONID&year=BGN_YEAR&month=BGN_MONTH&day=BGN_DAY&year=END_YEAR&month=END_MONTH&day=END_DAY&pcode=PCODES"
        }else if(timescale=="hourly"){
            baseURL="https://www.usbr.gov/pn-bin/instant.pl?station=STATIONID&year=BGN_YEAR&month=BGN_MONTH&day=BGN_DAY&year=END_YEAR&month=END_MONTH&day=END_DAY&pcode=PCODES&print_hourly=1"
        }else if(timescale=="daily"){
            baseURL="https://www.usbr.gov/pn-bin/daily.pl?station=STATIONID&year=BGN_YEAR&month=BGN_MONTH&day=BGN_DAY&year=END_YEAR&month=END_MONTH&day=END_DAY&pcode=PCODES"
        }

        # Build our call url (to get data) from the appropriate base URL, and by gsubbing the information into our string
        callURL=baseURL %>%
            gsub(pattern = "STATIONID", replacement = site_id) %>%
            gsub(pattern = "BGN_YEAR", replacement = BGN_YEAR) %>%
            gsub(pattern = "BGN_MONTH", replacement = BGN_MONTH) %>%
            gsub(pattern = "BGN_DAY", replacement = BGN_DAY) %>%
            gsub(pattern = "END_YEAR", replacement = END_YEAR) %>%
            gsub(pattern = "END_MONTH", replacement = END_MONTH) %>%
            gsub(pattern = "END_DAY", replacement = END_DAY) %>%
            gsub(pattern = "PCODES", replacement = PCODES)

        # Store the return of the call
        returnedData=readLines(callURL, warn = F)
        #print(tail(returnedData))

        # If we got enough data, we'll parse that. Othewise we'll just message that nothing was there.
        #if(any(grepl(pattern = "BEGIN DATA", x = returnedData))&any(grepl(pattern = "END DATA", x = returnedData))){
        if(any(grepl(pattern = "BEGIN DATA", x = returnedData))){
            # Get the data start + stop in the return
            dataStartIndex=grep(pattern = "BEGIN DATA", x = returnedData)+2
            if(any(grepl(pattern = "END DATA", x = returnedData))){
                dataEndIndex=grep(pattern = "END DATA", x = returnedData)-1
            }else{
                dataEndIndex=length(returnedData)-1
                actualStartDate=stringr::str_extract(returnedData[dataStartIndex], pattern = "[0-9]{2}/[0-9]{2}/[0-9]{4}") %>%
                    as.POSIXct(format="%m/%d/%Y") %>%
                    as.character()

                actualEndDate=stringr::str_extract(returnedData[dataEndIndex], pattern = "[0-9]{2}/[0-9]{2}/[0-9]{4}") %>%
                    as.POSIXct(format="%m/%d/%Y") %>%
                    as.character()


                warning(paste0("----------\nWARNING!\n  The full requested preriod was NOT returned.\n  Date range returned was ", actualStartDate, " to ", actualEndDate,"\n---------"))
            }

            # get the names of the data
            colnames=strsplit(x=returnedData[dataStartIndex-1], split = ",") %>% unlist() %>% trimws()

            # if we got at least one row of data, parse it and return (end of function)
            #if(dataEndIndex>dataStartIndex){
            dataOut=returnedData[dataStartIndex:dataEndIndex] %>%
                strsplit(split = ",") %>%
                lapply(FUN = trimws) %>%
                do.call(what = rbind) %>%
                as.data.frame() %>%
                `colnames<-`(value=colnames)

            return(dataOut)
            # if no data, say so
            # }else{
            #     message(paste0("No data found for parameters entered. \nData URL tried: \n", callURL))
            # }
            # if no data, say so
        }else{
            message(paste0("No data found for parameters entered. \nData URL tried: \n", callURL))
        }

        # Error in selected timestamp
    }else{
        stop("timestamp parameter must be one of the following: subhourly, hourly, daily, monthly")
    }

}
