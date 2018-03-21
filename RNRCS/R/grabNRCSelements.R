##############################################################################################
#' @title grabNRCS.elements

#' @author Josh Roberti \email{jaroberti87@gmail.com}

#' @description A search tool that grabs element metadata (reporting variables) for sites within networks managed by the NRCS. Element level metadata can be pulled for the following networks: i) SCAN, ii) SNOTEL (SNTL), iii) SNOTEL-Lite (SNTLT), or iv) OTHER\cr

#' @param site_id site id(s) of NRCS site(s) to grab metadata for.  Default is 'SCAN:2221'.  The user should note that it may take several minutes to download element level data for an individual NRCS network, e.g., SCAN.

#' @return A list of n dataframes comprising the element level metadata for n NRCS sites

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, weather

#' @examples
#' grabNRCS.elements(site_id='SCAN:2221')
#'

#' @references Downloads <https://www.wcc.nrcs.usda.gov/web_service/AWDB_Web_Service_Reference.htm>
#'
#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Josh Roberti (2017-02-21)
#     original creation
#   Josh Roberti (2017-02-28)
#     some minor edits and QC of site searching
##############################################################################################

#this function will return detailed variable metadata for the NRCS site:
grabNRCS.elements<-function(site_id="SCAN:2221"){
    #grab full site_id code (which includes network) to assign at the end
    site_id.label<-site_id
    #site.QC.check: check to see if site_id is from one of the accepted networks:
    ntwrks.unique<-unique(gsub(":.*","",site_id))
    ntwrks.cntrl<-c("SCAN","SNTL","SNTLT","OTHER")
    ntwrks.QC<-ntwrks.unique %in% ntwrks.cntrl
    if(any(ntwrks.QC)==FALSE){
        stop("Unable to pull metadata. Metadata can only be pulled for the following NRCS networks: 'SCAN','SNTL','SNTLT', and 'OTHER'")
    }
    #strip any "ntwrk" name out of the site_ids if reading in from other function:
    site_id<-gsub(".*:","",site_id)
    #dynamically create URLs:
    urls<-lapply(site_id,function(x) paste0("https://wcc.sc.egov.usda.gov/nwcc/site?sitenum=",x))
    #grab element level metadata from each of the sites:
    site.elements<-lapply(urls, function(x) xml2::read_html(x))
    #define %>% globally or it won't work:
    `%>%` <-magrittr::`%>%`
    site.elements.sub<-lapply(site.elements, function(x) x %>% rvest::html_nodes("option"))
    #clean the html tags:
    elements.clean<-lapply(site.elements.sub, function(x) gsub("<.*?>", "", x))
    #assign names now - can use this later to properly name the resulting list of sites:
    names(elements.clean)<-site_id.label
    #find sites with no element level metadata:
    noElementMeta.sites<-which(lapply(elements.clean, function(x) length(x))==0)
    #remove sites with  no element level metadata:
    if(length(noElementMeta.sites)>0){
        elements.clean<-elements.clean[-noElementMeta.sites]
        if(length(elements.clean)==0){
            stop("No element level metadata for site of interest.")
        }
    }
    #subset the data based on grep
    elementSubset<-lapply(elements.clean, function(x) x[(grep("Individual elements",x)+1):(grep("Daily",x)-1)])
    #clean the data elements:
    variables.init<-lapply(elementSubset,function(x) gsub("[[:digit:]]|\\(|\\)","",x))
    #a bit more cleaning:
    variables<-lapply(variables.init, function(x) substr(x,1,nchar(x)-3))
    #grab the dates:
    dates<-lapply(elementSubset, function(x) gsub(".*\\((.*)\\).*", "\\1", x))
    #put information into dataframe and output:
    elementData<-lapply(Map(cbind,variables,dates), function(x) data.frame(x))
    #set names within each dataframe:
    elementData<-lapply(elementData, stats::setNames, nm = c("element","date.start"))
    #final Check to make sure element level data and dates actually exist:
    QC.check<-lapply(elements.clean, function(x) ((grep("Individual elements",x)+1)-(grep("Daily",x)-1)))
    QC.check.index<-which(unlist(QC.check)>0)  #this checks to see if an NRCS site doesn't exist or doesn't have element data
    #replace those that fail QC test with NA
    elementData[QC.check.index]<-NA
    #set the names of each nested dataframe to the NRCS station id:
    names(elementData)<-names(elements.clean)
    #return the list of dataframes comprising element level data:
    return(elementData)
}#end function



