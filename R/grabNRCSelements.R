##############################################################################################
#' @title grabNRCS.elements

#' @author Josh Roberti \email{jaroberti87@gmail.com} 

#' @description A search tool that grabs element (reporting variables) metadata for site within the NRCS network\cr

#' @param site_id site id(s) of NRCS site(s) to grab metadata for.  Default is NRCS site 2221.  The user should note that it may take several minutes to download element level data for an entire NRCS network.

#' @return A list of n dataframes comprising the element level metadata for n NRCS sites

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, weather

#' @examples
#' grabNRCS.elements(site_id=2221,verbose=FALSE)
#' grabNRCS.elements(site_id=c(2221,1277,1252))
#' \cr
#' #You can make use of both 'grab' functions, \code{grabNRCS.meta()} & \code{grabNRCS.elements()} to grab site and element level data for a specific network
#' SNTL.sites<-grabNRCS.meta(ntwrks="SNTL")
#' grabNRCS.elements(site_id=SNTL.sites$SNTL$site_id)
#' #It may take up to several minutes to download element level data for an entire NRCS network

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Josh Roberti (2017-02-21)
#     original creation
##############################################################################################

####add example where someone can run the grabNRCSmeta and use the results to grab element data#### something like this:
#grabNRCS.elements(site_id=grabNRCS.elements("SCAN")[[2]])

#this function will return detailed variable metadata for the NRCS site:
grabNRCS.elements<-function(site_id=2221){
    #dynamically create URLs:
    urls<-lapply(site_id,function(x) paste0("https://wcc.sc.egov.usda.gov/nwcc/site?sitenum=",x))
    #grab element level metadata from each of the sites:
    site.elements<-lapply(urls, function(x) xml2::read_html(x))
    #define %>% globally or it won't work:
    `%>%` <-magrittr::`%>%`
    site.elements.sub<-lapply(site.elements, function(x) x %>% rvest::html_nodes("option"))
    #clean the html tags:
    elements.clean<-lapply(site.elements.sub, function(x) gsub("<.*?>", "", x))
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
    elementData<-lapply(elementData, setNames, nm = c("element","date.start"))
    #final Check to make sure element level data and dates actually exist:
    QC.check<-lapply(elements.clean, function(x) ((grep("Individual elements",x)+1)-(grep("Daily",x)-1)))  
    QC.check.index<-which(unlist(QC.check)>0)  #this checks to see if an NRCS site doesn't exist or doesn't have element data
    #replace those that fail QC test with NA
    elementData[QC.check.index]<-NA
    #set the names of each nested dataframe to the NRCS station id:
    names(elementData)<-paste0("NRCS_ID:",site_id)
    #return the list of dataframes comprising element level data:
    return(elementData)
}#end function



