##############################################################################################
#' @title grabNRCS.meta

#' @author Josh Roberti \email{jaroberti87@gmail.com}

#' @description A search tool that grabs site level metadata for NRCS networks: SCAN, SNOTEL, SNOTEL-LITE, SNOW, MPRC, and OTHER

#' @param ntwrks Name(s) of NRCS network(s) to grab metadata for.  Options are: 'SCAN','SNTL','SNTLT','SNOW','MPRC','OTHER','COOP','USGS','MSNT','BOR','CLMIND' and/or 'ALL'. If the user enters the term 'ALL,' site level metadata for all networks will be returned.  Default is 'SCAN'\cr
#' @param cnvrt.elev Optional. If set to TRUE, the elevation data will be converted from feet to meters.  Default is set to FALSE, i.e., elevation data are output in units of feet.

#' @return A list of n dataframes comprising the site level metadata for n NRCS networks\cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, weather\cr

#' @references Downloads <https://www.wcc.nrcs.usda.gov/web_service/AWDB_Web_Service_Reference.htm>

#' @examples
#' grabNRCS.meta(ntwrks="SCAN")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Josh Roberti (2017-02-20)
#     original creation
#   Josh Roberti (2017-20-28)
#     added COOP, USGS, MSNT, BOR, and CLMIND stations to search
##############################################################################################

grabNRCS.meta<-function(ntwrks="SCAN",cnvrt.elev=FALSE){
    #QC check:
    ntwrks.cntrl<-c("SCAN","SNTL","SNTLT","SNOW","MPRC","OTHER","COOP","USGS","MSNT","BOR","CLMIND","ALL")
    ntwrks.QC<-ntwrks %in% ntwrks.cntrl
    if(any(ntwrks.QC)==FALSE){
        stop("Incorrect ntwrk name(s). Please use the controlled terms: 'SCAN','SNTL','SNTLT','SNOW','MPRC','OTHER','COOP','USGS','MSNT','BOR','CLMIND', or you may use 'ALL' to pull metadata for all NRCS networks")
    }
    if(ntwrks[1]=="ALL"){
        #grab all networks from controlled list:
        ntwrks<-ntwrks.cntrl[1:(length(ntwrks.cntrl)-1)]
    }
    #ntwrks<-"SCAN"
    ntwrks.Lcase<-tolower(ntwrks)
    #create dynamic url link using "ntwrks" vector to access the data we want:
    urls<-lapply(ntwrks.Lcase,function(x) paste0("https://wcc.sc.egov.usda.gov/nwcc/yearcount?network=",x,"&counttype=listwithdiscontinued&state="))
    #grab metadata from each of the sites:
    stationMetadata<-lapply(urls, function(x) xml2::read_html(x))
    #define %>% globally or it won't work:
    `%>%` <-magrittr::`%>%`
    #grab headers:
    headers.NRCS<-lapply(stationMetadata, function(x) x %>% rvest::html_nodes("th"))
    #clean the headers of HTML code:
    metaHeaders<-lapply(headers.NRCS, function(x) gsub("<.*?>", "", x))
    #grab the actual metadata from the urls:
    metadata.NRCS<-lapply(stationMetadata, function(x) x %>% rvest::html_nodes("td"))
    #find the first entry of metadata using the NRCS station type
    #searchNtwks<-paste0("\\<td>")
    searchNtwks<-paste(paste0("\\<td>",ntwrks),collapse="|")
    findLineStart<-lapply(metadata.NRCS, function(x)  grep(searchNtwks,x))
    #clean the metadata of HTML tags:
    cleanMeta<-lapply(metadata.NRCS,function(x) gsub("<.*?>", "", x))
    #loop thru and finish the cleanup and conversion to DF:
    NRCS.metadata<-list()
    for(i in 1:length(cleanMeta)){
        #subset to only keep metadata from table:
        startIndex<-findLineStart[[i]][1]
        endIndex<-findLineStart[[i]][length(findLineStart[[i]])]+length(metaHeaders[[i]])-1
        cleanMeta.sub<-cleanMeta[[i]][startIndex:endIndex]
        #split data into equal chunks
        metadata.NRCS.clean<-split(cleanMeta.sub, ceiling(seq_along(cleanMeta.sub)/length(metaHeaders[[i]])))
        #put data into df:
        NRCS.metadata[[i]]<-data.frame(do.call(rbind,metadata.NRCS.clean))
        #add headers as names:
        names(NRCS.metadata[[i]])<-metaHeaders[[i]]
        #remove excess white space within the ntwrks column:
        NRCS.metadata[[i]]$ntwk<-trimws(NRCS.metadata[[i]]$ntwk,"both")
        #partition the site name and site ID:
        NRCS.metadata[[i]]$site_id<-paste0(NRCS.metadata[[i]]$ntwk,":",gsub(".*\\((.*)\\).*", "\\1", NRCS.metadata[[i]]$site_name))
        #add the ntwrk name to the siteID for more accurate reference:
        #NRCS.metadata[[i]]$site_id
        #remove the site id from the "site.name" column
        NRCS.metadata[[i]]$site_name<-gsub("\\s*\\([^\\)]+\\)","",as.character(NRCS.metadata[[i]]$site_name))
        #convert feet to meters:
        if(cnvrt.elev==TRUE){
            #convert elevation column
            NRCS.metadata[[i]]$elev<-as.numeric(levels(unlist(NRCS.metadata[[i]]$elev)))[unlist(NRCS.metadata[[i]]$elev)]*0.3048
            #rename to elev_m
            names(NRCS.metadata[[i]])[names(NRCS.metadata[[i]]) == 'elev'] <- 'elev_m'
        }
        else{
            #change the 'elev' column name to 'elev_ft' to make avoid ambiguity
            names(NRCS.metadata[[i]])[names(NRCS.metadata[[i]]) == 'elev'] <- 'elev_ft'
        }

    }
    #set the names of the dataframes to the corresponding network name:
    names(NRCS.metadata)<-ntwrks
    return(NRCS.metadata)#output the metadata to user

}#end Function
