# Example API calls

# Current SWE and Snow Depth for the state of Colorado
https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultipleStationReport/daily/state=%22CO%22%20AND%20network=%22SNTLT%22,%22SNTL%22%20AND%20element=%22SNWD%22%20AND%20outServiceDate=%222100-01-01%22|name/0,0/name,stationId,WTEQ::value,WTEQ::delta,SNWD::value,SNWD::delta
#                                                           ^Report type          ^Timescale     ^state                   ^Networks                              ^Required site elements           ^code for 'today' (2100-01-01)       ^Returned columns (everything after the '/')
# Hourly data for a site in Alaska over the past 23 days:
https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/hourly/1062:AK:SNTL/-23,0/WTEQ::value,SNWD::value,PREC::value,TOBS::value,TMAX::value,TMIN::value,TAVG::value
#                                                                               ^Timescale  ^siteID  ^("from 23 days ago to now")     ^Returned columns (everything after the '/')

# SNOTEL report for POR
https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/start_of_period/816:WY:SNTL|id=%22%22|name~0/POR_BEGIN,POR_END/WTEQ::value,WTEQ::delta,SNWD::value,SNWD::delta
