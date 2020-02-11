#' Pass list of user specifications (including API key) to return data from BEA API.
#' 
#' @param beaSpec 	A list of user specifications (required). In this example, 'GetData' specifies that we want data values (rather than metadata), 'NIPA' specifies the dataset, 'A' specifies that we want annual data, 'TableID' = '68' gets a specific table, and 'X' gets all years. See BEA API documentation or use metadata methods for complete lists of parameters.
#' @param asString Return result body as a string (default: FALSE) 
#' @param asList 	 Return result body as a list  (default: FALSE)
#' @param asTable  Return result body as a data.table (default: TRUE)
#' @param asWide 	 Return data.table in wide format (default: TRUE)
#' @param iTableStyle If "asWide = TRUE", setting "iTableStyle = TRUE" will return data.table in same format as shown on BEA website, with dates and attributes as column headers and series as rows; otherwise, results have series codes as column headers (default: TRUE)
#' @param isMeta 	 Special parameter meant to interact with metadata functions (default: FALSE)
#' @return By default, an object of class 'list' of several dimensions. View list structure using 'str(yourList)'.
#' @import httr
#' @export 
#' @examples 
#' userSpecList <- list('UserID' = 'yourAPIKey' ,
#'									'Method' = 'GetData',
#'									'datasetname' = 'NIPA',
#'									'Frequency' = 'A',
#'									'TableID' = '68',
#'									'Year' = 'X')	
#' BDT <- beaGet(userSpecList, asTable = TRUE)

beaGet <- function(beaSpec, asString=FALSE, asList=FALSE, asTable=TRUE, asWide=TRUE, isMeta=FALSE, iTableStyle=TRUE) { 
#, asTS=FALSE
	if(class(beaSpec) != 'list'){
		warning('Please specify API parameters as a list. For example:
			beaGet(list("UserID" = "YourKey", "Method" = "GetData", [your remaining parameters]))')
		return(paste0('Invalid object class passed to beaGet([list of API parameters]): ', class(beaSpec), '. Should be of class "list"'))
	}
	
	
	
	requireNamespace('httr', quietly = TRUE)
	attributes(beaSpec)$names <- tolower(attributes(beaSpec)$names)

	if(class(beaSpec$userid) != 'character'){
		warning(paste0('Invalid API key of class ', class(beaSpec$userid)))
		return(paste0('Invalid API key of class ', class(beaSpec$userid)))
	}

	beaSpec$userid <- gsub(' ', '', beaSpec$userid, fixed=T)
	
	if(nchar(beaSpec$userid) != 36){
		warning(paste0('Invalid API key: ', beaSpec$userid))
		return(paste0('Invalid API key: ', beaSpec$userid))
	}

	#Parse user settings into API URL
	beaUrl <- utils::URLencode(
		paste0(
			'https://apps.bea.gov/api/data?UserID=', 
			beaSpec$userid, 
			'&', 
			paste(
				paste(
					attributes(beaSpec)$names[!grepl('userid', attributes(beaSpec)$names)], 
					beaSpec[!grepl('userid', attributes(beaSpec)$names)], 
					sep = '='
				), 
				collapse = '&'
			), '&beaR=v1',
		collapse  = NULL)
	)
	
	#If the user just wants to return a list or table (default), use JSON 
	#Drop this later if we solve XML problem in section 1 
	if(asTable||asList) {
	#||asTS
			beaUrl <- gsub(
				"resultformat=xml", 
				"ResultFormat=json", 
				beaUrl, 
				ignore.case=TRUE
			)
	}
	#Use httr GET to make the API call 
	beaPayload <- httr::GET(beaUrl)

#Give user format they want 
	#if (asTS) {
	#	beaResults <- bea2TS(beaPayload)
	#	return(beaResults)		
	#	}
	# else {
		if (asTable) {
			userWide <- asWide
			userTabStyle <- iTableStyle
			beaResults <- bea.R::bea2Tab(beaPayload, asWide = userWide, iTableStyle = userTabStyle)
			return(beaResults)
		}
		else {
			if(asList) {
				metaMethod <- isMeta
				beaResponse <- bea.R::bea2List(beaPayload, isMeta = metaMethod)
				return(beaResponse)
				}
			else {
				if(asString) {
					beaContent <- httr::content(beaPayload, as = 'text')
					return(beaContent) 
				}
				else { return(beaPayload) }
			}
		}
	 #}
}
