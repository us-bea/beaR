#' Gives list of values possible for a given dataset's parameters 
#' 
#' @param beaKey Your API key
#' @param setName Name of BEA dataset (e.g., NIPA)
#' @param paramName Name of BEA dataset parameter (e.g., TableID)
#' @return A metadata object of class 'list' of several dimensions. View list structure using 'str(yourList)'.
#' @keywords metadata
#' @export
#' @examples 
#' beaParamVals('yourAPIkey', 'RegionalData', 'keycode')

beaParamVals <- function(beaKey, setName, paramName) {
	beaMetaSpecs <- list(
		'method' = 'GetParameterValues',
		'UserID' = beaKey,
		'datasetname'=setName,
		'ParameterName'=paramName,
		'ResultFormat' = 'json'
	)
	beaResponse <- bea.R::beaGet(beaMetaSpecs, asList = TRUE, asTable = FALSE, isMeta = TRUE)	

	return(beaResponse)
}