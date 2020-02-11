#' Gives list of parameters possible for a given dataset 
#' 
#' @param beaKey Your API key
#' @param setName Name of BEA dataset (e.g., 'NIPA')
#' @keywords metadata
#' @return A metadata object of class 'list' of several dimensions. View list structure using 'str(yourList)'.
#' @export
#' @examples 
#' beaParams('yourAPIkey', 'RegionalData')

beaParams <- function(beaKey, setName) {
	beaMetaSpecs <- list(
		'UserID' = beaKey ,
		'method' = 'GetParameterList',
		'datasetname'=setName,
		'ResultFormat' = 'json'
	)

	beaResponse <- bea.R::beaGet(beaMetaSpecs, asList = TRUE, asTable = FALSE, isMeta = TRUE)	

	return(beaResponse)
}
