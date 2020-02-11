#' Returns a list of all datasets
#' 
#' @param beaKey Your API key
#' @keywords metadata
#' @return A metadata object of class 'list' of several dimensions. View list structure using 'str(yourList)'.
#' @export
#' @examples 
#' beaSets('yourAPIkey')

beaSets <- function(beaKey) {
	#Set up spec for it 
	beaMetaSpecs <- list(
		'method' = 'GetDataSetList',
		'UserID' = beaKey ,
		'ResultFormat' = 'json'
	)

	#Set list using beaGet 
	beaResponse <- bea.R::beaGet(beaMetaSpecs, asList = TRUE, asTable = FALSE, isMeta = TRUE)	
	
	return(beaResponse)
}