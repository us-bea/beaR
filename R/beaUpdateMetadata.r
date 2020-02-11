#' Download BEA metadata into library/data folder if needed
#' 
#' @param datasetList list of BEA datasets to update local metadata file for (e.g., list('NIPA', 'FixedAssets'))
#' @param beaKey Your API key
#' @keywords metadata search
#' @return Nothing. This updates local .RData files to be used in beaSearch.
#' @import httr data.table
#' @importFrom jsonlite fromJSON
#' @export
#' @examples 
#' beaUpdateMetadata(list('RegionalData', 'NIPA'), beaKey = 'yourAPIkey')


beaUpdateMetadata <- function(datasetList, beaKey){

 'Datasetname'			<- NULL
 'MetaDataUpdated'	<- NULL
 'DatasetName'			<- NULL
 'TableID'					<- NULL
 'Line'							<- NULL
 '.'								<- NULL
 'SeriesCode'				<- NULL
 'RowNumber'				<- NULL
 'LineDescription'	<- NULL
 'LineNumber'				<- NULL
 'ParentLineNumber'	<- NULL
 'Tier'							<- NULL
 'Path'							<- NULL
 'APITable'					<- NULL
 'TableName'				<- NULL
 'ReleaseDate'			<- NULL
 'NextReleaseDate'	<- NULL
 'Parameter'				<- NULL
 'ParamValue'				<- NULL

	#datasetList <- list('nipa','niunderlyingdetail','fixedassets','regionalproduct','regionalincome')
	#update as of 2017-07-12: 'regionaldata' dataset removed from API, merged into regionalproduct and regionalincome
	
	requireNamespace('data.table', quietly = TRUE)
	requireNamespace('httr', quietly = TRUE)
	requireNamespace('jsonlite', quietly = TRUE)

	beaMetadataStore <- paste0(.libPaths()[1], '/beaR/data')

	beaMetaSpecs <- list(
		'UserID' = beaKey ,
		'method' = 'GetData',
		'datasetname' = 'APIDatasetMetaData',
		'dataset' = paste(datasetList, collapse = ','),
		'ResultFormat' = 'json'
	)
	
	#Get as httr response
	beaResponse <- bea.R::beaGet(beaMetaSpecs, asList = FALSE, asTable = FALSE, isMeta = TRUE)	
	
	#Check to ensure it is httr response
	if(class(beaResponse) != 'response'){
		warning('API metadata not returned.  Verify that you are using a valid API key, represented as a character string.')
		return('API metadata not returned.  Verify that you are using a valid API key, represented as a character string.')
	}
	
	lapply(datasetList, function(outdat){
		try(suppressWarnings(file.remove(paste0(beaMetadataStore,'/', outdat, '.RData'))), silent = TRUE)
	})

	
	
	#Get JSON String
	respStr <- httr::content(beaResponse, as = 'text')
	
	#Actually, we can get this same info faster using GetParamValsList or something
	#The line below should be suppressed if fixed - JSON was malformed due to missing commas
	#respStr <- gsub('}{', '},{', respStr, fixed = TRUE)
	metaList <-jsonlite::fromJSON(respStr)
	metasetInfo <- data.table::as.data.table(metaList$BEAAPI$Datasets)
	if(dim(metasetInfo)[1] == 0){
		warning('API metadata not returned.  Verify that you are using a valid API key, represented as a character string.')
		return('API metadata not returned.  Verify that you are using a valid API key, represented as a character string.')
	}

	#bind dataset metadata together
	 #This is a bit of a time drag, so we want to only do it if we need to
	 #And do it separately for each dataset
	if('nipa' %in% tolower(datasetList)){try({
		nipaMDU <- metasetInfo[tolower(Datasetname) == 'nipa', MetaDataUpdated]
		nipaTabs <- data.table::rbindlist(metasetInfo[tolower(Datasetname) == 'nipa', APITable])
		nipaTabs[, DatasetName := 'NIPA']
		#TableIDN has become obsolete; we should no longer overwrite to rename
		#setnames(nipaTabs, old = names(nipaTabs)[grepl('tableidn', tolower(names(nipaTabs)),fixed = T)], new = 'TableID')
		#...however, there does appear to be an issue with capitalization
		setnames(nipaTabs, old = names(nipaTabs)[tolower(names(nipaTabs)) == 'tableid'], new = 'TableID')		
	
		#Backend issue: Sometimes, NIPA table 38 has a NULL table for the line descriptions. Handle and warn the user. 
		handler <- c()
		
		nipaRowList <- lapply(nipaTabs[, TableID], function(thisTab){
			tabPart <- nipaTabs[TableID == thisTab, data.table::as.data.table(Line[[1]])]
			tryCatch({tabPart[, TableID := thisTab]}, error = function(e){handler <<- c(handler, paste0(e, ': NIPA Table ', thisTab))})
			return(tabPart)
		})
		
		nipaRows <- data.table::rbindlist(nipaRowList, use.names = TRUE)
		
		data.table::setkey(nipaTabs, key = TableID)
		data.table::setkey(nipaRows, key = TableID)
		
		nipaIndex <- nipaTabs[nipaRows][,.(
			SeriesCode, 
			RowNumber, 
			LineDescription, 
			LineNumber, 
			ParentLineNumber, 
			Tier, 
			Path, 
			TableID,
			DatasetName,
			TableName, 
			ReleaseDate,
			NextReleaseDate,
			MetaDataUpdated = nipaMDU
		)]
		save(nipaIndex, file=paste0(beaMetadataStore, '/NIPA.RData'))
	})}
	

	if('niunderlyingdetail' %in% tolower(datasetList)){try({
		niudMDU <- metasetInfo[tolower(Datasetname) == 'niunderlyingdetail', MetaDataUpdated]
		niudTabs <- data.table::rbindlist(metasetInfo[tolower(Datasetname) == 'niunderlyingdetail', APITable])
		niudTabs[, DatasetName := 'NIUnderlyingDetail']
		#TableIDN has become obsolete; we should no longer overwrite to rename
		#setnames(niudTabs, old = names(niudTabs)[grepl('tableidn', tolower(names(niudTabs)),fixed = T)], new = 'TableID')
		#...however, there does appear to be an issue with capitalization
		setnames(niudTabs, old = names(niudTabs)[tolower(names(niudTabs)) == 'tableid'], new = 'TableID')		

		niudRows <- data.table::rbindlist(lapply(niudTabs[, TableID], function(thisTab){
			tabPart <- niudTabs[TableID == thisTab, data.table::as.data.table(Line[[1]])]
			tabPart[, TableID := thisTab]
			return(tabPart)
		}))
	 
		data.table::setkey(niudTabs, key = TableID)
		data.table::setkey(niudRows, key = TableID)
		
		niudIndex <- niudTabs[niudRows][,.(
			SeriesCode, 
			RowNumber, 
			LineDescription, 
			LineNumber, 
			ParentLineNumber, 
			Tier, 
			Path, 
			TableID,
			DatasetName,
			TableName, 
			ReleaseDate,
			NextReleaseDate, 
			MetaDataUpdated = niudMDU
		)]
	
		save(niudIndex, file=paste0(beaMetadataStore, '/NIUnderlyingDetail.RData'))
	})}
	
	
	if('fixedassets' %in% tolower(datasetList)){try({
		fixaMDU <- metasetInfo[tolower(Datasetname) == 'fixedassets', MetaDataUpdated]
		fixaTabs <- data.table::rbindlist(metasetInfo[tolower(Datasetname) == 'fixedassets', APITable])
		fixaTabs[, DatasetName := 'FixedAssets']
		#No TableIDN here
		#setnames(fixaTabs, old = names(fixaTabs)[grepl('tableidn', tolower(names(fixaTabs)),fixed = T)], new = 'TableID')
		#...however, there does appear to be an issue with capitalization
		setnames(fixaTabs, old = names(fixaTabs)[tolower(names(fixaTabs)) == 'tableid'], new = 'TableID')		
		
		fixaRows <- data.table::rbindlist(lapply(fixaTabs[, TableID], function(thisTab){
			tabPart <- fixaTabs[TableID == thisTab, data.table::as.data.table(Line[[1]])]
			tabPart[, TableID := thisTab]
			return(tabPart)
		}))
	
		data.table::setkey(fixaTabs, key = TableID)
		data.table::setkey(fixaRows, key = TableID)
		
		fixaIndex <- fixaTabs[fixaRows][,.(
			SeriesCode, 
			RowNumber, 
			LineDescription, 
			LineNumber, 
			ParentLineNumber, 
			Tier, 
			Path, 
			TableID,
			DatasetName,
			TableName, 
			ReleaseDate,
			NextReleaseDate,
			MetaDataUpdated = fixaMDU
		)]
		
		save(fixaIndex, file=paste0(beaMetadataStore, '/FixedAssets.RData'))
	})}


	#Regional data: Treated differently from National data 

	#Set "RegionalData"
	if('regionaldata' %in% tolower(datasetList)){
		message('The RegionalData dataset has been removed from the API; please use RegionalIncome and RegionalProduct instead.');
		return('The RegionalData dataset has been removed from the API; please use RegionalIncome and RegionalProduct instead.');
#	try({
#	
#		rdatMDU <- metasetInfo[tolower(Datasetname) == 'regionaldata', MetaDataUpdated]
#		rdatParam <- metaList$BEAAPI$Datasets$Parameter[[grep('regionaldata', tolower(metaList$BEAAPI$Datasets$Datasetname), fixed=T)]]
#		#rbindlist(rdatParam[[1]])[ParamValue != 'NULL']
#		rdatKeys <- as.data.table(rdatParam$Keycode$ParamValue[[1]])
#		rdatKeys[, Parameter := 'Keycode']
#		rdatFips <- as.data.table(rdatParam$GeoFIPS$ParamValue[[2]])
#		rdatFips[, Parameter := 'GeoFIPS']
#	
#		rdatIndex <- rbindlist(list(rdatKeys, rdatFips), use.names = TRUE)
#		rdatIndex[, DatasetName := 'RegionalData']
#		rdatIndex[, MetaDataUpdated := rdatMDU]
#	
#		save(rdatIndex, file=paste0(beaMetadataStore, '/RegionalData.RData'))
#	})
	}
	
	#Dataset "RegionalProduct"
	if('regionalproduct' %in% tolower(datasetList)){try({
		rprdMDU <- metasetInfo[tolower(Datasetname) == 'regionalproduct', MetaDataUpdated]
		rprdParams <- metaList$BEAAPI$Datasets$Parameters[[grep('regionalproduct', tolower(metaList$BEAAPI$Datasets$Datasetname), fixed=T)]]
		rprdParNms <- attributes(rprdParams)$names
		
		rprdPages <- data.table::rbindlist(rprdParams)[ParamValue != 'NULL', ParamValue]

		rprdIndex <- data.table::rbindlist(lapply(1:length(rprdPages), function(x){
			rprdDT <- data.table::as.data.table(rprdPages[[x]])
			rprdDT[, Parameter := rprdParNms[x]]
			return(rprdDT)
		}))

		rprdIndex[, DatasetName := 'RegionalProduct']
		rprdIndex[, MetaDataUpdated := rprdMDU]
		
		save(rprdIndex, file=paste0(beaMetadataStore, '/RegionalProduct.RData'))
	}, silent = TRUE)}	
	
	#Dataset "RegionalIncome"
	if('regionalincome' %in% tolower(datasetList)){try({
		rincMDU <- metasetInfo[tolower(Datasetname) == 'regionalincome', MetaDataUpdated]
		rincParams <- metaList$BEAAPI$Datasets$Parameters[[grep('regionalincome', tolower(metaList$BEAAPI$Datasets$Datasetname), fixed=T)]]
		rincParNms <- attributes(rincParams)$names
		
		rincPages <- data.table::rbindlist(rincParams)[ParamValue != 'NULL', ParamValue]
	
		rincIndex <- data.table::rbindlist(lapply(1:length(rincPages), function(x){
			rincDT <- data.table::as.data.table(rincPages[[x]])
			rincDT[, Parameter := rincParNms[x]]
			return(rincDT)
		}))
		
		rincIndex[, DatasetName := 'RegionalIncome']
		rincIndex[, MetaDataUpdated := rincMDU]
	
	save(rincIndex, file=paste0(beaMetadataStore, '/RegionalIncome.RData'))
	}, silent = TRUE)}	
	
#	if(length(datasetList) > length(metasetInfo[, Datasetname])){
#		staleList <- datasetList[
#			!(tolower(datasetList) %in% tolower(metasetInfo[, Datasetname]))
#		]
#		message('beaR attempted to update metadata for the following dataset(s) which could not be returned from the API: ')
#		message(paste(
#			toupper(staleList), 
#			collapse = ', '
#		))
#		message('Removing stale data from local storage...')
##		return(staleList)
#	}# else {return(list())}
	

}
