#' Search a selection of indexed BEA data table names, series labels, and series codes.
#' 
#' @param searchTerm A word or phrase of class 'character' to be found in BEA datasets
#' @param beaKey Character string representation of user API key. Necessary for first time use and updates; recommended for anything beyond one-off searches from the console.
#' @param asHtml Option to return results as DT markup, viewable in browser.  Allows search WITHIN YOUR ALREADY-FILTERED RESULTS ONLY. Requires package 'DT' to be installed.
#' @keywords search
#' @description Searches indexed dataset table name, label, and series codes.  CAUTION: Currently only works with NATIONAL datasets (NIPA, NIUnderlyingDetail, FixedAssets), temporarily excluding FixedAssets, and REGIONAL datasets (RegionalProduct, RegionalIncome)
#' @return An object of class 'data.table' with information about all indexed sets in which the search term was found.
#' @import data.table 
#' @importFrom DT datatable
#' @export
#' @examples 
#' beaSearch('gross domestic product', asHtml = TRUE)

 beaSearch <- function(searchTerm, beaKey = NULL, asHtml = FALSE){ 
# beaSearch <- function(searchTerm, searchFilter = list(), justParents = FALSE, justChildren = FALSE){ 
	if (is.null(beaKey)){warning('Searching without specifying beaKey, e.g., 
	   beaSearch("tobacco", beaKey = "[your 36-character API key]") 
is not recommended, as the key is needed to update locally stored metadata.')}
#beaSearch throws spurious NOTEs on check() without this due to data.table Depends
 'LineDescription' <- NULL
 'SeriesCode'      <- NULL
 'Key'             <- NULL
 'LineNumber'      <- NULL
 'Tier'            <- NULL
 'ParentLine'      <- NULL
 'Desc'     			 <- NULL
 'DatasetName'     <- NULL
 'Dataset'		     <- NULL
 'TableID'         <- NULL
 'TableName'       <- NULL
 'Parameter'       <- NULL
 'APImtime'        <- NULL
 'mtime' 	      	 <- NULL
 'Account'         <- NULL
 '.'               <- NULL
 'apiCall'         <- NULL
 'nipaIndex'       <- NULL
 'niudIndex'       <- NULL
 'fixaIndex'       <- NULL
# 'rdatIndex'       <- NULL
 'rprdIndex'       <- NULL
 'rincIndex'       <- NULL
 'JSONUpdateDate'  <- NULL
 'XMLUpdateDate'   <- NULL

	requireNamespace('data.table', quietly = TRUE)
	beaMetadataStore <- paste0(.libPaths()[1], '/beaR/data')
	
	beaMetaFiles <- list.files(path = beaMetadataStore, full.names = TRUE);
	beaMetaFilesTimes <- file.info(beaMetaFiles, extra_cols = TRUE)
	beaMetaFilesTimes$Dataset <- gsub(
		paste0(beaMetadataStore, '/'), 
		'', 
		attributes(beaMetaFilesTimes)$row.names, 
		fixed=T
	)
	beaMetaMtime <- data.table::as.data.table(beaMetaFilesTimes)[, 
		.(
			Dataset = gsub('.RData', '', Dataset, fixed=T), 
			mtime
		)
	]
	data.table::setkey(beaMetaMtime, key = Dataset)
	
	#Add FixedAssets in future, but regionaldata has been merged into regionalproduct and regionalincome on the API
	beaKnownMetaSets <- list(
		'nipa',
		'niunderlyingdetail',
		'fixedassets',
#		'regionaldata',
		'regionalproduct',
		'regionalincome'
	)

	if ((length(beaMetaFiles) == 0) & is.null(beaKey)){
		warning(paste0('No API key provided and no local metadata storage detected in ', beaMetadataStore, '. 
		Please provide a valid key to use beaSearch.'))
		return(paste0('No API key provided and no local metadata storage detected in ', beaMetadataStore, '. Please provide a valid key to use beaSearch.'))
	}
#Check to see if this is the first time using the search function; if so, update all metadata currently handled.
	if (length(beaMetaFiles) < 5){
	#Create directory and make single call to get all metadata if there are missing meta .RData files
		message('Creating first-time local copy of metadata for all datasets - only done once.')
		message('Datasets will be updated only if timestamps indicate metadata obsolete in future searches,')
		message("and only obsolete metadata sets will be updated (it's faster this way).")
		message("")
		dir.create(beaMetadataStore, showWarnings = FALSE, recursive = TRUE)
		
		#call function to update metadata - remember to specify beaR namespace
		beaUpdateMetadata(beaKnownMetaSets,	beaKey)
		
	} else {
	 if (!is.null(beaKey)){
		#Make a "GetParameterValues" call to get timestamps of latest metadata update
		beaMetaTimeSpec <- list(
			'UserID' = beaKey ,
			'method' = 'GetParameterValues',
			'datasetname' = 'APIDatasetMetaData',
			'parametername' = 'dataset',
			'ResultFormat' = 'json'
		)
		#Get metadata response with timestamps we need to check for updates as list
		beaMetaParams <- bea.R::beaGet(beaMetaTimeSpec, asList = TRUE, asTable = FALSE, isMeta = TRUE)	
		
		beaMetaInfo <- data.table::as.data.table(beaMetaParams$ParamValue)
		
		data.table::setkey(beaMetaInfo, key = Dataset)
		
		tryCatch({
		#If JSON has been updated, set check param = false
		
			timeCompare <- beaMetaMtime[beaMetaInfo][, .(
				Dataset, 
				mtime, 
				APImtime = as.POSIXct(
					JSONUpdateDate, 
					format = "%Y-%m-%dT%H:%M:%S"
				)
			)][!is.na(APImtime)]

			outdatedLocalMeta <- timeCompare[
				(is.na(mtime) & !is.na(APImtime)) | 
				APImtime > mtime,
				Dataset
				]
			
			beaMetaFirstToCache <- FALSE 
			if(length(timeCompare[is.na(APImtime) & Dataset %in% beaKnownMetaSets, Dataset]) > 0){
				beaMetaFirstToCache <- TRUE
			}
		},
		error = function(e){
			beaMetaFirstToCache <- TRUE
			beaUpdateMetadata(beaKnownMetaSets,	beaKey)
		}, 
		finally = {''})

		if(length(outdatedLocalMeta[!tolower(outdatedLocalMeta) %in% beaKnownMetaSets]) > 0){
			warning('BEA API contains newly-available metadata for datasets not handled.
			This version of beaR is either not the latest, or will soon be replaced.')
			outdatedLocalMeta <- outdatedLocalMeta[tolower(outdatedLocalMeta) %in% beaKnownMetaSets]
		}

		if(beaMetaFirstToCache){
			beaUpdateMetadata(beaKnownMetaSets,	beaKey)
		} else {
			if(length(outdatedLocalMeta) > 0){
				beaUpdateMetadata(as.list(tolower(outdatedLocalMeta)),	beaKey)
			}
		}
	 }
	}

	beaMetaFiles <- list.files(path = beaMetadataStore, full.names = TRUE);

	missingNat <- FALSE;
	missingReg <- FALSE;
	
#Remove RegionalData, but add FixedAssets later
	if(
		length(grep('FixedAssets', beaMetaFiles, fixed = TRUE)) == 0 | 
		length(grep('NIPA', beaMetaFiles, fixed = TRUE)) == 0 | 
		length(grep('NIUnderlyingDetail', beaMetaFiles, fixed = TRUE)) == 0  
	){
		warning(paste0('National metadata is missing from ',beaMetadataStore,' and may be locked for updating on the BEA API; searching regional metadata only.'))
		missingNat <- TRUE;
	}
	
	
	if(
#		length(grep('RegionalData', beaMetaFiles, fixed = TRUE)) == 0 | 
		length(grep('RegionalProduct', beaMetaFiles, fixed = TRUE)) == 0 | 
		length(grep('RegionalIncome', beaMetaFiles, fixed = TRUE)) == 0 
	){
			warning(paste0('Regional metadata is missing from ',beaMetadataStore,' and may be locked for updating on the BEA API; searching national metadata only.'))
			missingReg <- TRUE;
#			return(paste0('Metadata is missing from ',beaMetadataStore,' and may be locked for updating on the BEA API; please try beaSearch again later.'))
	} 
	
	if(missingNat && missingReg){
		message(paste0('Metadata is missing from ',beaMetadataStore,' and may be locked for updating on the BEA API; please try beaSearch again later.'))	
		return(paste0('Metadata is missing from ',beaMetadataStore,' and may be locked for updating on the BEA API; please try beaSearch again later.'))	
	} else {
#Remove RegionalData permanently, but add FixedAssets later
	try({
		if(!missingNat){
			load(paste0(beaMetadataStore, '/FixedAssets.RData'))
			load(paste0(beaMetadataStore, '/NIPA.RData'))
			load(paste0(beaMetadataStore, '/NIUnderlyingDetail.RData'))
			#Remove RegionalData, add FixedAssets later (fixaIndex)
			nationalIndex <- rbindlist(list(nipaIndex, niudIndex, fixaIndex), use.names = TRUE, fill=F)
			nationalIndex[, Account := 'National']
			data.table::setkey(nationalIndex, key = DatasetName, TableID, LineNumber)

		#Search national economic accounts for term 
			nPerfectMatch <- nationalIndex[
				grep(
					tolower(searchTerm), 
					tolower(
						paste(
							LineDescription, 
							TableName, 
							SeriesCode, 
							DatasetName
						)
					), fixed=TRUE
				)
			]
		
		#	nPerfectMatch[ ,
		#		Parameter := NA
		#	]
		#	nPerfectMatch[ ,
		#		Key := NA
		#	]
			
			nPerfectMatch[,
				apiCall := 
					paste0(
						"beaGet(list('UserID' = '[your_key]', 'Method' = 'GetData', 'DatasetName' = '",
						DatasetName, 
						"', 'TableName' = '", 
						TableID, 
						"', ...))"
					)
			]
			
		
			nReasonableMatch <- nationalIndex[
				grep(
					searchTerm, 
					paste(
						LineDescription, 
						TableName, 
						SeriesCode, 
						DatasetName
					), ignore.case=TRUE
				)
			]
		
		#	nReasonableMatch[ ,
		#		Parameter := NA
		#	]
		#	nReasonableMatch[ ,
		#		Key := NA
		#	]
			
			nReasonableMatch[,
				apiCall := 
					paste0(
						"beaGet(list('UserID' = '[your_key]', 'Method' = 'GetData', 'DatasetName' = '",
						DatasetName, 
						"', 'TableName' = '", 
						TableID, 
						"', ...))"
					)
			]
			
		#FixedAssets is different from NIPA and NIUnderlyingDetail; handler here
		nPerfectMatch[tolower(DatasetName) == 'fixedassets', apiCall := gsub("', 'TableName' = '", "', 'TableID' = '", apiCall, fixed = T)]
		nReasonableMatch[tolower(DatasetName) == 'fixedassets', apiCall := gsub("', 'TableName' = '", "', 'TableID' = '", apiCall, fixed = T)]
			
			
			
		}
		
		if(!missingReg){
			load(paste0(beaMetadataStore, '/RegionalProduct.RData'))
			load(paste0(beaMetadataStore, '/RegionalIncome.RData'))
			#		load(paste0(beaMetadataStore, '/RegionalData.RData'))

			#Removed rdatIndex, which was used for RegionalData
			regionalIndex <- rbindlist(list(rprdIndex, rincIndex), use.names = TRUE, fill=F)
			try(regionalIndex[, Account := 'Regional'])
			data.table::setkey(regionalIndex, key = DatasetName, Parameter, Key)
		
		
			#Search regional accounts for the term
			rPerfectMatch <- regionalIndex[
				grep(
					tolower(searchTerm), 
					tolower(
						paste(
							Desc, 
							Key, 
							DatasetName
						)
					), fixed=TRUE
				)
			]
		
		#	rPerfectMatch[ ,
		#		TableID := NA
		#	]
		#	rPerfectMatch[ ,
		#		LineNumber := NA
		#	]
		#	rPerfectMatch[ ,
		#		SeriesCode := NA
		#	]
		#	rPerfectMatch[ ,
		#		LineDescription := NA
		#	]
		#	rPerfectMatch[ ,
		#		tier := NA
		#	]
		#	rPerfectMatch[ ,
		#		rootTabLine := NA
		#	]
			
				
			rPerfectMatch[,
				apiCall := 
					paste0(
						"beaGet(list('UserID' = '[your_key]', 'Method' = 'GetData', 'DatasetName' = '",
						DatasetName, 
						"', '", 
						Parameter, 
						"' = '", 
						Key, 
						"', ...))"
					)
			]
			
		
		
			rReasonableMatch <- regionalIndex[
				grep(
					searchTerm, 
					paste(
						Desc, 
						Key, 
						DatasetName
					), ignore.case=TRUE
				)
			]
		
		#	rReasonableMatch[ ,
		#		TableID := NA
		#	]
		#	rReasonableMatch[ ,
		#		LineNumber := NA
		#	]
		#	rReasonableMatch[ ,
		#		SeriesCode := NA
		#	]
		#	rReasonableMatch[ ,
		#		LineDescription := NA
		#	]
		#	rReasonableMatch[ ,
		#		tier := NA
		#	]
		#	rReasonableMatch[ ,
		#		rootTabLine := NA
		#	]
		
			rReasonableMatch[,
				apiCall := 
					paste0(
						"beaGet(list('UserID' = '[your_key]', 'Method' = 'GetData', 'DatasetName' = '",
						DatasetName, 
						"', '", 
						Parameter, 
						"' = '", 
						Key, 
						"', ...))"
					)
			]
			}

	#TODO: figure out how to sort list by var name s.t. it concatenates lazily instead of this if-then stuff
		if(!(missingNat) && !(missingReg)){
			searchMatch <- unique(
				rbindlist(
					list(
	#					nPerfectMatch[, .(apiCall, datasetName, TableID, Description, paramType, Key, LineNumber, SeriesCode, LineDescription, rootTabLine)], 
	#					rPerfectMatch[, .(apiCall, datasetName, TableID, Description, paramType, Key, LineNumber, SeriesCode, LineDescription, rootTabLine)], 
	#					nReasonableMatch[, .(apiCall, datasetName, TableID, Description, paramType, Key, LineNumber, SeriesCode, LineDescription, rootTabLine)],  
	#					rReasonableMatch[, .(apiCall, datasetName, TableID, Description, paramType, Key, LineNumber, SeriesCode, LineDescription, rootTabLine)]
						nPerfectMatch,
						rPerfectMatch,
						nReasonableMatch,
						rReasonableMatch
					),
					use.names = TRUE,
					fill = TRUE
				)
			)
		}
		
		if(missingNat && !(missingReg)){
			searchMatch <- unique(
				rbindlist(
					list(
						rPerfectMatch,
						rReasonableMatch
					),
					use.names = TRUE,
					fill = TRUE
				)
			)
		}
		
		if(!(missingNat) && missingReg){
			searchMatch <- unique(
				rbindlist(
					list(
						nPerfectMatch,
						nReasonableMatch
					),
					use.names = TRUE,
					fill = TRUE
				)
			)
		}
		
		if(requireNamespace('DT', quietly = TRUE) && asHtml == TRUE){
			requireNamespace('DT', quietly = TRUE)
			searchMatch <- DT::datatable(searchMatch)
		}
		else{
			if (asHtml == TRUE){
				message('Note: Returning as data.table.  You must have package DT installed to return browser-viewable table.')
			}
		}
		return(searchMatch)
	})
 }
}
 