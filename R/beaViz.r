#' Visualize BEA API response payload
#' 
#' @param beaPayload An httr response from call to BEA API
#' @param beaKey Your 36-digit BEA API key
#' @description When entered into the R console, the function below starts an interactive dashboard. CAUTION: Currently only works with NATIONAL datasets (NIPA, NIUnderlyingDetail, FixedAs-sets). R Studio users must opt to "show in browser" for this method to be fully functional.
#' @import data.table googleVis shiny shinydashboard ggplot2 stringr
#' @export
#' @examples 
#' userSpecList <- list('UserID' = 'yourKey' ,
#'									'Method' = 'GetData',
#'									'datasetname' = 'NIPA',
#'									'Frequency' = 'A',
#'									'TableID' = '68',
#'									'Year' = 'X')		
#' resp <- beaGet(userSpecList)
#' BDF <- beaViz(resp)

beaViz <- function(beaPayload = NULL, beaKey = NULL) {
	if(!requireNamespace('googleVis', quietly = TRUE)){
		stop(
			'Package googleVis needed to use beaViz.', 
			call. = FALSE
		)
	}
 
	if(!requireNamespace('shinydashboard', quietly = TRUE)){
		stop(
			'Package shinydashboard needed to use beaViz.', 
			call. = FALSE
		)
	}

	if(!requireNamespace('shiny', quietly = TRUE)){
		stop(
			'Package shiny needed to use beaViz.', 
			call. = FALSE
		)
	}

	requireNamespace('data.table', quietly = TRUE)
	requireNamespace('googleVis', quietly = TRUE)
	requireNamespace('ggplot2', quietly = TRUE)
	requireNamespace('shiny', quietly = TRUE)
	requireNamespace('shinydashboard', quietly = TRUE)
	
	
	#For some reason,  ifelse() does not work on bea2Tab([data.table class of beaPayload]) 
	# and, for now, we must transform back to LONG format for beaViz.  Change this later.
	if(!is.null(attributes(beaPayload)$is.wide)){
		if(attributes(beaPayload)$is.wide){
			beaTab <- bea.R::bea2Tab(beaPayload, asWide = FALSE)
		} else {
			beaTab <- beaPayload
		}
	} else {

		beaTab <- ifelse(
			(
				'response' %in% class(beaPayload)  ||
				'list' %in% class(beaPayload)
			),
			bea.R::bea2Tab(beaPayload, asWide = FALSE),
			beaPayload
			)
	}
	
	beaRespChk <- ifelse(
		!is.null(attributes(beaTab)$params), 
		TRUE, 
		FALSE
	)
	
	if(beaRespChk) {
#so uglyyyyyy
 'LineNumber'        <- NULL
 '.'                 <- NULL
 'TimePeriod'        <- NULL
 'DataValue'         <- NULL
 'LineDescription'   <- NULL
 'SeriesCode'        <- NULL
 'TableID'           <- NULL
 'DatasetName'       <- NULL
 'i.LineDescription' <- NULL
 'i.LineNumber'      <- NULL
 'TimePeriod'        <- NULL
 'LineNumber'        <- NULL
  'LineDescription'  <- NULL
  '.'                <- NULL
 'node'              <- NULL
 'root'              <- NULL
 'DataValue'         <- NULL
 'lnNo'              <- NULL
 'size'              <- NULL
 'lag1'              <- NULL
 'lag2'              <- NULL
 'hue'               <- NULL
 'pctChgNew'         <- NULL
 'pctChgOld'         <- NULL
 'absz'              <- NULL
 'Tier'              <- NULL
 'Description'			 <- NULL
 'TableName'  			 <- NULL
 'Account'  			 	 <- NULL
 'ParentLineNumber'	 <- NULL

	message('')
	message('Press "ESC" to exit the beaViz function.')
	message('Note: If you are using RStudio, you will need to "Open in Browser" to view graphs/table.')
	message('')
	message('****You can safely ignore the following errors:****')
	#beaTab <- bea.R::bea2Tab(beaList)
	
		#Get info about the dataset and request
		thisDatasetLoc <- grep(
			'DATASETNAME', 
			attributes(beaTab)$params$ParameterName
		)

		thisDataset <- attributes(beaTab)$params$ParameterValue[thisDatasetLoc]

		thisUserIDLoc <- grepl(
			'USERID', 
			attributes(beaTab)$params$ParameterName
		)
		
		
		thisUserID <- attributes(beaTab)$params$ParameterValue[thisUserIDLoc]

	
	beaFreqCheck <- ifelse(
		nchar(
			beaTab[1, TimePeriod]
		) > 
		nchar(
			gsub('M', '', beaTab[1, TimePeriod])
		), 
		'M', 
		ifelse(
			nchar(
				beaTab[1, TimePeriod]
			) > 
			nchar(
				gsub('Q', '', beaTab[1, TimePeriod])
			), 
			'Q', 
			'A'
		)
	)
	
	if(beaFreqCheck == 'A'){ 
			
		dateRange <- unique(
			beaTab[,as.Date(TimePeriod, format = '%Y')] 
		)	
	} else {
		if (beaFreqCheck == 'Q') {
			dateRange <- unique(beaTab[, as.Date(
				paste0(
					substr(TimePeriod, 1, 4), 
					substr(paste0(
						'0',
						3 * as.numeric(
							substr(TimePeriod, 6, 6)
						)), 
						nchar(paste0(
							'0',
							3 * as.numeric(
								substr(TimePeriod, 6, 6)
							))
						)-1,
						nchar(paste0(
						'0',
						3 * as.numeric(
							substr(TimePeriod, 6, 6)
						)))
					),	
					'01'
				),
				format = '%Y%m%d')
			]);
		} else {
			dateRange <- unique(beaTab[, as.Date(paste0(
					gsub('M','',
						stringr::str_extract(	
							pattern = '([:digit:]{4})(M)([:digit:]{2})',
							TimePeriod
						), 
						fixed = TRUE
					), '01'),
					format = '%Y%m%d' 
				)
			]);
		}	
	}
	
	beaBar <- beaTab[
		LineNumber==1, 
		.(
			TimePeriod, 
			DataPoint = as.numeric(gsub(',', '', DataValue, fixed=TRUE))
		)
	][order(rank(TimePeriod))]
	
	topName <- unique(beaTab[LineNumber==1, LineDescription])
	
	attributes(beaBar)$names <- c('Time Period', topName)

		thisTabIDLoc <- grep(
			'TABLEID', 
			attributes(beaTab)$params$ParameterName
		)

		thisTabID <- attributes(beaTab)$params$ParameterValue[thisTabIDLoc]
		
		nationalIndex <- bea.R::beaSearch(' ', beaKey = thisUserID)[Account == 'National']
		data.table::setkey(nationalIndex, key = DatasetName, TableID, LineNumber)

#/IF NATIONAL
	if(!(tolower(paste0(thisDataset, thisTabID)) %in% nationalIndex[, tolower(paste0(DatasetName, TableID))])){
		message('beaViz is not available for this dataset.')
	} else {
		#theseSeries <-  unique(beaTab[,SeriesCode])
		hierTab <- unique(
			nationalIndex[
				(TableID == thisTabID) & 
				(toupper(DatasetName) == toupper(thisDataset)),
				TableName
			]
		)

			thisIndex <- unique(
				nationalIndex[
					(TableID == thisTabID) & 
					(toupper(DatasetName) == toupper(thisDataset)),
				]
			)
			thisIndex[LineNumber == 0, LineDescription := TableName]
			data.table::setkey(thisIndex, key = LineNumber)

			thisRoots <- unique(
				nationalIndex[
					(TableID == thisTabID) & 
#					(ParentLineNumber %in% beaTab[, LineNumber]) &
					(toupper(DatasetName) == toupper(thisDataset)),
				]
			)
			thisRoots[LineNumber == 0, LineDescription := TableName]
			data.table::setkey(thisRoots, key = ParentLineNumber)
			
			hierTree <- thisIndex[thisRoots][, 
				.(
					node = paste0(
						i.LineDescription, 
						' [Line ', 
						i.LineNumber, 
						']'
					), 
					root = ifelse(
						is.na(LineNumber),
						NA,
						paste0(
							LineDescription, 
							' [Line ', 
							LineNumber, 
							']'
						)
					),
					LineNumber = i.LineNumber
				)			
			]
		hierTree[root == node, root := NA]	
		data.table::setkey(hierTree, key=LineNumber)
		
		
		#Get a list of possible datasets
		beaAllSets <- bea.R::beaSets(thisUserID)$Dataset

	#Create list of names for select box
#		setList <- as.list(
#			beaAllSets$DatasetDescription
#		)
		setStarter <- grepl(
			toupper(thisDataset), 
			toupper(beaAllSets$DatasetName)
		)

		setOptions <- as.list(c(
			beaAllSets$DatasetDescription[setStarter], 
			beaAllSets$DatasetDescription[!setStarter])
		)
		
		#We can now move on to create dashboard
	ui <- shinydashboard::dashboardPage(
		shinydashboard::dashboardHeader(title = 'beaViz'),
		shinydashboard::dashboardSidebar(
			shiny::uiOutput('dataset'),
#      tags$head(tags$style("#treemap{height:45vh !important;}")),
#      tags$head(tags$style("#treemap{width:45vw !important;}")),
#      tags$head(tags$style("#topbar{width:45vw !important;}")),
#			shiny::tags$head(shiny::HTML("<script>
#				//create trigger to resizeEnd event     
#				window.getWinWidth = function() {
#//						if(this.resizeTO) clearTimeout(this.resizeTO);
#//						this.resizeTO = setTimeout(function() {
#//								$(this).trigger('resizeEnd');
#//						}, 500);
#					window.gvisWidths = $( window ).width();
#				};
#
#				//redraw graph when window resize is completed  
#				$(window).on('resizeEnd', function() {
#						getWinWidth();
#				});
#				</script>")
#			),
			shiny::uiOutput('apiInp1'),
			shiny::uiOutput('apiInp2'),
			shiny::uiOutput('apiInp3'),
			shiny::uiOutput('apiInp4'),
			shiny::uiOutput('apiInp5'),
			shiny::uiOutput('apiInp6'),
			shiny::uiOutput('apiInp7'),
			shiny::uiOutput('apiInp8'),
			shiny::uiOutput('apiInp9'),
			shiny::uiOutput('seriesbox')
			#Removed with treemap
			#,
			#shiny::uiOutput('slidebar'),
			#shiny::tags$p("Time period:"),
      #shiny::verbatimTextOutput("userPd")

			#Removed prior to removal of treemap
			#,
			#shiny::tags$p("Possible params:"),
      #shiny::verbatimTextOutput("userSetParams"),
			#shiny::tags$p("Dataset:"),
      #shiny::verbatimTextOutput("userSet"),
			#shiny::tags$p("Series:"),
      #shiny::verbatimTextOutput("userSer")			
		),
		shinydashboard::dashboardBody(
			# Boxes need to be put in a row (or column)
			shiny::fluidRow(
				#shinydashboard::box(shiny::htmlOutput("treemap"), height = 300),
				shinydashboard::box(shiny::htmlOutput("topbar"), height = 300), 
				shinydashboard::box(shiny::verbatimTextOutput("dataDetail"), height = 300), 
				height = 300
			),
			shiny::fluidRow(
				shinydashboard::box(
					shiny::downloadButton('downloadData', 'Download'),
					shiny::htmlOutput("vistab"), 
					width = 9
				),
				shinydashboard::box(
					shiny::tags$p("beaGet() call:"),
					shiny::verbatimTextOutput("apiCall"), 
					width = 3
				)
			)
		)
	)

	server <- function(input, output, session) {			
	#			shiny::fluidRow(
	#				box(
	#					title = "Date Control",
		#Removed with treemap
#		output$slidebar <- shiny::renderUI({shiny::sliderInput("timePd", 
#						label = shiny::h5("Select Period:"), 
#						min(dateRange), 
#						max(dateRange), 
#						max(dateRange),
#						step = ifelse(
#							(	nchar(max(beaTab[, TimePeriod])) > 
#								nchar(gsub('M', '', max(beaTab[, TimePeriod]), fixed = TRUE))
#							),
#							30.5, ifelse(
#								(	nchar(max(beaTab[, TimePeriod])) > 
#									nchar(gsub('Q', '', max(beaTab[, TimePeriod]), fixed = TRUE))
#								), 
#								91, 364.25
#							)
#						)	
#						, 
#						timeFormat = ifelse(
#							(	nchar(max(beaTab[, TimePeriod])) > 
#								nchar(gsub('M', '', max(beaTab[, TimePeriod]), fixed = TRUE))
#							),
#							'%Ym%m', ifelse(
#								(	nchar(max(beaTab[, TimePeriod])) > 
#									nchar(gsub('Q', '', max(beaTab[, TimePeriod]), fixed = TRUE))
#								), 
#								'%Ym%m', '%Y'
#							)
#						)	
##					)
##				)
#			)
#		})
	

		output$dataset <- shiny::renderUI({
			shiny::selectInput(
				"userSetname", 
				label = shiny::h5("Select dataset:"), 
				choices = setOptions, 
				selected =	1)
		})
		
		output$userSet <- shiny::renderPrint({
			input$userSetname
		})

		userSelectedSet <- shiny::reactive({
			toupper(
				beaAllSets$DatasetName[
					beaAllSets$DatasetDescription == input$userSetname
				]
			)
		})
		
		#output$userSetParams <- shiny::reactive({
		userSetParams <- shiny::reactive({
			
			theseParams <- bea.R::beaParams(thisUserID, userSelectedSet())$Parameter
			
			#return(str(theseParams))
		return(theseParams)
		})
			

		output$apiInp1 <- shiny::renderUI({

			selectedSet <- userSelectedSet()
			theseParams <- userSetParams()

			thisParamDesc <- theseParams[[3]][1]
			thisParamCode <- theseParams[[1]][1]
			
			allValCheck <- ifelse(is.character(theseParams$AllValue[1]),
				ifelse(
					nchar(theseParams$AllValue[1]) > 0, 
					TRUE,
					FALSE
				), 
				FALSE
			)
			
			if(is.null(thisParamCode)){
				return()
			}
			
			if(is.na(thisParamCode)){
				return()
			} else {
			
				allParamSet <- bea.R::beaParamVals(thisUserID, selectedSet, thisParamCode)$ParamValue
				if(allValCheck){
					allParamDesc <- as.list(
						c(theseParams$AllValue[1], allParamSet[[1]])
					)
					attributes(allParamDesc)$names <- c('All', allParamSet[[2]])
					
				} else {
					allParamDesc <- as.list(
						allParamSet[[1]]
					)
					attributes(allParamDesc)$names <- ifelse(nchar(substr(allParamSet[[2]], 1, 35) ) < nchar(allParamSet[[2]]), paste(substr(allParamSet[[2]], 1, 35), '...'), allParamSet[[2]])
				}
				
				inputGetter <- shiny::selectInput(
						"apiParam1", 
						label = shiny::h5(paste0('Select ', tolower(thisParamDesc), ':')), 
						choices = allParamDesc
				)
				
				
				thisInput <- switch(selectedSet, 
					"REGIONALDATA" = inputGetter,
					"NIPA" = inputGetter,
					"NIUNDERLYINGDETAIL" = inputGetter,
					"MNE" = inputGetter,
					"FIXEDASSETS" = inputGetter,
					"ITA" = inputGetter,
					"IIP" = inputGetter,
					"GDPBYINDUSTRY" = inputGetter,
					"REGIONALINCOME" = inputGetter,
					"REGIONALPRODUCT" = inputGetter,
					"INPUTOUTPUT" = inputGetter,
					"UNDERLYINGGDPBYINDUSTRY" = inputGetter
				);
				
				return(thisInput);
			}
		})
		
		output$apiInp2 <- shiny::renderUI({

			selectedSet <- userSelectedSet()
			theseParams <- userSetParams()

			thisParamDesc <- theseParams[[3]][2]
			thisParamCode <- theseParams[[1]][2]
			
			allValCheck <- ifelse(is.character(theseParams$AllValue[2]),
				ifelse(
					nchar(theseParams$AllValue[2]) > 0, 
					TRUE,
					FALSE
				), 
				FALSE
			)
			
			if(is.null(thisParamCode)){
				return()
			}

			if(is.na(thisParamCode)){
				return()
			} else {
			
				allParamSet <- bea.R::beaParamVals(thisUserID, selectedSet, thisParamCode)$ParamValue
				if(allValCheck){
					allParamDesc <- as.list(
						c(theseParams$AllValue[1], allParamSet[[1]])
					)
					attributes(allParamDesc)$names <- c('All', allParamSet[[2]])
					
				} else {
					allParamDesc <- as.list(
						allParamSet[[1]]
					)
					attributes(allParamDesc)$names <- ifelse(nchar(substr(allParamSet[[2]], 1, 35) ) < nchar(allParamSet[[2]]), paste(substr(allParamSet[[2]], 1, 35), '...'), allParamSet[[2]])
				}
				
				
				inputGetter <- shiny::selectInput(
						"apiParam2", 
						label = shiny::h5(paste0('Select ', tolower(thisParamDesc), ':')), 
						choices = allParamDesc
				)
				
				
				thisInput <- switch(selectedSet, 
					"REGIONALDATA" = inputGetter,
					"NIPA" = inputGetter,
					"NIUNDERLYINGDETAIL" = inputGetter,
					"MNE" = inputGetter,
					"FIXEDASSETS" = inputGetter,
					"ITA" = inputGetter,
					"IIP" = inputGetter,
					"GDPBYINDUSTRY" = inputGetter,
					"REGIONALINCOME" = inputGetter,
					"REGIONALPRODUCT" = inputGetter,
					"INPUTOUTPUT" = inputGetter,
					"UNDERLYINGGDPBYINDUSTRY" = inputGetter
				);
				
				return(thisInput);
			}
		})
		
		output$apiInp3 <- shiny::renderUI({

			selectedSet <- userSelectedSet()
			theseParams <- userSetParams()

			thisParamDesc <- theseParams[[3]][3]
			thisParamCode <- theseParams[[1]][3]

			allValCheck <- ifelse(is.character(theseParams$AllValue[3]),
				ifelse(
					nchar(theseParams$AllValue[3]) > 0, 
					TRUE,
					FALSE
				), 
				FALSE
			)
					
			if(is.null(thisParamCode)){
				return()
			}

			if(is.na(thisParamCode)){
				return()
			} else {
			
				allParamSet <- bea.R::beaParamVals(thisUserID, selectedSet, thisParamCode)$ParamValue
				if(allValCheck){
					allParamDesc <- as.list(
						c(theseParams$AllValue[1], allParamSet[[1]])
					)
					attributes(allParamDesc)$names <- c('All', allParamSet[[2]])
					
				} else {
					allParamDesc <- as.list(
						allParamSet[[1]]
					)
					attributes(allParamDesc)$names <- ifelse(nchar(substr(allParamSet[[2]], 1, 35) ) < nchar(allParamSet[[2]]), paste(substr(allParamSet[[2]], 1, 35), '...'), allParamSet[[2]])
				}
				
				
				inputGetter <- shiny::selectInput(
						"apiParam3", 
						label = shiny::h5(paste0('Select ', tolower(thisParamDesc), ':')), 
						choices = allParamDesc
				)
				
				
				thisInput <- switch(selectedSet, 
					"REGIONALDATA" = inputGetter,
					"NIPA" = inputGetter,
					"NIUNDERLYINGDETAIL" = inputGetter,
					"MNE" = inputGetter,
					"FIXEDASSETS" = inputGetter,
					"ITA" = inputGetter,
					"IIP" = inputGetter,
					"GDPBYINDUSTRY" = inputGetter,
					"REGIONALINCOME" = inputGetter,
					"REGIONALPRODUCT" = inputGetter,
					"INPUTOUTPUT" = inputGetter,
					"UNDERLYINGGDPBYINDUSTRY" = inputGetter
				);
				
				return(thisInput);
			}
		})
		
		output$apiInp4 <- shiny::renderUI({

			selectedSet <- userSelectedSet()
			theseParams <- userSetParams()

			thisParamDesc <- theseParams[[3]][4]
			thisParamCode <- theseParams[[1]][4]
			
			allValCheck <- ifelse(is.character(theseParams$AllValue[4]),
				ifelse(
					nchar(theseParams$AllValue[4]) > 0, 
					TRUE,
					FALSE
				), 
				FALSE
			)
						
			if(is.null(thisParamCode)){
				return()
			}

			if(is.na(thisParamCode)){
				return()
			} else {
			
				allParamSet <- bea.R::beaParamVals(thisUserID, selectedSet, thisParamCode)$ParamValue
				if(allValCheck){
					allParamDesc <- as.list(
						c(theseParams$AllValue[1], allParamSet[[1]])
					)
					attributes(allParamDesc)$names <- c('All', allParamSet[[2]])
					
				} else {
					allParamDesc <- as.list(
						allParamSet[[1]]
					)
					attributes(allParamDesc)$names <- ifelse(nchar(substr(allParamSet[[2]], 1, 35) ) < nchar(allParamSet[[2]]), paste(substr(allParamSet[[2]], 1, 35), '...'), allParamSet[[2]])
				}
				
				
				inputGetter <- shiny::selectInput(
						"apiParam4", 
						label = shiny::h5(paste0('Select ', tolower(thisParamDesc), ':')), 
						choices = allParamDesc
				)
				
				
				thisInput <- switch(selectedSet, 
					"REGIONALDATA" = inputGetter,
					"NIPA" = inputGetter,
					"NIUNDERLYINGDETAIL" = inputGetter,
					"MNE" = inputGetter,
					"FIXEDASSETS" = inputGetter,
					"ITA" = inputGetter,
					"IIP" = inputGetter,
					"GDPBYINDUSTRY" = inputGetter,
					"REGIONALINCOME" = inputGetter,
					"REGIONALPRODUCT" = inputGetter,
					"INPUTOUTPUT" = inputGetter,
					"UNDERLYINGGDPBYINDUSTRY" = inputGetter
				);
				
				return(thisInput);
			}
		})
		

		output$apiInp5 <- shiny::renderUI({

			selectedSet <- userSelectedSet()
			theseParams <- userSetParams()

			thisParamDesc <- theseParams[[3]][5]
			thisParamCode <- theseParams[[1]][5]
			
			allValCheck <- ifelse(is.character(theseParams$AllValue[5]),
				ifelse(
					nchar(theseParams$AllValue[5]) > 0, 
					TRUE,
					FALSE
				), 
				FALSE
			)
			
			if(is.null(thisParamCode)){
				return()
			}

			if(is.na(thisParamCode)){
				return()
			} else {
			
				allParamSet <- bea.R::beaParamVals(thisUserID, selectedSet, thisParamCode)$ParamValue
				if(allValCheck){
					allParamDesc <- as.list(
						c(theseParams$AllValue[1], allParamSet[[1]])
					)
					attributes(allParamDesc)$names <- c('All', allParamSet[[2]])
					
				} else {
					allParamDesc <- as.list(
						allParamSet[[1]]
					)
					attributes(allParamDesc)$names <- ifelse(nchar(substr(allParamSet[[2]], 1, 35) ) < nchar(allParamSet[[2]]), paste(substr(allParamSet[[2]], 1, 35), '...'), allParamSet[[2]])
				}
				
				
				inputGetter <- shiny::selectInput(
						"apiParam5", 
						label = shiny::h5(paste0('Select ', tolower(thisParamDesc), ':')), 
						choices = allParamDesc
				)
				
				
				thisInput <- switch(selectedSet, 
					"REGIONALDATA" = inputGetter,
					"NIPA" = inputGetter,
					"NIUNDERLYINGDETAIL" = inputGetter,
					"MNE" = inputGetter,
					"FIXEDASSETS" = inputGetter,
					"ITA" = inputGetter,
					"IIP" = inputGetter,
					"GDPBYINDUSTRY" = inputGetter,
					"REGIONALINCOME" = inputGetter,
					"REGIONALPRODUCT" = inputGetter,
					"INPUTOUTPUT" = inputGetter,
					"UNDERLYINGGDPBYINDUSTRY" = inputGetter
				);
				
				return(thisInput);
			}
		})
		
		output$apiInp6 <- shiny::renderUI({

			selectedSet <- userSelectedSet()
			theseParams <- userSetParams()

			thisParamDesc <- theseParams[[3]][6]
			thisParamCode <- theseParams[[1]][6]
			allValCheck <- ifelse(is.character(theseParams$AllValue[6]),
				ifelse(
					nchar(theseParams$AllValue[6]) > 0, 
					TRUE,
					FALSE
				), 
				FALSE
			)


			if(is.null(thisParamCode)){
				return()
			}
			
			if(is.na(thisParamCode)){
				return()
			} else {
			
				allParamSet <- bea.R::beaParamVals(thisUserID, selectedSet, thisParamCode)$ParamValue
				if(allValCheck){
					allParamDesc <- as.list(
						c(theseParams$AllValue[1], allParamSet[[1]])
					)
					attributes(allParamDesc)$names <- c('All', allParamSet[[2]])
					
				} else {
					allParamDesc <- as.list(
						allParamSet[[1]]
					)
					attributes(allParamDesc)$names <- ifelse(nchar(substr(allParamSet[[2]], 1, 35) ) < nchar(allParamSet[[2]]), paste(substr(allParamSet[[2]], 1, 35), '...'), allParamSet[[2]])
				}
				
				
				inputGetter <- shiny::selectInput(
						"apiParam6", 
						label = shiny::h5(paste0('Select ', tolower(thisParamDesc), ':')), 
						choices = allParamDesc
				)
				
				
				thisInput <- switch(selectedSet, 
					"REGIONALDATA" = inputGetter,
					"NIPA" = inputGetter,
					"NIUNDERLYINGDETAIL" = inputGetter,
					"MNE" = inputGetter,
					"FIXEDASSETS" = inputGetter,
					"ITA" = inputGetter,
					"IIP" = inputGetter,
					"GDPBYINDUSTRY" = inputGetter,
					"REGIONALINCOME" = inputGetter,
					"REGIONALPRODUCT" = inputGetter,
					"INPUTOUTPUT" = inputGetter,
					"UNDERLYINGGDPBYINDUSTRY" = inputGetter
				);
				
				return(thisInput);
			}
		})
		
		output$apiInp7 <- shiny::renderUI({

			selectedSet <- userSelectedSet()
			theseParams <- userSetParams()

			thisParamDesc <- theseParams[[3]][7]
			thisParamCode <- theseParams[[1]][7]
			allValCheck <- ifelse(is.character(theseParams$AllValue[7]),
				ifelse(
					nchar(theseParams$AllValue[7]) > 0, 
					TRUE,
					FALSE
				), 
				FALSE
			)

			if(is.null(thisParamCode)){
				return()
			}
			
			if(is.na(thisParamCode)){
				return()
			} else {
			
				allParamSet <- bea.R::beaParamVals(thisUserID, selectedSet, thisParamCode)$ParamValue
				if(allValCheck){
					allParamDesc <- as.list(
						c(theseParams$AllValue[1], allParamSet[[1]])
					)
					attributes(allParamDesc)$names <- c('All', allParamSet[[2]])
					
				} else {
					allParamDesc <- as.list(
						allParamSet[[1]]
					)
					attributes(allParamDesc)$names <- ifelse(nchar(substr(allParamSet[[2]], 1, 35) ) < nchar(allParamSet[[2]]), paste(substr(allParamSet[[2]], 1, 35), '...'), allParamSet[[2]])
				}
				
				
				inputGetter <- shiny::selectInput(
						"apiParam7", 
						label = shiny::h5(paste0('Select ', tolower(thisParamDesc), ':')), 
						choices = allParamDesc
				)
								
				thisInput <- switch(selectedSet, 
					"REGIONALDATA" = inputGetter,
					"NIPA" = inputGetter,
					"NIUNDERLYINGDETAIL" = inputGetter,
					"MNE" = inputGetter,
					"FIXEDASSETS" = inputGetter,
					"ITA" = inputGetter,
					"IIP" = inputGetter,
					"GDPBYINDUSTRY" = inputGetter,
					"REGIONALINCOME" = inputGetter,
					"REGIONALPRODUCT" = inputGetter,
					"INPUTOUTPUT" = inputGetter,
					"UNDERLYINGGDPBYINDUSTRY" = inputGetter
				);
				
				return(thisInput);
			}
		})
		
		output$apiInp8 <- shiny::renderUI({

			selectedSet <- userSelectedSet()
			theseParams <- userSetParams()

			thisParamDesc <- theseParams[[3]][8]
			thisParamCode <- theseParams[[1]][8]
			allValCheck <- ifelse(is.character(theseParams$AllValue[8]),
				ifelse(
					nchar(theseParams$AllValue[8]) > 0, 
					TRUE,
					FALSE
				), 
				FALSE
			)
			
			if(is.null(thisParamCode)){
				return()
			}

			if(is.na(thisParamCode)){
				return()
			} else {
			
				allParamSet <- bea.R::beaParamVals(thisUserID, selectedSet, thisParamCode)$ParamValue
				if(allValCheck){
					allParamDesc <- as.list(
						c(theseParams$AllValue[1], allParamSet[[1]])
					)
					attributes(allParamDesc)$names <- c('All', allParamSet[[2]])
					
				} else {
					allParamDesc <- as.list(
						allParamSet[[1]]
					)
					attributes(allParamDesc)$names <- ifelse(nchar(substr(allParamSet[[2]], 1, 35) ) < nchar(allParamSet[[2]]), paste(substr(allParamSet[[2]], 1, 35), '...'), allParamSet[[2]])
				}
				
				
				inputGetter <- shiny::selectInput(
						"apiParam8", 
						label = shiny::h5(paste0('Select ', tolower(thisParamDesc), ':')), 
						choices = allParamDesc
				)
				
				thisInput <- switch(selectedSet, 
					"REGIONALDATA" = inputGetter,
					"NIPA" = inputGetter,
					"NIUNDERLYINGDETAIL" = inputGetter,
					"MNE" = inputGetter,
					"FIXEDASSETS" = inputGetter,
					"ITA" = inputGetter,
					"IIP" = inputGetter,
					"GDPBYINDUSTRY" = inputGetter,
					"REGIONALINCOME" = inputGetter,
					"REGIONALPRODUCT" = inputGetter,
					"INPUTOUTPUT" = inputGetter,
					"UNDERLYINGGDPBYINDUSTRY" = inputGetter
				);
				
				return(thisInput);
			}
		})

		output$apiInp9 <- shiny::renderUI({

			selectedSet <- userSelectedSet()
			theseParams <- userSetParams()

			thisParamDesc <- theseParams[[3]][9]
			thisParamCode <- theseParams[[1]][9]
			allValCheck <- ifelse(is.character(theseParams$AllValue[9]),
				ifelse(
					nchar(theseParams$AllValue[9]) > 0, 
					TRUE,
					FALSE
				), 
				FALSE
			)

			
			if(is.null(thisParamCode)){
				return()
			}

			if(is.na(thisParamCode)){
				return()
			} else {
			
				allParamSet <- bea.R::beaParamVals(thisUserID, selectedSet, thisParamCode)$ParamValue
				if(allValCheck){
					allParamDesc <- as.list(
						c(theseParams$AllValue[1], allParamSet[[1]])
					)
					attributes(allParamDesc)$names <- c('All', allParamSet[[2]])
					
				} else {
					allParamDesc <- as.list(
						allParamSet[[1]]
					)
					attributes(allParamDesc)$names <- ifelse(nchar(substr(allParamSet[[2]], 1, 35) ) < nchar(allParamSet[[2]]), paste(substr(allParamSet[[2]], 1, 35), '...'), allParamSet[[2]])
				}
				
				
				inputGetter <- shiny::selectInput(
						"apiParam8", 
						label = shiny::h5(paste0('Select ', tolower(thisParamDesc), ':')), 
						choices = allParamDesc
				)
				
				thisInput <- switch(selectedSet, 
					"REGIONALDATA" = inputGetter,
					"NIPA" = inputGetter,
					"NIUNDERLYINGDETAIL" = inputGetter,
					"MNE" = inputGetter,
					"FIXEDASSETS" = inputGetter,
					"ITA" = inputGetter,
					"IIP" = inputGetter,
					"GDPBYINDUSTRY" = inputGetter,
					"REGIONALINCOME" = inputGetter,
					"REGIONALPRODUCT" = inputGetter,
					"INPUTOUTPUT" = inputGetter,
					"UNDERLYINGGDPBYINDUSTRY" = inputGetter
				);
				
				return(thisInput);
			}
		})

#Removed with treemap		
#		output$userPd <- shiny::renderPrint({
#			userPeriod <-	ifelse(	
#				nchar(max(beaTab[, TimePeriod])) > 
#				nchar(gsub('Q', '', max(beaTab[, TimePeriod]), fixed = TRUE)),
#				paste0(
#					format(input$timePd, '%Y'), 'Q',
#					floor((as.numeric(format(input$timePd, '%m'))+2)/3)
#				),
#				ifelse(
#					(	nchar(max(beaTab[, TimePeriod])) > 
#						nchar(gsub('M', '', max(beaTab[, TimePeriod]), fixed = TRUE))
#					),
#					format(input$timePd, '%YM%m'), format(input$timePd, '%Y')
#				)
#			)
#			return(userPeriod)
#		})

		output$seriesbox <- shiny::renderUI({
			#Create list of names for select box
			lineOptions <- as.list(
				unique(
					beaTab[order(as.numeric(LineNumber)),LineDescription]
				)
			)
#			#Assign names so that select box understands - may be bad?
#			attributes(lineOptions)$names <- paste0(
#				'Choice ', 
#				seq(1, length(lineOptions)))
			shiny::selectInput(
				"userSeries", 
				label = shiny::h5("Select series:"), 
				choices = lineOptions, 
				selected = 1
			)
		})
		
		output$userSer <- shiny::renderPrint({
			input$userSeries
		})
	
####Treemap section suppressed - Misleading	
##		output$treemap <- googleVis::renderGvis({
##		#output$treemap <- shiny::renderPlot({
##				#Get latest datapoint for each series within context of user specs
##			userPeriod <-	ifelse(	
##				nchar(max(beaTab[, TimePeriod])) > 
##				nchar(gsub('Q', '', max(beaTab[, TimePeriod]), fixed = TRUE)),
##				paste0(
##					format(input$timePd, '%Y'), 'Q',
##					floor((as.numeric(format(input$timePd, '%m'))+1)/3)
##				),
##				ifelse(
##					(	nchar(max(beaTab[, TimePeriod])) > 
##						nchar(gsub('M', '', max(beaTab[, TimePeriod]), fixed = TRUE))
##					),
##					format(input$timePd, '%YM%m'), format(input$timePd, '%Y')
##				)
##			)
##			
##			latestTime <- ifelse(
##				nchar(userPeriod) < 4, 
##				max(beaTab[, TimePeriod]),
##				userPeriod
##			)
##			latestVals <- beaTab[TimePeriod == latestTime]
##			data.table::setkey(latestVals, key='LineNumber')
##
##			#Get previous period's datapoint 
##			latestTlag <- max(beaTab[TimePeriod < latestTime, TimePeriod])
##			latestVlag <- beaTab[TimePeriod == latestTlag]
##			data.table::setkey(latestVlag, key='LineNumber')
##
##			#Create treemap using latest levels for size, change in pct chg for color
##			tmVal <- hierTree[latestVals][,
##				.(node, 
##					root,
##					size = as.numeric(gsub(',', '', DataValue, fixed = TRUE)),
##					lnNo = LineNumber
##				)
##			]
##
##			root0chk <- tmVal[grep(' [Line 0]', root, fixed=T), root]
##			#Special handler for "line 0"
##			if(length(root0chk) > 0){
##			
##				root0topV <- data.table::as.data.table(
##					list(
##						'lnNo' = '0',
##						'node' = unique(root0chk),
##						'root' = NA,
##						'size' = 1
##					)
##				) 
##				root0topL1 <- data.table::as.data.table(
##					list(
##						'lnNo' = '0',
##						'node' = unique(root0chk),
##						'root' = NA,
##						'size' = 1,
##						'lag1' = 1								
##					)
##				) 
##				root0topL2 <- data.table::as.data.table(
##					list(
##						'lnNo' = '0',
##						'node' = unique(root0chk),
##						'root' = NA,
##						'size' = 1,
##						'lag1' = 1,
##						'lag2' = 1								
##					)
##				) 
##
##
##
##				tmVal <- data.table::rbindlist(
##					list(
##						tmVal,
##						root0topV 
##					),
##					use.names = TRUE
##				)
##				
##				
##			}
##			
##			data.table::setkey(tmVal, key = lnNo)
##
##			if(length(unique(beaTab[,TimePeriod])) >= 3) {
##				#Get period before previous datapoint 
##				secondTlag <- max(beaTab[TimePeriod < latestTlag, TimePeriod])
##				secondVlag <- beaTab[TimePeriod == secondTlag]
##				data.table::setkey(secondVlag, key='LineNumber')
##			
##				tmLag1 <- tmVal[latestVlag][, 
##					.(lnNo, node, root, size,
##						lag1 = as.numeric(gsub(',','', DataValue, fixed = TRUE))
##					)
##				]
##
##				tmLag2 <- tmLag1[secondVlag][, 
##					.(lnNo, node, root, size, lag1,
##						lag2 = as.numeric(gsub(',','', DataValue, fixed = TRUE))
##					)
##				]
##
##				if(length(root0chk) > 0){
##					tmLag1 <- data.table::rbindlist(
##						list(
##							tmLag1,
##							root0topL1 
##						),
##						use.names = TRUE
##					)
##				
##					tmLag2 <- data.table::rbindlist(
##						list(
##							tmLag2,
##							root0topL2
##						),
##						use.names = TRUE
##					)
##				}
##				
##				data.table::setkey(tmLag1, key = lnNo)
##				data.table::setkey(tmLag2, key = lnNo)
##
##			
##				tmDT <- tmLag2[
##					!is.na(lnNo)	& 
##						(
##							!(node %in% tmLag2[
##								!(root %in% tmLag2[, node]), 
##									node
##								]
##							) | (
##								lnNo == 1 | lnNo == 0
##							)
##						),
##					.(lnNo, node, root, size, lag1, lag2,
##						pctChgNew = (size / lag1) - 1,
##						pctChgOld = (lag1 / lag2) - 1
##					)
##				]
##			} else {
##				if(length(unique(beaTab[,TimePeriod])) == 2) {
##					tmLag1 <- tmVal[latestVlag][, 
##						.(lnNo, node, root, size,
##							lag1 = as.numeric(gsub(',','', DataValue, fixed = TRUE))
##						)
##					]
##				
##					#Special handler for "line 0"
##					if(length(root0chk) > 0){
##						tmLag1 <- data.table::rbindlist(
##							list(
##								tmLag1,
##								root0topL1 
##							),
##							use.names = TRUE
##						)
##					}
##					data.table::setkey(tmLag1, key = lnNo)
##			
##					tmDT <- tmLag1[
##						!is.na(lnNo)	& 
##							(
##								!(node %in% tmLag1[
##									!(root %in% tmLag1[, node]), 
##										node
##									]
##								) | (
##									lnNo == 1 | lnNo == 0
##								)
##							),
##						.(lnNo, node, root, size, lag1, 
##							pctChgNew = size,
##							pctChgOld = lag1
##						)
##					]					
##				} else {
##					tmDT <- tmVal[
##						!is.na(lnNo)	& 
##							(
##								!(node %in% tmVal[
##									!(root %in% tmVal[, node]), 
##										node
##									]
##								) | (
##									lnNo == 1 | lnNo == 0
##								)
##							),
##						.(lnNo, node, root, size, 
##							pctChgNew = size,
##							pctChgOld = 0
##						)
##					]										
##				}
##			}
##
##			
##			#Special hue when node is changed from row 1 in sidebar
##				if(input$userSeries != unique(beaTab[LineNumber == 1, LineDescription]) 
##				) {				
##					tmDT[!is.na(node), hue := 
##						ifelse(
##							(node == paste0(input$userSeries, ' [Line ', lnNo, ']')) 
##							, 
##							1, ifelse(!is.na(root) &
##								substr(
##									root, 
##									1, 
##									regexpr(' [Line ', root, fixed=TRUE)-1
##								) == input$userSeries, 
##								0.1, 
##								-1
##							)
##						)
##					]
##			} else {
##				tmDT[!is.na(node), hue :=
##					ifelse(
##						(pctChgNew - pctChgOld) > 0,
##						ifelse(
##							pctChgNew > 0, 
##							ifelse(
##								pctChgOld < 0, 3, 2
##							), 1
##						), 
##						ifelse(
##							(pctChgNew - pctChgOld) > 0,
##							ifelse(
##								pctChgNew < 0, 
##								ifelse(
##									pctChgOld > 0, -3, -2
##								), -1
##							), 0
##						)
##					)
##				]
##			}
##
##
##			tmDT[, absz := abs(size)]
##			#tmDT[root == ' [Line 0]', root := '']
##			
##			#Convert treemap data.table to data.frame, eliminate empty nodes
##			tmDF <- as.data.frame(tmDT[!is.na(node)])
##
##			maxTiers <- max(
##				as.numeric(
##					unique(
##						thisIndex[
##						as.numeric(LineNumber) %in% 
##							as.numeric(beaTab[LineDescription == input$userSeries, LineNumber]
##						),
##						Tier
##					])
##				)
##			)
##			session$clientData$output_treemap_width
##
##			Tree <- googleVis::gvisTreeMap(tmDF, 
##				idvar 		= 'node', 
##				parentvar	= 'root', 
##				sizevar  	= 'absz',
##				colorvar 	= 'hue', 
##				options 	= list(
##					title = paste0(hierTab, ' [relative levels]'),
###					titleTextStyle = '{fontSize:9}',
###					maxDepth = 1,
###					maxPostDepth = maxTiers,
##					maxDepth = ifelse(is.character(input$userSeries), 
##						ifelse(maxTiers > 1, maxTiers-1, 1), 
##						1
##					),
##					minColor = ifelse(is.character(input$userSeries), 
##						ifelse( 
##							input$userSeries != unique(beaTab[LineNumber == 1, LineDescription]),
##							'#ababab', '#990000'
##						),
##					'#990000'),
##					midColor = '#ffffcc',
##					maxColor = ifelse(is.character(input$userSeries), 
##						ifelse( 
##							input$userSeries != unique(beaTab[LineNumber == 1, LineDescription]),
##							'#0a5eff', '#339933'
##						), 
##					'#339933'),
##					headerHeight = 15,
##					fontColor = 'black',
##					showScale = FALSE,
##					highlightOnMouseOver = TRUE,
###					width = 'floor(0.15 * screen.width);',   
##					width = '100%',
###					width = 'gvisWidths',
##					height = 275
##				)
##			)
##			return(Tree)
##		})
		
		output$topbar <-  googleVis::renderGvis({
			session$clientData$output_topbar_width

			beaBar <- beaTab[
				LineDescription==input$userSeries, 
				.(
					TimePeriod, 
					DataPoint = as.numeric(gsub(',', '', DataValue, fixed=TRUE))
				)
			][order(rank(TimePeriod))]
			
			topName <- unique(beaTab[LineDescription == input$userSeries, LineDescription])
			
			attributes(beaBar)$names <- c('Time Period', topName)			
			
			Bar <- googleVis::gvisSteppedAreaChart(
				beaBar, 
				xvar='Time Period', 
				yvar=topName,
				options=list(
					isStacked=TRUE,
					height = 275,
#					width = 'floor(0.15 * screen.width);',   
					width = '100%', 
#					width = 'gvisWidths',
					legend = 'none',
					title = topName,
					vAxis = '{}'
					)
			)
			return(Bar)
		})

		output$vistab <-  googleVis::renderGvis({
			preTab <- try(as.data.frame(
				bea.R::bea2Tab(beaTab, asWide = TRUE)[
					order(
						as.numeric(
							LineNumber
						)
					)
				]
			), silent = TRUE)
			
			ptNames <- names(preTab)
			
			ptnClean <- gsub('DataValue_', '', ptNames, fixed = TRUE)
			ptnClean <- gsub('CL_UNIT', 'Units', ptnClean, fixed = TRUE)
			ptnClean <- gsub('UNIT_MULT', 'Multiplier', ptnClean, fixed = TRUE)
					
			names(preTab) <- ptnClean
			
			vTab <- googleVis::gvisTable(
				preTab, 
				options=list(
#					title = hierTab,
					page='enable',
					height='automatic',
					width='automatic')
				)
			return(vTab)
		})

		output$dataDetail <-  shiny::renderText({
			beaTabDets <- attributes(beaPayload)$detail
			beaTabList <- lapply(
				attributes(beaTabDets)$names, 
				function(thisAtr){
				if(class(beaTabDets[[thisAtr]]) == 'character'){
					beaTabElem <- beaTabDets[[thisAtr]]
				} else {
				#Add exception for notes
					if(thisAtr == 'Notes') {
						beaTabElem <- paste(beaTabDets$Notes$NoteText, collapse = "\n")
					} else{
						beaElemDets <- attributes(beaTabDets[[thisAtr]])
						beaTabElem <- paste(lapply(beaElemDets$names, function(thisElem){
							return(paste0(thisElem, ': ', beaTabDets[[thisAtr]][[thisElem]]))
						}), collapse = "\n")
					}
				}
										
				if(thisAtr != 'Dimensions'){
					if(tolower(thisAtr) %in% c('statistic', 'utcproductiontime')){
						return(paste0(thisAtr, ": ", beaTabElem))
					} else {
						return(paste0(thisAtr, ": \n", beaTabElem))
					}
				} else {
					return('')
				} 
				
			})
			detailStr <- paste(beaTabList, collapse = "\n")
			return(detailStr)
		})

#		output$apiCall <-  shiny::renderPrint({
		output$apiCall <-  shiny::renderText({
		#' userSpecList <- list('UserID' = 'yourKey' ,
#'									'Method' = 'GetData',
#'									'datasetname' = 'NIPA',
#'									'Frequency' = 'A',
#'									'TableID' = '68',
#'									'Year' = 'X')

			selectedSet <- userSelectedSet()
			theseParams <- userSetParams()

			userDefPrms <- theseParams$ParameterName[
				!(
					nchar(theseParams$AllValue) > 0 & 
					is.character(theseParams$AllValue)
				)
			]
			
			
			allValsPrms <- theseParams$ParameterName[
				(
					nchar(theseParams$AllValue) > 0 & 
					is.character(theseParams$AllValue)
				)
			]
			
			allValsSetr <- theseParams$AllValue[
				(
					nchar(theseParams$AllValue) > 0 & 
					is.character(theseParams$AllValue)
				)
			]
			
			allValsPrms <- ifelse(is.null(allValsPrms), c(NA, NA), allValsPrms)
			allValsSetr <- ifelse(is.null(allValsSetr), c(NA, NA), allValsSetr)
			
			paramDescs <- sapply(1:length(userDefPrms), function(x){
				apiInStr <- paste0('input$apiParam', x);
				apiDesc <- apiInStr #eval(parse(apiInStr));
				return(apiDesc);
			})

			paramAttrs <- sapply(1:length(userDefPrms), function(x){
				apiInStr <- paste0('attributes(input$apiParam', x, ')');
				apiAttr <- apiInStr #eval(parse(apiInStr));
				return(apiAttr);
			})
			
			specStr <- paste0("beaData <- bea.R::beaGet( \n list(\n 'UserID' = '", thisUserID, "', \n 'Method' = 'GetData', \n 'DatasetName' = '", selectedSet,"'",
				ifelse(
					!is.null(userDefPrms[1]),
					ifelse(!is.na(userDefPrms[1]), 
					paste0(", \n '", userDefPrms[1], "' = '", input$apiParam1,"'"),
					""), ""
				), 
				ifelse(
					!is.null(userDefPrms[2]),
				ifelse( 
					!is.na(userDefPrms[2]), 
					paste0(", \n '", userDefPrms[2], "' = '", input$apiParam2,"'"),
					""), ""
				), 
				ifelse( 
					!is.null(userDefPrms[3]), 
				ifelse( 
					!is.na(userDefPrms[3]), 
					paste0(", \n '", userDefPrms[3], "' = '", input$apiParam3,"'"),
					""), ""
				), 
				ifelse( 
					!is.null(userDefPrms[4]), 
				ifelse( 
					!is.na(userDefPrms[4]), 
					paste0(", \n '", userDefPrms[4], "' = '", input$apiParam4,"'"),
					""), ""
				), 
				ifelse( 
					!is.null(userDefPrms[5]), 
				ifelse( 
					!is.na(userDefPrms[5]), 
					paste0(", \n '", userDefPrms[5], "' = '", input$apiParam5,"'"),
					""), ""
				), 
				ifelse( 
					!is.null(userDefPrms[6]), 
				ifelse( 
					!is.na(userDefPrms[6]), 
					paste0(", \n '", userDefPrms[6], "' = '", input$apiParam6,"'"),
					""), ""
				), 
				ifelse( 
					!is.null(userDefPrms[7]), 
				ifelse( 
					!is.na(userDefPrms[7]), 
					paste0(", \n '", userDefPrms[7], "' = '", input$apiParam7,"'"),
					""), ""
				), 
				ifelse( 
					!is.null(userDefPrms[8]), 
				ifelse( 
					!is.na(userDefPrms[8]), 
					paste0(", \n '", userDefPrms[8], "' = '", input$apiParam8,"'"),
					""), ""
				), 
				ifelse( 
					!is.null(userDefPrms[9]), 
				ifelse( 
					!is.na(userDefPrms[9]), 
					paste0(", \n '", userDefPrms[9], "' = '", input$apiParam9,"'"),
					""), ""
				), 
				ifelse( 
					!is.null(allValsPrms[1]), 
				ifelse( 
					!is.na(allValsPrms[1]), 
					paste0(", \n '", allValsPrms[1], "' = '", allValsSetr[1],"'"),
					""), ""
				), 
				ifelse( 
					!is.null(allValsPrms[2]), 
				ifelse( 
					!is.na(allValsPrms[2]), 
					paste0(", \n '", allValsPrms[2], "' = '", allValsSetr[2],"'"),
					""), ""
				), 
				ifelse( 
					!is.null(allValsPrms[3]), 
				ifelse( 
					!is.na(allValsPrms[3]), 
					paste0(", \n '", allValsPrms[3], "' = '", allValsSetr[3],"'"),
					""), ""
				), 
				ifelse( 
					!is.null(allValsPrms[4]), 
				ifelse( 
					!is.na(allValsPrms[4]), 
					paste0(", \n '", allValsPrms[4], "' = '", allValsSetr[4],"'"),
					""), ""
				), 
				ifelse( 
					!is.null(allValsPrms[5]), 
				ifelse( 
					!is.na(allValsPrms[5]), 
					paste0(", \n '", allValsPrms[5], "' = '", allValsSetr[5],"'"),
					""), ""
				), 
				ifelse( 
					!is.null(allValsPrms[6]), 
				ifelse( 
					!is.na(allValsPrms[6]), 
					paste0(", \n '", allValsPrms[6], "' = '", allValsSetr[6],"'"),
					""), ""
				), 
				ifelse( 
					!is.null(allValsPrms[7]), 
				ifelse( 
					!is.na(allValsPrms[7]), 
					paste0(", \n '", allValsPrms[7], "' = '", allValsSetr[7],"'"),
					""), ""
				), 
				"))"	
			)
			
			#return(writeLines(specStr))
			return(specStr)

		})
		
		output$downloadData <- downloadHandler(
			filename = function() { paste(
				userSelectedSet(), 
				input$apiParam1,
				input$apiParam2,
				input$apiParam3,
				input$apiParam4,
				input$apiParam5,
				input$apiParam6,
				input$apiParam7,
				input$apiParam8,
				input$apiParam9,
				'.csv', sep='') },
			content = function(file) {
				utils::write.csv(bea.R::bea2Tab(beaTab, asWide = TRUE)[
						order(
							as.numeric(
								LineNumber
							)
						)
					], file
				)
			}
		)

	}
	
	suppressWarnings(shiny::shinyApp(ui, server))
	
	
		#if(length(unique(hierTree[, nodeID])) > unique(tmFnl[,nodeID])){
		#	warning('Some rows of this data table may be missing from treemap.')
		#}
	}
	}	else{
			warning("Error in API response. Returning error information.")
			return(beaTab)		
	}
}
