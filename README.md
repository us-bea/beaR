# Patch/issue notes

*GetParameterValuesFiltered method for list of parameters:* If your request returns the error message "The dataset requested requires parameters that were missing from the request" and the `beaParamVals()` method returns a very large number of results, [see the help request here](https://github.com/us-bea/bea.R/pull/5) for an example of how to use `beaGet()` to return a filtered list of parameters.

*Patch 1.0.5 Note:* The parameter used to make a beaGet() call for NIPA and NIUnderlyingDetail data sets have changed as of February 2018. "TableID" has been discontinued, and "TableName" has been added, with parameter values that differ from the obsolete TableID values.

To find the new parameters to use with TableName, use any one of the following:

```R
    beaSearch('NIPA', '[your 36-digit BEA API key]') 
    beaParamVals('[your 36-digit BEA API key]', 'NIPA', 'TableName')
    beaSearch('NIUnderlyingDetail', '[your 36-digit BEA API key]') 
    beaParamVals('[your 36-digit BEA API key]', 'NIUnderlyingDetail', 'TableName')
```

*Patch 1.0.2 Note:* The RegionalIncome and RegionalProduct data sets replaced RegionalData at the end of May 2017. The "RegionalData" dataset has been discontinued, and the package was patched (v1.0.2) to update the beaSearch function accordingly.


# Instructions

Thank you for using BEA's R library. The library is intended to make it easier to retrieve and work with BEA data. 

After you test the library, please send any feedback and, if possible, code that you have written to Developers@bea.gov (feedback and code can be provided together as an .Rmd file, or can be provided separately, in the formats of your preference).
## To Install and Load the stable, published version of the bea.R Library (recommended)

```R
    install.packages('bea.R')
    library(bea.R)
```

## To Install and Load the development version of the bea.R Library (not recommended)

**Please take the following steps:** 

1. Run the following line of code to install the 'httr' and 'devtools' packages:
```r
    install.packages(c('devtools', 'httr'))
```

2. Load the packages listed in Step 1 using the 'library' function:
```r
    library(httr)
    library(devtools)
```

3. Install the bea.R library from the BEA GitHub repo: 
```r
    httr::set_config( config( ssl_verifypeer = 0L ))           #zero L
    devtools::install_github('us-bea/bea.R')
```


4. Load bea.R using the library function.
```r
    library(bea.R)
```
You are now ready to use bea.R! 

## To Get Started
You must first  [register for an API key](http://www.bea.gov/API/signup/index.cfm) from BEA by providing your name and email address. The key will be emailed to you. 

Once you have received your BEA API key, save it to a variable to make it easier to use later: 

```r
beaKey 	<- 'YOUR 36-DIGIT API KEY'
```

## To Use beaSearch and beaGet

Currently, the bea.R library offers two main methods: beaSearch and beaGet. 

### beaSearch
This method allows you to search for BEA data by keyword. For example, to find all datasets in which the term "personal consumption" appears, use the following:  

```r
beaSearch('personal consumption', beaKey)
```

Please note that that beaSearch currently searches only national and regional data.

You may also specify "asHtml = TRUE" to view in-browser:
```r
beaSearch('gross domestic', beaKey, asHtml = TRUE)
```

The contents of this function are automatically updated using a new metadata component of BEA's API; as such, we recommend that you use it with your API key, and the first use of this function requires that you use your key or it will be unable to extract the metadata.

If you do not wish to automatically update the metadata (e.g., you have conducted a study using the search function), simply searching for the term without also passing your key to the function will do a search only using your locally stored version.

However, *this approach is not advised.* If you would like to retain metadata for posterity, please copy it from the "bea.R/data" area of your .libPaths() directory to local storage elsewhere on your machine; this will help prevent accidental overwrite, and will not interfere with the "freshness" of your searches.

### beaGet

Once you have identified the TableID number and other information, you can use beaGet to access the data. The following code, for example, returns the NIPA table with 2015 data for TableID no. 66. 

```r
beaSpecs <- list(
	'UserID' = beaKey ,
	'Method' = 'GetData',
	'datasetname' = 'NIPA',
	'TableName' = 'T20305',
	'Frequency' = 'Q',
	'Year' = 'X',
	'ResultFormat' = 'json'
);
beaPayload <- beaGet(beaSpecs);
```

To retrieve a limited selection of multiple years, list all the years you want to retrieve. For example, to retrieve data for 2011-2015, use "Year"="2011,2012,2013,2014,2015"

The  [API documentation](http://www.bea.gov/API/bea_web_service_api_user_guide.htm) includes information about the specific parameters required by beaGET. 

Setting asWide = FALSE gives results closest to the way they are actually returned by the API (every column is a variable, every row is an observation):
```r
beaLong <- beaGet(beaSpecs, asWide = FALSE)
```

To return in a format in which each column represents a series, set iTableStyle = FALSE.

This returns columns named with a concatenation of the descriptive column values, whereas rows are populated with numeric DataValues for each TimePeriod, and has one column named "TimePeriod" filled with dates.  
```r
beaStatTab <- beaGet(beaSpecs, iTableStyle = FALSE)
```

By default, asWide = TRUE and iTableStyle = TRUE, as this format is the most similar to our iTables; the "beaPayload" object in our first beaGet example at the beginning of this section is in the default format.


## To Use beaViz  

The bea.R library also includes an experimental method to create a visual dashboard. This method is still under development. Currently, it is designed to work with the standard R Console interface—not with other interfaces such as R Studio. (However, if you want to experiment with beaViz in R Studio, click on "Open in Browser" at the top of pop-up box after you execute the beaViz method.  

The beaViz method allows you to pass a variable generated from beaGet to create a dashboard. To use the "beaPayload" example given above, enter the following command:

```r
beaViz(beaPayload)
```
Please note that beaViz is currently only available for use with the NIPA and NIUnderlyingDetail datasets and the associated metadata. 
BEA is open to any thoughts you may have about visually representing BEA data.


# About bea.R
bea.R is a library for use with BEA’s API and the R programming language, version 3.2.1 or higher.

This library serves two core purposes:

1.	To Extract/Transform/Load data [beaGet] from the BEA API as R-friendly formats in the user's workspace. Transformation done by default in beaGet is analogous to the format used in [BEA's iTables](http://www.bea.gov/itable/index.cfm), but this can be modified using beaGet's optional parameters.

2.	To enable the search of descriptive metadata [beaSearch].

Other features of the library exist mainly as intermediate methods or are in early stages of development.


# Disclaimer
The United States Department of Commerce (DOC) GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. DOC has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any claims against the Department of Commerce stemming from the use of its GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

Use of this library will result in data being stored on users' local machines. Specifically, local copies of BEA API metadata will be stored and automatically updated in the .libPaths() "/bea.R/data" directory in order to improve performance of beaSearch.
