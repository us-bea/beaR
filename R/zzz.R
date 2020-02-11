.onLoad <- function(lib, pkg) {
  packageStartupMessage("Note: As of February 2018, beaGet() requires 'TableName' for NIPA and NIUnderlyingDetail data instead of 'TableID.' See https://github.us-bea/bea.R for details.")
}
