pkgLoad <- function( packages = "favourites" ) {
  
  if( length( packages ) == 1L && packages == "favourites" ) {
    packages <- c( "gridExtra", "ggplot2", "survival", "survminer", "dplyr", 
                   "dfcrm", "visreg", "purrr", "plotly", "flexsurvcure",
                   "Rlab", "gridExtra", "cmprsk", "visNetwork", "tidyr",
                   "parallel", "cmprsk", "scales"
    )
  }
  
  packagecheck <- match( packages, utils::installed.packages()[,1] )
  
  packagestoinstall <- packages[ is.na( packagecheck ) ]
  
  if( length( packagestoinstall ) > 0L ) {
    utils::install.packages( packagestoinstall
    )
  } else {
    print( "All requested packages already installed" )
  }
  
  for( package in packages ) {
    suppressPackageStartupMessages(
      library( package, character.only = TRUE, quietly = TRUE )
    )
  }
  
}


pkgLoad()
