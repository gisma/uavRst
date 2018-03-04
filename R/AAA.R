.onLoad <- function(libname = find.package("uavRst"), pkgname = "uavRst"){

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    # get rid of NOTEs in R CMD check for "undefined global functions or variables"
    utils::globalVariables(
      c("path_input","path_data","path_fun","path_idx","path_data_training","path_output_index",
        "path_OTB","path_output","path_run","path_tmp","sagaCmd","projRootDir","zipfn",
        "Fusion","projViewOutput","glcm")

    )

  invisible()
}



s<-sessionInfo()
