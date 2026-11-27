# R/common.R (formatters, the session count) into the global environment, so
# every R file the tests source finds it, as the pipeline's tar_source() does.
sys.source(testthat::test_path("..", "..", "R", "common.R"), envir = globalenv())
