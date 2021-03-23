test_that("stopwatchR", {

  start_time()

  Sys.sleep(1)
  elapsed_time <- capture.output(paste(split_time()))[1]
  elapsed_time <- as.numeric(strsplit(elapsed_time, " ")[[1]][1])
  expect_equal(elapsed_time, 1, tolerance = 0.1)

  Sys.sleep(2)

  elapsed_time <- capture.output(paste(split_time()))[1]
  elapsed_time <- as.numeric(strsplit(elapsed_time, " ")[[1]][1])
  expect_equal(elapsed_time, 2, tolerance = 0.1)

  Sys.sleep(3)

  elapsed_time <- capture.output(paste(stop_time()))[1]
  elapsed_time <- as.numeric(strsplit(elapsed_time, " ")[[1]][1])
  expect_equal(elapsed_time, 3, tolerance = 0.1)
})


