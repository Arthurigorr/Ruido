test_that("bgn can input tuneR objects, work with default arguments and alternative arguments", {

  library(tuneR)
  samprate = 12050
  dur = 60
  n = samprate * dur
  set.seed(413)
  noise = rnorm(n)
  fade = seq(1, 0, length.out = n)
  signal = noise * fade

  wave1 = Wave(left = signal, right = signal,
              samp.rate = samprate,
              bit = 16)
  wave2 = mono(wave1)

  expect_type(bgn(wave1), "list")
  expect_type(bgn(wave1, timeBin = NULL), "list")
  expect_type(bgn(wave1, channel = "mono"), "list")
  expect_type(bgn(wave2), "list")
  expect_type(bgn(wave2, wl = 256), "list")
  expect_error(bgn("made-up-nonsense.png"))

})
