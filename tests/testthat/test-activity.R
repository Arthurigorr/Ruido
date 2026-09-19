test_that("activity() can read noise.matrix objects and produces another noise.matrix object correctly", {

  expect_s4_class(activity(sampleBGN), "noise.matrix")

})

test_that("activity() can read tuneR Wave class objects", {

  samprate = 12050
  dur = 60
  n = samprate * dur

  set.seed(413)
  noise = rnorm(n)

  fade = seq(1, 0, length.out = n)

  signal = noise * fade

  wave = tuneR::Wave(
    left = signal,
    right = signal,
    samp.rate = samprate,
    bit = 16
  )

  expect_s4_class(activity(wave), "noise.matrix")

})
