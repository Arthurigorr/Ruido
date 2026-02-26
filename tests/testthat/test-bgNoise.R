testWave <-

test_that("bgNoise() stops at wrong audio format", {
  expect_error(bgNoise("made/up/for/tests.png"))
})

test_that("bgNoise() reads tuneR wave objects", {

  samprate <- 12050
  dur <- 60
  n <- samprate * dur

  noise <- rnorm(n)

  signal <- noise

  wave <- tuneR::Wave(
    left = signal,
    samp.rate = samprate,
    bit = 16
  )

  result <- bgNoise(wave, channel = "left")

  expect_equal(bgNoise(wave, channel = "left"), result)

})
