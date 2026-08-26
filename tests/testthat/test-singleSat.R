test_that("singleSat() reads noise.matrix objects", {

  expect_type(singleSat(sampleBGN), "list")

})

test_that("Argument beta in singleSat() works", {

  expect_type(singleSat(sampleBGN, beta = FALSE), "list")

})

test_that("singleSat() can read tuneR Wave class objects", {

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

  expect_type(singleSat(wave), "list")

})

test_that("singleSat() can pass down arguments to argHandler()",
          {

            expect_error(singleSat(sampleBGN, channel = 1))
            expect_error(singleSat(sampleBGN, timeBin = "one"))
            expect_error(singleSat(sampleBGN, dbThreshold = 50))
            expect_error(singleSat(sampleBGN, targetSampRate = -50))
            expect_error(singleSat(sampleBGN, wl = -50))
            expect_error(singleSat(sampleBGN, wl = "empty"))
            expect_error(singleSat(sampleBGN, window = c(1, 3, 5, 9)))
            expect_error(singleSat(sampleBGN, overlap = -50))
            expect_error(singleSat(sampleBGN, histbreaks = "two"))
            expect_error(singleSat(sampleBGN, DCfix = "both"))
            expect_error(singleSat(sampleBGN, powthr = "three"))
            expect_error(singleSat(sampleBGN, powthr = c(10, 2, 1)))
            expect_error(singleSat(sampleBGN, powthr = c(1, 5, "three")))
            expect_error(singleSat(sampleBGN, powthr = c(-5, -1, -2)))
            expect_error(singleSat(sampleBGN, powthr = c(4, 6)))
            expect_error(singleSat(sampleBGN, bgnthr = "three"))
            expect_error(singleSat(sampleBGN, bgnthr = c(10, 2, 1)))
            expect_error(singleSat(sampleBGN, bgnthr = c(1, 5, "three")))
            expect_error(singleSat(sampleBGN, bgnthr = c(-5, -1, -2)))
            expect_error(singleSat(sampleBGN, bgnthr = c(4, 6)))
            expect_error(singleSat(sampleBGN, beta = 5))
            expect_error(singleSat("empty.wav"))

          })
