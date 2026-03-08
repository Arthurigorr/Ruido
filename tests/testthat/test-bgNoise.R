test_that("bgNoise() stops at wrong audio format", {
  expect_error(bgNoise("made/up/for/tests.png"))
})

test_that("bgNoise() reads tuneR Wave objects", {

  samprate <- 12050
  dur <- 60
  n <- samprate * dur

  set.seed(413)
  noise <- rnorm(n)

  fade <- seq(1, 0, length.out = n)

  signal <- noise * fade

  wave <- tuneR::Wave(
    left = signal,
    right = signal,
    samp.rate = samprate,
    bit = 16
  )

  bgn <- bgNoise(wave)

  show(bgn)

  plot(bgn, yunit = "khz")

  expect_s4_class(bgn, "noise.matrix")

})

test_that("bgNoise reads audio files directly", {

  dir <- paste(tempdir(), "forExample", sep = "/")
  dir.create(dir)
  rec <- paste0("GAL24576_20250401_", sprintf("%06d", 0), ".wav")
  recDir <- paste(dir, rec , sep = "/")
  url <- paste0("https://zenodo.org/records/17575795/files/",
                rec,
                "?download=1")

  download.file(url, destfile = recDir, mode = "wb")

  bgn <- bgNoise(recDir)

  show(bgn)

  expect_s4_class(bgn, "noise.matrix")

})

test_that("noise.matrix objects can be plotted and printed", {

  show(sampleBGN)

  plot(sampleBGN)

})
