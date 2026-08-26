test_that("bgNoise() stops at wrong audio format", {
  expect_error(bgNoise("made/up/for/tests.png"))
})

test_that("bgNoise() reads tuneR Wave objects", {

  samprate = 12050
  dur = 60
  n = samprate * dur

  set.seed(413)
  noise = rnorm(n)

  fade = seq(1, 0, length.out = n)

  signal = noise * fade

  wave1 = tuneR::Wave(
    left = signal,
    right = signal,
    samp.rate = samprate,
    bit = 16
  )
  wave2 = tuneR::Wave(
    left = signal,
    samp.rate = samprate,
    bit = 16
  )

  bgn1 = bgNoise(wave1)
  bgn2 = bgNoise(wave2, channel = "mono")
  bgn3 = bgNoise(wave1, timeBin = 10)
  bgn4 = bgNoise(wave1, timeBin = 30)

  show(bgn1)
  show(bgn2)
  show(bgn3)
  show(bgn4)

  plot(bgn1, yunit = "khz")
  plot(bgn2)
  plot(bgn3, index = "POW")
  plot(bgn4, channel = "left")

  expect_s4_class(bgn1, "noise.matrix")
  expect_s4_class(bgn2, "noise.matrix")
  expect_s4_class(bgn3, "noise.matrix")
  expect_s4_class(bgn4, "noise.matrix")
  expect_s4_class(bgNoise(wave1, timeBin = 30, channel = "left"), "noise.matrix")
  expect_s4_class(bgNoise(wave1, timeBin = 30, channel = "right"), "noise.matrix")
  expect_s4_class(bgNoise(wave1, targetSampRate = 6650), "noise.matrix")
  expect_s4_class(bgNoise(wave1, window = signal::hanning(512)), "noise.matrix")
  expect_s4_class(bgNoise(wave1, histbreaks = 100), "noise.matrix")
  expect_s4_class(bgNoise(wave1, histbreaks = "Sturges"), "noise.matrix")
  expect_s4_class(bgNoise(wave1, DCfix = FALSE), "noise.matrix")
  expect_s4_class(bgNoise(wave1, wl = 256), "noise.matrix")
  expect_s4_class(bgNoise(wave1, dbThreshold = -60), "noise.matrix")
  expect_s4_class(bgNoise(wave1, overlap = 128), "noise.matrix")
  expect_s4_class(bgNoise(wave1, channel = "mono", targetSampRate = 6650), "noise.matrix")
  expect_s4_class(bgNoise(wave1,
                          channel = "mono",
                          timeBin = 10,
                          dbThreshold = -60,
                          targetSampRate = 6650,
                          wl = 256,
                          window = signal::hanning(256),
                          overlap = ceiling(length(256)/2),
                          histbreaks = "scott",
                          DCfix = FALSE), "noise.matrix")

})

test_that("bgNoise reads audio files directly", {

  dir = paste(tempdir(), "forExample", sep = "/")
  dir.create(dir)
  rec = paste0("GAL24576_20250401_", sprintf("%06d", 0), ".wav")
  recDir = paste(dir, rec , sep = "/")
  url = paste0("https://zenodo.org/records/17575795/files/",
                rec,
                "?download=1")

  download.file(url, destfile = recDir, mode = "wb")

  bgn = bgNoise(recDir)

  show(bgn)

  expect_s4_class(bgn, "noise.matrix")

})

test_that("noise.matrix objects can be plotted and printed", {

  show(sampleBGN)

  plot(sampleBGN)

})
