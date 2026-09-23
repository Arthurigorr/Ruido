test_that("ACIspec() reads audio files directly and from tuneR Wave objects. It can pass down arguments to argHandler()", {

  library(tuneR)

  dir = paste(tempdir(), "forExample", sep = "/")
  dir.create(dir)
  rec = paste0("GAL24576_20250401_", sprintf("%06d", 0), ".wav")
  recDir = paste(dir, rec , sep = "/")
  url = paste0("https://zenodo.org/records/17575795/files/",
                rec,
                "?download=1")

  download.file(url, destfile = recDir, mode = "wb")

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
  wave2 = mono(wave1)

  expect_s4_class(ACIspec(recDir), "noise.matrix")
  expect_s4_class(ACIspec(recDir, channel = "mono"), "noise.matrix")
  expect_s4_class(ACIspec(recDir, j = 800), "noise.matrix")
  expect_s4_class(ACIspec(recDir, targetSampRate = 12250), "noise.matrix")
  expect_s4_class(ACIspec(wave1), "noise.matrix")
  expect_s4_class(ACIspec(wave2), "noise.matrix")
  expect_error(ACIspec(recDir, j = "five"))
  expect_error(ACIspec(recDir, wl = -100))
  expect_error(ACIspec("completely-made-up-for-example.png"))

})
