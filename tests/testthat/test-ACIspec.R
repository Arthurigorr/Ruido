test_that("ACIspec reads audio files directly and can pass down arugments to argHandler()", {

  dir <- paste(tempdir(), "forExample", sep = "/")
  dir.create(dir)
  rec <- paste0("GAL24576_20250401_", sprintf("%06d", 0), ".wav")
  recDir <- paste(dir, rec , sep = "/")
  url <- paste0("https://zenodo.org/records/17575795/files/",
                rec,
                "?download=1")

  download.file(url, destfile = recDir, mode = "wb")

  expect_s4_class(ACIspec(recDir), "noise.matrix")
  expect_s4_class(ACIspec(recDir, channel = "mono"), "noise.matrix")
  expect_s4_class(ACIspec(recDir, j = 800), "noise.matrix")
  expect_s4_class(ACIspec(recDir, targetSampRate = 12250), "noise.matrix")
  expect_error(ACIspec(recDir, j = "five"))

})
