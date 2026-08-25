test_that("ACIspec reads audio files directly and can pass down arugments to argHandler()", {

  dir <- paste(tempdir(), "forExample", sep = "/")
  dir.create(dir)
  rec <- paste0("GAL24576_20250401_", sprintf("%06d", 0), ".wav")
  recDir <- paste(dir, rec , sep = "/")
  url <- paste0("https://zenodo.org/records/17575795/files/",
                rec,
                "?download=1")

  download.file(url, destfile = recDir, mode = "wb")

  aci <- ACIspec(recDir)

  show(aci)

  expect_s4_class(aci, "noise.matrix")
  expect_error(ACIspec(recDir, j = "five"))

})
