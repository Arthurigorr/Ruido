test_that("soundSat() exports a list correctly", {
  options(timeout = 500)
  dir = paste(tempdir(), "forExample", sep = "/")
  dir.create(dir)
  recName = paste0("GAL24576_20250401_", sprintf("%06d", seq(0, 200000, by = 50000)),".wav")
  recDir = paste(dir, recName, sep = "/")

  for(rec in recDir) {
   print(rec)
   url = paste0("https://zenodo.org/records/17575795/files/", basename(rec), "?download=1")
   download.file(url, destfile = rec, mode = "wb")
  }

  expect_type(soundSat(dir), "list")
})
