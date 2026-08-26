test_that("soundMat() exports a list and can pass arguments down to argHandler() correctly", {
  options(timeout = 500)

  dir = paste(tempdir(), "forExample", sep = "/")
  dir.create(dir)
  recName = paste0("GAL24576_20250401_", sprintf("%06d", seq(0, 200000, by = 50000)), ".wav")
  recDir = paste(dir, recName, sep = "/")

  for (rec in recName) {
    print(rec)
    url = paste0("https://zenodo.org/records/17575795/files/",
                  rec,
                  "?download=1")
    download.file(url, destfile = paste(dir, rec, sep = "/"), mode = "wb")
  }

  expect_type(soundMat(dir, beta = TRUE), "list")
})
