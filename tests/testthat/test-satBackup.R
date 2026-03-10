test_that("satBackup can be tricked!", {

  options(timeout = 500)

  dir <- paste(tempdir(), "forExample", sep = "/")
  dir.create(dir)
  recName <- paste0("GAL24576_20250401_", sprintf("%06d", seq(0, 200000, by = 50000)),".wav")
  recDir <- paste(dir, recName, sep = "/")

  for(rec in recDir) {
    print(rec)
    url <- paste0("https://zenodo.org/records/17575795/files/", basename(rec), "?download=1")
    download.file(url, destfile = rec, mode = "wb")
  }

  mockupBackup <- list()

  mockupBackup[["ogARGS"]] <- list(
    channel = "stereo",
    timeBin = 60,
    dbThreshold = -90,
    targetSampRate = NULL,
    wl = 512,
    window = signal::hamming(512),
    overlap = 256,
    histbreaks = "FD",
    DCfix = TRUE,
    powthr = c(5, 20, 1),
    bgnthr = c(0.5, 0.9, 0.05),
    normality = "ad.test",
    beta = TRUE,
    type = "soundSat",
    od = dir,
    nFiles = length(list.files(dir, pattern = ".wav")),
    concluded = 1
  )

  backup <- paste0(dir, "/SATBACKUP.RData")

  saveRDS(mockupBackup, file = backup)

  expect_type(satBackup(backup), "list")

})
