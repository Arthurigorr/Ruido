dir <- paste(tempdir(), "forExample", sep = "/")
dir.create(dir)
rec <- paste0("GAL24576_20250401_", sprintf("%06d", 0), ".wav")
recDir <- paste(dir, rec , sep = "/")
url <- paste0("https://zenodo.org/records/17243660/files/",
              rec,
              "?download=1")

download.file(url, destfile = recDir, mode = "wb")

sampleBGN <- Ruido::bgNoise(recDir)

usethis::use_data(sampleBGN, overwrite = T)
