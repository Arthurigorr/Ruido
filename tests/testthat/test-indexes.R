test_that("acoustic indices functions works correctly", {

  library(tuneR)

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
  wave2 = tuneR::Wave(left = signal,
                      samp.rate = samprate,
                      bit = 16)
  wave3 = tuneR::Wave(
    left = rep(c(1, 0), length.out = samprate * dur),
    right = rep(c(0, 1), length.out = samprate * dur),
    samp.rate = samprate,
    bit = 16
  )
  wave4 = tuneR::Wave(
    left = signal,
    right = rev(signal),
    samp.rate = samprate,
    bit = 8
  )

  mockupBackup = list()

  mockupBackup[["ogARGS"]] = list(
    channel = "stereo",
    timeBin = 60,
    dbThreshold = -90,
    targetSampRate = NULL,
    wl = 512,
    window = hamming(512),
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

  expect_s4_class(ACIspec(recDir[1]), "noise.matrix")
  expect_s4_class(ACIspec(recDir[1], channel = "mono"), "noise.matrix")
  expect_s4_class(ACIspec(recDir[1], j = 800), "noise.matrix")
  expect_s4_class(ACIspec(recDir[1], targetSampRate = 12250), "noise.matrix")
  expect_s4_class(ACIspec(wave1), "noise.matrix")
  expect_s4_class(ACIspec(wave2), "noise.matrix")
  expect_error(ACIspec(recDir, j = "five"))
  expect_error(ACIspec(recDir, wl = -100))
  expect_error(ACIspec("completely-made-up-for-example.png"))

  expect_s4_class(activity(wave1), "noise.matrix")
  expect_s4_class(activity(wave2), "noise.matrix")
  expect_s4_class(activity(wave1, beta = FALSE), "noise.matrix")
  expect_s4_class(activity(sampleBGN), "noise.matrix")

  expect_type(bgn(recDir[1]), "list")
  expect_type(bgn(wave1), "list")
  expect_type(bgn(wave1, timeBin = NULL), "list")
  expect_type(bgn(wave1, channel = "mono"), "list")
  expect_type(bgn(wave2), "list")
  expect_type(bgn(wave2, wl = 256), "list")
  expect_type(bgn(wave3, channel = "right"), "list")
  expect_type(bgn(wave3, DCfix = FALSE, dbThreshold = -60), "list")
  expect_type(bgn(wave3, histbreaks = 100), "list")
  expect_type(bgn(wave4, timeBin = 10), "list")
  expect_type(bgn(wave4, timeBin = 180), "list")
  expect_type(bgn(wave5, targetSampRate = samprate / 2), "list")
  expect_type(bgn(wave5, targetSampRate = samprate * 2), "list")
  expect_type(
    bgn(
      wave1,
      channel = "mono",
      timeBin = 10,
      dbThreshold = -50,
      targetSampRate = 6025,
      wl = 256,
      histbreaks = "Sturges",
      DCfix = FALSE
    ),
    "list"
  )
  expect_error(bgn("made-up-nonsense.png"))

  bgn1 = bgNoise(wave1)
  bgn2 = bgNoise(wave2, channel = "mono")
  bgn3 = bgNoise(wave1, timeBin = 10)
  bgn4 = bgNoise(wave1, timeBin = 30)
  bgn5 = bgNoise(wave2, timeBin = 10)
  bgn6 = bgNoise(wave3)
  bgn7 = bgNoise(wave4)
  bgn8 = bgNoise(recDir[1])

  show(bgn1)
  show(bgn2)
  show(bgn3)
  show(bgn4)
  show(bgn5)
  show(bgn6)
  show(bgn7)
  show(bgn8)

  plot(bgn1, yunit = "khz")
  plot(bgn2)
  plot(bgn3, index = "POW")
  plot(bgn4, channel = "left")
  plot(bgn5)
  plot(bgn6)
  plot(bgn7)

  expect_s4_class(bgn1, "noise.matrix")
  expect_s4_class(bgn2, "noise.matrix")
  expect_s4_class(bgn3, "noise.matrix")
  expect_s4_class(bgn4, "noise.matrix")
  expect_s4_class(bgn5, "noise.matrix")
  expect_s4_class(bgn6, "noise.matrix")
  expect_s4_class(bgn7, "noise.matrix")
  expect_s4_class(bgn8, "noise.matrix")
  expect_s4_class(bgNoise(wave1, timeBin = 30, channel = "left"), "noise.matrix")
  expect_s4_class(bgNoise(wave1, timeBin = 30, channel = "left"), "noise.matrix")
  expect_s4_class(bgNoise(wave1, timeBin = 30, channel = "right"), "noise.matrix")
  expect_s4_class(bgNoise(wave1, targetSampRate = 6650), "noise.matrix")
  expect_s4_class(bgNoise(wave1, window = hamming(512)), "noise.matrix")
  expect_error(bgNoise(wave1, window = hamming(-5)))
  expect_error(bgNoise(wave1, wl = -7))
  expect_s4_class(bgNoise(wave1, histbreaks = 100), "noise.matrix")
  expect_s4_class(bgNoise(wave1, histbreaks = "Sturges"), "noise.matrix")
  expect_s4_class(bgNoise(wave1, histbreaks = "scott"), "noise.matrix")
  expect_s4_class(bgNoise(wave1, DCfix = FALSE), "noise.matrix")
  expect_s4_class(bgNoise(wave1, timeBin = NULL), "noise.matrix")
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
                          window = hanning(256),
                          overlap = ceiling(length(256)/2),
                          histbreaks = "scott",
                          DCfix = FALSE), "noise.matrix")
  expect_s4_class(bgNoise(wave2, channel = "stereo"), "noise.matrix")
  expect_error(bgNoise("made/up/for/tests.png"))

  show(sampleBGN)

  plot(sampleBGN)

  expect_s4_class(ENTspec(recDir), "noise.matrix")
  expect_s4_class(ENTspec(recDir, channel = "mono"), "noise.matrix")
  expect_error(ENTspec(recDir, j = 800))
  expect_s4_class(ENTspec(recDir, targetSampRate = 12250), "noise.matrix")
  expect_s4_class(ENTspec(wave1), "noise.matrix")
  expect_s4_class(ENTspec(wave2), "noise.matrix")
  expect_error(ENTspec(recDir, wl = -100))
  expect_error(ENTspec("completely-made-up-for-example.png"))
  expect_s4_class(ENTspec(recDir, timeBin = NULL), "noise.matrix")

  expect_type(multActivity(dir), "list")
  expect_type(multActivity(dir, channel = "right"), "list")
  expect_type(multActivity(dir, backup = dir), "list")

  backup = paste0(dir, "/SATBACKUP.RData")

  saveRDS(mockupBackup, file = backup)

  expect_type(satBackup(backup), "list")

  expect_type(singleSat(sampleBGN), "list")
  expect_type(singleSat(sampleBGN, channel = "mono"), "list")
  expect_type(singleSat(activity(sampleBGN)), "list")

  expect_type(singleSat(sampleBGN, beta = FALSE), "list")

  expect_type(singleSat(wave1), "list")

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
  expect_error(singleSat(sampleBGN, powthr = -1))
  expect_error(singleSat(sampleBGN, bgnthr = "three"))
  expect_error(singleSat(sampleBGN, bgnthr = c(10, 2, 1)))
  expect_error(singleSat(sampleBGN, bgnthr = c(1, 5, "three")))
  expect_error(singleSat(sampleBGN, bgnthr = c(-5, -1, -2)))
  expect_error(singleSat(sampleBGN, bgnthr = c(4, 6)))
  expect_error(singleSat(sampleBGN, bgnthr = -1))
  expect_error(singleSat(sampleBGN, beta = 5))
  expect_error(singleSat("empty.wav"))

  expect_type(soundMat(dir), "list")
  expect_type(soundMat(dir, beta = FALSE), "list")
  expect_type(soundMat(dir, channel = "mono"), "list")
  expect_type(soundMat(dir, backup = dir), "list")

  expect_type(soundSat(dir), "list")
  expect_type(soundSat(dir, channel = "mono"), "list")
  expect_error(soundSat(dir, powthr = "three"))
  expect_error(soundSat(dir, powthr = c(10, 2, 1)))
  expect_error(soundSat(dir, powthr = c(1, 5, "three")))
  expect_error(soundSat(dir, powthr = c(-5, -1, -2)))
  expect_error(soundSat(dir, powthr = c(4, 6)))
  expect_error(soundSat(dir, bgnthr = "three"))
  expect_error(soundSat(dir, bgnthr = c(10, 2, 1)))
  expect_error(soundSat(dir, bgnthr = c(1, 5, "three")))
  expect_error(soundSat(dir, bgnthr = c(-5, -1, -2)))
  expect_error(soundSat(dir, bgnthr = c(4, 6)))
  expect_error(soundSat(dir, beta = 5))
  expect_type(soundSat(dir, backup = dir), "list")

})
