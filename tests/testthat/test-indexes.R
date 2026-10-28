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
    dbRef = 10,
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
    saveNoise = TRUE,
    type = "soundSAT",
    od = dir,
    nFiles = length(list.files(dir, pattern = ".wav")),
    concluded = 1
  )

  expect_s4_class(ACIspec(recDir[1]), "noise.matrix")
  expect_s4_class(ACIspec(recDir[1], channel = "mono"), "noise.matrix")
  expect_s4_class(ACIspec(recDir[1], j = 800), "noise.matrix")
  expect_s4_class(ACIspec(recDir[1], targetSampRate = 12250), "noise.matrix")
  expect_s4_class(ACIspec(wave1), "noise.matrix")
  expect_s4_class(ACIspec(wave1, channel = "right"), "noise.matrix")
  expect_s4_class(ACIspec(wave2), "noise.matrix")
  expect_s4_class(ACIspec(wave3), "noise.matrix")
  expect_error(ACIspec(recDir, j = "five"))
  expect_error(ACIspec(recDir, wl = -100))
  expect_error(ACIspec("completely-made-up-for-example.png"))

  expect_s4_class(ACTspec(wave1), "noise.matrix")
  expect_s4_class(ACTspec(wave2), "noise.matrix")
  expect_s4_class(ACTspec(wave1, beta = FALSE), "noise.matrix")
  expect_s4_class(ACTspec(sampleBGN), "noise.matrix")

  expect_type(BGN(recDir[1]), "list")
  expect_type(BGN(wave1), "list")
  expect_type(BGN(wave1, timeBin = NULL), "list")
  expect_type(BGN(wave1, channel = "mono"), "list")
  expect_type(BGN(wave2), "list")
  expect_type(BGN(wave2, wl = 256), "list")
  expect_type(BGN(wave3, channel = "right"), "list")
  expect_type(BGN(wave3, DCfix = FALSE, dbThreshold = -60), "list")
  expect_type(BGN(wave3, histbreaks = 100), "list")
  expect_type(BGN(wave4, timeBin = 10), "list")
  expect_type(BGN(wave4, timeBin = 180), "list")
  expect_type(BGN(wave1, targetSampRate = samprate / 2), "list")
  expect_type(
    BGN(
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
  expect_error(BGN("made-up-nonsense.png"))

  bgn1 = BGNspec(wave1)
  bgn2 = BGNspec(wave2, channel = "mono")
  bgn3 = BGNspec(wave1, timeBin = 10)
  bgn4 = BGNspec(wave1, timeBin = 30)
  bgn5 = BGNspec(wave2, timeBin = 10)
  bgn6 = BGNspec(wave3)
  bgn7 = BGNspec(wave4)
  bgn8 = BGNspec(recDir[1])

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
  plot(bgn8)

  expect_s4_class(bgn1, "noise.matrix")
  expect_s4_class(bgn2, "noise.matrix")
  expect_s4_class(bgn3, "noise.matrix")
  expect_s4_class(bgn4, "noise.matrix")
  expect_s4_class(bgn5, "noise.matrix")
  expect_s4_class(bgn6, "noise.matrix")
  expect_s4_class(bgn7, "noise.matrix")
  expect_s4_class(bgn8, "noise.matrix")
  expect_s4_class(BGNspec(wave1, timeBin = 30, channel = "left"), "noise.matrix")
  expect_s4_class(BGNspec(wave1, timeBin = 30, channel = "left"), "noise.matrix")
  expect_s4_class(BGNspec(wave1, timeBin = 30, channel = "right"), "noise.matrix")
  expect_s4_class(BGNspec(wave1, targetSampRate = 6650), "noise.matrix")
  expect_s4_class(BGNspec(wave1, window = hamming(512)), "noise.matrix")
  expect_error(BGNspec(wave1, window = hamming(-5)))
  expect_error(BGNspec(wave1, wl = -7))
  expect_s4_class(BGNspec(wave1, histbreaks = 100), "noise.matrix")
  expect_s4_class(BGNspec(wave1, histbreaks = "Sturges"), "noise.matrix")
  expect_s4_class(BGNspec(wave1, histbreaks = "scott"), "noise.matrix")
  expect_s4_class(BGNspec(wave1, DCfix = FALSE), "noise.matrix")
  expect_s4_class(BGNspec(wave1, timeBin = NULL), "noise.matrix")
  expect_s4_class(BGNspec(wave1, wl = 256), "noise.matrix")
  expect_s4_class(BGNspec(wave1, dbRef = 20), "noise.matrix")
  expect_s4_class(BGNspec(wave1, dbThreshold = -60), "noise.matrix")
  expect_s4_class(BGNspec(wave1, overlap = 128), "noise.matrix")
  expect_s4_class(BGNspec(wave1, channel = "mono", targetSampRate = 6650), "noise.matrix")
  expect_s4_class(BGNspec(wave1,
                          channel = "mono",
                          timeBin = 10,
                          dbThreshold = -60,
                          targetSampRate = 6650,
                          wl = 256,
                          window = hanning(256),
                          overlap = ceiling(length(256)/2),
                          histbreaks = "scott",
                          DCfix = FALSE), "noise.matrix")
  expect_s4_class(BGNspec(wave2, channel = "stereo"), "noise.matrix")
  expect_error(BGNspec("made/up/for/tests.png"))

  show(sampleBGN)

  plot(sampleBGN)

  expect_s4_class(ENTspec(recDir[[1]]), "noise.matrix")
  expect_s4_class(ENTspec(recDir[[1]], channel = "mono"), "noise.matrix")
  expect_error(ENTspec(recDir[[1]], j = 800))
  expect_s4_class(ENTspec(recDir[[1]], targetSampRate = 12250), "noise.matrix")
  expect_s4_class(ENTspec(wave1), "noise.matrix")
  expect_s4_class(ENTspec(wave2), "noise.matrix")
  expect_error(ENTspec(recDir[[1]], wl = -100))
  expect_error(ENTspec("completely-made-up-for-example.png"))
  expect_s4_class(ENTspec(recDir[[1]], timeBin = NULL), "noise.matrix")

  expect_s4_class(EVNspec(recDir[[1]]), "noise.matrix")
  expect_s4_class(EVNspec(recDir[[1]], channel = "mono"), "noise.matrix")
  expect_error(EVNspec(recDir[[1]], j = 800))
  expect_s4_class(EVNspec(recDir[[1]], targetSampRate = 12250), "noise.matrix")
  expect_s4_class(EVNspec(wave1), "noise.matrix")
  expect_s4_class(EVNspec(wave2), "noise.matrix")
  expect_error(EVNspec(recDir[[1]], wl = -100))
  expect_error(EVNspec("completely-made-up-for-example.png"))
  expect_s4_class(EVNspec(recDir[[1]], timeBin = NULL), "noise.matrix")

  expect_type(soundACT(dir), "list")
  expect_type(soundACT(dir, channel = "right"), "list")
  expect_type(soundACT(dir, backup = dir), "list")

  backup = paste0(dir, "/SATBACKUP.RData")

  saveRDS(mockupBackup, file = backup)

  expect_type(satBackup(backup), "list")

  expect_type(SAT(sampleBGN), "list")
  expect_type(SAT(sampleBGN, channel = "mono"), "list")
  expect_type(SAT(ACTspec(sampleBGN)), "list")

  expect_type(SAT(sampleBGN, beta = FALSE), "list")

  expect_type(SAT(wave1), "list")

  expect_error(SAT(sampleBGN, channel = 1))
  expect_error(SAT(sampleBGN, timeBin = "one"))
  expect_error(SAT(sampleBGN, dbThreshold = 50))
  expect_error(SAT(sampleBGN, targetSampRate = -50))
  expect_error(SAT(sampleBGN, wl = -50))
  expect_error(SAT(sampleBGN, wl = "empty"))
  expect_error(SAT(sampleBGN, window = c(1, 3, 5, 9)))
  expect_error(SAT(sampleBGN, overlap = -50))
  expect_error(SAT(sampleBGN, histbreaks = "two"))
  expect_error(SAT(sampleBGN, DCfix = "both"))
  expect_error(SAT(sampleBGN, powthr = "three"))
  expect_error(SAT(sampleBGN, powthr = c(10, 2, 1)))
  expect_error(SAT(sampleBGN, powthr = c(1, 5, "three")))
  expect_error(SAT(sampleBGN, powthr = c(-5, -1, -2)))
  expect_error(SAT(sampleBGN, powthr = c(4, 6)))
  expect_error(SAT(sampleBGN, powthr = -1))
  expect_error(SAT(sampleBGN, bgnthr = "three"))
  expect_error(SAT(sampleBGN, bgnthr = c(10, 2, 1)))
  expect_error(SAT(sampleBGN, bgnthr = c(1, 5, "three")))
  expect_error(SAT(sampleBGN, bgnthr = c(-5, -1, -2)))
  expect_error(SAT(sampleBGN, bgnthr = c(4, 6)))
  expect_error(SAT(sampleBGN, bgnthr = -1))
  expect_error(SAT(sampleBGN, beta = 5))
  expect_error(SAT("empty.wav"))

  expect_type(soundMAT(dir), "list")
  expect_type(soundMAT(dir, beta = FALSE), "list")
  expect_type(soundMAT(dir, channel = "mono"), "list")
  expect_type(soundMAT(dir, backup = dir), "list")

  expect_type(soundSAT(dir), "list")
  expect_type(soundSAT(dir, channel = "mono"), "list")
  expect_error(soundSAT(dir, powthr = "three"))
  expect_error(soundSAT(dir, powthr = c(10, 2, 1)))
  expect_error(soundSAT(dir, powthr = c(1, 5, "three")))
  expect_error(soundSAT(dir, powthr = c(-5, -1, -2)))
  expect_error(soundSAT(dir, powthr = c(4, 6)))
  expect_error(soundSAT(dir, bgnthr = "three"))
  expect_error(soundSAT(dir, bgnthr = c(10, 2, 1)))
  expect_error(soundSAT(dir, bgnthr = c(1, 5, "three")))
  expect_error(soundSAT(dir, bgnthr = c(-5, -1, -2)))
  expect_error(soundSAT(dir, bgnthr = c(4, 6)))
  expect_error(soundSAT(dir, beta = 5))
  expect_type(soundSAT(dir, backup = dir), "list")
  expect_type(soundSAT(dir, normality = "ad.test"), "list")
  expect_type(soundSAT(dir, normality = "cvm.test"), "list")
  expect_type(soundSAT(dir, normality = "shapiro.test"), "list")
  expect_type(soundSAT(dir, saveNoise = TRUE), "list")

  expectedpath = recDir[[1]]
  expectedchannel = "stereo"

  BGNTest = BGNspec(recDir[[1]])
  ACITest = ACIspec(recDir[[1]])
  EVNTest = EVNspec(recDir[[1]])
  ACTTest = ACTspec(recDir[[1]])
  ENTTest = ENTspec(recDir[[1]])

  expect_equal(BGNTest@path, expectedpath)
  expect_equal(ACITest@path, expectedpath)
  expect_equal(EVNTest@path, expectedpath)
  expect_equal(ACTTest@path, expectedpath)
  expect_equal(ENTTest@path, expectedpath)

  expect_equal(BGNTest@channel, expectedchannel)
  expect_equal(ACITest@channel, expectedchannel)
  expect_equal(EVNTest@channel, expectedchannel)
  expect_equal(ACTTest@channel, expectedchannel)
  expect_equal(ENTTest@channel, expectedchannel)
})
