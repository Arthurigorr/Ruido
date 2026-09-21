bgn = function(soundfile,
               channel = "stereo",
               timeBin = 60,
               dbThreshold = -90,
               targetSampRate = NULL,
               wl = 512,
               histbreaks = "FD",
               DCfix = TRUE) {
  argHandler(
    FUN = "bgn",
    soundfile = soundfile,
    channel = channel,
    timeBin = timeBin,
    dbThreshold = dbThreshold,
    histbreaks = histbreaks,
    DCfix = DCfix
  )

  audio = typeof(soundfile)

  if (audio == "character") {
    if (tolower(tools::file_ext(soundfile)) == "wav") {
      soundfile = wav::read_wav(soundfile)
    } else {
      stop("The audio file must be in the WAV format.")
    }
  } else if (audio == "S4") {
    tempSamp = soundfile@samp.rate
    if (soundfile@stereo) {
      soundfile = matrix(c(soundfile@left, soundfile@right),
                         nrow = 2,
                         byrow = TRUE)
    } else {
      soundfile = matrix(soundfile@left, nrow = 1, byrow = TRUE)
    }
    attr(soundfile, "sample_rate") = tempSamp
  }

  savedAttr = attributes(soundfile)

  if (channel == "mono" && savedAttr$dim[1] > 1) {
    sampRate = attr(soundfile, "sample_rate")
    soundfile = matrix((soundfile[1, ] + soundfile[2, ]) / 2, nrow = 1)
    attr(soundfile, "sample_rate") = sampRate
    savedAttr$dim = dim(soundfile)
  }
  if (channel == "stereo" && nrow(soundfile) == 1) {
    message("Audio is not stereo, defaulting to left channel.")
    channel = "mono"
  }
  if (!is.null(targetSampRate)) {
    audioLen = length(soundfile[1, ])
    keepIdx  = round(seq(1, audioLen, length = targetSampRate * audioLen / savedAttr$sample_rate))
    soundfile = soundfile[1:savedAttr$dim[1], keepIdx, drop = FALSE]
    attr(soundfile, "sample_rate") = targetSampRate
  }

  soundfile = abs(soundfile)

  allSamples = if (is.null(timeBin)) {
    data.frame(b = 1, e = length(soundfile[1, ]))
  } else {
    getSampleBins(length(soundfile[1, ]),
                  attr(soundfile, "sample_rate"),
                  timeBin)
  }

  channelData = switch(
    channel,
    "stereo" = list(left  = soundfile[1, ], right = soundfile[2, ]),
    "mono"   = list(mono  = soundfile[1, ]),
    "left"   = list(left  = soundfile[1, ]),
    "right"  = list(right = soundfile[2, ])
  )

  hBreak = hBreaks(histbreaks)

  lapply(channelData, function(x) {
    apply(allSamples, 1, function(y) {
      samples = x[y[1]:y[2]]
      mat = matrix(samples[seq_len(floor(length(samples) / wl) * wl)],
                   nrow = wl)
      db = 10 * log10(apply(mat, 2, max))

      if (!is.null(dbThreshold)) {
        db[db < dbThreshold] = dbThreshold
      }

      dbMin = min(db)
      dbMax = max(db)
      num_bins = hBreak(db)
      breaks = seq(dbMin, dbMax, length.out = num_bins + 1)
      modalBin = which.max(tabulate(findInterval(x = db, vec = breaks)))
      modalIntensity = dbMin + modalBin * (breaks[2] - breaks[1])
      c(BGN = modalIntensity, POW = dbMax - modalIntensity)
    })
  })

}
