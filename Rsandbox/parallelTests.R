library(parallel)

BGNspecPARALLEL = function(soundfile,
                   channel = "stereo",
                   timeBin = 60,
                   dbThreshold = -90,
                   dbRef = 10,
                   targetSampRate = NULL,
                   wl = 512,
                   window = hamming(wl),
                   overlap = ceiling(length(window) / 2),
                   histbreaks = "FD",
                   DCfix = TRUE,
                   nCores = parallel::detectCores() - 1) {
  argHandler(
    FUN = "BGNspec",
    soundfile = soundfile,
    channel = channel,
    timeBin = timeBin,
    dbThreshold = dbThreshold,
    dbRef = dbRef,
    targetSampRate = targetSampRate,
    wl = wl,
    window = window,
    overlap = overlap,
    histbreaks = histbreaks,
    DCfix = DCfix
  )

  channelData = .importAudio(soundfile = soundfile,
                             channel = channel,
                             targetSampRate = targetSampRate)

  BGNexp = processChannel.BGNPARALLEL(
    channelData,
    samp.rate = attr(channelData, "sample_rate"),
    channel = attr(channelData, "channel"),
    timeBin = timeBin,
    wl = wl,
    overlap = overlap,
    dbThreshold = dbThreshold,
    dbRef = dbRef,
    window = window,
    histbreaks = histbreaks,
    DCfix = DCfix,
    nCores = nCores,
    noiseOBJ = new("noise.matrix")
  )

  BGNexp@wl = if (BGNexp@channel == "stereo") {
    nrow(BGNexp@values$left$BGN)
  } else {
    nrow(BGNexp@values[[BGNexp@channel]]$BGN)
  }

  BGNexp@path = attr(channelData, "path")

  return(BGNexp)

}

processChannel.BGNPARALLEL = function(channelData,
                              samp.rate,
                              channel,
                              timeBin,
                              wl,
                              overlap,
                              dbThreshold,
                              dbRef,
                              window,
                              histbreaks,
                              DCfix,
                              nCores,
                              noiseOBJ) {
  allSamples = if (is.null(timeBin)) {
    data.frame(b = 1, e = length(channelData[1, ]))
  } else {
    getSampleBins(length(channelData[1, ]), samp.rate, timeBin)
  }

  frameBin = nrow(allSamples)

  hBreak = hBreaks.MAT(histbreaks)

  channelData = switch(
    channel,
    "stereo" = list(left  = channelData[1, ], right = channelData[2, ]),
    "mono"   = list(mono  = channelData[1, ]),
    "left"   = list(left  = channelData[1, ]),
    "right"  = list(right = channelData[2, ])
  )

  if (DCfix) {
    channelData = lapply(channelData, function(x)
      x - mean(x))
  }

  bVec = allSamples$b
  eVec = allSamples$e

  noiseOBJ@values = lapply(channelData, function(x) {

    BGNPOWdf = parallel::mclapply(seq_along(bVec), function(i) {
      tempHolder = abs(.spect(
        x = x[bVec[i]:eVec[i]],
        n = wl,
        window = window,
        overlap = overlap
      ))
      singleBin = dbRef * log10(tempHolder / max(tempHolder))
      if (!is.null(dbThreshold)) {
        singleBin[singleBin < dbThreshold] = dbThreshold
      }

      dbDif = matrixStats::rowRanges(singleBin)
      dbMax = dbDif[, 2]
      dbMin = dbDif[, 1]

      numBins = hBreak(singleBin)
      binWidth = (dbMax - dbMin) / numBins
      bins = floor((singleBin - dbMin) / binWidth) + 1L

      modalBin = vapply(seq_len(wl / 2), function(i) {
        which.max(tabulate(bins[i, ], nbins = numBins[i]))
      }, integer(1))
      modalIntensity = dbMin + (modalBin - 0.5) * binWidth
      rbind(BGN = modalIntensity, POW = dbMax - modalIntensity)
    }, mc.cores = nCores)

    return(list(
      BGN = as.data.frame(do.call(
        cbind, lapply(BGNPOWdf, function(df)
          df[1, ])
      )) |>
        setNames(paste0("BGN", 1:frameBin)),
      POW = as.data.frame(do.call(
        cbind, lapply(BGNPOWdf, function(df)
          df[2, ])
      )) |>
        setNames(paste0("POW", 1:frameBin))
    ))

  })

  noiseOBJ@index = c("BGN", "POW")
  noiseOBJ@timeBins = setNames(round((allSamples$e - allSamples$b) / samp.rate), paste0("BIN", seq(frameBin)))
  noiseOBJ@sampRate = samp.rate
  noiseOBJ@channel = channel

  return(noiseOBJ)

}
