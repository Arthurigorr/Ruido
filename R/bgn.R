#' @title Summarized Background Noise and Soundscape Power Index
#'
#' @description Calculate the summarized Background Noise and Soundscape Power values of a single audio using the methodology proposed in Towsey 2017
#'
#' @param soundfile wav package numeric matrix, tuneR package Wave object or path to a `.wav` file
#' @param channel channel where the metric values will be extracted from. Available channels are: `"stereo"`, `"mono"`, `"left"` or `"right"`. Defaults to `"stereo"`
#' @param timeBin size (in seconds) of the time bin. Set to `NULL` to use the entire audio as a single bin. Defaults to `60`
#' @param dbThreshold minimum allowed value of dB for the spectrograms. Set to `NULL` to leave db values unrestricted Defaults to `-90`, as set by Towsey 2017
#' @param targetSampRate desired sample rate of the audios.  This argument is only used to down sample the audio. If `NULL`, then audio's sample rate remains the same. Defaults to `NULL`
#' @param wl length of each waveform frame in samples. The waveform is divided into non-overlapping frames of this length before the envelope is calculated. Defaults to `512`
#' @param histbreaks breaks used to calculate Background Noise. Available breaks are: `"FD"`, `"Sturges`", `"scott"` and `100`. Defaults to `"FD"`.
#' <br>Can also be set to any numerical value to limit or increase the amount of breaks.
#' @param DCfix if the DC offset should be removed before the metrics are calculated. Defaults to `TRUE`
#'
#' @returns A list containing the `BGN` and `POW` values calculated for each time bin and selected channel.
#'
#' @details Background Noise (`BGN`) is an acoustic metric that estimates the dominant background sound level within a time bin from the waveform amplitude envelope. Following the approach described by Towsey (2017), each time bin is divided into non-overlapping waveform frames, and the maximum absolute amplitude of each frame is used to construct the waveform envelope.
#'
#' For each waveform frame \eqn{i}, the envelope amplitude \eqn{A_i} is converted to decibels as:
#'
#' \deqn{dB_i = 10 \log_{10}(A_i)}
#'
#' where \eqn{A_i} is the maximum absolute amplitude within the frame. Incomplete frames at the end of a time bin are discarded.
#'
#' The resulting dB values are grouped into histogram bins, and `BGN` is estimated from the modal histogram bin, representing the most frequently occurring sound level within the time bin:
#'
#' \deqn{BGN = \mathrm{mode}(dB_i)}
#'
#' `BGN` therefore represents an estimate of the dominant background level of the recording segment, rather than the background level of a particular frequency band.
#'
#' Soundscape Power (`POW`) quantifies the difference between the maximum observed sound level and the estimated background level within the same time bin. It is defined as:
#'
#' \deqn{POW = \max(dB_i) - BGN}
#'
#' Higher `POW` values indicate a greater difference between the strongest acoustic event and the estimated background level.
#'
#' @seealso [bgNoise()] to calculate Spectral Background Noise and Soundscape Power.
#'
#' @references
#' Towsey, M. W. (2017). The calculation of acoustic indices derived from long-duration recordings of the natural environment. In eprints.qut.edu.au. https://eprints.qut.edu.au/110634/
#' <br>Lamel, L., Rabiner, L., Rosenberg, A., & Wilpon, J. (1981). An improved endpoint detector for isolated word recognition. \emph{IEEE Transactions on Acoustics, Speech, and Signal Processing}, 29(4), 777-785 https://doi.org/10.1109/TASSP.1981.1163642
#'
#'@export
#'@importFrom tuneR readWave
#'@importFrom tuneR downsample
#'@importFrom wav read_wav
#'@importFrom grDevices nclass.FD
#'@importFrom grDevices nclass.Sturges
#'@importFrom grDevices nclass.scott
#'@importFrom matrixStats colMaxs
#'
#' @examples
#' ### For our main example we'll create an artificial audio with
#' ### white noise to test its Background Noise
#' # We'll use the package tuneR
#' library(tuneR)
#'
#' # Define the audio sample rate, duration and number of samples
#' samprate = 12050
#' dur = 60
#' n = samprate * dur
#'
#' # Then we generate white noise
#' set.seed(413)
#' noise = rnorm(n)
#'
#' # Linear fade-out envelope
#' fade = seq(1, 0, length.out = n)
#'
#' # Apply fade
#' signal = noise * fade
#'
#' wave = Wave(left = signal, right = signal,
#'             samp.rate = samprate,
#'             bit = 16)
#'
#' # Heres our artificial audio
#'
#' wave
#'
#' # Running the bgNoise function with all the default arguments
#' BGN = bgn(wave)
#'
#' # See the results
#' bgn
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
    wl = wl,
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
      soundfile = rbind(soundfile@left, soundfile@right)
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
    if (DCfix) {
      x = x - mean(x)
    }
    x = abs(x)
    apply(allSamples, 1, function(y) {
      samples = x[y[1]:y[2]]
      mat = matrix(samples[seq_len(floor(length(samples) / wl) * wl)],
                   nrow = wl)
      db = 10 * log10(matrixStats::colMaxs(mat))

      if (!is.null(dbThreshold)) {
        db[db < dbThreshold] = dbThreshold
      }

      dbMin = min(db)
      dbMax = max(db)
      numBins = hBreak(db)
      breaks = seq(dbMin, dbMax, length.out = numBins + 1)
      modalBin = which.max(tabulate(findInterval(x = db, vec = breaks)))
      modalIntensity = dbMin + modalBin * (breaks[2] - breaks[1])
      c(BGN = modalIntensity, POW = dbMax - modalIntensity)
    })

  })

}
