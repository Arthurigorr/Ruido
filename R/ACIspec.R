#' @title Spectral Acoustic Complexity Index
#'
#' @description TBA
#'
#' @param soundfile tuneR Wave object or path to a valid audio file
#' @param channel channel where the metric values will be extract from. Available channels are: `"stereo"`, `"mono"`, `"left"` or `"right"`. Defaults to `"stereo"`
#' @param timeBin size (in seconds) of the time bin. Set to `NULL` to use the entire audio as a single bin. Defaults to `60`
#' @param j size (in seconds) of the cluster interval. Set to `NULL` to use the entire bin as a single cluster. Defaults to `5`
#' @param weight TBA
#' @param targetSampRate desired sample rate of the audios.  This argument is only used to down sample the audio. If `NULL`, then audio's sample rate remains the same. Defaults to `NULL`
#' @param wl window length of the spectrogram. Defaults to `512`
#' @param window window used to smooth the spectrogram. Switch to `signal::hanning(wl)` to use hanning instead. Defaults to `signal::hammning(wl)`
#' @param overlap overlap between the spectrogram windows. Defaults to `wl/2` (half the window length)
#'
#' @returns This function returns a [noise.matrix-class] object.
#'
#' @details TBA
#'
#' @references
#' Pieretti, N., Farina, A., & Morri, D. (2011). A new methodology to infer the singing activity of an avian community: The Acoustic Complexity Index (ACI). Ecological Indicators, 11(3), 868–873. https://doi.org/10.1016/j.ecolind.2010.11.005
#'
#' @export
#'@importFrom signal specgram
#'@importFrom tuneR readWave
#'@importFrom tuneR readMP3
#'@importFrom tuneR downsample
#'
#' @examples
ACIspec = function(soundfile,
                   channel = "stereo",
                   timeBin = 60,
                   j = 5,
                   weight = FALSE,
                   targetSampRate = NULL,
                   wl = 512,
                   window = signal::hamming(wl),
                   overlap = ceiling(length(window) / 2)) {

  argHandler(FUN = "ACIspec", soundfile, channel, timeBin, j, weight, targetSampRate, wl,
             window, overlap)

  if (!is.null(timeBin)) {
    j <- if (is.null(j)) timeBin else min(j, timeBin)
  }

  audio <- if (is.character(soundfile)) {
    fileExt <- tolower(tools::file_ext(soundfile))
    if (fileExt %in% c("mp3", "wav")) {
      if (fileExt == "mp3") {
        tuneR::readMP3(soundfile)
      } else {
        tuneR::readWave(soundfile)
      }
    } else {
      stop("The audio file must be in MP3 or WAV format.")
    }
  } else {
    soundfile
  }

  if(channel == "mono" && audio@stereo) {
    audio <- tuneR::mono(audio, which = "both")
  }

  if (channel == "stereo" && !audio@stereo) {
    message("Audio is not stereo, defaulting to left channel.")
    channel <- "mono"
  }

  if (!is.null(targetSampRate)) {
    audio <- tuneR::downsample(audio, targetSampRate)
  }

  ACIexp <- processChannel.ACI(
    audio,
    channel = channel,
    timeBin = timeBin,
    j = j,
    weight = weight,
    wl = wl,
    overlap = overlap,
    window = window,
    noiseOBJ = new("noise.matrix")
  )

  if (ACIexp@channel == "stereo") {
    ACIexp@wl <- nrow(ACIexp@values$left$ACI)

  } else {
    ACIexp@wl <- nrow(ACIexp@values[[channel]]$ACI)

  }

  return(ACIexp)

}
