#' @title Spectral Acoustic Complexity Index
#'
#' @description Calculate the Acoustic Complexity Index values of a single audio using the methodology proposed in Pieretti, et al. 2011
#'
#' @param soundfile wav package numeric matrix, tuneR package Wave object or path to a valid audio file
#' @param channel channel where the metric values will be extract from. Available channels are: `"stereo"`, `"mono"`, `"left"` or `"right"`. Defaults to `"stereo"`
#' @param timeBin size (in seconds) of the time bin. Set to `NULL` to use the entire audio as a single bin. Defaults to `60`
#' @param j size (in seconds) of the cluster interval. Set to `NULL` to use the entire bin as a single cluster. Defaults to `5`
#' @param targetSampRate desired sample rate of the audios.  This argument is only used to down sample the audio. If `NULL`, then audio's sample rate remains the same. Defaults to `NULL`
#' @param wl window length of the spectrogram. Defaults to `512`
#' @param window window used to smooth the spectrogram. Switch to `signal::hanning(wl)` to use hanning instead. Defaults to `signal::hammning(wl)`
#' @param overlap overlap between the spectrogram windows. Defaults to `wl/2` (half the window length)
#'
#' @returns This function returns a [noise.matrix-class] object.
#'
#' @details The Acoustic Complexity Index (`ACI`) quantifies the average proportional change in spectral amplitude between adjacent time steps  across frequency bins. Because biological sounds, particularly bird vocalizations, often exhibit rapid and irregular amplitude fluctuations over time, `ACI` captures this temporal variability as a proxy for acoustic complexity.
#'
#' In `Ruido`, `ACI` is computed independently within each time bin. Within a time bin, the signal is further subdivided into smaller temporal segments, here referred to as cluster intervals \eqn{j}.
#'
#' For a given frequency bin \eqn{f_l}, acoustic intensity values \eqn{I_k} are evaluated across consecutive time steps \eqn{k} within each cluster interval \eqn{j}. The absolute differences between adjacent time steps are calculated as:
#'
#' \deqn{d_k = |I_k - I_{k+1}|}
#'
#' These differences are summed within each cluster interval:
#'
#' \deqn{D_j = \sum_{k = 1}^{N} d_k}
#'
#' where \eqn{N} is the number of time steps \eqn{\Delta t_k} in interval \eqn{j}. The `ACI` for each cluster interval is then:
#'
#' \deqn{ACI_j = \frac{D_j}{\sum_{k = 1}^{N} I_k}}
#'
#' where \eqn{\sum_{k = 1}^{N} I_k} is the total acoustic intensity within the same interval.
#'
#' For each frequency bin \eqn{f_l}, `ACI` values are summed across all cluster intervals within the time bin:
#'
#' \deqn{ACI_{f_l} = \sum_{j = 1}^{m} ACI_j}
#'
#' where \eqn{m} is the number of cluster intervals in the time bin.
#'
#' The result is a frequency-resolved representation of `ACI` for each time bin, rather than a single scalar value for the entire recording.
#'
#' In the original formulation (Pieretti et al., 2011), `ACI` is further summed across all frequency bins:
#'
#' \deqn{ACI_{tot} = \sum_{l = 1}^{q} ACI_{f_l}}
#'
#' where \eqn{q} is the total number of frequency bins. This final aggregation step is not performed in this package.
#'
#' @references
#' Pieretti, N., Farina, A., & Morri, D. (2011). A new methodology to infer the singing activity of an avian community: The Acoustic Complexity Index (ACI). Ecological Indicators, 11(3), 868–873. https://doi.org/10.1016/j.ecolind.2010.11.005
#'
#' @export
#'@importFrom signal specgram
#'@importFrom tuneR readWave
#'@importFrom tuneR readMP3
#'@importFrom tuneR downsample
#'@importFrom wav read_wav
#'
#' @examples
#' \donttest{
#' ### This is a secondary example using audio from a real soundscape
#' ### These audios are originated from the Escutadô Project, a project
#' ### that records the soundscapes of the brazilian semiarid
#' # Getting audiofile from the online Zenodo library
#' dir <- paste(tempdir(), "forExample", sep = "/")
#' dir.create(dir)
#' rec <- paste0("GAL24576_20250401_", sprintf("%06d", 0), ".wav")
#' recDir <- paste(dir, rec , sep = "/")
#' url <- paste0("https://zenodo.org/records/17575795/files/",
#'               rec,
#'               "?download=1")
#'
#' # Downloading the file, might take some time denpending on your internet
#' download.file(url, destfile = recDir, mode = "wb")
#'
#' # Running the ACIspec function with all the default arguments
#' aci <- ACIspec(recDir)
#'
#' # Here's the result
#' aci
#'
#' # Plot ACI values
#' plot(aci)
#'}
#'
ACIspec = function(soundfile,
                   channel = "stereo",
                   timeBin = 60,
                   j = 5,
                   targetSampRate = NULL,
                   wl = 512,
                   window = signal::hamming(wl),
                   overlap = ceiling(length(window) / 2)) {

  argHandler(FUN = "ACIspec", soundfile, channel, timeBin, j, targetSampRate, wl,
             window, overlap)

  if (!is.null(timeBin)) {
    j <- if (is.null(j)) timeBin else min(j, timeBin)
  }

  audio = soundfile

  if (is.character(audio)) {
    if (tolower(tools::file_ext(soundfile)) == "wav") {
      soundfile = wav::read_wav(soundfile)
      samp.rate = attr(soundfile, "sample_rate")
      wavPKG = "wav"

      if (channel == "mono" && nrow(soundfile) > 1) {
        soundfile = (soundfile[1, ] + soundfile[2, ]) / 2
        attr(soundfile, "sample_rate") = samp.rate
      }

      if (channel == "stereo" && nrow(soundfile) == 1) {
        message("Audio is not stereo, defaulting to left channel.")
        channel = "mono"
      }

    } else {
      stop("The audio file must be in the WAV format.")
    }
  } else if (class(audio)[1] == "Wave") {
    samp.rate = soundfile@samp.rate
    wavPKG = "tuneR"
    if (channel == "mono" && soundfile@stereo) {
      soundfile = tuneR::mono(soundfile, which = "both")
    }
    if (channel == "stereo" && !soundfile@stereo) {
      message("Audio is not stereo, defaulting to left channel.")
      channel = "mono"
    }
    if (!is.null(targetSampRate)) {
      soundfile <- tuneR::downsample(soundfile, targetSampRate)
    }
  } else {
    samp.rate = attr(soundfile, "sample_rate")
    wavPKG = "wav"
    if (channel == "mono" && nrow(soundfile) > 1) {
      soundfile = (soundfile[1, ] + soundfile[2, ]) / 2
      attr(soundfile, "sample_rate") = samp.rate
    }

    if (channel == "stereo" && nrow(soundfile) == 1) {
      message("Audio is not stereo, defaulting to left channel.")
      channel = "mono"
    }
  }

  rm(audio)

  ACIexp <- processChannel.ACI(
    soundfile,
    samp.rate = samp.rate,
    channel = channel,
    timeBin = timeBin,
    j = j,
    wl = wl,
    overlap = overlap,
    window = window,
    wavPKG = wavPKG,
    noiseOBJ = new("noise.matrix")
  )

  if (ACIexp@channel == "stereo") {
    ACIexp@wl <- nrow(ACIexp@values$left$ACI)

  } else {
    ACIexp@wl <- nrow(ACIexp@values[[channel]]$ACI)

  }

  return(ACIexp)

}
