#' @title Background Noise and Soundscape Power Index
#'
#' @description Compute the Background Noise and Soundscape Power values of an audio using Towsey 2017 methodology
#'
#' @param soundfile A tuneR Wave object or the path to a valid audio in your computer
#' @param channel The channel you want to computer of your soundfile. Available channels are: "stereo", "mono", "left" or "right"
#' @param timeBin The size (in seconds) of the time bin (default = 60)
#' @param dbThreshold The minimum possible value of dB for the spectrograms (default = -90)
#' @param targetSampRate The sampling rate of your audio (this argument is only used to down sample your audio)
#' @param wl The window length of your spectrogram (default = 512)
#' @param window The window used to smooth the signal (default = hamming(wl))
#' @param overlap Overlap between the spectrogram windows (the default is half your window length)
#' @param histbreaks Which breaks to use to calculate background noise. Available breaks are: "FD", "Sturges", "scott" and 100
#'
#' @returns A list containing three objects: The first and second one contains a matrix with the values of background noise and soundscape power respectively to each time bin and for each frequency window of your soundfile. The third object is the duration in second of your time bins.
#' @details Background noise is an index that measures the most common continuous baseline level of acoustic energy in a frequency window and in a time bin. It is calculated by taking the modal value of intensity values in temporal bin c in frequency window f:
#'
#'\deqn{BGN_{f} = mode(dB_{cf})}
#'
#'Soundscape power represents a measure of signal-to-noise ratio. It measures the relation of background noise to the loudest intensities in temporal bin c in frequency window f:
#'
#'\deqn{POW_{f} = max(dB_{cf}) - BGN_{cf}}
#'
#' @references Towsey, Michael W. (2017) The calculation of acoustic indices derived from long-duration recordings of the natural environment.
#'
#' Pijanowski, B. C. (2024). Principles of Soundscape Ecology.
#'
#'@export
#'@importFrom signal specgram
#'@importFrom tuneR readWave
#'@importFrom tuneR readMP3
#'@importFrom tuneR downsample
#'
#' @examples
#' # Getting audiofile from the online Zenodo library
#' dir <- tempdir()
#' rec <- paste0("GAL24576_20250401_", sprintf("%06d", 0),".wav")
#' recDir <- paste(dir,rec , sep = "\\")
#' url <- paste0("https://zenodo.org/records/17575795/files/", rec, "?download=1")
#'
#' # Downloading the file, might take some time denpending on your internet
#' download.file(url, destfile = recDir, mode = "wb")
#'
#' # Running the bgNoise function with all the default arguments
#' bgn <- bgNoise(recDir)
#'
#' # Print the results
#' print(bgn)
#'
#' # Plotting background noise and soundscape profile for the first minute of the recording
#' par(mfrow = c(1,2))
#' plot(x = bgn$left$BGN$BGN1, y = seq(1,24000, length.out = 256),
#' xlab = "Background Noise (dB)", ylab = "Frequency (hz)")
#' plot(x = bgn$left$POW$POW1, y = seq(1,24000, length.out = 256),
#' xlab = "Soundscape Power (dB)", ylab = "Frequency (hz)")
#'
#'unlink(recDir)
#'
bgNoise <- function(soundfile,
                    channel = "stereo",
                    timeBin = 60,
                    dbThreshold = -90,
                    targetSampRate = NULL,
                    wl = 512,
                    window = signal::hamming(wl),
                    overlap = ceiling(length(window) / 2),
                    histbreaks = "FD") {
  if (!channel %in% c("left", "right", "stereo", "mono")) {
    stop("Please provide a valid channel: 'left', 'right', 'stereo', or 'mono'.")
  }

  audio <- if (is.character(soundfile)) {
    if (tools::file_ext(soundfile) %in% c("mp3", "wav")) {
      if (tools::file_ext(soundfile) == "mp3") {
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

  if (!audio@stereo && channel == "stereo") {
    print("Audio is not stereo, defaulting to left channel.")
    channel <- "left"
  }

  if (!is.null(targetSampRate)) {
    audio <- tuneR::downsample(audio, targetSampRate)
  }

  return(
    processChannel(
      audio,
      channel = channel,
      timeBin = timeBin,
      wl = wl,
      overlap = overlap,
      dbThreshold = dbThreshold,
      window = window,
      histbreaks = histbreaks
    )
  )

}
