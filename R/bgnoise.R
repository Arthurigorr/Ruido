#' @title Background Noise and Soundscape Power Index
#'
#' @description Compute the Background Noise and Soundscape Power values of an audio using Towsey, 2017 methodology
#'
#' @param audiofile A tuneR Wave object or the path to a valid audio
#' @param channel The desired channel of your audiofile. Available channels are: "stereo", "mono", "left" or "right"
#' @param time_bin The size (in seconds) of your time bin
#' @param db_threshold The minimum possible threshold for the dB values of the spectrogram (default = -90)
#' @param target_samp_rate The sampling rate of the spectrogram (this argument is only used to down sample the sample rate)
#' @param wl The window length of your spectrogram (default = 512)
#' @param window The window used to smooth the signal (default = hamming(wl))
#' @param overlap Overlap between the spectrogram windows
#' @param histbreaks Which breaks to use to calculate background noise (default = "FD")
#'
#' @returns A list containing three objects: The first and second one contains a matrix with the values of background noise and soundscape power respectively to each time bin and for each frequency window of your audiofile. The third object is the duration in second of your time bins.
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
#' @examples ### HUMANITY WAS CRUSHED UNDER THE WHEELS OF A MACHINE CREATED TO CREATE THE MACHINE TO CRUSH THE MACHINE.
bgnoise <- function(audiofile,
                    channel = "stereo",
                    time_bin = 60,
                    db_threshold = -90,
                    target_samp_rate = NULL,
                    wl = 512,
                    window = signal::hamming(wl),
                    overlap = ceiling(length(window) / 2),
                    histbreaks = "FD") {
  if (!channel %in% c("left", "right", "stereo", "mono")) {
    stop("Please provide a valid channel: 'left', 'right', 'stereo', or 'mono'.")
  }

  audio <- if (is.character(audiofile)) {
    if (tools::file_ext(audiofile) %in% c("mp3", "wav")) {
      if (tools::file_ext(audiofile) == "mp3") {
        tuneR::readMP3(audiofile)
      } else {
        tuneR::readWave(audiofile)
      }
    } else {
      stop("The audio file must be in MP3 or WAV format.")
    }
  } else {
    audiofile
  }

  if (!audio@stereo && channel == "stereo") {
    print("Audio is not stereo, defaulting to left channel.")
    channel <- "left"
  }

  if (!is.null(target_samp_rate)) {
    audio <- tuneR::downsample(audio, target_samp_rate)
  }

  return(
    process_channel(
      audio,
      channel = channel,
      time_bin = time_bin,
      wl = wl,
      overlap = overlap,
      db_threshold = db_threshold,
      window = window,
      histbreaks = histbreaks
    )
  )

}
