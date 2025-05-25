
devtools::document()

roxygen2::roxygenise()

#' Compute the background noise and soundscape power of an audio
#' 
#' @param audiofile A tuneR Wave object or the path to a valid audio
#' @param channel The desired channel of your audiofile. Available channels are: "stereo", "mono", "left" or "right"
#' @param time_bin The size (in seconds) of your time bin.
#' @param db_threshold The minimum possible threshold for the dB values of the spectrogram (default = -90)
#' @param target_samp_rate The sampling rate of the spectrogram (this argument is only used to down sample your actual saple rate)
#' @param wl The window length of your spectrogram (default = 512)
#' @param window The window used to smooth the signal (default = hamming(wl))
#' @param overlap Overlap between the spectrogram windows
#' @param histbreaks Which breaks to use to calculate background noise (default = "FD")

bgnoise <- function(audiofile,
                    channel = "stereo",
                    time_bin = 60,
                    db_threshold = -90,
                    target_samp_rate = NULL,
                    wl = 512,
                    window = signal::hamming(wl),
                    overlap = ceiling(length(window) / 2),
                    histbreaks = "FD") {
  
  # source("BGN_POW/internal_functions.R")
  
  if(!channel %in% c("left", "right", "stereo", "mono")) {
    stop("Please provide a valid channel: 'left', 'right', 'stereo', or 'mono'.")
  }
  
  # Check the audio extension
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
  
  if(!audio@stereo && channel == "stereo") {
    print("Audio is not stereo, defaulting to left channel.")
    channel <- "left"
  }
  
  # Downsample if requested
  if (!is.null(target_samp_rate)) {
    audio <- tuneR::downsample(audio, target_samp_rate)
  }
  
  return(process_channel(audio, channel = channel, time_bin = time_bin,
                             wl = wl, overlap = overlap, db_threshold = db_threshold,
                             window = window, histbreaks = histbreaks))
  
}
