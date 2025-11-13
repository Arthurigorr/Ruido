#' @title Single Soundscape Saturation Index
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
#' @param powthr The value to evaluate the activity matrix for soundscape power (in dB)
#' @param bgnthr The value to evaluate the activity matrix for background noise (in dB)
#'
#' @export
#' @returns A data frame containing the saturation values for all time bins of the inputed file
#' @details Soundscape Saturation is a measure of the proportion of frequency bins that are acoustically active in a determined window of time. It was developed by Zuzana Burivalova as an index to test the acoustic niche hypothesis.
#' To calculate this function, first you need to generate an activity matrix for each time bin of your recording with the following formula:
#'
#'\deqn{a_{mf} = 1\  if (BGN_{mf} > \theta_{1})\  or\  (POW_{mf} > \theta_{2});\  otherwise,\  a_{mf} = 0,}
#'
#'Where \eqn{\theta_{1}} is the threshold of BGN values and \eqn{\theta_{2}} is a threshold of dB values.
#'Since we define a single threshold for both in this function, we don't have to worry about generating a saturation value for many different combinations.
#'For the selected threshold a soundscape saturation measure will be taken with the following formula:
#'
#'\deqn{S_{m} = \frac{\sum_{f = 1}^N a_{mf}}{N}}
#'
#'@references Burivalova, Z., Towsey, M., Boucher, T., Truskinger, A., Apelis, C., Roe, P., & Game, E. T. (2017). Using soundscapes to detect variable degrees of human influence on tropical forests in Papua New Guinea. Conservation Biology, 32(1), 205–215. https://doi.org/10.1111/cobi.12968
#'
#' @examples
#' # Getting audiofile from the online Zenodo library
#' dir <- tempdir()
#' rec <- paste0("GAL24576_20250401_", sprintf("%06d", 0),".wav")
#' recDir <- paste(dir,rec , sep = "/")
#' url <- paste0("https://zenodo.org/records/17575795/files/", rec, "?download=1")
#'
#' # Downloading the file, might take some time denpending on your internet
#' download.file(url, destfile = recDir, mode = "wb")
#'
#' # Now we calculate soundscape saturation for both sides of the recording
#' sat <- singleSat(recDir, wl = 256)
#'
#' # Printing the results
#' print(sat)
#'
#' barplot(c(sat$left, sat$right), col = c("darkgreen", "red"),
#'        names.arg = c("Left", "Right"), ylab = "Soundscape Saturation (%)")
#'
#' unlink(recDir)

singleSat <- function(soundfile,
                      channel = "stereo",
                      timeBin = 60,
                      dbThreshold = -90,
                      targetSampRate = NULL,
                      wl = 512,
                      window = signal::hamming(wl),
                      overlap = ceiling(length(window) / 2),
                      histbreaks = "FD",
                      powthr = 10,
                      bgnthr = 0.8) {

  halfWl <- round(wl / 2)

  BGNPOW <- bgNoise(
    soundfile,
    timeBin = timeBin,
    targetSampRate = targetSampRate,
    window = window,
    overlap = overlap,
    channel = channel,
    dbThreshold = dbThreshold,
    wl = wl,
    histbreaks = histbreaks
  )

  if (all(c("left", "right") %in% names(BGNPOW))) {
    BGNsaturation <- sapply(c("left", "right"), function(side) {
      list(apply(BGNPOW[[side]]$BGN, 2, function(BGN) {
        Q <- quantile(BGN, bgnthr)
        BGN > Q
      }))
    })

    POWsaturation <- sapply(c("left", "right"), function(side) {
      list(apply(BGNPOW[[side]]$POW, 2, function(POW) {
        POW > powthr
      }))
    })

    singSat <- as.data.frame(do.call(cbind, sapply(c("left", "right"), function(side) {
      list(sapply(1:length(BGNPOW$timeBins), function(i) {
        sum(BGNsaturation[[side]][, i] |
              POWsaturation[[side]][, i]) / halfWl
      }))
    })))

  } else if ("mono" %in% names(BGNPOW)) {
    BGNsaturation <- list(mono = apply(BGNPOW$mono$BGN, 2, function(BGN) {
      Q <- quantile(BGN, bgnthr)
      BGN > Q
    }))


    POWsaturation <- list(mono = apply(BGNPOW$mono$POW, 2, function(POW) {
      POW > powthr
    }))

    singSat <- data.frame("mono" = do.call(cbind, list(sapply(1:length(BGNPOW$timeBins), function(i) {
      sum(BGNsaturation$mono[, i] |
            POWsaturation$mono[, i]) / halfWl
    }))))

  } else {
    realChannel <- c("left", "right")[c("left", "right") %in% names(BGNPOW)]

    BGNsaturation <- list(apply(BGNPOW[[realChannel]]$BGN, 2, function(BGN) {
      Q <- quantile(BGN, bgnthr)
      BGN > Q
    })) |>
      setNames(realChannel)

    POWsaturation <- list(apply(BGNPOW[[realChannel]]$POW, 2, function(POW) {
      POW > powthr
    })) |>
      setNames(realChannel)

    singSat <- data.frame(do.call(cbind, list(sapply(1:length(BGNPOW$timeBins), function(i) {
      sum(BGNsaturation[[realChannel]][, i] |
            POWsaturation[[realChannel]][, i]) / halfWl
    })))) |>
      setNames(realChannel)

  }

  singSat

}
