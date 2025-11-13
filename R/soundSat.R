#' @title Soundscape Saturation Index
#'
#' @description Calculate Soundscape Saturation for a combination of recordings using Burivalova 2018 methodology
#'
#' @param soundpath A path of multiple paths to your recordings. This path must direct to a folder or combination of folders.
#' @param channel The channel you want to computer of your audiofile. Available channels are: "stereo", "mono", "left" or "right"
#' @param timeBin The size (in seconds) of the time bin (default = 60)
#' @param dbThreshold The minimum possible value of dB for the spectrograms (default = -90)
#' @param targetSampRate The sampling rate of your audio (this argument is only used to down sample your audio)
#' @param wl The window length of your spectrogram (default = 512)
#' @param window The window used to smooth the signal (default = signal::hamming(wl))
#' @param overlap Overlap between the spectrogram windows (the default is half your window length)
#' @param histbreaks Which breaks to use to calculate background noise. Available breaks are: "FD", "Sturges", "scott" and 100
#' @param powthr The a vector of values to evaluate the activity matrix for soundscape power (in dB). The first value corresponds to the lowest dB value and the second corresponds to the highest, the third value is the step
#' @param bgnthr The values to evaluate the activity matrix for background noise (in %). The first value corresponds to the lowest quantile value and the second corresponds to the highest, the third value is the step
#' @param normality The normality test to determine which threshold has the most normal distribution of values. We recommend you pick any test from the "nortest" package, but if you have few recordings and your bins won't exceeds 5000 we recommend using the shapiro.test function.
#' @param backup A path in case you wish to backup your saturation values in case you need to turn of the computer or in case you cannot be sure the computer will be on for the entire process.
#'
#' @returns A list containing five objects. The first and second objects (powthresh and bgnthresh) are the threshold values that yielded the most normal distribution of saturation values. The third (normality) contains the p values of the normality test that yielded the most normal distribution. The fourth object (values) contains a data.frame with the the values of saturation for each bin of each recording and the size of the bin in seconds. The fifth contains a data.frame with errors that ocurred with specific files during the function.
#'
#' @details Soundscape Saturation is a measure of the proportion of frequency bins that are acoustically active in a determined window of time. It was developed by Zuzana Burivalova as an index to test the acoustic niche hypothesis.
#' To calculate this function, first you need to generate an activity matrix for each time bin of your recording with the following formula:
#'
#'\deqn{a_{mf} = 1\  if (BGN_{mf} > \theta_{1})\  or\  (POW_{mf} > \theta_{2});\  otherwise,\  a_{mf} = 0,}
#'
#'Where \eqn{\theta_{1}} is the threshold of BGN values and \eqn{\theta_{2}} is a threshold of dB values.
#'Since we define a interval for both the threshold, this means that an activity matrix will be generated for each bin of each recording.
#'For each combination of threshold a soundscape saturation measure will be taken with the following formula:
#'
#'\deqn{S_{m} = \frac{\sum_{f = 1}^N a_{mf}}{N}}
#'
#'After these equations are done, we check every threshold combination for normality and pick the combination that yields the most normal distribution of saturation values.
#'
#'If you set a path for the "path" argument, a single R object will be written in your path for each audiofile individually. These objects can be loaded again through the "sat_backup" function to continue the calculation of saturation in case the proccess is stopped.
#'
#'@references Burivalova, Z., Towsey, M., Boucher, T., Truskinger, A., Apelis, C., Roe, P., & Game, E. T. (2017). Using soundscapes to detect variable degrees of human influence on tropical forests in Papua New Guinea. Conservation Biology, 32(1), 205–215. https://doi.org/10.1111/cobi.12968
#'
#'@export
#'@importFrom methods is
#'@importFrom methods slot
#'@importFrom stats IQR
#'@importFrom stats quantile
#'@importFrom stats setNames
#'@importFrom stats shapiro.test
#'@importFrom nortest ad.test
#'
#' @examples
#' ### Downloading audiofiles from public Zenodo library
#' dir <- tempdir()
#' recName <- paste0("GAL24576_20250401_", sprintf("%06d", seq(0, 200000, by = 50000)),".wav")
#' recDir <- paste(dir, recName, sep = "\\")
#'
#' for(rec in recName) {
#'  print(rec)
#'  url <- paste0("https://zenodo.org/records/17575795/files/", rec, "?download=1")
#'  download.file(url, destfile = paste(dir, rec, sep = "\\"), mode = "wb")
#' }
#'
#' ### Running the function
#' sat <- soundSat(dir)
#'
#' ### Preparing the plot
#' timeSplit <- strsplit(sat$values$AUDIO, "_")
#' sides <- ifelse(grepl("left", sat$values$BIN), "left", "right")
#' date <- sapply(timeSplit, function(x)
#'   x[2])
#' time <- sapply(timeSplit, function(x)
#'   substr(x[3],1,6))
#' datePos <- paste(substr(date,1,4), substr(date,5,6), substr(date,7,8), sep = "-")
#' timePos <- paste(substr(time,1,2), substr(time,3,4), substr(time,5,6), sep = ":")
#' dateTime <- as.POSIXct(paste(datePos, timePos), format = "%Y-%m-%d %H:%M:%OS")
#' leftEar <- data.frame(SAT = sat$values$SAT[sides == "left"], HOUR = dateTime[sides == "left"])
#' rightEar <- data.frame(SAT = sat$values$SAT[sides == "right"], HOUR = dateTime[sides == "right"])
#'
#' ### Plotting results
#'
#' plot(SAT~HOUR, data = leftEar, ylim = c(range(sat$values$SAT)),
#' col = "darkgreen", pch = 16,
#'      ylab = "Soundscape Saturation (%)", xlab = "Time of Day", type = "b")
#' points(SAT~HOUR, data = rightEar, ylim = c(range(sat$values$SAT)),
#' col = "red", pch = 16, type = "b")
#' legend("topright", legend = c("Left Ear", "Right Ear"),
#'        col = c("darkgreen", "red"), lty = 1)
#'
#' unlink(recDir)
soundSat <- function(soundpath,
                     channel = "stereo",
                     timeBin = 60,
                     dbThreshold = -90,
                     targetSampRate = NULL,
                     wl = 512,
                     window = signal::hamming(wl),
                     overlap = ceiling(length(window) / 2),
                     histbreaks = "FD",
                     powthr = c(5, 20, 1),
                     bgnthr = c(0.5, 0.9, 0.05),
                     normality = "ad.test",
                     backup = NULL) {
  if (all(!dir.exists(soundpath)))
    stop("all provided soundpaths must be valid.")

  if (!is.null(backup) && !dir.exists(backup))
    stop("you must provide a valid folder for backup.")

  soundfiles <- list.files(soundpath, full.names = TRUE, recursive = TRUE)
  soundfiles <- soundfiles[tools::file_ext(soundfiles) %in% c("mp3", "wav")]

  if (length(soundfiles) < 3)
    stop("you must provide at least 3 recordings!")

  if (normality == "shapiro.test") {
    answernorm <- readline(
      "
      Using shapiro.test can be dangerous since you WILL lose all your progress if the
      number of total bins exceeds 5000.
      Do you wish to use Anderson-Darling test instead? (Y/N)."
    )

    if (answernorm == "Y") {
      normality <- "ad.test"
    } else if (answernorm == "N") {
      print("Using shapiro.test to test normality.")
    } else {
      stop("Please answer with Y or N next time.")
    }

  }

  powthreshold <- seq(powthr[1], powthr[2], powthr[3])
  names(powthreshold) <- powthreshold
  bgnthreshold <- seq(bgnthr[1], bgnthr[2], bgnthr[3])

  thresholdCombinations <- setNames(expand.grid(powthreshold, bgnthreshold),
                                    c("powthreshold", "bgnthreshold"))

  combinations <- paste(thresholdCombinations[, 1], thresholdCombinations[, 2], sep = "/")

  print(
    paste(
      "Calculating saturation values for",
      length(soundfiles),
      "recordings using",
      length(combinations),
      "threshold combinations"
    )
  )

  halfWl <- round(wl / 2)

  SATdf <- list()

  if (!is.null(backup)) {
    SATdf[["ogARGS"]] <- list(
      channel = channel,
      timeBin = timeBin,
      dbThreshold = dbThreshold,
      targetSampRate = targetSampRate,
      wl = wl,
      window = window,
      overlap = overlap,
      histbreaks = histbreaks,
      powthr = powthr,
      bgnthr = bgnthr,
      normality = normality
    )
  }

  fiveSteps <- 1

  for (soundfile in soundfiles) {
    fiveSteps <- fiveSteps + 1

    BGNPOW <- tryCatch(
      bgNoise(
        soundfile,
        timeBin = timeBin,
        targetSampRate = targetSampRate,
        window = window,
        overlap = overlap,
        channel = channel,
        dbThreshold = dbThreshold,
        wl = wl,
        histbreaks = histbreaks
      ),
      error = function(e)
        e,
      warning = function(w)
        w
    )

    SATdf[[soundfile]] <- if (is(BGNPOW, "error") ||
                              is(BGNPOW, "warning")) {
      cat("\n",
          basename(soundfile),
          "is not valid!\nError:",
          BGNPOW$message,
          "\n")

      BGNPOW

    } else {
      if (all(c("left", "right") %in% names(BGNPOW))) {
        BGNQleft <- apply(BGNPOW$left$BGN, 2, function(n)
          setNames(
            quantile(n, probs = seq(bgnthr[1], bgnthr[2], bgnthr[3])),
            seq(bgnthr[1], bgnthr[2], bgnthr[3])
          ))

        BGNQright <- apply(BGNPOW$right$BGN, 2, function(n)
          setNames(
            quantile(n, probs = seq(bgnthr[1], bgnthr[2], bgnthr[3])),
            seq(bgnthr[1], bgnthr[2], bgnthr[3])
          ))

        BGNsaturation <- list(
          left = sapply(colnames(BGNQleft), function(Q) {
            list(sapply(BGNQleft[, Q], function(P)
              P < BGNPOW$left$BGN[, Q]))
          }),
          right = sapply(colnames(BGNQright), function(Q) {
            list(sapply(BGNQright[, Q], function(P)
              P < BGNPOW$right$BGN[, Q]))
          })
        )

        POWsaturation <- sapply(c("left", "right"), function(side) {
          list(sapply(colnames(BGNPOW[[side]][["POW"]]), function(Q) {
            list(sapply(powthreshold, function(P)
              P < BGNPOW[[side]][["POW"]][, Q]))
          }))
        })

        singsat <- as.data.frame(do.call(rbind, sapply(c(
          "left", "right"
        ), function(side) {
          list(
            mapply(
              function(bgnthresh, powthresh) {
                sapply(1:length(BGNPOW$timeBins), function(i) {
                  sum(BGNsaturation[[side]][[paste0("BGN", i)]][, paste(bgnthresh)] |
                        POWsaturation[[side]][[paste0("POW", i)]][, paste(powthresh)]) / halfWl
                })
              },
              thresholdCombinations$bgnthreshold,
              thresholdCombinations$powthreshold
            )
          )

        })))

        binsUnique <- paste(rep(c('left', 'right'), each = nrow(singsat) /
                                  2), seq(nrow(singsat) / 2), sep = "_")

        DURATION <- rep(BGNPOW$timeBins, 2)
        SAMPRATE <- rep(BGNPOW$sampRate, 2)

      } else if ("mono" %in% names(BGNPOW)) {
        BGNQ <- apply(BGNPOW$mono$BGN, 2, function(n)
          setNames(
            quantile(n, probs = seq(bgnthr[1], bgnthr[2], bgnthr[3])),
            seq(bgnthr[1], bgnthr[2], bgnthr[3])
          ))

        BGNsaturation <- list(mono = sapply(colnames(BGNQ), function(Q) {
          list(sapply(BGNQ[, Q], function(P)
            P < BGNPOW$mono$BGN[, Q]))
        }))

        POWsaturation <- list(mono = sapply(colnames(BGNPOW$mono$POW), function(Q) {
          list(sapply(powthreshold, function(P)
            P < BGNPOW$mono$POW[, Q]))
        }))


        singsat <- as.data.frame(
          mapply(
            function(bgnthresh, powthresh) {
              sapply(1:length(BGNPOW$timeBins), function(i) {
                sum(BGNsaturation$mono[[paste0("BGN", i)]][, paste(bgnthresh)] |
                      POWsaturation$mono[[paste0("POW", i)]][, paste(powthresh)]) / halfWl
              })
            },
            thresholdCombinations$bgnthreshold,
            thresholdCombinations$powthreshold
          )
        )

        binsUnique <- paste("mono", seq(nrow(singsat)), sep = "_")

        DURATION <- BGNPOW$timeBins
        SAMPRATE <- BGNPOW$sampRate

      } else {
        realChannel <- c("left", "right")[c("left", "right") %in% names(BGNPOW)]

        BGNQ <- apply(BGNPOW[[realChannel]][["BGN"]], 2, function(n)
          setNames(
            quantile(n, probs = seq(bgnthr[1], bgnthr[2], bgnthr[3])),
            seq(bgnthr[1], bgnthr[2], bgnthr[3])
          ))

        BGNsaturation <- setNames(list(sapply(colnames(BGNQ), function(Q) {
          list(sapply(BGNQ[, Q], function(P)
            P < BGNPOW[[realChannel]][["BGN"]][, Q]))
        })), realChannel)

        POWsaturation <- setNames(list(sapply(colnames(BGNPOW[[realChannel]][['POW']]), function(Q) {
          list(sapply(powthreshold, function(P)
            P < BGNPOW[[realChannel]][['POW']][, Q]))
        })), realChannel)


        singsat <- as.data.frame(
          mapply(
            function(bgnthresh, powthresh) {
              sapply(1:length(BGNPOW$timeBins), function(i) {
                sum(BGNsaturation[[realChannel]][[paste0("BGN", i)]][, paste(bgnthresh)] |
                      POWsaturation[[realChannel]][[paste0("POW", i)]][, paste(powthresh)]) / halfWl
              })
            },
            thresholdCombinations$bgnthreshold,
            thresholdCombinations$powthreshold
          )
        )

        binsUnique <- paste(realChannel, seq(nrow(singsat)), sep = "_")

        DURATION <- BGNPOW$timeBins
        SAMPRATE <- BGNPOW$sampRate

      }

      cat(
        "\r(",
        basename(soundfile),
        ") ",
        match(soundfile, soundfiles),
        " out of ",
        length(soundfiles),
        " recordinds concluded!",
        sep = ""
      )

      gc()

      list(
        SAT = singsat,
        DUR = DURATION,
        SMP = SAMPRATE,
        BIN = binsUnique,
        NAME = soundfile
      )

    }

    if (!is.null(backup) && fiveSteps %% 5 == 0) {
      saveRDS(SATdf, file = paste0(backup, "/SATBACKUP.RData"))
    }

  }

  if (!is.null(backup)) {
    SATdf["ogARGS"] <- NULL
    file.remove(paste0(backup, "/SATBACKUP.RData"))

  }

  which.error <- sapply(SATdf, function(x)
    is(x, "error") || is(x, "warning"))
  ERRORS <- SATdf[which.error]
  DURATIONS <- c(sapply(SATdf[!which.error], function(x)
    x[["DUR"]]))
  SAMPRATES <- c(sapply(SATdf[!which.error], function(x)
    x[["SMP"]]))
  PATHS <- c(sapply(SATdf[!which.error], function(x)
    rep(x[["NAME"]], length(x[["BIN"]]))))
  BINS <- c(sapply(SATdf[!which.error], function(x)
    x[["BIN"]]))
  SATdf <- do.call(rbind, lapply(SATdf[!which.error], function(x)
    x[["SAT"]]))

  colnames(SATdf) <- combinations

  normal <- if (normality == "shapiro.test") {
    apply(SATdf, 2, function(x)
      ifelse(length(unique(x)) != 1, shapiro.test(x)$p.value, 0))

  } else {
    apply(SATdf, 2, function(Q)
      eval(parse(text = paste0(
        normality, "(c(", paste((Q), collapse = ","), "))"
      )))$p.value)

  }

  thresholds <- unlist(strsplit(names(which.max(normal)), split = "/"))

  cat(
    "\n           Soundscape Saturation Results\n\n",
    "POW Threshold = ",
    as.numeric(thresholds[1]),
    " dB        ",
    "BGN Threshold = ",
    as.numeric(thresholds[2]) * 100,
    "%\n",
    "            Normality test result = ",
    as.numeric(max(normal)),
    "\n ",
    sep = ""
  )

  export <- list(
    powthresh = numeric(0),
    bgntresh = numeric(0),
    normality = numeric(0),
    values = data.frame(),
    errors = data.frame()
  )

  export["powthresh"] <- as.numeric(thresholds[1])
  export["bgntresh"] <- as.numeric(thresholds[2]) * 100
  export["normality"] <- as.numeric(as.numeric(max(normal)))
  export[["values"]] <- data.frame(
    PATH = dirname(PATHS),
    AUDIO = basename(PATHS),
    BIN = BINS,
    DURATION = DURATIONS,
    SAMPRATE = SAMPRATES,
    SAT = SATdf[, which.max(normal)]
  )
  export[["errors"]] <- data.frame(file = soundfiles[which.error], do.call(rbind, ERRORS))

  return(export)

}
