#' @title Soundscape Saturation Index
#'
#' @description Calculate Soundscape Saturation for a combination of recordings using Burivalova, 2018 methodology
#'
#' @param soundpath A path of multiple paths to your recordings. This path must direct to a folder or combination of folders.
#' @param time_bin The size (in seconds) of your time bin.
#' @param target_samp_rate The sampling rate of the spectrogram (this argument is only used to down sample the sample rate)
#' @param wl The window length of your spectrogram (default = 512)
#' @param window The window used to smooth the signal (default = hamming(wl))
#' @param overlap Overlap between the spectrogram windows
#' @param channel The desired channel of your audiofile. Available channels are: "stereo", "mono", "left" or "right"
#' @param db_threshold The minimum possible threshold for the dB values of the spectrogram (default = -90)
#' @param histbreaks Which breaks to use to calculate background noise (default = "FD")
#' @param powthr The values to evaluate the activity matrix for soundscape power (in dB)
#' @param bgnthr The values to evaluate the activity matrix for background noise (in %)
#' @param normality The normality test to determine which threshold has the most normal distribution of values
#' @param backup A path in case you wish to backup your saturation values in case you need to turn of the computer or just to be safe.
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
#'@references Burivalova, Z., Towsey, M., Boucher, T., Truskinger, A., Apelis, C., Roe, P., & Game, E. T. (2017). Using soundscapes to detect variable degrees of human influence on tropical forests in Papua New Guinea. Conservation Biology, 32(1), 205–215. https://doi.org/10.1111/cobi.12968
#'
#' @examples ### Whatever in creation exists without my knowledge exists without my consent.
soundsat <- function(soundpath,
                     time_bin = 60,
                     target_samp_rate = NULL,
                     wl = 512,
                     window = signal::hamming(wl),
                     overlap = ceiling(length(window) / 2),
                     channel = "stereo",
                     db_threshold = -90,
                     histbreaks = "FD",
                     powthr = c(5.1, 20, 0.1),
                     bgnthr = c(0.51, 0.99, 0.02),
                     normality = "ks.test",
                     backup = NULL) {
  require(tuneR)
  require(signal)

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
      Do you wish to use Kolmogorov-Smirnov test instead? (Y/N)."
    )

    if (answernorm == "Y") {
      normality <- "ks.test"
    } else if (answernorm == "N") {
      print("You were warned.")
    } else {
      stop("We will consider that a No.")
    }

  }

  powthreshold <- seq(powthr[1], powthr[2], powthr[3])
  names(powthreshold) <- powthreshold
  bgnthreshold <- seq(bgnthr[1], bgnthr[2], bgnthr[3])

  threshold_combinations <- setNames(expand.grid(powthreshold, bgnthreshold),
                                     c("powthreshold", "bgnthreshold"))

  combinations <- paste(threshold_combinations[, 1], threshold_combinations[, 2], sep = "/")

  print(
    paste(
      "Calculating saturation values for",
      length(soundfiles),
      "recordings using",
      length(combinations),
      "threshold combinations"
    )
  )

  half_wl <- wl / 2

  SAT_df <- lapply(soundfiles, function(soundfile) {
    BGN_POW <- tryCatch(
      bgnoise(
        soundfile,
        time_bin = time_bin,
        target_samp_rate = target_samp_rate,
        window = window,
        overlap = overlap,
        channel = channel,
        db_threshold = db_threshold,
        wl = wl,
        histbreaks = histbreaks
      ),
      error = function(e)
        e,
      warning = function(w)
        w
    )

    if (is(BGN_POW, "error") || is(BGN_POW, "warning")) {
      cat("\n",
          basename(soundfile),
          "is not valid!\nError:",
          BGN_POW$message,
          "\n")

      return(BGN_POW)

    } else {
      if (all(c("left", "right") %in% names(BGN_POW))) {
        BGN_Q_left <- apply(BGN_POW$left$BGN, 2, function(n)
          setNames(
            quantile(n, probs = seq(bgnthr[1], bgnthr[2], bgnthr[3])),
            seq(bgnthr[1], bgnthr[2], bgnthr[3])
          ))

        BGN_Q_right <- apply(BGN_POW$right$BGN, 2, function(n)
          setNames(
            quantile(n, probs = seq(bgnthr[1], bgnthr[2], bgnthr[3])),
            seq(bgnthr[1], bgnthr[2], bgnthr[3])
          ))

        BGN_saturation <- list(
          left = sapply(colnames(BGN_Q_left), function(Q) {
            list(sapply(BGN_Q_left[, Q], function(P)
              P < BGN_POW$left$BGN[, Q]))
          }),
          right = sapply(colnames(BGN_Q_right), function(Q) {
            list(sapply(BGN_Q_right[, Q], function(P)
              P < BGN_POW$right$BGN[, Q]))
          })
        )

        POW_saturation <- sapply(c("left", "right"), function(side) {
          list(sapply(colnames(BGN_POW[[side]][["POW"]]), function(Q) {
            list(sapply(powthreshold, function(P)
              P < BGN_POW[[side]][["POW"]][, Q]))
          }))
        })

        singsat <- as.data.frame(do.call(rbind, sapply(c(
          "left", "right"
        ), function(side) {
          list(
            mapply(
              function(bgnthresh, powthresh) {
                sapply(1:length(BGN_POW$time_bins), function(i) {
                  sum(BGN_saturation[[side]][[paste0("BGN", i)]][, paste(bgnthresh)] |
                        POW_saturation[[side]][[paste0("POW", i)]][, paste(powthresh)]) / half_wl
                })
              },
              threshold_combinations$bgnthreshold,
              threshold_combinations$powthreshold
            )
          )

        })))

        rownames(singsat) <- paste0(basename(soundfile),
                                    rep(c("_left", "_right"), each = nrow(singsat) / 2),
                                    "_bin",
                                    seq(nrow(singsat) / 2))

        DURATION <- rep(BGN_POW$time_bins, 2)

      } else if ("mono" %in% names(BGN_POW)) {
        BGN_Q <- apply(BGN_POW$mono$BGN, 2, function(n)
          setNames(
            quantile(n, probs = seq(bgnthr[1], bgnthr[2], bgnthr[3])),
            seq(bgnthr[1], bgnthr[2], bgnthr[3])
          ))

        BGN_saturation <- list(mono = sapply(colnames(BGN_Q), function(Q) {
          list(sapply(BGN_Q[, Q], function(P)
            P < BGN_POW$mono$BGN[, Q]))
        }))

        POW_saturation <- list(mono = sapply(colnames(BGN_POW$mono$POW), function(Q) {
          list(sapply(powthreshold, function(P)
            P < BGN_POW$mono$POW[, Q]))
        }))


        singsat <- data.frame(
          mapply(
            function(bgnthresh, powthresh) {
              sapply(1:length(BGN_POW$time_bins), function(i) {
                sum(BGN_saturation$mono[[paste0("BGN", i)]][, paste(bgnthresh)] |
                      POW_saturation$mono[[paste0("POW", i)]][, paste(powthresh)]) / half_wl
              })
            },
            threshold_combinations$bgnthreshold,
            threshold_combinations$powthreshold
          )
        )

        rownames(singsat) <- paste0(basename(soundfile), "_mono", "_bin", seq(nrow(singsat)))

        DURATION <- BGN_POW$time_bins

      } else {

        real_channel <- c("left","right")[c("left", "right") %in% names(BGN_POW)]

        BGN_Q <- apply(BGN_POW[[real_channel]][["BGN"]], 2, function(n)
          setNames(
            quantile(n, probs = seq(bgnthr[1], bgnthr[2], bgnthr[3])),
            seq(bgnthr[1], bgnthr[2], bgnthr[3])
          ))

        BGN_saturation <- setNames(list(sapply(colnames(BGN_Q), function(Q) {
          list(sapply(BGN_Q[, Q], function(P)
            P < BGN_POW[[real_channel]][["BGN"]][, Q]))
        })), real_channel)

        POW_saturation <- setNames(list(sapply(colnames(BGN_POW[[real_channel]][['POW']]), function(Q) {
          list(sapply(powthreshold, function(P)
            P < BGN_POW[[real_channel]][['POW']][, Q]))
        })), real_channel)


        singsat <- data.frame(
          mapply(
            function(bgnthresh, powthresh) {
              sapply(1:length(BGN_POW$time_bins), function(i) {
                sum(BGN_saturation[[real_channel]][[paste0("BGN", i)]][, paste(bgnthresh)] |
                      POW_saturation[[real_channel]][[paste0("POW", i)]][, paste(powthresh)]) / half_wl
              })
            },
            threshold_combinations$bgnthreshold,
            threshold_combinations$powthreshold
          )
        )

        rownames(singsat) <- paste0(basename(soundfile), "_", real_channel, "_bin", seq(nrow(singsat)))

        DURATION <- BGN_POW$time_bins

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

      if (!is.null(backup)) {

        save_l <- list(SAT = singsat, DUR = DURATION)

        saveRDS(save_l,
                file = paste0(backup, "/", basename(soundfile), ".RData"))

        rm(save_l)
      }

      gc()

      return(list(SAT = singsat, DUR = DURATION))
    }

  })

  if (!is.null(backup))
    file.remove(paste0(backup, "/", basename(soundfiles), ".txt"))

  which.error <- sapply(SAT_df, function(x)
    is(x, "error") || is(x, "warning"))
  ERRORS <- SAT_df[which.error]
  DURATIONS <- as.numeric(sapply(SAT_df[!which.error], function(x)
    x[["DUR"]]))
  SAT_df <- do.call(rbind, lapply(SAT_df[!which.error], function(x)
    x[["SAT"]]))

  colnames(SAT_df) <- combinations

  normal <- if (normality == "ks.test") {
    apply(SAT_df, 2, function(Q)
      ks.test(Q, pnorm)$p.value)
  } else if (normality == "shapiro.test") {
    apply(SAT_df, 2, function(x)
      ifelse(length(unique(x)) != 1, shapiro.test(x)$p.value, 0))

  } else {
    apply(SAT_df, 2, function(Q)
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
  export[["values"]] <- data.frame(AUDIO = rownames(SAT_df),
                                   DURATION = DURATIONS,
                                   SAT = SAT_df[, which.max(normal)])
  export[["errors"]] <- data.frame(file = soundfiles[which.error], do.call(rbind, ERRORS))

  return(export)

}
