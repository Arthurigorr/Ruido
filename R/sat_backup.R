#' Backup for Soundscape Saturation Index
#'
#' @param backup_path The same path you set in your "backup" in the soundsat function. Audiofiles already finished will be drawn from this path.
#' @param od The path or paths containing your original audiofiles.
#' @param time_bin The size (in seconds) of your time bin.
#' @param target_samp_rate The sampling rate of the spectrogram (this argument is only used to down sample the sample rate)
#' @param wl The window length of your spectrogram (default = 512)
#' @param window The window used to smooth the signal (default = hamming(wl))
#' @param overlap Overlap between the spectrogram windows
#' @param channel The desired channel of your audiofile. Available channels are: "stereo", "mono", "left" or "right"
#' @param db_threshold The minimum possible threshold for the dB values of the spectrogram (default = -90)
#' @param histbreaks Which breaks to use to calculate background noise (default = "FD")
#' @param powthr The a vector of values to evaluate the activity matrix for soundscape power (in dB). The first value corresponds to the lowest dB value and the second corresponds to the highest, the third value is the step of each value.
#' @param bgnthr The values to evaluate the activity matrix for background noise (in %). The first value corresponds to the lowest quantile value and the second corresponds to the highest, the third value is the step of each value.
#' @param normality The normality test to determine which threshold has the most normal distribution of values. We recommend you pick any test from the "nortest" package, but if you have few recordings and your bins doesn't exceeds 5000 we reccomend using the shapiro.test function.
#'
#' @description
#' This function is a way to backup an unfinished proccess of the soundsat function.
#'
#' @returns
#' A list containing five objects. The first and second objects (powthresh and bgnthresh) are the threshold values that yielded the most normal distribution of saturation values. The third (normality) contains the p values of the normality test that yielded the most normal distribution. The fourth object (values) contains a data.frame with the the values of saturation for each bin of each recording and the size of the bin in seconds. The fifth contains a data.frame with errors that ocurred with specific files during the function.
#'
#' @export
#'
#' @examples ### I played a game with eleven fools who told me not to break the rules
#' ### But when have angels ever helped me yet?
sat_backup <- function(backup_path,
                       od,
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
                       normality = "ad.test") {
  # source("BGN_POW/SATURATION.R")

  backup = backup_path

  backfiles <- list.files(backup_path, pattern = ".R", full.names = TRUE)
  originalfiles <- list.files(od, full.names = TRUE, recursive = TRUE)
  originalfiles <- originalfiles[tools::file_ext(originalfiles) %in% c("mp3", "wav")]

  remainingfiles <- originalfiles[!(basename(originalfiles) %in% basename(tools::file_path_sans_ext(backfiles)))]

  powthreshold <- seq(powthr[1], powthr[2], powthr[3])
  names(powthreshold) <- powthreshold
  bgnthreshold <- seq(bgnthr[1], bgnthr[2], bgnthr[3])

  threshold_combinations <- setNames(expand.grid(powthreshold, bgnthreshold),
                                     c("powthreshold", "bgnthreshold"))

  combinations <- paste(threshold_combinations[, 1], threshold_combinations[, 2], sep = "/")

  half_wl <- wl / 2

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
      print("You were warned.")
    } else {
      stop("We will consider that a No.")
    }

  }

  if (length(remainingfiles) == 0) {
    message("All files have already been processed.")

    SAT_df <- lapply(backfiles, readRDS)

  } else {
    SAT_df <- lapply(remainingfiles, function(soundfile) {
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


          singsat <- as.data.frame(
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
          real_channel <- c("left", "right")[c("left", "right") %in% names(BGN_POW)]

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


          singsat <- as.data.frame(
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

          rownames(singsat) <- paste0(basename(soundfile),
                                      "_",
                                      real_channel,
                                      "_bin",
                                      seq(nrow(singsat)))

          DURATION <- BGN_POW$time_bins

        }

        cat(
          "\r(",
          basename(soundfile),
          ") ",
          match(soundfile, originalfiles),
          " out of ",
          length(originalfiles),
          " recordinds concluded!",
          sep = ""
        )

        if (!is.null(backup)) {
          save_l <- list(SAT = singsat, DUR = DURATION)

          saveRDS(save_l, file = paste0(backup, "/", basename(soundfile), ".RData"))

          rm(save_l)
        }

        gc()

        return(list(SAT = singsat, DUR = DURATION))
      }

    })

    SAT_df <- c(lapply(backfiles, readRDS), SAT_df)

  }

  which.error <- sapply(SAT_df, function(x)
    is(x, "error") || is(x, "warning"))
  ERRORS <- SAT_df[which.error]
  DURATIONS <- as.numeric(unlist(sapply(SAT_df[!which.error], function(x)
    x[["DUR"]])))
  SAT_df <- do.call(rbind, lapply(SAT_df[!which.error], function(x)
    x[["SAT"]]))

  colnames(SAT_df) <- combinations

  normal <- if (normality == "shapiro.test") {
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
  export[["errors"]] <- data.frame(file = originalfiles[which.error], do.call(rbind, ERRORS))

  return(export)

}
