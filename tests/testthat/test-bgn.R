test_that("bgn can input tuneR objects, work with default arguments and alternative arguments",
          {
            library(tuneR)

            samprate = 12050
            dur = 60
            n = samprate * dur

            set.seed(413)
            noise = rnorm(n)

            fade = seq(1, 0, length.out = n)

            signal = noise * fade

            wave1 = tuneR::Wave(
              left = signal,
              right = signal,
              samp.rate = samprate,
              bit = 16
            )
            wave2 = tuneR::Wave(left = signal,
                                samp.rate = samprate,
                                bit = 16)
            wave3 = tuneR::Wave(
              left = rep(c(1, 0), length.out = samprate * dur),
              right = rep(c(0, 1), length.out = samprate * dur),
              samp.rate = samprate,
              bit = 16
            )
            wave4 = tuneR::Wave(
              left = signal,
              right = rev(signal),
              samp.rate = samprate,
              bit = 8
            )
            wave5 = mono(wave1)

            expect_type(bgn(wave1), "list")
            expect_type(bgn(wave1, timeBin = NULL), "list")
            expect_type(bgn(wave1, channel = "mono"), "list")
            expect_type(bgn(wave2), "list")
            expect_type(bgn(wave2, wl = 256), "list")
            expect_type(bgn(wave3, channel = "right"), "list")
            expect_type(bgn(wave3, DCfix = FALSE, dbThreshold = -60), "list")
            expect_type(bgn(wave3, histbreaks = 100), "list")
            expect_type(bgn(wave4, timeBin = 10), "list")
            expect_type(bgn(wave4, timeBin = 180), "list")
            expect_type(bgn(wave5, targetSampRate = samprate / 2), "list")
            expect_type(bgn(wave5, targetSampRate = samprate * 2), "list")
            expect_type(
              bgn(
                wave1,
                channel = "mono",
                timeBin = 10,
                dbThreshold = -50,
                targetSampRate = 6025,
                wl = 256,
                histbreaks = "Sturges",
                DCfix = FALSE
              ),
              "list"
            )
            expect_error(bgn("made-up-nonsense.png"))

          })
