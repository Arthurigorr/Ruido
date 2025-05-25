get_sample_bins <- function(samples, samp.rate, bin_size) {
  b <- seq(1, samples, by = samp.rate * bin_size)
  e <- pmin(b + samp.rate * bin_size - 1, samples)

  keepthese <- ((samp.rate * bin_size) * 0.1) < e - b

  if (length(b) == length(e)) {
    data.frame(b, e)[keepthese, ]
  } else {
    data.frame(b, e = c(e, samples))
  }

}
