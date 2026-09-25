rowsFD = function(z, digits = 5) {
  h = 2 * matrixStats::rowIQRs(z. <- signif(z, digits = digits))

  if (any(h == 0)) {
    who = which(h == 0)

    al = 1 / 4
    alMin = 1 / 512

    while (length(who) > 0 &&
           (al = al / 2) >= alMin) {
      q = matrixStats::rowQuantiles(z.[who, , drop = FALSE], probs = c(al, 1 - al)
                                    , drop = FALSE)
      h[who] = (q[, 2] - q[, 1]) / (1 - 2 * al)
      who = who[h[who] == 0]
    }
  }

  if (any(h == 0)) {
    who = which(h == 0)
    h[who] = 3.5 * matrixStats::rowSds(z.[who, , drop = FALSE])
  }

  out = ceiling(matrixStats::rowDiffs(matrixStats::rowRanges(z)) / h  * ncol(z)^(1 / 3)) + 1
  out[h < 1] = 1

  return(c(out))
}

rowsSturges = function(z) {
  rep(ceiling(log(ncol(z), base = 2) + 1), nrow(z))
}

rowsscott = function(z) {
  h = 3.5 * sqrt(matrixStats::rowVars(z)) * ncol(z)^(-1 / 3)
  positive = which(h > 0)
  negative = which(h < 0)
  h[negative] = 1
  h[positive] = pmax(1, ceiling(matrixStats::rowDiffs(matrixStats::rowRanges(z)) / h)) + 1

  return(h)
}

rowsNumeric = function(z, n) {
  rep(n + 1, nrow(z))
}
