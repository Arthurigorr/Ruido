test_that("getSampleBins() converts seconds to samples", {
  expect_equal(getSampleBins(723000, 12050, 10), data.frame(
    b = c(1, 120501, 241001, 361501, 482001, 602501),
    e = c(120500, 241000, 361500, 482000, 602500, 723000)
  ))
})
