test_that("getSampleBins() converts seconds to samples", {

  expect_equal(getSampleBins(723000, 12050, 10), data.frame(
    b = c(1, 120501, 241001, 361501, 482001, 602501),
    e = c(120500, 241000, 361500, 482000, 602500, 723000)
  ))

})

test_that("hanning() and hamming() can't generate windows of length 1", {

  expect_error(hanning(1))
  expect_error(hamming(1))

})
