# Ruido 1.1.0

# Ruido 1.0.3

- Added `noise.matrix` object and associated methods to improve function output handling
- Added two internal helper functions for error handling and normality testing:
  - `argHandler()`
  - `normHandler()`
- Added the `DCfix` argument across all functions to allow users to optionally skip DC offset removal
- Added sample dataset `sampleBGN` for testing
- Added package `testhat` to package suggests
- Updated the `timeBin` argument to actually accept `NULL` (#10)
- Updated `singleSat()` output to be consistent when working with both stereo and mono files (#11)
- Moved all auxiliary functions to a single file

# Ruido 1.0.2

- Added functions:
 - `activity()`
 - `multActivity()`
- Updated the output of already existing functions to include channel, normality statistics and path to audio. New functions also follow the same output pattern
- Added extra examples to show more of the package's utilities. Also updated existing examples to accommodate the changes made to the outputs
- Greatly reduced the number of lines by improving how audios are processed and managed inside the functions
- Improved error handling for all functions
- Overall documentation improvements

# Ruido 1.0.1

- `bgnoise()`, `soundSat()` and `soundMat()` now have better mono audio files handling
- In `soundSat()` and `soundMat()` warnings messages will no longer be included in the output
- `satBackup()` can now continue an unfinished process of `soundMat()`
- Removed the `spelling` package from suggests

# Ruido 1.0.0

* This is the initial CRAN submission.

- Functions:
  - `soundSat()`
  - `singleSat()`
  - `bgnNoise()`
  - `satBackup()`
  - `soundMat()`
