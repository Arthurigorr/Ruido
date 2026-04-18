# Ruido 1.0.3

- Created `noise.matrix` object and methods for better function output handling and calling
- Added argument `DCfix` to all functions to give the user the option to skip DC offset removal
- Updated argument `timeBin` to accept `NULL` (#10)
- Made `singleSat()` output consistent when working with both stereo and mono files (#11)
- Improved error and normality handling with new internal functions:
  - `argHandler()`
  - `normHandler()`
- Moved all auxiliary functions to a single file
- Added sample data for tests (sampleBGN)
- Added package `testhat` to package suggests
- Overall functions improvement

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
