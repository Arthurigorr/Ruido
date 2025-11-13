## R CMD check results

Duration: 1m 20.2s

❯ checking top-level files ... NOTE
  Non-standard file/directory found at top level:
    'paper.md'

❯ checking R code for possible problems ... NOTE
  satBackup: no visible binding for global variable 'powthr'
  satBackup: no visible binding for global variable 'bgnthr'
  satBackup: no visible binding for global variable 'wl'
  satBackup: no visible binding for global variable 'timeBin'
  satBackup: no visible binding for global variable 'targetSampRate'
  satBackup: no visible binding for global variable 'window'
  satBackup: no visible binding for global variable 'overlap'
  satBackup: no visible binding for global variable 'channel'
  satBackup: no visible binding for global variable 'dbThreshold'
  satBackup: no visible binding for global variable 'histbreaks'
  satBackup : <anonymous>: no visible binding for global variable
    'bgnthr'
  satBackup: no visible binding for global variable 'normality'
  satBackup : <anonymous>: no visible binding for global variable
    'normality'
  Undefined global functions or variables:
    bgnthr channel dbThreshold histbreaks normality overlap powthr
    targetSampRate timeBin window wl
  Consider adding
    importFrom("stats", "window")
  to your NAMESPACE file.

0 errors | 0 warnings | 2 notes

## Resubmission
This is a resubmission! In this new version I have:

* Converted the DESCRIPTION title to title case.

* Updated the examples to use less heavy files, making them take less time to process and reduce error chance.

* Added sample rate to the output of bgNoise and soundSat.
