## R CMD check results

Duration: 3m 40.1s

❯ checking DESCRIPTION meta-information ... NOTE
  License stub is invalid DCF.

❯ checking R code for possible problems ... NOTE
  satBackup: no visible binding for global variable 'powthr'
  satBackup: no visible binding for global variable 'bgnthr'
  satBackup: no visible binding for global variable 'wl'
  satBackup: no visible binding for global variable 'timeBin'
  satBackup: no visible binding for global variable 'targetSampRate'
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
    targetSampRate timeBin wl

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

## Resubmission
This is a resubmission! In this new version I have:

* Changed the package sub-title in the DESCRIPTION and switched it to title case.

* Updated the examples to use less heavy files, making them take less time to process and reduce error chance.

* Updated the examples so they can work normally on Linux distros.

* Added sample rate to the output of bgNoise and soundSat.

* Added more detailed to the output of soundSat.

* Switched how the threshold combination picks the most normal distribution.
