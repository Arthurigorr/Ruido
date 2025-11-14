## Resubmission
This is a resubmission!

## R CMD check results
Duration: 48.6s

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Arthur Igor da Fonseca-Freire <arthur.igorr@gmail.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  BGN (10:100)

Found the following URLs which should use \doi (with the DOI name only):
  File 'bgNoise.Rd':
    https://doi.org/10.1109/TASSP.1981.1163642
  File 'singleSat.Rd':
    https://doi.org/10.1111/cobi.12968
  File 'soundSat.Rd':
    https://doi.org/10.1111/cobi.12968

0 errors ✔ | 0 warnings ✔ | 1 notes ✔

* BGN is not misspelled

* I prefer to leave the DOIs as links, so I'll ignore these notes

## In this new version I have:

* Changed the package sub-title in the DESCRIPTION and switched it to title case.

* Updated the examples to use locally generated audios and moved the examples that download audios from Zenodo to \dontrun{} to reduce check time

* Updated the functions and examples so they can work normally on Linux distros.

* Updated bgNoise and soundSat outputs.

* Fixed the author citations.

* Updated .Rbuildignore to exclude non-related files.

* Added soundSat arguments to globalVariables() to tell the check that the arguments used in satBack are loaded from an external file
