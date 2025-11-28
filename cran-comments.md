## Resubmission
This is a resubmission!

## R CMD check results
Duration: 2m 27.8s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## In this new version I have:

* Added the function soundMat()

* Rewritten the package title and description in the DESCRIPTION to add more details about the package.

* Added references to package description, linking the original articles of the methods used

* Switch \dontrun{} to \donttest{} in the examples, with the exception of satBackup(), which needs to manually be stopped to run

* Changed all print() and cat() to warning() or message() depending on the context

* Updated .Rbuildignore to exclude non-related files

* Updated WORDLIST

* Made examples return the user's par() once they finish running
