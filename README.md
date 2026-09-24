# Ruido - Development Branch

<img src="man/figures/ruidoIconDEV.png" alt="Icon of Ruido Development Branch" align="right" height="300"/>

[![R-CMD-check](https://github.com/Arthurigorr/Ruido/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Arthurigorr/Ruido/actions/workflows/R-CMD-check.yaml) ![lastGitCommit](https://img.shields.io/github/last-commit/Arthurigorr/Ruido) [![codecov](https://codecov.io/gh/Arthurigorr/Ruido/branch/dev/graph/badge.svg?token=QT8GPOPEDN)](https://codecov.io/gh/Arthurigorr/Ruido)

> **⚠️ Development version:** Code in this branch may be incomplete, experimental, or broken. Use the `main` branch for the stable version of Ruido!

This is the development branch of **Ruido**. Its main purpose is to test new features and changes before they are merged into the `main` branch, helping prevent untested or broken code from affecting the stable version of the package.

### Current Goals:
- [x] Create functions to calculate summarized versions of Background Noise and Soundscape Power — **90%**
  - [x] Function
  - [x] Documentation
  - [x] Examples
  - [x] Tests
  - [ ] Optimize
- [ ] Create functions for the remaining spectral indices — **0%**
  - [ ] Events per Second — **0%**
  - [ ] Spectral Peaks — **0%**
- [ ] Create a function to calculate and plot false-color spectrograms — **0%**
- [ ] Improve processing speed for `bgNoise()` — **3%**

### Current bottleneck and optimization targets in `bgNoise()`:
```text
processChannel.BGN
│
├── Spectrogram generation ⚠️ ........ ~31%
│   ├── .spect .................... ~31% total
│   └── mvfft ...................... ~7%
│
├── Histogram bin calculation ⚠️ ..... ~23%
│   └── Freedman–Diaconis
│       ├── IQR / quantile ........ ~19%
│       └── sorting ............... ~8%
│
└── Other operations .............. ~46%
```
