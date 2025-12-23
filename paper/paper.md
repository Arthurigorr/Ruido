---
title: 'Ruido: A Streamline Package To Calculate Soundscape Saturation'
tags:
  - R
  - Ecoacoustics
  - Soundscape
  - Ecology
  - Package
authors:
- name: Arthur Igor da Fonseca-Freire
  orcid: 0009-0002-4546-7731
  corresponding: true
  affiliation: 1
- name: Weslley Geremias dos Santos
  orcid: 0000-0002-0749-9081
  affiliation: 2
- name: Lucas Rodriguez Forti
  orcid: 0000-0003-3057-2141
  affiliation: 1, 2
affiliations:
- name: Observatório Ambiental do Semiárido (OAS), Centro de Ciências Biológicas e da Saúde (CCBS), Universidade Federal Rural do Semi-Árido (UFERSA)
  index: 1
- name: Faculdade de Engenharia Elétrica e de Computação (FEEC), Universidade Estadual de Campinas (UNICAMP)
  index: 2
date: 5 December 2025
bibliography: paper.bib
---

# Summary

Noise is defined as disruptive sound that interferes with acoustic communication and may affect biodiversity and human health. The increasing production of large-scale acoustic data has intensified the need for tools to quantify background noise within soundscapes in a efficient way. Already existing R packages provide useful tools for computing acoustic metrics, but lack newer indices. To address this gap, we introduce `Ruido`, an R package that computes three key metrics: Background Noise (BGN), Soundscape Power (POW), and Soundscape Saturation (SAT). BGN represents persistent sound intensity, POW measures the distinctiveness of transient acoustic events, and SAT estimates the density of acoustic activity as a proxy for biological richness. These metrics enable more comprehensive characterization of soundscape dynamics and facilitate large-scale ecoacoustic analyses.

# Statement of Need

Given the growing use of acoustic metrics in soundscape analysis, recent efforts have focused on developing accessible and standardized tools to support ecoacoustic research [@alcocer_acoustic_2022; @bradfer-lawrence_guidelines_2019]. In response, several R packages, such as soundecology [@pijanowski_soundecology_2018], seewave [@sueur_seewave_2025], and warbleR [@araya-salas_warbler_2017], have made it easier to compute acoustic metrics and visualize sound data, even for researchers with limited programming skills.

Although these R packages offer a wide range of acoustic metrics, they often lack support for some newer measures. To address this gap, we present `Ruido`, an R package designed to efficiently compute three key acoustic metrics: Background Noise (BGN), Soundscape Power (POW), and Soundscape Saturation (SAT). Building upon the conceptual framework introduced by @towsey_calculation_2017, these metrics are implemented in a streamlined, scalable, and fully reproducible workflow within the R environment, enabling their application across large audio datasets.

In this work, we define BGN as the modal decibel intensity within each frequency window of a spectrogram. The output of the BGN function is a matrix in which each cell represents the background noise level for a specific time-frequency bin. To complement the BGN evaluation, @towsey_calculation_2017 proposed the POW, formerly referred to as the Signal-to-Noise Ratio. POW is defined as the difference between the maximum and modal decibel values within each frequency window. It quantifies how distinct episodic acoustic events are from persistent background noise. Both metrics provide valuable insights into the temporal and spectral dynamics of background noise.

The third index, SAT, has gained traction in recent ecoacoustic studies as a proxy for biological activity and acoustic richness [@burivalova_using_2018]. While its conceptual foundations are less formally established than BGN or POW, SAT is complementary and reflects the proportion of time-frequency bins exceeding a given amplitude threshold, which captures the density of acoustic events across the soundscape. This metric aligns with the acoustic niche hypothesis, which proposes that coexisting species adjust their vocal timing or frequency to minimize overlap and competition for acoustic space [@krause_niche_1987]. In more stable environments, species tend to partition the soundscape by occupying distinct frequency bands, resulting in higher frequency occupancy [@krause_measuring_2011]. Consequently, greater acoustic saturation often indicates a richer vocal community [@gasc_future_2017].

Finally, by operationalizing these emerging metrics alongside established indices, `Ruido` simplifies ecoacoustic workflows and enhances researchers’ capacity to analyze large soundscape collections with minimal effort.

# Example

To illustrate the application of the available metrics in the `Ruido` package, we present a first example depicting the diurnal variation of Soundscape Saturation using the soundSat() function. This acoustic metric was derived from 24 stereo recordings captured over a full day using a Song Meter SM4 (Wildlife Acoustics), programmed to record 3 minutes every 10 minutes at a 48 kHz sampling rate and 16 bit resolution. We selected only the first recording of each hour to analyse. The left and right channels were configured with gains of 16 dB and 10 dB, respectively. Data were gathered on April 1st during the rainy season in a *Mimosa tenuiflora* forest at the campus of the Universidade Federal Rural do Semi-Árido (UFERSA) in Mossoró-RN, Northeastern Brazil. All recordings used in this example are available at <https://zenodo.org/records/17243660>, ensuring full reproducibility and allowing users to replicate the analyses presented.

`Ruido` package provides a straightforward method for calculating the SAT via the soundSat() function. In this example, we computed SAT for each 24-hour cycle to quantify the proportion (%) of occupied frequency bands. After organizing the audio files into the appropriate directory structure, we proceeded with the analysis using the default settings for the function arguments, which are: channel = “stereo”, time_bin = 60, dbThreshold = -90, targetSampRate = NULL, wl = 512, window = signal::hamming(wl), overlap = ceiling(length(window) / 2), histbreaks = “FD”, powthr = c(5.1, 20, 0.1), bgnthr = c(0.51, 0.99, 0.02), normality = “ad.test”, beta = TRUE and backup = NULL. The The script lines are as follows:

``` r
dir <- tempdir()
recName <- paste0("GAL24576_20250401_", sprintf("%06d", seq(0, 230000, by = 10000)),".wav")
for(rec in recName) {
 print(rec)
 url <- paste0("https://zenodo.org/records/17243660/files/", rec, "?download=1")
 download.file(url, destfile = paste(dir, rec, sep = "/"), mode = "wb")
}

sat <- soundSat(dir)

# The resulted output is exhibited as follows:
> head(sat$values)
             PATH                        AUDIO CHANNEL DURATION BIN SAMPRATE       SAT
1 /tmp/RtmptnAWmi GAL24576_20250401_000000.wav    left       60   1    48000 0.3906250
2 /tmp/RtmptnAWmi GAL24576_20250401_000000.wav    left       60   2    48000 0.4492188
3 /tmp/RtmptnAWmi GAL24576_20250401_000000.wav    left       60   3    48000 0.4257812
4 /tmp/RtmptnAWmi GAL24576_20250401_000000.wav   right       60   1    48000 0.5312500
5 /tmp/RtmptnAWmi GAL24576_20250401_000000.wav   right       60   2    48000 0.5117188
6 /tmp/RtmptnAWmi GAL24576_20250401_000000.wav   right       60   3    48000 0.6171875

> sat$powthresh
[1] 20
> sat$bgntresh
[1] 60
> sat$normality
$test
[1] "ad.test"

$statistic
[1] 1.730193
```

We subsequently calculated mean values for each hour of the day, along with corresponding confidence intervals, to visualize the results in a time series graph (Figure 1). This approach provides a practical example of how users can process and explore their own data to identify temporal patterns and assess acoustic variability.

![**Figure 1.** SAT variation throughout the day. The line and dots represent the mean values of each hour of the day and the colored ribbon represents standard deviancy for each side.](figures/SAT.png)

The SAT showed substantial variation throughout the day, with pronounced peaks occurring both during the early nighttime hours and at dusk. The nocturnal acoustic richness, likely driven by vocalizing frogs and insects, reflects characteristic soundscape patterns of the rainy season in semiarid environments.

# Conclusion

Finally, our practical examples highlighted that the package offers a user-friendly and flexible framework for acoustic data analysis, enabling researchers to efficiently process large datasets, replicate workflows across extended temporal scales, and extract meaningful patterns with minimal coding effort. Its accessibility empowers broader adoption of ecoacoustic methods and fosters deeper insights into soundscape ecology.

# Acknowledgements

We are grateful to the Financiadora de Estudos e Projetos (FINEP) for financial support through the Escutadô Project (grant number 01.23.0702.00).

# References
