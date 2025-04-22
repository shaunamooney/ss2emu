# ss2emu

**An R Package for Calculating Estimated Modern Contraceptive Use (EMU) from Family Planning Service Statistics**

`ss2emu` is an open-source R package developed to improve the accuracy, usability, and reproducibility of EMU data used in family planning monitoring. It implements the most up-to-date version of the SS-to-EMU process, including uncertainty quantification, and produces outputs including an EMU database and visualisation.

---

## 📦 Features

- Calculates EMU from four types of service statistics data
- Applies a time-varying, probabilistic private sector adjustment to account for missing private sector contributions
- Produces output suitable for use in the Family Planning Estimation Tool for informing estimates of modern contraceptive prevalence
- Shiny app interface available for use in workshop settings

---

## 🚀 Installation

```r
# Install from GitHub (requires devtools)
install.packages("devtools")
devtools::install_github("shaunamooney/ss2emu")
