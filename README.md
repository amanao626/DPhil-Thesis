# R Scripts and Materials for Doctoral Thesis  
**Intelligibility and Comprehensibility of Accented Speech: The Case of Japanese Speakers of English**

## Overview
This repository contains R scripts and related materials used in my doctoral thesis submitted to the University of Oxford.  
The thesis has two main aims:  
1) to investigate factors that affect the intelligibility of Japanese-accented English; and  
2) to examine the potential for rapid adaptation to Japanese-accented English following training.

Because of space constraints in the thesis, this repository provides additional resources and the code required to reproduce the statistical analyses and figures (with synthetic or example-ready structures where original data cannot be shared).

## How to use
1. **Clone** this repository or download it as a ZIP.  
2. Open the `.Rproj` file (or individual scripts) in **RStudio**.  
3. Run scripts in the relevant experiment folder (e.g., `scripts/Experiment 1/`) to reproduce analyses and regenerate figures.  

> 💡 Although original data are not included, scripts are fully annotated so you can follow the workflow and reproduce results with similarly structured data.

## Requirements
- R (≥ 4.x) and RStudio (recommended)  
- Key packages: `tidyverse`, `lme4`, `emmeans`, `DHARMa`, `ggplot2`, `performance`  
  - Install with:
    ```r
    install.packages(c("tidyverse","lme4","emmeans","DHARMa","ggplot2","performance"))
    ```

## Contact
For questions about the code, figures, or analysis pipeline:  
`naosuke.amano@sant.ox.ac.uk`


