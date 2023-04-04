# Pemba-wealth

Data, code and manuscript for "Cooperation and partner choice in rural Colombia", published in Evolution and Human Behavior
----------------------------

# The manuscript can be found at:

- Evolutiona and Human Behavior: https://www.sciencedirect.com/science/article/abs/pii/S1090513822000484
- psyArxiv (preprint): https://psyarxiv.com/ekwu8/

# Requirements for analyses:

- R: https://cran.r-project.org
- STRAND: https://github.com/ctross/STRAND
- cmdstanr: https://mc-stan.org/cmdstanr/

# Packages used for data processing and visualisation:

- Rethinking: https://xcelab.net/rm/statistical-rethinking/
- iGraph: https://igraph.org/r/
- Rcolorbrewer: https://cran.r-project.org/web/packages/RColorBrewer/index.html
- tidyverse: https://www.tidyverse.org

# Instructions:

In R, set the working directory to that containing this readme file. On Mac, for example, you could say

```
setwd('~/Desktop/friendship-Colombia')
```

Check to see if you're in the right place by typing dir() and see whether this readme file is present.

# Details

To reproduce the results presented in the manuscript, please first go into the 'code/' folder of the repository. It would probably be best to review the many different scripts for data processing, analysis and visualisation. Then, if you would like to reproduce the results reported in the publication, first call the data processing files for each specific community. This can be done by simply downloading the repository, and then (for example) calling:

``````````
source("./code/process_BS_data.R")
``````````

in the R console. To run the analyses, you can then call (e.g.,):

``````````
source("./code/BS_analyses.R")
``````````

To make the plots *for all sites* that are shown in the main manuscript, you can call

``````````
source("./code/merged_plots.R")
``````````

after running all analysis scripts.

The project is maintained by Daniel Redhead (daniel_redhead@eva.mpg.de) and Cody Ross (cody_ross@eva.mpg.de) and is hosted at https://github.com/danielRedhead
