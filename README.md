# CHdataimprovement

Goal: To improve data usage in the CH process by scripting calculation indicators and automating the importation of CH direct evidence and contributing factors from food security assesments

## Example usage:

Make sure that the following packages are installed:

```R
require(haven)
require(tidyverse)
require(openxlsx)
require(readxl)
require(labelled)
```

and if not install the missing ones.

Open script *MLI_201909_ENSAN_CH* script to select, standardize and calculate CH direct evidence indicators (FCS, HDDS, etc) as well as country specific contributing factors from household survey processed micro data file - *MLI_201909_ENSAN_external.sav*

```
library(haven)
library(tidyverse)
library(labelled)
 

# Load the data
data  <- read_sav("/path/to/MLI_201909_ENSAN_external.sav")

# Create the codebook
codebook <- create_codebook(data)
```