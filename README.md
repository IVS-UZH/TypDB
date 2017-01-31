# TypDB - a collection of typological datasets

## Installation

Make sure you have `devtools` package installed. Then, enter the following command in your R console:

    devtools::install_github("IVS-UZH/TypDB")
    
## Usage

TypDB imports a number of typological databases as R data frames. For a list of the available data, see `?TypDB`. You can directly use the datasets by referring to them with their names, e.g.

    head(glottolog)
    
TypDB maintains a language code mapping table (`?language_ids`). This table contains mappings of common language codes, such as ISO 639-3 and Glottolog codes. A utility function is provided for merging different datasets by common language code. See `?typdb.data.merge` for more information. 

## Citing the data

All included datasets are property of their respective owners. TypDB merely includes them in a ready-to-use format for user convenience. Please cite the respective source if you are using the data in a publication. 

