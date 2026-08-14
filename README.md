# Dataset

Dataset published at https://ipt.biodiversity.aq/resource?r=rov-benthic-morphotaxa-wap-tango-2023-2024
Registered to GBIF, added to OBIS and SCAR network.

GitHub issue for this dataset: https://github.com/biodiversity-aq/ADVANCE/issues/252

# Paper

https://onlinelibrary.wiley.com/doi/10.1002/ece3.73392

# Dryad

dryad: https://doi.org/10.5061/dryad.stqjq2cj3
zenodo: https://doi.org/10.5281/zenodo.16736897

Dataset DOI: [10.5061/dryad.stqjq2cj3](https://doi.org/10.5061/dryad.stqjq2cj3)

## Description of the data and file structure

Since the identification level varied among taxa (species, genus, or family), we use the term "morphotaxa" to refer to the identified biota throughout this study. Given the distance, resolution of images, habitat type, and size and colour of some taxa, many taxa will not have been observable or fully accounted for. The example images (Table S2) demonstrate that sub-centimetre sized organism, those with camouflage or cryptic habitat choices, or any infauna are unlikely to be well represented, if at all. Whilst other collection methods were used during the second TANGO expedition, only organisms that could be confidently observed from overhead photography were analysed for this study.

For more common morphotaxa, we were able to confidently assign species, as some were hand-picked by SCUBA divers. However, when labels lacked the precision required for this study, we chose to omit the corresponding images. The resulting abundance dataset comprised 336 annotated photos which included 2410 individual annotations, 91 identified CATAMI categories (of which 13 were substrate features). Individual animals were kept as counts, but the abundance of macroalgae and colonial animals was converted into percentage (%) cover.

The data were compiled into a matrix for each image and the abundance or coverage of each morphotaxa: tango2_abundance_raw.csv. The matrix is structured with the rows representing 336 different images and the 81 columns representing the abundance or percentage (%) cover of the morphotaxa identified in those photographs. The first column gives the image number and subsequent columns give morphotaxa names in alphabetical order.

## Access information

Other publicly accessible locations of the data:

* [https://doi.org/10.5281/zenodo.16736897](https://doi.org/10.5281/zenodo.16736897)

---

# Paper's Supplementary info
https://onlinelibrary.wiley.com/doi/10.1002/ece3.73392

material | description
---|---
Table S1 | Description and characteristics of most abundant SIMPROF groups.
Table S2 | Summary table of station biodiversity indexes.
Figure S1 | Boxplots of biodiversity metrics of studied sites. (a) Shannon–Wiener diversity index. (b) Pielou's evenness index. (c) Number of different morphotaxa. (d) Number of different microhabitats. SIMPER Analysis results on microhabitat groupings


---

# CATAMI label

https://zenodo.org/records/12653521

# Questions

> Individual animals were kept as counts, but the abundance of macroalgae and colonial animals was converted into percentage (%) cover.

- check depth range from paper and dataset

---

## Log

The abundance files contain `\r`. Remove it with command:

```bash
tr -d '\r' < tango1_abundance_raw.csv > tango1_abundance_remove-cr.csv
tr -d '\r' < tango2_abundance_raw.csv > tango2_abundance_remove-cr.csv
```
