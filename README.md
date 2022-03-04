---
title: "Rasterizing crop cover information"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Summary of the process

This document summarizes the procedures used for rasterizing and reclassifying the vector files of crop data for the period 2011-2020. 

### Procedure 01

Documented in the *Proc01_RasterizeShp.R* file. 

A DEM of 10m-spatial resolution for the whole Denmark was considered as a reference layer in the rasterization process.

Then the resulting raster layers were converted into categorical rasters and saved as a raster stack in the *CropData2011_2020_10m.tif* file. The *NamesCropData2011_2020_10m.rds* file contains the name of each raster layer. 

### Procedure 02

Documentes in the *Proc02_Crop_drain_req.R* file. The *crop_drain_req.csv* file was used to reclassify the values of the crop cover based on the drainage requirements.

The sum for each class and the total sum were computed and the final result is saved in the *IMK_sum.tif* and *IMK_sum_clipped.tif*. The later is the resulting layer from the sum operation but clipped with the boundaries of Denmark.

