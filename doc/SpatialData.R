## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache=FALSE, message=FALSE, warning=FALSE)

## ----load-libs----------------------------------------------------------------
library(SpatialData)
library(SingleCellExperiment)

## ----blobs-read---------------------------------------------------------------
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
(x <- readSpatialData(x, anndataR=FALSE))  # VJC Dec 8, test effect of absence of anndataR

## ----get, results="hide"------------------------------------------------------
shapeNames(x) # there are two images
shapes(x) # this is a list of them
shape(x, i=1) # this is the 1st one
shape(x, "blobs_polygons") # this is the 3nd one
x$shapes$blobs_circles # list-like handling also works

## ----data/meta----------------------------------------------------------------
data(shape(x))
meta(shape(x)) 

## ----tbl----------------------------------------------------------------------
# retrieve 'table' for an element
(t <- getTable(x, i <- "blobs_labels"))
head(colData(t))
meta(t)
# get values from an element's 'table'
valTable(x, i, "channel_0_sum")
# add an artificial 'table' to an element
i <- "blobs_points"
# ...passing a preconstructed 'data.frame'
id <- unique(point(x)$instance_id)
df <- data.frame(n=runif(length(id)))
y <- setTable(x, i, df)
head(colData(getTable(y, i)))
# ...using a list of data generating functions
f <- list(
    numbers=\(n) runif(n),
    letters=\(n) sample(letters, n, TRUE))
args <- c(list(x, i), f)
y <- do.call(setTable, args)
head(colData(getTable(y, i)))

## ----cs-methods---------------------------------------------------------------
z <- meta(label(x))
axes(z)
CTdata(z)
CTname(rmvCT(z, "scale"))
CTname(addCT(z, 
    name="D'Artagnan", 
    type="scale", 
    data=c(19, 94)))

## ----cs-graph-----------------------------------------------------------------
(g <- CTgraph(x))
plotCoordGraph(g)

## ----cs-path, results="hold"--------------------------------------------------
# the following are equivalent
i <- "blobs_labels"
j <- "sequence"
y <- label(x, i)
CTpath(x, i, j)
invisible(CTpath(y, j))

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

