#' @title Get Region Data
#' @description This method retrieves data for a specified region from a SpatialData object.
#' @param x A SpatialData object from which the region data is to be retrieved.
#' @param re A character string specifying the region for which data is to be retrieved.
#' @return A SingleCellExperiment object containing the data for the specified region.
#' @examples
#' # Assuming `spatial_data` is a SpatialData object and "region_name" is the name of the region
#' region_data <- getRegionData(spatial_data, "region_name")
#' @export
setMethod("getRegionData", c("SpatialData", "character"), \(x, re) {
    se <- getTable(x, region = re)
    md <- metadata(se)[[1]]
    se <- se[, se[[md$region_key]] == re]
    list("sce" = se, "md" = md)
})

setMethod("getTable", c("SpatialData", "ANY", "ANY"), \(x, region = NULL, table_name = NULL) {
    if (is.null(region) && is.null(table_name)) {
        stop("Either region or table_name must be specified")
    }
    
    if (!is.null(table_name)) {
        return(tables(x)[[table_name]])
    }

    if (!is.null(region)) {
        annotators <- getElementAnnotators(x, region)
        if (length(annotators) == 0) {
            stop("No table found for region ", region)
        }
        if (length(annotators) > 1) {
            stop("Multiple tables found for region ", region)
        }
        annotators[[1]] # in all other cases, only 1 table was found
    }
})

setMethod("getElementAnnotators", c("SpatialData", "character"), \(x, element) {
    annotators <- lapply(tables(x), \(table) {
        md <- metadata(table)[[1]]
        regions <- md$region
        if (element %in% regions) {
            table
        }
    })
    annotators[lengths(annotators) != 0] # filter out when no table was found
})

# very very basic
# tested with PointArray
getValues <- function(value_key, element=NULL, sdata=NULL, element_name=NULL, table_name=NULL) {
    if(!is.null(element) && value_key %in% names(element)){
        return(element[[value_key]])
    }

    if(!is.null(sdata) && !is.null(element_name) && !is.null(table_name)){
        table <- getRegionData(sdata, re = element_name)
        if(value_key %in% rownames(table)){
            return(table[value_key])
        }
        if(value_key %in% colnames(table)){
            return(table[value_key])
        }
    }

}

#get_values(value_key="channel_0_sum", sdata=sdata, element_name="blobs_labels", table_name="table")
