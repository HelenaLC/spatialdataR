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
    md <- getMetadata(x, re)
    se <- se[, se[[md$region_key]] == re]
    list("sce" = se, "md" = md)
})

setMethod("getMetadata", c("SpatialData", "character"), \(x, re) {
    se <- getTable(x, region = re)
    md <- metadata(se)[[1]]
    md
})

getTable <- function(x, region = NULL, table_name = NULL){
    if(region == NULL && table_name == NULL){
        stop("Either region or table_name must be specified")
    }
    
    if(table_name != NULL){
        return(tables(x)$table_name)
    }

    if(region != NULL){}
        annotators <- getElementAnnotators(x, region)
        if(length(annotators) == 0){
            stop("No table found for region ", region)
        }
        if(length(annotators) > 1){
            stop("Multiple tables found for region ", region)
        }
        annotators[[1]] # in all other cases, only 1 table was found
    }
}

getElementAnnotators <- function(x, element){
    annotators <- lapply(tables(x), function(table){
        md <- metadata(table)[[1]]
        regions <- md$region
        if(element %in% regions){
            table
        }
    })
    annotators[lengths(annotators) != 0] # filter out when no table was found
}
