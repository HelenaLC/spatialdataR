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
        md <- getTableAttrs(table)
        regions <- md$region
        if (element %in% regions) {
            table
        }
    })
    annotators[lengths(annotators) != 0] # filter out when no table was found
})

setMethod("getTableAttrs", c("SingleCellExperiment"), \(x) {
    int_metadata(x)$spatialdata_attrs
})

# very very basic
# tested with PointArray
# get the values from an element:
# value_key: name of the column/channel
# element: the spatial element to get the values from
# sdata: the spatial data object
# element_name: the name of the element to get the values from
# table_name: the name of the table to get the values from

# if you provide a table name, subset using element_name if porvided, and get the value_key column or channel
getValues <- function(value_key, element=NULL, sdata=NULL, element_name=NULL, table_name=NULL) {

    # if table name is provided, get the table and subset using element_name if provided
    # then return the value_key column or channel
    if(!is.null(table_name)){
        table <- getTable(sdata, table_name=table_name)

        if(!is.null(element_name)){
            md <- getTableAttrs(table)
            table <- table[, table[[md$region_key]] == element_name]
        }

        if(value_key %in% colnames(table)){
            return(table[,value_key])
        }
        if(value_key %in% rownames(table)){
            return(table[value_key])
        }

    }

}

#get_values(value_key="channel_0_sum", sdata=sdata, element_name="blobs_labels", table_name="table")
