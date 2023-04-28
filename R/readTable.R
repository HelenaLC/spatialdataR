

readTable <- function(path){
  proc <- basilisk::basiliskStart(anndata_env)
  on.exit(basilisk::basiliskStop(proc))

  sce <- basilisk::basiliskRun(proc, fun=function(arg1) {
    ad <- reticulate::import("anndata")
    obj <- ad$read_zarr(arg1)
    output <- zellkonverter::AnnData2SCE(obj)
    # The return value MUST be a pure R object, i.e., no reticulate
    # Python objects, no pointers to shared memory.
    output
  }, arg1=path)
  sce
}
