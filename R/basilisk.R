
anndata_env <- basilisk::BasiliskEnvironment(envname="anndata_loader_env",
                                pkgname="SpatialData",
                              packages=c("anndata==0.9.1", "zarr==2.14.2")
)
