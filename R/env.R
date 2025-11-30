.pip = c("session_info==1.0.0", "spatialdata==0.3.0", "spatialdata_io==0.1.7", 
    "pillow==11.1.0", "anndata==0.11.3", "annotated_types==0.7.0", "asciitree==0.3.3", 
    "attr==0.3.2", "certifi==2025.01.31", "charset_normalizer==3.4.1", 
    "click==8.1.8", "cloudpickle==3.1.1", "cycler==0.12.1", "dask==2024.4.1", 
    "dask_image==2024.5.3", "datashader==0.17.0", 
    "deprecated==1.2.18", "distributed==2024.4.1", 
    "flowio==1.3.0", "fsspec==2025.2.0", "geopandas==1.0.1", "h5py==3.12.1", 
    "idna==3.10", "imagecodecs==2024.12.30", "imageio==2.37.0", "jinja2==3.1.5", 
    "joblib==1.4.2", "kiwisolver==1.4.8", "lazy_loader==0.4", "legacy_api_wrap==1.4.1", 
    "llvmlite==0.44.0", "locket==1.0.0", "markupsafe==3.0.2", "matplotlib==3.10.0", 
    "more_itertools==10.3.0", "msgpack==1.1.0", "multipledispatch==0.6.0", 
    "multiscale_spatial_image==2.0.2", "natsort==8.4.0", "networkx==3.4.2", 
    "numba==0.61.0", "numcodecs==0.15.1", "numpy==2.1.3", "ome_types==0.5.3", 
    "ome_zarr==0.10.3", "packaging==24.2", "pandas==2.2.3", "param==2.2.0", 
    "pims==0.7", "platformdirs==4.3.6", "psutil==7.0.0", "pyarrow==19.0.0", 
    "pyct==0.5.0", "pydantic==2.10.6", "pydantic_compat==0.1.2", 
    "pydantic_core==2.27.2", "pygments==2.19.1", "pyparsing==3.2.1", 
    "pyproj==3.7.0", "pytz==2025.1", "readfcs==2.0.1", "requests==2.32.3", 
    "rich==13.9.4", "scanpy==1.11.0", "scipy==1.15.1", "setuptools==75.8.0", 
    "shapely==2.0.7", "six==1.17.0", "scikit-image==0.25.1", "scikit-learn==1.5.2", 
    "slicerator==1.1.0", "sortedcontainers==2.4.0", "spatial_image==1.1.0", 
    "tblib==3.0.0", "threadpoolctl==3.5.0", "tifffile==2025.1.10", 
    "toolz==1.0.0", "tornado==6.4.2", "tqdm==4.67.1", 
    "typing_extensions==4.12.2", "urllib3==2.3.0", "wrapt==1.17.2", 
    "xarray==2024.11.0", "xarray_dataclasses==1.9.1", "xarray_schema==0.0.3", 
    "zarr==2.18.4", "zict==3.0.0")
# notes from VJC -- readSpatialData was modified below so
# that if anndataR = FALSE, spatialdata.read_zarr is used
# to get the whole zarr store, and then the tables are
# transformed via zellkonverter.  this gives a 10x speedup
# for ingesting the visium_hd_3.0.0 example but fails on
# the blobs dataset in example("table-utils") because
# of matters related to metadata/hasTable behavior
.env <- basilisk::BasiliskEnvironment(
    pkgname="SpatialData", 
    envname="sd_env", pip=.pip,
    packages=c("python==3.14.0", "zarr==2.18.4"))
