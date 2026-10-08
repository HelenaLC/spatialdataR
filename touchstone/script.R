# see `help(run_script, package='touchstone')` on how to run this
# interactively

# TODO OPTIONAL Add directories you want to be available in this file or during the
# benchmarks.
# touchstone::pin_assets("some/dir")

# installs branches to benchmark
touchstone::branch_install()

touchstone::benchmark_run(
    pkg_load=library(spatialdataR),
    n=25
)

touchstone::benchmark_run(
    {
        library(spatialdataR)
    },
    read_full=readSpatialData(
        system.file("extdata", "blobs_v3.zarr", package="spatialdataR")
    ),
    n=20
)

touchstone::benchmark_run(
    {
        library(spatialdataR)
        x <- readSpatialData(
            system.file("extdata", "blobs_v3.zarr", package="spatialdataR")
        )
    },
    crop=crop(x, list(xmin=20, xmax=30, ymin=20, ymax=30)),
    n=20
)

touchstone::benchmark_run(
    {
        library(spatialdataR)
        x <- readSpatialData(
            system.file("extdata", "blobs_v3.zarr", package="spatialdataR")
        )
    },
    show=show(x),
    n=20
)

touchstone::benchmark_analyze()