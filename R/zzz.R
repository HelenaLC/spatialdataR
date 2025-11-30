#' @importFrom S7 S4_register methods_register
.onLoad <- \(...) {
    S4_register(ImageArray)
    S4_register(LabelArray)
    S4_register(ShapeFrame)
    S4_register(PointFrame)
    S4_register(sdArray)
    S4_register(sdFrame)
    S4_register(Zattrs)
    methods_register()
}
