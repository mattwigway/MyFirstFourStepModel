#' Build the OSM network (using Julia)
#' @param osm The OSM file to use
#' @param highway_types Character vector of highway=* tags to retain
#' @param installJulia Whether to install Julia if it is not found (default TRUE)
#' @return A list with members $network (igraph network) and $network_geo (sf DataFrame with geographic outline of network)
build_network = function (osm, highway_types, installJulia=T) {
    # set up Julia
    Sys.setenv(JULIA_PROJECT=path_package("MyFirstFourStepModel", "julia"))
    julia = julia_setup(installJulia=installJulia)
    julia$eval("import Pkg; Pkg.resolve()") # make sure packages are up to date
    julia$source(path_package("MyFirstFourStepModel", "julia", "create_osm_network.jl"))

    # build the graph
    G = julia$call("build_graph", osm, highway_types)
    julia$call("remove_islands!", G)

    # store in temporary files
    graph_file = file_temp(ext=".graphml")
    julia$call("graph_to_graphml", graph_file, G)

    gpkg_file = file_temp(ext=".gpkg")
    julia$call("graph_to_gis", gpkg_file, G)

    # read into R
    result = list(
        network = read_graph(graph_file, format="graphml"),
        network_geo = read_sf(gpkg_file)
    )

    # clean up
    file_delete(graph_file)
    file_delete(gpkg_file)

    return(result)
}