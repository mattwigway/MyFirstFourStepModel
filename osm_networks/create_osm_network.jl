# This converts an OSM network into a graph that we use in assignment

using ArgParse, OpenStreetMapPBF, Graphs, EzXML, MetaGraphsNext, Logging,
    GeoDataFrames, ArchGDAL, DataFrames, Geodesy

const DEFAULT_SPEEDS_KMH = Dict{String, Float64}(
    "motorway" => 105.0,
    "motorway_link" => 72.0,
    "trunk" => 72.0,
    "primary" => 72.0,
    "trunk_link" => 40.0,
    "primary_link" => 40.0
)

const MILES_TO_KILOMETERS = 1.609

# These are used as node labels in the graph, they just wrap an Int64, but
# we use a separate type as recommended by the MetaGraphs documentation to avoid
# confusion with one-based IDs
struct OSMID
    id::Int64
end

# Data we retain about an edge
const EdgeData = @NamedTuple begin
    eid::Int64
    lanes_per_direction::Int64
    length_m::Float64
    highway_type::String
    maxspeed_kph::Float64
    geometry::ArchGDAL.IGeometry{ArchGDAL.wkbLineString}
end

const NodeData = @NamedTuple begin
    lat::Float64
    lon::Float64
end

function main(raw_args)
    argtable = ArgParseSettings()
    @add_arg_table! argtable begin
        "pbf"
            help = "OSM PBF file"
        "out"
            help = "output graph file"
        "--highway", "-H"
            help = "Highway tags to retain (comma-separated)"
            default = "motorway,motorway_link,trunk,trunk_link,primary,primary_link"
        "--pretty"
            help = "Pretty-print GraphML output"
            action = :store_true
    end

    args = parse_args(raw_args, argtable)

    highway_tags = split(lowercase(args["highway"]), ",")
    @info "Retaining highway types:" highway_tags

    G = build_graph(args["pbf"], highway_tags)

    remove_islands!(G)

    graph_to_gis(args["out"] * ".gpkg", G)
    graph_to_graphml(args["out"] * ".graphml", G, pretty=args["pretty"])
end

function build_graph(pbf, highway_tags)
    # nodes that are parts of highways
    highway_nodes = Set{Int64}()

    # nodes that are intersections between highways
    intersection_nodes = Set{Int64}()

    n_ways = 0

    @info "Pass 1: retain highway nodes"
    scan_ways(pbf) do way
        if haskey(way.tags, "highway") && lowercase(way.tags["highway"]) ∈ highway_tags
            n_ways += 1
            for node in way.nodes
                if node ∈ highway_nodes
                    # we've seen this node before, highways cross here
                    push!(intersection_nodes, node)
                else
                    # this is an intermediate node, or an intersection we haven't seen yet
                    push!(highway_nodes, node)
                end
            end
        end
    end

    @info "Retained $n_ways ways, with $(length(highway_nodes)) nodes including $(length(intersection_nodes)) intersections"

    @info "Pass 2: reading nodes"
    nodes = Dict{Int64, Node}()

    scan_nodes(pbf) do node
        if node.id ∈ highway_nodes
            nodes[node.id] = node
        end
    end

    @info "Pass 3: Building graph"
    G = MetaGraph(
        DiGraph();
        label_type=OSMID,
        vertex_data_type=NodeData,
        edge_data_type=EdgeData
    )

    next_id = 1

    scan_ways(pbf) do way
        if haskey(way.tags, "highway") && lowercase(way.tags["highway"]) ∈ highway_tags
            if forward_access(way.tags)
                next_id = create_edges!(G, way.tags, way.nodes, way.id, nodes, intersection_nodes, next_id)
            end

            if reverse_access(way.tags)
                next_id = create_edges!(G, way.tags, reverse(way.nodes), way.id, nodes, intersection_nodes, next_id)
            end
        end
    end

    return G
end

# NB not checking access tags, assuming anything with desired highway tags allows cars
forward_access(tags) = !haskey(tags, "oneway") || !(lowercase(tags["oneway"]) ∈ ["-1", "reverse"])
# NB not handling roundabouts
reverse_access(tags) = (!haskey(tags, "oneway") && lowercase(tags["highway"]) ∉ ["motorway", "motorway_link"]) || haskey(tags, "oneway") && lowercase(tags["oneway"]) ∈ ["-1", "reverse", "no"]

node_dist_m(n1, n2) = euclidean_distance(LatLon{Float64}(n1.lat, n1.lon), LatLon{Float64}(n2.lat, n2.lon))

function get_max_speed(tags)::Union{Float64, Missing}
    if haskey(tags, "maxspeed")
        speed_text = tags["maxspeed"]
        try
            return parse(Float64, speed_text)
        catch
            # not a raw km/hr number
            mtch = match(r"([0-9]+)(?: ?)([a-zA-Z/]+)", speed_text)
            if isnothing(mtch)
                @warn "unable to parse speed limit $speed_text"
                # fall back to defaults
            else
                speed_scalar = parse(Float64, mtch.captures[1])
                units = lowercase(mtch.captures[2])

                if (units == "kph" || units == "km/h" || units == "kmph")
                    return speed_scalar
                elseif units == "mph"
                    return speed_scalar * MILES_TO_KILOMETERS
                elseif units == "knots"
                    return speed_scalar * KNOTS_TO_KMH
                else
                    @warn "unknown speed unit $units"
                    # fall back to defaults
                end
            end
        end
    end

    if haskey(DEFAULT_SPEEDS_KMH, lowercase(tags["highway"]))
        return DEFAULT_SPEEDS_KMH[lowercase(tags["highway"])]
    else
        return DEFAULT_SPEEDS_KMH["default"]
    end
end

function get_lanes_per_direction(tags)
    # assume motorways have 2 lanes per direction if not specified
    lanes_per_direction = lowercase(tags["highway"]) == "motorway" ? 2 : 1
    if haskey(tags, "lanes")
        try
            lanes_per_direction = parse(Int64, tags["lanes"])
            if forward_access(tags) && reverse_access(tags)
                # not oneway
                lanes_per_direction ÷= 2
            end
        catch
            lanes_str = tags["lanes"]
            @warn "could not parse lanes values $lanes_str"
            lanes_per_direction = 1
        end
    end

    # every road has at least one lane
    return max(lanes_per_direction, 1)
end

node_loc(node) = NodeData((node.lat, node.lon))

# Create edges for a way
function create_edges!(G, tags, waynodes, wayid, nodes, intersection_nodes, next_id)
    edge_nodes = Int64[]

    max_speed_kmh = get_max_speed(tags)
    lanes = get_lanes_per_direction(tags)

    for (node_seq, node) in enumerate(waynodes)
        if !haskey(nodes, node)
            @warn "Node $node referenced by way $wayid but not in PBF"
            continue
        end
        push!(edge_nodes, node)

        # if it's an intersection node and not at the start, or if it's at the end, create the edge
        if length(edge_nodes) > 1 && (node ∈ intersection_nodes || node_seq == length(waynodes))
            # calculate length
            length_m = 0.0
            prev, rest = Iterators.peel(edge_nodes)

            geom = [(nodes[prev].lon, nodes[prev].lat)]

            for nid in rest
                length_m += node_dist_m(nodes[prev], nodes[nid])
                push!(geom, (nodes[nid].lon, nodes[nid].lat))
            end

            data = EdgeData((
                next_id,
                lanes,
                length_m,
                tags["highway"],
                max_speed_kmh,
                ArchGDAL.createlinestring(geom)
            ))

            from = OSMID(first(edge_nodes))
            to = OSMID(last(edge_nodes))
            
            # make sure they exist
            G[from] = node_loc(nodes[from.id])
            G[to] = node_loc(nodes[to.id])

            # add the edge
            G[from, to] = data

            # prepare for next iteration
            edge_nodes = [to.id]
            next_id += 1
        end
    end

    return next_id
end

function remove_islands!(G)
    components = strongly_connected_components(G)
    sort!(components, by=length, rev=true)
    @info "Largest component has size $(length(first(components))). Removing $(length(components) - 1) components with $(length(components[[2]])) or fewer vertices"

    vs_to_remove = Int64[]

    for component in components[begin + 1:end]
        for v in component
            push!(vs_to_remove, v)
        end
    end

    # remove back to front to avoid changing indices affecting removal
    sort!(vs_to_remove, rev=true)

    @info "Removing $(length(vs_to_remove)) vertices"
    for v in vs_to_remove
        rem_vertex!(G, v)
    end
end

function graph_to_gis(out, G)
    gdf = DataFrame(map(x->G[x[1], x[2]], edge_labels(G)))
    GeoDataFrames.write(out, gdf)
end

function graph_to_graphml(out, G; pretty=false)
    doc = XMLDocument()
    root = ElementNode("graphml")
    root["xmlns"] = "http://graphml.graphdrawing.org/xmlns"  
    root["xmlns:xsi"] ="http://www.w3.org/2001/XMLSchema-instance"
    root["xsi:schemaLocation"]="http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd"

    setroot!(doc, root)

    # encode data types
    lanes = ElementNode("key")
    lanes["id"] = "lanes_per_direction"
    lanes["for"] = "edge"
    lanes["attr.name"] = "lanes_per_direction"
    lanes["attr.type"] = "int"
    link!(root, lanes)

    length_m = ElementNode("key")
    length_m["id"] = "length_m"
    length_m["for"] = "edge"
    length_m["attr.name"] = "length_m"
    length_m["attr.type"] = "double"
    link!(root, length_m)

    highway_type = ElementNode("key")
    highway_type["id"] = "highway_type"
    highway_type["for"] = "edge"
    highway_type["attr.name"] = "highway_type"
    highway_type["attr.type"] = "string"
    link!(root, highway_type)

    maxspeed_kph = ElementNode("key")
    maxspeed_kph["id"] = "maxspeed_kph"
    maxspeed_kph["for"] = "edge"
    maxspeed_kph["attr.name"] = "maxspeed_kph"
    maxspeed_kph["attr.type"] = "double"
    link!(root, maxspeed_kph)

    lat = ElementNode("key")
    lat["id"] = "lat"
    lat["for"] = "node"
    lat["attr.name"] = "lat"
    lat["attr.type"] = "double"
    link!(root, lat)

    lon = ElementNode("key")
    lon["id"] = "lon"
    lon["for"] = "node"
    lon["attr.name"] = "lon"
    lon["attr.type"] = "double"
    link!(root, lon)

    gph = ElementNode("graph")
    gph["id"] = "G"
    gph["edgedefault"] = "directed"
    link!(root, gph)

    for node in labels(G)
        data = G[node]
        n = ElementNode("node")
        n["id"] = node.id
        link!(gph, n)

        lat = ElementNode("data")
        lat["key"] = "lat"
        link!(lat, TextNode("$(data.lat)"))
        link!(n, lat)

        lon = ElementNode("data")
        lon["key"] = "lon"
        link!(lon, TextNode("$(data.lon)"))
        link!(n, lon)
    end

    for edge in edge_labels(G)
        data = G[edge[1], edge[2]]::EdgeData

        e = ElementNode("edge")
        e["id"] = data.eid
        e["source"] = edge[1].id
        e["target"] = edge[2].id
        link!(gph, e)

        lanes = ElementNode("data")
        lanes["key"] = "lanes_per_direction"
        link!(lanes, TextNode("$(data.lanes_per_direction)"))
        link!(e, lanes)

        length_m = ElementNode("data")
        length_m["key"] = "length_m"
        link!(length_m, TextNode("$(data.length_m)"))
        link!(e, length_m)

        highway = ElementNode("data")
        highway["key"] = "highway_type"
        link!(highway, TextNode(data.highway_type))
        link!(e, highway)

        maxspeed_kph = ElementNode("data")
        maxspeed_kph["key"] = "maxspeed_kph"
        link!(maxspeed_kph, TextNode("$(data.maxspeed_kph)"))
        link!(e, maxspeed_kph)
    end

    if pretty
        open(out, "w") do os
            prettyprint(os, doc)
        end
    else
        write(out, doc)
    end
end

main(ARGS)