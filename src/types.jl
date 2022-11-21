mutable struct ScreenConfig
    visible::Bool
end


struct SVG
    xml::XML.Document
end


mutable struct Screen <: Makie.MakieScreen
    scene::Scene
    config::ScreenConfig
    # TODO io still needed?
    io::IOBuffer
    svg::SVG
end
