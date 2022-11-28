mutable struct ScreenConfig
    visible::Bool
end


struct SVG
    xml::XML.Document
end


mutable struct Screen <: Makie.MakieScreen
    scene::Scene
    config::ScreenConfig
    svg::SVG
end
