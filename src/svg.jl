function SVG(; version="1.1", width=1000, height=1000)
    svg = SVG(Document())
    setroot!(svg, Element("svg"))
    svg.width = width
    svg.height = height
    svg.version = version
    svg.xmlns = "http://www.w3.org/2000/svg"
    svg."xmlns:xlink" = "http://www.w3.org/1999/xlink"
    push!(root(svg), Element("defs"))
    return svg
end


function Base.getproperty(svg::SVG, p::Symbol)
    return getproperty(root(svg), p)
end
function Base.setproperty!(svg::SVG, p::Symbol, val)
    return setproperty!(root(svg), p, val)
end
Base.getproperty(svg::SVG, s::AbstractString) = getproperty(root(svg), s)
Base.setproperty!(svg::SVG, s::AbstractString, val) = setproperty!(root(svg), s, val)

# we would like to have these methods, so we pirate them here
Base.getproperty(node::XML.AbstractXMLNode, s::AbstractString) = getproperty(node, Symbol(s))
Base.setproperty!(node::XML.AbstractXMLNode, s::AbstractString, val) = setproperty!(node, Symbol(s), val)
Base.push!(element::XML.Element, el) = push!(getfield(element,:children), el)
Base.append!(element::XML.Element, els) = append!(getfield(element,:children), els)


xml(svg::SVG) = getfield(svg, :xml)
prolog(svg::SVG) = xml(svg).prolog
root(svg::SVG) = xml(svg).root
defs(svg::SVG) = xml(svg).root[1]
setprolog!(svg::SVG, prolog) = xml(svg).prolog = prolog
setroot!(svg::SVG, root) = xml(svg).root = root


Base.getindex(svg::SVG, idx) = getindex(root(svg), idx)
Base.setindex!(svg::SVG, val, idx) = setindex!(root(svg), idx, el)

Base.push!(svg::SVG, el) = push!(root(svg), el)
Base.append!(svg::SVG, els) = append!(root(svg), els)


function Base.show(io::IO, ::MIME"text/plain", svg::SVG)
    println(io, "SVGMakie.SVG")
    XML.showxml(io, xml(svg))
    return
end


Base.write(io::IO, svg::SVG) = write(io, xml(svg))
