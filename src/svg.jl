function SVG(; version="1.1", width=1000, height=1000)
    svg = SVG(Document())
    setroot!(svg, Element("svg"))
    svg.width = width
    svg.height = height
    svg.version = version
    svg.xmlns = "http://www.w3.org/2000/svg"
    return svg
end


function Base.getproperty(svg::SVG, p::Symbol)
    return getproperty(root(svg), p)
end
function Base.setproperty!(svg::SVG, p::Symbol, val)
    return setproperty!(root(svg), p, val)
end


xml(svg::SVG) = getfield(svg, :xml)
prolog(svg::SVG) = xml(svg).prolog
root(svg::SVG) = xml(svg).root
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
