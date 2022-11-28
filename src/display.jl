function Base.display(screen::Screen, scene::Scene; connect=false)
    return screen
end

function Makie.backend_show(screen::Screen, io::IO, ::MIME"image/svg+xml", scene::Scene)
    Makie.push_screen!(scene, screen)
    svg_draw(screen, scene)
    write(io, screen.svg)
    # TODO Implement salting
    return screen
end

Makie.backend_showable(::Type{Screen}, ::MIME{SYM}) where SYM = string(SYM) == "image/svg+xml"
