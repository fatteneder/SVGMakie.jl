function Base.display(screen::Screen, scene::Scene; connect=false)
    # This here is done for CairoMakie.
    # return screen

    # But drawing happens in backend_show, so we call it explicitly here for now.
    return Makie.backend_show(screen, stdout, MIME("image/svg+xml"), scene)
end

# stolen from CairoMakie/src/display.jl
function Makie.backend_show(screen::Screen, io::IO, ::MIME"image/svg+xml", scene::Scene)
    Makie.push_screen!(scene, screen)
    # Display the plot on a new screen writing to a string, so that we can manipulate the
    # result (the io in `screen` should directly write to the file we're saving)
    svg = sprint(sizehint=4096) do io2
        screen2 = Screen(screen, io2)
        svg_draw(screen2, scene)
        write(io2, screen2.svg)
    end

    # TODO Implement salting

    # TODO This should be applied to io, but probably somewhere else.
    # Or should io be connected with a stream/pipe to an image viewer?
    svgfile = "/tmp/svgmakie.svg"
    open(svgfile, "w") do file
        write(file, svg)
    end

    return screen
end


Makie.backend_showable(::Type{Screen}, ::MIME{SYM}) where SYM = string(SYM) == "image/svg+xml"
