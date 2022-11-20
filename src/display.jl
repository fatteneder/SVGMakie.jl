function Base.display(screen::Screen, scene::Scene; connect=false)
    # This here is done for CairoMakie.
    # return screen

    # But drawing happens in backend_show, so we call it explicitly here for now.
    return Makie.backend_show(screen, stdout, MIME("image/svg+xml"), scene)
end

# stolen from CairoMakie/src/display.jl
# What I think we could do first: Write either to a file and open with `feh` or directly
# pipe into `feh` or any other image viewer.
function Makie.backend_show(screen::Screen, io::IO, ::MIME"image/svg+xml", scene::Scene)
    Makie.push_screen!(scene, screen)
    # Display the plot on a new screen writing to a string, so that we can manipulate the
    # result (the io in `screen` should directly write to the file we're saving)
    println("or here?")
    svg = sprint(sizehint=4096) do io2
        # ???
        screen2 = Screen(screen, io2)
        # TODO setup document structure
        # println(screen2.io,
        screen2 >> """
        <svg version="1.1"
         width="300" height="200"
         xmlns="http://www.w3.org/2000/svg">

         <rect width="100%" height="100%" fill="red" />

         <circle cx="150" cy="100" r="80" fill="green" />

         <text x="150" y="125" font-size="60" text-anchor="middle" fill="white">SVG</text>
        </svg>
        """

        svg_draw(screen2, scene)
        # Cairo.flush(screen2.surface)
        # Cairo.finish(screen2.surface)
    end

    # TODO Implement salting

    print(io, svg)

    # TODO This should be applied to io, but probably somewhere else.
    # Or should io be connected with a stream/pipe to an image viewer?
    svgfile = "/tmp/svgmakie.svg"
    open(svgfile, "w") do file
        print(file, svg)
    end

    return screen
end


Makie.backend_showable(::Type{Screen}, ::MIME{SYM}) where SYM = string(SYM) == "image/svg+xml"
