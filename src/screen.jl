const LAST_INLINE = Ref(true)


function activate!(; inline=LAST_INLINE[], type="png", screen_config...)
    Makie.inline!(inline)
    LAST_INLINE[] = inline
    Makie.set_screen_config!(SVGMakie, screen_config)
    Makie.set_active_backend!(SVGMakie)
    return
end


function Base.empty!(screen::Screen)
    empty!(screen.io)
end


Base.close(screen::Screen) = empty!(screen)
Base.size(screen::Screen) = (screen.svg.width, screen.svg.height)
# we render the scene directly, since we have
# # no screen dependent state like in e.g. opengl
Base.insert!(screen::Screen, scene::Scene, plot) = nothing
# Currently, we rerender every time, so nothing needs to happen here.
Base.delete!(screen::Screen, scene::Scene, plot) = nothing
Base.show(io::IO, ::MIME"text/plain", screen::Screen) = println(io, "SVGMakie.Screen")


function Makie.apply_screen_config!(
        screen::Screen, config::ScreenConfig, scene::Scene, io::IO, m::MIME{SYM}) where {SYM}
    # TODO Do we need to copy whatever is in io to screen.svg?
    # Could we use XML.Document for that?
    apply_config!(screen, config)
    return screen
end


function Makie.apply_screen_config!(screen::Screen, config::ScreenConfig, scene::Scene, args...)
    apply_config!(screen, config)
    return screen
end


function apply_config!(screen::Screen, config::ScreenConfig)
    screen.config = config
    return
end


function Screen(scene, config::ScreenConfig)
    w, h = round.(Int, size(scene))
    Screen(scene, config, IOBuffer(), SVG(width=w, height=h))
end


function Screen(scene::Scene; screen_config...)
    config = Makie.merge_screen_config(ScreenConfig, screen_config)
    return Screen(scene, config)
end


function Screen(screen::Screen, io::IOBuffer)
    w, h = round.(Int, size(screen.scene))
    return Screen(screen.scene, screen.config, io, SVG(width=w, height=h))
end


function Makie.colorbuffer(screen::Screen)
    error("Not implemented!")
end
