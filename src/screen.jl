const LAST_INLINE = Ref(true)


function activate!(; inline=LAST_INLINE[], type="png", screen_config...)
    Makie.inline!(inline)
    LAST_INLINE[] = inline
    Makie.set_screen_config!(SVGMakie, screen_config)
    Makie.set_active_backend!(SVGMakie)
    return
end


mutable struct ScreenConfig
    visible::Bool
end


mutable struct Screen <: Makie.MakieScreen
    scene::Scene
    config::ScreenConfig
    # Should we use an IOContext instead, seems like it could be useful in managing
    # the xml subsections.
    io::IOBuffer
end


function Base.empty!(screen::Screen)
    empty!(screen.io)
end


Base.close(screen::Screen) = empty!(screen)
Base.size(screen::Screen) =  (100, 100)
# we render the scene directly, since we have
# # no screen dependent state like in e.g. opengl
Base.insert!(screen::Screen, scene::Scene, plot) = nothing
# Currently, we rerender every time, so nothing needs to happen here.
Base.delete!(screen::Screen, scene::Scene, plot) = nothing
Base.show(io::IO, ::MIME"text/plain", screen::Screen) = println(io, "SVGMakie.Screen")


function Makie.apply_screen_config!(
        screen::Screen, config::ScreenConfig, scene::Scene, io::IO, m::MIME{SYM}) where {SYM}
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


Screen(scene, config::ScreenConfig) = Screen(scene, config, IOBuffer())


function Screen(scene::Scene; screen_config...)
    config = Makie.merge_screen_config(ScreenConfig, screen_config)
    return Screen(scene, config)
end


function Screen(screen::Screen, io::IOBuffer)
    return Screen(screen.scene, screen.config, io)
end


function Makie.colorbuffer(screen::Screen)
    error("Not implemented!")
end


Base.:(>)(screen::Screen, s::AbstractString) = print(screen.io, s)
Base.:(>>)(screen::Screen, s::AbstractString) = println(screen.io, s)
