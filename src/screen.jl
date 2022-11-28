function activate!(; type="png", screen_config...)
    Makie.inline!(false)
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
        screen::Screen, config::ScreenConfig, scene::Scene, io::IO, m::MIME)
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
    Screen(scene, config, SVG(width=w, height=h))
end


function Screen(scene::Scene; screen_config...)
    config = Makie.merge_screen_config(ScreenConfig, screen_config)
    return Screen(scene, config)
end


function Screen(screen::Screen)
    w, h = round.(Int, size(screen.scene))
    return Screen(screen.scene, screen.config, SVG(width=w, height=h))
end

# Needed for Makie's overloads of FileIO.save
function Screen(screen::Screen, io_or_path::Union{Nothing, String, IO}, typ::Union{MIME, Symbol})
    return Screen(screen.scene)
end

# Needed for Makie's overloads of FileIO.save
function Screen(scene::Scene, config::ScreenConfig, io_or_path::Union{Nothing, String, IO},
        typ::Union{MIME, Symbol})
    return Screen(scene, config)
end


function Makie.colorbuffer(screen::Screen)
    error("Not implemented!")
end
