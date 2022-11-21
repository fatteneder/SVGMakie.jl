module SVGMakie

using Makie, LinearAlgebra
using Colors, GeometryBasics, FileIO
using XML

# CairoMakie uses those for salting, whatever that means ...
# import SHA
# import Base64

using Makie: Scene, Lines, Text, Image, Heatmap, Scatter, @key_str, broadcast_foreach
using Makie: convert_attribute, @extractvalue, LineSegments, to_ndim, NativeFont
using Makie: @info, @get_attribute, Combined, MakieScreen
using Makie: to_value, to_colormap, extrema_nan
using Makie.Observables
using Makie: spaces, is_data_space, is_pixel_space, is_relative_space, is_clip_space
using Makie: numbers_to_colors

# re-export Makie, including deprecated names
for name in names(Makie, all=true)
    if Base.isexported(Makie, name)
        @eval using Makie: $(name)
        @eval export $(name)
    end
end

include("types.jl")
include("svg.jl")
include("screen.jl")
include("display.jl")
include("infrastructure.jl")
include("utils.jl")
# include("primitives.jl")
# include("overrides.jl")

# function __init__()
#     activate!()
# end
#
# include("precompiles.jl")


function __init__()
    # Shouldn't each backend add its defaults to `Makie.minimal_default` by itself?
    Makie.minimal_default[:SVGMakie] = Attributes(visible = true)
    # Need to update this global manually ...
    Makie.CURRENT_DEFAULT_THEME[:SVGMakie] = Makie.minimal_default[:SVGMakie]
end

end
