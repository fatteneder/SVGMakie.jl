################################################################################
#                             Lines, LineSegments                              #
################################################################################

function draw_atomic(scene::Scene, screen::Screen, @nospecialize(primitive::Union{Lines, LineSegments}))

    fields = @get_attribute(primitive, (color, linewidth, linestyle))
    linestyle = Makie.convert_attribute(linestyle, Makie.key"linestyle"())

    svg = screen.svg
    model = primitive[:model][]
    positions = primitive[1][]

    isempty(positions) && return

    # workaround for a LineSegments object created from a GLNormalMesh
    # the input argument is a view of points using faces, which results in
    # a vector of tuples of two points. we convert those to a list of points
    # so they don't trip up the rest of the pipeline
    # TODO (CairoMakie.jl) this shouldn't be necessary anymore!
    if positions isa SubArray{<:Point3, 1, P, <:Tuple{Array{<:AbstractFace}}} where P
        positions = let
            pos = Point3f[]
            for tup in positions
                push!(pos, tup[1])
                push!(pos, tup[2])
            end
            pos
        end
    end

    space = to_value(get(primitive, :space, :data))
    projected_positions = project_position.(Ref(scene), Ref(space), positions, Ref(model))

    if color isa AbstractArray{<: Number}
        color = numbers_to_colors(color, primitive)
    end

    # TODO Figure out whether this comment is also relevant for SVGMakie.
    # # The linestyle can be set globally, as we do here.
    # # However, there is a discrepancy between Makie
    # # and Cairo when it comes to linestyles.
    # # For Makie, the linestyle array is cumulative,
    # # and defines the "absolute" endpoints of segments.
    # # However, for Cairo, each value provides the length of
    # # alternate "on" and "off" portions of the stroke.
    # # Therefore, we take the diff of the given linestyle,
    # # to convert the "absolute" coordinates into "relative" ones.
    # if !isnothing(linestyle) && !(linewidth isa AbstractArray)
    #     Cairo.set_dash(ctx, diff(Float64.(linestyle)) .* linewidth)
    # end

    if color isa AbstractArray || linewidth isa AbstractArray
        # stroke each segment separately, this means disjointed segments with probably
        # wonky dash patterns if segments are short
        # TODO we can hide the gaps by setting the line cap to round
        draw_multi(primitive, svg, projected_positions, color, linewidth,
            isnothing(linestyle) ? nothing : diff(Float64.(linestyle))
        )
    else
        # stroke the whole line at once if it has only one color
        # this allows correct linestyles and line joins as well and will be the
        # most common case
        draw_single(primitive, svg, projected_positions, color, linewidth,
                    isnothing(linestyle) ? nothing : diff(Float64.(linestyle)))
    end

    nothing
end


function draw_single(primitive::Lines, svg, positions, color, linewidth, dash)

    # user path instead of line to make it one svg element
    path = Element("path")
    # estimate buffer size from number of points to connect
    io = IOBuffer(sizehint =
        (2 #= x, y components =# * 10 #= ~ avg nr significant digits for Float32 =# +
         3 #= path commands like M,L,Z =#) * length(positions) * sizeof(UInt8))

    n = length(positions)
    @inbounds for i in 1:n
        p = positions[i]
        # only take action for non-NaNs
        if !isnan(p)
            # new line segment at beginning or if previously NaN
            if i == 1 || isnan(positions[i-1])
                print(io, (i == 1) ? "M " : " M ", join(p, " "))
            else
                print(io, " L ", join(p, " "))
            end
        end
    end

    path.d = String(take!(io))
    path.fill = "none"
    path.stroke = svg_color(color)
    path."stroke-width" = linewidth
    path."stroke-opacity" = svg_color_alpha(color)
    path."stroke-linecap" = "round"
    if !isnothing(dash)
        path."stroke-dasharray" = join(dash .* linewidth, ",")
    end
    push!(svg, path)
end


function draw_single(primitive::LineSegments, svg, positions, color, linewidth, dash)

    @assert iseven(length(positions))

    # user path instead of line to make it one svg element
    path = Element("path")
    # estimate buffer size from number of points to connect
    io = IOBuffer(sizehint =
        (2 #= x, y components =# * 10 #= ~ avg nr significant digits for Float32 =# +
         3 #= path commands like M,L,Z =#) * length(positions) * sizeof(UInt8))

    @inbounds for i in 1:2:length(positions)-1
        p1 = positions[i]
        p2 = positions[i+1]

        if isnan(p1) || isnan(p2)
            continue
        else
            print(io, i == 1 ? "M " : " M ", join(p1, " "), " L ", join(p2, " "))
        end
    end

    path.d = String(take!(io))
    path.fill = "none"
    path.stroke = svg_color(color)
    path."stroke-width" = linewidth
    path."stroke-opacity" = svg_color_alpha(color)
    path."stroke-linecap" = "round"
    if !isnothing(dash)
        path."stroke-dasharray" = join(dash .* linewidth, ",")
    end
    push!(svg, path)
end

# if linewidth is not an array
function draw_multi(primitive, svg, positions, colors::AbstractArray, linewidth, dash)
    draw_multi(primitive, svg, positions, colors, [linewidth for c in colors], dash)
end

# if color is not an array
function draw_multi(primitive, svg, positions, color, linewidths::AbstractArray, dash)
    draw_multi(primitive, svg, positions, [color for l in linewidths], linewidths, dash)
end

function draw_multi(primitive::Union{Lines, LineSegments}, svg, positions, colors::AbstractArray, linewidths::AbstractArray, dash)

    if primitive isa LineSegments
        @assert iseven(length(positions))
    end
    @assert length(positions) == length(colors)
    @assert length(linewidths) == length(colors)

    iterator = if primitive isa Lines
        1:length(positions)-1
    elseif primitive isa LineSegments
        1:2:length(positions)
    end

    for i in iterator

        if isnan(positions[i+1]) || isnan(positions[i])
            continue
        end

        # need to use line instead of path here, because a gradient for the stroke color is
        # not recognized for paths
        line = Element("line")
        line.x1 = positions[i][1]
        line.y1 = positions[i][2]
        line.x2 = positions[i+1][1]
        line.y2 = positions[i+1][2]

        c1 = colors[i]
        c2 = colors[i+1]
        # # we can avoid the more expensive gradient if the colors are the same
        # # this happens if one color was given for each segment
        if c1 == c2
            line.stroke = svg_color(c1)
            line."stroke-opacity" = svg_color_alpha(c1)
        else
            lingrad = Element("linearGradient")
            id = "linearGradient"*string(hash(positions[i]))
            lingrad.id = id
            lingrad.x1 = "0"
            lingrad.y1 = "0"
            lingrad.x2 = "1"
            lingrad.y2 = "1"
            stop1 = Element("stop")
            stop1.offset = "0"
            stop1."stop-color" = svg_color(c1)
            stop1."stop-opacity" = svg_color_alpha(c1)
            stop2 = Element("stop")
            stop2.offset = "0"
            stop2."stop-color" = svg_color(c2)
            stop2."stop-opacity" = svg_color_alpha(c2)
            push!(lingrad, stop1)
            push!(lingrad, stop2)
            push!(defs(svg), lingrad)
            line.stroke = svg_url(id)
        end

        if linewidths[i] != linewidths[i+1]
            # TODO Is this correct for SVG?
            error("SVG doesn't support two different line widths ($(linewidths[i]) and $(linewidths[i+1])) at the endpoints of a line.")
        end

        line."stroke-width" = linewidths[i]
        line."stroke-linecap" = "round"
        if !isnothing(dash)
            line."stroke-dasharray" = join(dash .* linewidths[i], ",")
        end

        push!(svg, line)
    end

end

################################################################################
#                                   Scatter                                    #
################################################################################

function draw_atomic(scene::Scene, screen::Screen, @nospecialize(primitive::Scatter))

end

################################################################################
#                                     Text                                     #
################################################################################

function p3_to_p2(p::Point3{T}) where T
    if p[3] == 0 || isnan(p[3])
        Point2{T}(p[Vec(1,2)]...)
    else
        error("Can't reduce Point3 to Point2 with nonzero third component $(p[3]).")
    end
end

function draw_atomic(scene::Scene, screen::Screen,
        @nospecialize(primitive::Text{<:Tuple{<:Union{AbstractArray{<:Makie.GlyphCollection}, Makie.GlyphCollection}}}))
    # nothing to do here
end

function draw_atomic(scene::Scene, screen::Screen,
        @nospecialize(primitive::Text{<:Tuple{<:Union{AbstractArray{<:Makie.GlyphCollection}, Makie.GlyphCollection}}}),
        text_signal)

    svg = screen.svg
    @get_attribute(primitive, (rotation, model, space, markerspace, offset))
    position = primitive.position[]
    # use cached glyph info
    glyph_collection = to_value(primitive[1])
    text = text_signal[]

    draw_glyph_collection(
        scene, svg, position, glyph_collection, text, remove_billboard(rotation),
        model, space, markerspace, offset
    )

    nothing
end

function draw_glyph_collection(
        scene, svg, positions, glyph_collections::AbstractArray, texts, rotation,
        model::Mat, space, markerspace, offset
    )

    # TODO(from CairoMakie): why is the Ref around model necessary? doesn't broadcast_foreach handle staticarrays matrices?
    broadcast_foreach(positions, glyph_collections, texts, rotation, Ref(model), space,
        markerspace, offset) do pos, glayout, text, ro, mo, sp, msp, off

        draw_glyph_collection(scene, svg, pos, glayout, text, ro, mo, sp, msp, off)
    end
end

_deref(x) = x
_deref(x::Ref) = x[]

function draw_glyph_collection(scene, svg, position, glyph_collection, text,
        rotation, _model, space, markerspace, offsets)

    glyphs = glyph_collection.glyphs
    glyphoffsets = glyph_collection.origins
    fonts = glyph_collection.fonts
    rotations = glyph_collection.rotations
    scales = glyph_collection.scales
    colors = glyph_collection.colors
    strokewidths = glyph_collection.strokewidths
    strokecolors = glyph_collection.strokecolors

    @assert length(glyphs) == length(text)

    model = _deref(_model)
    model33 = model[Vec(1, 2, 3), Vec(1, 2, 3)]
    id = Mat4f(I)

    glyph_pos = let
        transform_func = scene.transformation.transform_func[]
        p = Makie.apply_transform(transform_func, position)

        Makie.clip_to_space(scene.camera, markerspace) *
        Makie.space_to_clip(scene.camera, space) *
        model * to_ndim(Point4f, to_ndim(Point3f, p, 0), 1)
    end

    svg_text = Element("text")

    broadcast_foreach(glyphs, text, glyphoffsets, fonts, rotations, scales, colors, strokewidths, strokecolors, offsets) do glyph, char,
        glyphoffset, font, rotation, scale, color, strokewidth, strokecolor, offset

        # Not renderable by font (e.g. '\n')
        # TODO(from CairoMakie), filter out \n in GlyphCollection, and render unrenderables as box
        glyph == 0 && return

        tspan = Element("tspan")
        push!(svg_text, tspan)

        tspan.x = position[1]+glyphoffset[1]
        tspan.y = position[2]+glyphoffset[2]

        # TODO Should we set a fallback font in svg_text?
        if hasproperty(font, :family)
            tspan."font-family" = font.family
        end
        if hasproperty(font, :style)
            tspan."font-style" = font.style
        end

        p3_offset = to_ndim(Point3f, offset, 0)

        # When Cairo outputs as svg it sets colors this way.
        tspan.stroke = "none"
        tspan.fill = svg_color(color)
        tspan."fill-opacity" = svg_color_alpha(color)

        # offsets and scale apply in markerspace
        gp3 = glyph_pos[Vec(1, 2, 3)] ./ glyph_pos[4] .+ model33 * (glyphoffset .+ p3_offset)

        scale3 = scale isa Number ? Point3f(scale, scale, 0) : to_ndim(Point3f, scale, 0)

        # TODO Do we need such a matrix also for SVG? If yes, how should it look?

        # the CairoMatrix is found by transforming the right and up vector
        # of the character into screen space and then subtracting the projected
        # origin. The resulting vectors give the directions in which the character
        # needs to be stretched in order to match the 3D projection
        xvec = rotation * (scale3[1] * Point3f(1, 0, 0))
        yvec = rotation * (scale3[2] * Point3f(0, -1, 0))

        glyphpos = _project_position(scene, markerspace, gp3, id, true)
        xproj = _project_position(scene, markerspace, gp3 + model33 * xvec, id, true)
        yproj = _project_position(scene, markerspace, gp3 + model33 * yvec, id, true)

        xdiff = xproj - glyphpos
        ydiff = yproj - glyphpos

        push!(tspan, string(char))

        tspan."stroke" = svg_color(strokecolor)
        tspan."stroke-opacity" = svg_color_alpha(strokecolor)
        tspan."stroke-width" = strokewidth

        # TODO rotation & scale & offset

    end

    push!(svg, svg_text)

    return
end

################################################################################
#                                Heatmap, Image                                #
################################################################################

function draw_atomic(scene::Scene, screen::Screen, @nospecialize(primitive::Union{Heatmap, Image}))

end

################################################################################
#                                     Mesh                                     #
################################################################################

function draw_atomic(scene::Scene, screen::Screen, @nospecialize(primitive::Makie.Mesh))

end

################################################################################
#                                   Surface                                    #
################################################################################

function draw_atomic(scene::Scene, screen::Screen, @nospecialize(primitive::Makie.Surface))

end

################################################################################
#                                 MeshScatter                                  #
################################################################################

function draw_atomic(scene::Scene, screen::Screen, @nospecialize(primitive::Makie.MeshScatter))

end
