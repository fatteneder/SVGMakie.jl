################################################################################
#                             Lines, LineSegments                              #
################################################################################

function draw_atomic(scene::Scene, screen::Screen, @nospecialize(primitive::Union{Lines, LineSegments}))

    fields = @get_attribute(primitive, (color, linewidth, linestyle))
    linestyle = Makie.convert_attribute(linestyle, Makie.key"linestyle"())

    svg_el = last(root(screen.svg))
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
        # TODO(CairoMakie) we can hide the gaps by setting the line cap to round
        # TODO We are already doing this, but there is a question about how long the line
        # should be, because the caps extend beyond the vertices.
        draw_multi(primitive, svg_el, projected_positions, color, linewidth,
            isnothing(linestyle) ? nothing : diff(Float64.(linestyle))
        )
    else
        # stroke the whole line at once if it has only one color
        # this allows correct linestyles and line joins as well and will be the
        # most common case
        draw_single(primitive, svg_el, projected_positions, color, linewidth,
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
    fields = @get_attribute(primitive, (color, markersize, strokecolor, strokewidth, marker,
                                        marker_offset, rotations))
    @get_attribute(primitive, (transform_marker,))

    svg_el = last(root(screen.svg))
    model = primitive[:model][]
    positions = primitive[1][]
    isempty(positions) && return
    size_model = transform_marker ? model : Mat4f(I)

    font = to_font(to_value(get(primitive, :font, Makie.defaultfont())))

    colors = if color isa AbstractArray{<: Number}
        numbers_to_colors(color, primitive)
    else
        color
    end

    markerspace = to_value(get(primitive, :markerspace, :pixel))
    space = to_value(get(primitive, :space, :data))

    transfunc = scene.transformation.transform_func[]

    marker_conv = _marker_convert(marker)

    draw_atomic_scatter(scene, svg_el, transfunc, colors, markersize, strokecolor, strokewidth,
                        marker_conv, marker_offset, rotations, model, positions, size_model, font,
                        markerspace, space)
end

# an array of markers is converted to string by itself, which is inconvenient for the iteration logic
_marker_convert(markers::AbstractArray) =
    map(m -> convert_attribute(m, key"marker"(), key"scatter"()), markers)
_marker_convert(marker) = convert_attribute(marker, key"marker"(), key"scatter"())
# image arrays need to be converted as a whole
_marker_convert(marker::AbstractMatrix{<:Colorant}) =
    [ convert_attribute(marker, key"marker"(), key"scatter"()) ]

function draw_atomic_scatter(scene, svg_el, transfunc, colors, markersize, strokecolor, strokewidth,
        marker, marker_offset, rotations, model, positions, size_model, font, markerspace, space)

    broadcast_foreach(positions, colors, markersize, strokecolor,
            strokewidth, marker, marker_offset, remove_billboard(rotations)) do point, col,
            markersize, strokecolor, strokewidth, m, mo, rotation

        scale = project_scale(scene, markerspace, markersize, size_model)
        offset = project_scale(scene, markerspace, mo, size_model)

        pos = project_position(scene, transfunc, space, point, model)
        isnan(pos) && return

        svg_g = Element("g")
        svg_g.fill = svg_color(col)
        svg_g."fill-opacity" = svg_color_alpha(col)
        push!(svg_el, svg_g)

        marker_converted = Makie.to_spritemarker(m)
        # TODO Is this relevant for SVG?
        # Setting a markersize of 0.0 somehow seems to break Cairos global state?
        # At least it stops drawing any marker afterwards
        # TODO(from CairoMakie), maybe there's something wrong somewhere else?
        if !(norm(scale) ≈ 0.0)
            if marker_converted isa Char
                draw_marker(svg_g, marker_converted, best_font(m, font), pos, scale,
                            strokecolor, strokewidth, offset, rotation)
            else
                draw_marker(svg_g, marker_converted, pos, scale, strokecolor, strokewidth,
                            offset, rotation)
            end
        end
    end
    return
end

function draw_marker(svg_el, marker::Char, font, pos, scale, strokecolor, strokewidth,
        marker_offset, rotation)

    # We place the marker inside a <text> element so that we can then scale it.
    # We then wrap this into a <g> element which we can then scale, rotate, translate
    # to our needs independent of the marker's scale.
    g = Element("g")
    text = Element("text")
    push!(g, text)
    push!(svg_el, g)

    # Marker offset is meant to be relative to the
    # bottom left corner of the box centered at
    # `pos` with sides defined by `scale`, but
    # this does not take the character's dimensions
    # into account.
    # Here, we reposition the marker offset to be
    # relative to the center of the char.
    marker_offset = marker_offset .+ scale ./ 2

    charextent = Makie.FreeTypeAbstraction.get_extent(font, marker)
    inkbb = Makie.FreeTypeAbstraction.inkboundingbox(charextent)

    # scale normalized bbox by font size
    inkbb_scaled = Rect2f(origin(inkbb) .* scale, widths(inkbb) .* scale)

    # flip y for the centering shift of the character because in Cairo y goes down
    centering_offset = Vec2f(1, -1) .* (-origin(inkbb_scaled) .- 0.5f0 .* widths(inkbb_scaled))
    # this is the origin where we actually have to place the glyph so it can be centered
    charorigin = pos .+ Vec2f(marker_offset[1], -marker_offset[2])

    # First, we translate to the point where the
    # marker is supposed to go.
    transform = " translate(" * join(charorigin, ",") * ")"
    # Then, we rotate the context by the
    # appropriate amount,
    transform *= " rotate($(to_2d_rotation(rotation)))"
    # and apply a centering offset to account for
    # the fact that text is shown from the (relative)
    # bottom left corner.
    transform *= " translate(" * join(centering_offset, ",") * ")"

    g.transform = transform

    # TODO Why do we need these ifs and CairoMakie doesn't?
    if hasproperty(font, :family)
        text."font-family" = font.family
    end
    if hasproperty(font, :style)
        text."font-style" = font.style
    end

    push!(text, string(marker))
    text."stroke-width" = strokewidth
    text.stroke = svg_color(strokecolor)

    text.transform = "scale($(join(scale, ",")))"
    text."font-size" = 1
end

function draw_marker(svg_el, ::Type{<: Circle}, pos, scale, strokecolor, strokewidth,
        marker_offset, rotation)

    marker_offset = marker_offset + scale ./ 2
    # TODO How can we test if we need the sign here?
    pos += Point2f(marker_offset[1], -marker_offset[2])

    marker = if scale[1] != scale[2]
        ellipse = Element("ellipse")
        ellipse.cx = pos[1]
        ellipse.cy = pos[2]
        ellipse.rx = scale[1]
        ellipse.ry = scale[2]
        ellipse
    else
        circle = Element("circle")
        circle.cx = pos[1]
        circle.cy = pos[2]
        circle.r = scale[1]/2
        circle
    end

    sc = to_color(strokecolor)
    marker.stroke = svg_color(sc)
    marker."stroke-opacity" = svg_color_alpha(sc)
    marker."stroke-width" = strokewidth

    push!(svg_el, marker)
    return
end

function draw_marker(svg_el, ::Type{<: Rect}, pos, scale, strokecolor, strokewidth, marker_offset, rotation)

    # TODO How can we test if we need the sign here?
    pos = pos .+ Point2f(marker_offset[1], -marker_offset[2])

    rect = Element("rect")
    rect.transform = "rotate($(to_2d_rotation(rotation)))"
    rect.x = pos[1]
    rect.y = pos[2]
    rect.width = scale[1]
    rect.height = scale[2]

    sc = to_color(strokecolor)
    rect.stroke = svg_color(sc)
    rect."stroke-opacity" = svg_color_alpha(sc)
    rect."stroke-width" = strokewidth

    push!(svg_el, rect)
    return
end

function draw_marker(svg_el, beziermarker::BezierPath, pos, scale, strokecolor, strokewidth, marker_offset, rotation)

    path = Element("path")
    path.transform = "translate($(join(pos, ","))) rotate($(to_2d_rotation(rotation))) " *
                     "scale($(join(scale .* (1,-1), " ")))"

    path.d = ""
    draw_path(path, beziermarker)

    sc = to_color(strokecolor)
    path.stroke = svg_color(sc)
    path."stroke-opacity" = svg_color_alpha(sc)
    # TODO: Because we apply a scale trafo above, we need to rescale strokewidth.
    # But how to do that if strokewidth is a scalar and scale a vector?
    path."stroke-width" = strokewidth / scale[1]

    push!(svg_el, path)
    return
end

draw_path(path, bp::BezierPath) = foreach(x -> path_command(path, x), bp.commands)
path_command(path, c) = @warn "Command $c not implemented yet"
path_command(path, c::MoveTo) = path.d *= " M $(join(c.p, " "))"
path_command(path, c::LineTo) = path.d *= " L $(join(c.p, " "))"
path_command(path, c::CurveTo) =
    path.d *= " C $(join(c.c1, " ")), $(join(c.c2, " ")), $(join(c.p, " "))"
path_command(path, ::ClosePath) = path.d *= " Z"
function path_command(path, c::EllipticalArc)
    p2 = Vec2f(c.r1 * cos(c.a2), c.r2 * sin(c.a2))
    m = Mat2f(cos(c.angle), sin(c.angle), -sin(c.angle), cos(c.angle))
    p2 = Vec2f(m * p2) .+ c.c
    sweep_arc = Int64(c.a1 < c.a2)
    large_arc = Int64(abs(c.a1 - c.a2) > π)
    path.d *= " A $(c.r1) $(c.r2) $(c.angle) $large_arc $sweep_arc $(join(p2, " "))"
end

function draw_marker(svg_el, marker::Matrix{T}, pos, scale,
        strokecolor #= unused =#, strokewidth #= unused =#,
        marker_offset, rotation) where T<:Colorant

    encoded_marker = svg_encode_image(marker)

    w, h = size(marker)

    image = Element("image")
    image.width = scale[1]
    image.height = scale[2]
    image.x = pos[1] + marker_offset[1]
    image.y = pos[2] + marker_offset[2]
    image.transform = "rotate($(to_2d_rotation(rotation))) "
    image."xlink:href" = "data:image/png;base64,$encoded_marker"

    # needed to make markersize behavior consistent with CairoMakie, GLMakie:
    # markersize=100 -> marker is strechted to fill 100x100 pxs
    # markersize=(100,100*h/w) -> marker width is 100 and aspect is preserved
    image.preserveAspectRatio = "none"

    push!(svg_el, image)
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

    svg_el = last(root(screen.svg))
    @get_attribute(primitive, (rotation, model, space, markerspace, offset))
    positions = primitive.position[]
    space = to_value(get(primitive, :space, :data))
    projected_positions = project_position.(Ref(scene), Ref(space), positions, Ref(model))
    # use cached glyph info
    glyph_collection = to_value(primitive[1])
    text = text_signal[]

    draw_glyph_collection(
        scene, svg_el, projected_positions, glyph_collection, text, remove_billboard(rotation),
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

    char_idx = 0
    broadcast_foreach(glyphs, glyphoffsets, fonts, rotations, scales, colors, strokewidths, strokecolors, offsets) do glyph,
        glyphoffset, font, rotation, scale, color, strokewidth, strokecolor, offset

        # Not renderable by font (e.g. '\n')
        # TODO(from CairoMakie), filter out \n in GlyphCollection, and render unrenderables as box
        glyph == 0 && return

        tspan = Element("tspan")
        push!(svg_text, tspan)

        tspan.x = position[1]+glyphoffset[1]
        tspan.y = position[2]-glyphoffset[2]

        # TODO Should we set a fallback font in svg_text?
        # TODO Why do we need these ifs and CairoMakie doesnt?
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

        char_idx = nextind(text, char_idx)
        char = text[char_idx:nextind(text,char_idx)-1]
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

"""
    regularly_spaced_array_to_range(arr)
If possible, converts `arr` to a range.
If not, returns array unchanged.
"""
function regularly_spaced_array_to_range(arr)
    diffs = unique!(sort!(diff(arr)))
    step = sum(diffs) ./ length(diffs)
    if all(x-> x ≈ step, diffs)
        m, M = extrema(arr)
        if step < zero(step)
            m, M = M, m
        end
        # don't use stop=M, since that may not include M
        return range(m; step=step, length=length(arr))
    else
        return arr
    end
end

regularly_spaced_array_to_range(arr::AbstractRange) = arr

function premultiplied_rgba(a::AbstractArray{<:ColorAlpha})
    map(premultiplied_rgba, a)
end
premultiplied_rgba(a::AbstractArray{<:Color}) = RGBA.(a)

premultiplied_rgba(r::RGBA) = RGBA(r.r * r.alpha, r.g * r.alpha, r.b * r.alpha, r.alpha)
premultiplied_rgba(c::Colorant) = premultiplied_rgba(RGBA(c))

function draw_atomic(scene::Scene, screen::Screen, @nospecialize(primitive::Heatmap))

    svg_el = last(root(screen.svg))
    image = primitive[3][]
    xs, ys = primitive[1][], primitive[2][]
    model = primitive.model[]::Mat4f

    # Compared to Cairo, we removed the fast_path branch below, because SVG is always
    # a vector backend. However, using a Colorbar now hits the
    # ni + 1 != length(xs) || nj + 1 != length(ys) check below.
    # Turns out that if we remove the branching here (compared to CairoMakie),
    # then the colorbar can be rendered without problem.
    # TODO Figure out if this causes problems somewhere else.
    l, r = extrema(xs)
    N = size(image, 1)
    xs = range(l, r, length = N+1)
    l, r = extrema(ys)
    N = size(image, 2)
    ys = range(l, r, length = N+1)

    imsize = ((first(xs), last(xs)), (first(ys), last(ys)))
    # find projected image corners
    # this already takes care of flipping the image to correct cairo orientation
    space = to_value(get(primitive, :space, :data))
    xy = project_position(scene, space, Point2f(first.(imsize)), model)
    xymax = project_position(scene, space, Point2f(last.(imsize)), model)
    w, h = xymax .- xy

    # find projected image corners
    # this already takes care of flipping the image to correct cairo orientation
    space = to_value(get(primitive, :space, :data))
    xys = [project_position(scene, space, Point2f(x, y), model) for x in xs, y in ys]
    colors = to_rgba_image(image, primitive)

    # Note: xs and ys should have size ni+1, nj+1
    ni, nj = size(image)
    if ni + 1 != length(xs) || nj + 1 != length(ys)
        error("Error in conversion pipeline. xs and ys should have size ni+1, nj+1. Found: xs: $(length(xs)), ys: $(length(ys)), ni: $(ni), nj: $(nj)")
    end
    _draw_rect_heatmap(svg_el, xys, ni, nj, colors)
end

function _draw_rect_heatmap(svg_el, xys, ni, nj, colors)
    @inbounds for i in 1:ni, j in 1:nj
        p1 = xys[i, j]
        p2 = xys[i+1, j]
        p3 = xys[i+1, j+1]
        p4 = xys[i, j+1]

        # Rectangles and polygons that are directly adjacent usually show
        # white lines between them due to anti aliasing. To avoid this we
        # increase their size slightly.

        if alpha(colors[i, j]) == 1
            # sign.(p - center) gives the direction in which we need to
            # extend the polygon. (Which may change due to rotations in the
            # model matrix.) (i!=1) etc is used to avoid increasing the
            # outer extent of the heatmap.
            center = 0.25f0 * (p1 + p2 + p3 + p4)
            p1 += sign.(p1 - center) .* Point2f(0.5f0 * (i!=1),  0.5f0 * (j!=1))
            p2 += sign.(p2 - center) .* Point2f(0.5f0 * (i!=ni), 0.5f0 * (j!=1))
            p3 += sign.(p3 - center) .* Point2f(0.5f0 * (i!=ni), 0.5f0 * (j!=nj))
            p4 += sign.(p4 - center) .* Point2f(0.5f0 * (i!=1),  0.5f0 * (j!=nj))
        end

        path = Element("path")
        path.d = "M $(join(p1, ",")) L $(join(p2, ",")) L $(join(p3, ",")) L $(join(p4, ",")) Z"
        path.fill = svg_color(colors[i, j])
        path."fill-opacity" = svg_color_alpha(colors[i, j])

        push!(svg_el, path)
    end
end

function draw_atomic(scene::Scene, screen::Screen, @nospecialize(primitive::Image))

    svg_el = last(root(screen.svg))
    image = primitive[3][]
    xs, ys = primitive[1][], primitive[2][]
    model = primitive.model[]::Mat4f

    # TODO Need to rotate to be consistent with other backends.
    # This is weird, because for an up-right picture the user now has to call image(rotr90(img))
    # and here we undo it again ... see #389, #205.
    image = rotl90(image)
    iw, ih = size(image)

    imsize = ((first(xs), last(xs)), (first(ys), last(ys)))
    # find projected image corners
    # this already takes care of flipping the image to correct cairo orientation
    space = to_value(get(primitive, :space, :data))
    xy = project_position(scene, space, Point2f(first.(imsize)), model)
    xymax = project_position(scene, space, Point2f(last.(imsize)), model)
    w, h = xymax[1] - xy[1], xy[2] - xymax[2]

    encoded_image = svg_encode_image(image)

    image = Element("image")
    image.width = w
    image.height = h
    image.x = xs[1]
    image.y = xymax[2]
    # TODO How to apply rotatation? I think that is not support in any of the backends either.
    # image.transform = "rotate($(to_2d_rotation(rotation))) "
    image."xlink:href" = "data:image/png;base64,$encoded_image"

    # disable aspect ratio preservation
    image.preserveAspectRatio = "none"

    push!(svg_el, image)
end

################################################################################
#                                     Mesh                                     #
################################################################################

function barycentric_weights_triangle(p1, p2, p3, q)
    denom = (p2[2] - p3[2]) * (p1[1] - p3[1]) + (p3[1] - p2[1]) * (p1[2] - p3[2])
    w1 = ( (p2[2] - p3[2]) * (q[1] - p3[1]) + (p3[1] - p2[1]) * (q[2] - p3[2]) ) / denom
    w2 = ( (p3[2] - p1[2]) * (q[1] - p3[1]) + (p1[1] - p3[1]) * (q[2] - p3[2]) ) / denom
    w3 = 1 - w1 - w2
    return w1, w2, w3
    # return c1 * w1 + c2 * w2 + c3 * w3
end

function angle_align_upwards(d)
    if isapprox(d[1], 0)
        return d[2] > 0 ? 0 : -pi
    elseif isapprox(d[2], 0)
        return d[1] > 0 ? pi/2 : -pi/2
    else
        # println(d[1] > 0 ? "pos" : "neg")
        a = pi/2 - atan(d[2], d[1])
        return d[1] > 0 ? a : -a
    end
end

function is_inside_triangle(p1, p2, p3, q)

    d = p2 .- p1
    alpha = angle_align_upwards(d)
    rot = Mat2f(cos(alpha), sin(alpha), -sin(alpha), cos(alpha))

    up = rot * d
    r3 = rot * (p3 .- p1)
    qq = rot * (q .- p1)

    # qq and r3 lie on opposite sites
    qq[1] * r3[1] < 0 && return false

    # bring both to positive side
    qqx = abs(qq[1])
    r3x = abs(r3[1])

    slope_13 = r3[2] / r3x
    slope_23 = (r3[2] - up[2]) / r3x
    slope_1qq = qq[2] / qqx
    slope_2qq = (qq[2] - up[2]) / qqx

    return slope_1qq > slope_13 && slope_2qq < slope_23
end

function draw_atomic(scene::Scene, screen::Screen, @nospecialize(primitive::Makie.Mesh))
    mesh = primitive[1][]
    if Makie.cameracontrols(scene) isa Union{Camera2D, Makie.PixelCamera, Makie.EmptyCamera}
        draw_mesh2D(scene, screen, primitive, mesh)
    else
        if !haskey(primitive, :faceculling)
            primitive[:faceculling] = Observable(-10)
        end
        draw_mesh3D(scene, screen, primitive, mesh)
    end
    return nothing
end

function draw_mesh2D(scene, screen, @nospecialize(plot), @nospecialize(mesh))
    @get_attribute(plot, (color,))
    color = to_color(hasproperty(mesh, :color) ? mesh.color : color)
    vs =  decompose(Point2f, mesh)::Vector{Point2f}
    fs = decompose(GLTriangleFace, mesh)::Vector{GLTriangleFace}
    uv = decompose_uv(mesh)::Union{Nothing, Vector{Vec2f}}
    model = plot.model[]::Mat4f
    colormap = haskey(plot, :colormap) ? to_colormap(plot.colormap[]) : nothing
    colorrange = convert_attribute(to_value(get(plot, :colorrange, nothing)), key"colorrange"())::Union{Nothing, Vec2f}

    lowclip = get_color_attr(plot, :lowclip)
    highclip = get_color_attr(plot, :highclip)
    nan_color = get_color_attr(plot, :nan_color)

    cols = per_face_colors(
        color, colormap, colorrange, nothing, fs, nothing, uv,
        lowclip, highclip, nan_color)

    space = to_value(get(plot, :space, :data))::Symbol
    return draw_mesh2D(scene, screen, cols, space, vs, fs, model)
end

function draw_mesh2D(scene, screen, per_face_cols, space::Symbol,
        vs::Vector{Point2f}, fs::Vector{GLTriangleFace}, model::Mat4f)

    svg_el = last(root(screen.svg))
    svg_defs = defs(screen.svg)

    for (f, (c1, c2, c3)) in zip(fs, per_face_cols)

        t1, t2, t3 =  project_position.(scene, space, vs[f], (model,)) #triangle points

        xmin = min(t1[1], t2[1], t3[1])
        xmax = max(t1[1], t2[1], t3[1])
        ymin = min(t1[2], t2[2], t3[2])
        ymax = max(t1[2], t2[2], t3[2])
        w = xmax - xmin
        h = ymax - ymin

        # TODO Raster resolution Nx, Ny should determined from a parameter like px_per_units
        # which specifies resolution in screen space.
        Nx, Ny = 25, 25

        rxs, rys = LinRange(xmin, xmax, Nx), LinRange(ymin, ymax, Ny)
        color_matrix = rasterize_mesh(t1, t2, t3, c1, c2, c3, rxs, rys)
        s = 0.005
        v1, v2, v3 = stretch_vertices(t1, t2, t3, s)

        encoded_image = svg_encode_image(color_matrix)

        # apply a clip with the stretched vertices to get a (transparent) triangle
        id = "$(string(UUIDs.uuid1()))"
        clip = Element("clipPath")
        clip.id = id
        path = Element("path")
        path.d = "M $(join(v1,",")) L $(join(v2,",")) L $(join(v3,",")) Z"
        push!(clip, path)
        push!(svg_defs, clip)

        image = Element("image")
        image.width = w
        image.height = h
        image.x = xmin
        image.y = ymin
        image."xlink:href" = "data:image/png;base64,$encoded_image"
        image."clip-path" = "url(#$id)"
        image."clip-rule" = "nonzero"
        # disable aspect ratio preservation
        image.preserveAspectRatio = "none"

        push!(svg_el, image)
    end

    return
end

function average_z(positions, face)
    vs = positions[face]
    sum(v -> v[3], vs) / length(vs)
end

nan2zero(x) = !isnan(x) * x


function draw_mesh3D(scene, screen, attributes, mesh; pos = Vec4f(0), scale = 1f0)
    # Priorize colors of the mesh if present
    @get_attribute(attributes, (color,))

    colormap = haskey(attributes, :colormap) ? to_colormap(attributes.colormap[]) : nothing
    colorrange = convert_attribute(
        to_value(get(attributes, :colorrange, nothing)), key"colorrange"())::Union{Nothing, Vec2f}
    matcap = to_value(get(attributes, :matcap, nothing))

    color = hasproperty(mesh, :color) ? mesh.color : color
    meshpoints = decompose(Point3f, mesh)::Vector{Point3f}
    meshfaces = decompose(GLTriangleFace, mesh)::Vector{GLTriangleFace}
    meshnormals = decompose_normals(mesh)::Vector{Vec3f}
    meshuvs = texturecoordinates(mesh)::Union{Nothing, Vector{Vec2f}}

    lowclip = get_color_attr(attributes, :lowclip)
    highclip = get_color_attr(attributes, :highclip)
    nan_color = get_color_attr(attributes, :nan_color)

    per_face_col = per_face_colors(
        color, colormap, colorrange, matcap, meshfaces, meshnormals, meshuvs,
        lowclip, highclip, nan_color
    )

    @get_attribute(attributes, (shading, diffuse,
        specular, shininess, faceculling))

    model = attributes.model[]::Mat4f
    space = to_value(get(attributes, :space, :data))::Symbol

    draw_mesh3D(
        scene, screen, space, meshpoints, meshfaces, meshnormals, per_face_col, pos, scale,
        model, shading::Bool, diffuse::Vec3f,
        specular::Vec3f, shininess::Float32, faceculling::Int
    )
end

function draw_mesh3D(
        scene, screen, space, meshpoints, meshfaces, meshnormals, per_face_col, pos, scale,
        model, shading, diffuse,
        specular, shininess, faceculling
    )

    svg = screen.svg
    view = ifelse(is_data_space(space), scene.camera.view[], Mat4f(I))
    projection = Makie.space_to_clip(scene.camera, space, false)
    i = Vec(1, 2, 3)
    normalmatrix = transpose(inv(view[i, i] * model[i, i]))

    # Mesh data
    # transform to view/camera space
    func = Makie.transform_func_obs(scene)[]
    # pass func as argument to function, so that we get a function barrier
    # and have `func` be fully typed inside closure
    vs = broadcast(meshpoints, (func,)) do v, f
        # Should v get a nan2zero?
        v = Makie.apply_transform(f, v)
        p4d = to_ndim(Vec4f, scale .* to_ndim(Vec3f, v, 0f0), 1f0)
        view * (model * p4d .+ to_ndim(Vec4f, pos, 0f0))
    end

    ns = map(n -> normalize(normalmatrix * n), meshnormals)
    # Light math happens in view/camera space
    pointlight = Makie.get_point_light(scene)
    lightposition = if !isnothing(pointlight)
        pointlight.position[]
    else
        Vec3f(0)
    end

    ambientlight = Makie.get_ambient_light(scene)
    ambient = if !isnothing(ambientlight)
        c = ambientlight.color[]
        Vec3f(c.r, c.g, c.b)
    else
        Vec3f(0)
    end

    lightpos = (view * to_ndim(Vec4f, lightposition, 1.0))[Vec(1, 2, 3)]

    # Camera to screen space
    ts = map(vs) do v
        clip = projection * v
        @inbounds begin
            p = (clip ./ clip[4])[Vec(1, 2)]
            p_yflip = Vec2f(p[1], -p[2])
            p_0_to_1 = (p_yflip .+ 1f0) ./ 2f0
        end
        p = p_0_to_1 .* scene.camera.resolution[]
        return Vec3f(p[1], p[2], clip[3])
    end

    # Approximate zorder
    average_zs = map(f -> average_z(ts, f), meshfaces)
    zorder = sortperm(average_zs)

    # Face culling
    zorder = filter(i -> any(last.(ns[meshfaces[i]]) .> faceculling), zorder)

    draw_pattern(svg, zorder, shading, meshfaces, ts, per_face_col, ns, vs, lightpos,
                 shininess, diffuse, ambient, specular)
    return
end

function _calculate_shaded_vertexcolors(N, v, c, lightpos, ambient, diffuse, specular, shininess)
    L = normalize(lightpos .- v[Vec(1,2,3)])
    diff_coeff = max(dot(L, N), 0f0)
    H = normalize(L + normalize(-v[Vec(1, 2, 3)]))
    spec_coeff = max(dot(H, N), 0f0)^shininess
    c = RGBAf(c)
    # if this is one expression it introduces allocations??
    new_c_part1 = (ambient .+ diff_coeff .* diffuse) .* Vec3f(c.r, c.g, c.b) #.+
    new_c = new_c_part1 .+ specular * spec_coeff
    RGBAf(new_c..., c.alpha)
end

function draw_pattern(svg, zorder, shading, meshfaces, ts, per_face_col, ns, vs, lightpos,
        shininess, diffuse, ambient, specular)

    svg_el = last(root(svg))
    svg_defs = defs(svg)

    for k in reverse(zorder)
        f = meshfaces[k]
        # avoid SizedVector through Face indexing
        tt1 = ts[f[1]]
        tt2 = ts[f[2]]
        tt3 = ts[f[3]]

        facecolors = per_face_col[k]
        # light calculation
        if shading
            c1, c2, c3 = Base.Cartesian.@ntuple 3 i -> begin
                # these face index expressions currently allocate for SizedVectors
                # if done like `ns[f]`
                N = ns[f[i]]
                v = vs[f[i]]
                c = facecolors[i]
                _calculate_shaded_vertexcolors(N, v, c, lightpos, ambient, diffuse, specular, shininess)
            end
        else
            c1, c2, c3 = facecolors
        end

        # debug normal coloring
        # n1, n2, n3 = Vec3f(0.5) .+ 0.5ns[f]
        # c1 = RGB(n1...)
        # c2 = RGB(n2...)
        # c3 = RGB(n3...)

        t1, t2, t3 = Vec2f(tt1[1],tt1[2]), Vec2f(tt2[1],tt2[2]), Vec2f(tt3[1],tt3[2])

        xmin = min(t1[1], t2[1], t3[1])
        xmax = max(t1[1], t2[1], t3[1])
        ymin = min(t1[2], t2[2], t3[2])
        ymax = max(t1[2], t2[2], t3[2])
        w = xmax - xmin
        h = ymax - ymin

        # TODO Raster resolution Nx, Ny should determined from a parameter like px_per_units
        # which specifies resolution in screen space.
        Nx, Ny = 5, 5

        rxs, rys = LinRange(xmin, xmax, Nx), LinRange(ymin, ymax, Ny)
        color_matrix = rasterize_mesh(t1, t2, t3, c1, c2, c3, rxs, rys, true)
        s = 0.005
        v1, v2, v3 = stretch_vertices(t1, t2, t3, s)

        encoded_image = svg_encode_image(color_matrix)

        # apply a clip with the stretched vertices to get a (transparent) triangle
        id = "$(string(UUIDs.uuid1()))"
        clip = Element("clipPath")
        clip.id = id
        path = Element("path")
        path.d = "M $(join(v1,",")) L $(join(v2,",")) L $(join(v3,",")) Z"
        push!(clip, path)
        push!(svg_defs, clip)

        image = Element("image")
        image.width = w
        image.height = h
        image.x = xmin
        image.y = ymin
        image."xlink:href" = "data:image/png;base64,$encoded_image"
        image."clip-path" = "url(#$id)"
        image."clip-rule" = "nonzero"
        # disable aspect ratio preservation
        image.preserveAspectRatio = "none"

        push!(svg_el, image)
    end

    return
end

function rasterize_mesh(t1, t2, t3, # triangle's vertices
                        c1, c2, c3, # colors at vertices,
                        rxs, rys, # raster coordinates
                        sanitize_all=false)

    Nx, Ny = length(rxs), length(rys)
    color_matrix = zeros(RGBAf, (Nx, Ny))

    # TODO: Optimization: Keep the rasterized mesh as small as possible by aligning the
    # longest triangle side with the side of the raster. We would then have to
    # rotate the raster below.
    # d12 = Vec2f(t2 .- t1)
    # d13 = Vec2f(t3 .- t1)
    # d23 = Vec2f(t3 .- t2)
    # nd12 = norm(d12)
    # nd13 = norm(d13)
    # nd23 = norm(d23)
    # dd = nd12 > nd13 ? (nd12 > nd23 ? d12 : d23) : (nd13 > nd23 ? d13 : d23)
    # a = angle_align_upwards(dd)
    # rot = Mat2f(cos(a), sin(a), -sin(a), cos(a))
    # r1, r2, r3 = rot * (t1 - dd), rot * (t2 - dd), rot * (t3 - dd)

    for ix = 1:Nx, iy = 1:Ny
        p = Vec2f(rxs[ix], rys[iy])
        w1, w2, w3 = barycentric_weights_triangle(t1, t2, t3, p)
        color = RGBAf( w1*c1.r + w2*c2.r + w3*c3.r,
                       w1*c1.g + w2*c2.g + w3*c3.g,
                       w1*c1.b + w2*c2.b + w3*c3.b,
                       w1*c1.alpha + w2*c2.alpha + w3*c3.alpha )
        if !is_inside_triangle(t1, t2, t3, p) || sanitize_all
            # sanitize color if we 'extrapolated'
            r = max(min(color.r, 1.0), 0.0)
            g = max(min(color.g, 1.0), 0.0)
            b = max(min(color.b, 1.0), 0.0)
            alpha = max(min(color.alpha, 1.0), 0.0)
            color = RGBAf(isnan(r) ? 0.0 : r, isnan(g) ? 0.0 : g, isnan(b) ? 0.0 : g,
                          isnan(alpha) ? 0.0 : alpha)
        end
        color_matrix[ix,iy] = color
    end

    return color_matrix
end

function stretch_vertices(t1, t2, t3, s)

    # Triangle that are directly adjacent show white lines between, because the rasterized
    # images don't touch exactly. Here we artificially stretch the triangle's vertices
    # outwards (from each other) and use those for clipping the mesh image.
    # Stretching is done along the line that connects a vertex and the center of its
    # opposing triangle edge.
    # vector connecting a corner with an opposing edge's center
    s1 = Vec2f(@. t2 + (t3 - t2) / 2)
    s2 = Vec2f(@. t1 + (t3 - t1) / 2)
    s3 = Vec2f(@. t1 + (t2 - t1) / 2)
    # vector connecting vertex and opposing edge
    u1 = Vec2f(@. t1 - s1)
    u2 = Vec2f(@. t2 - s2)
    u3 = Vec2f(@. t3 - s3)
    # stretched vertices
    v1 = Vec2f(@. t1 + s * u1)
    v2 = Vec2f(@. t2 + s * u2)
    v3 = Vec2f(@. t3 + s * u3)

    return v1, v2, v3
end

################################################################################
#                                   Surface                                    #
################################################################################

function draw_atomic(scene::Scene, screen::Screen, @nospecialize(primitive::Makie.Surface))
    # Pretend the surface plot is a mesh plot and plot that instead
    mesh = surface2mesh(primitive[1][], primitive[2][], primitive[3][])
    old = primitive[:color]
    if old[] === nothing
        primitive[:color] = primitive[3]
    end
    if !haskey(primitive, :faceculling)
        primitive[:faceculling] = Observable(-10)
    end
    draw_mesh3D(scene, screen, primitive, mesh)
    primitive[:color] = old
    return
end

function surface2mesh(xs, ys, zs::AbstractMatrix)
    ps = Makie.matrix_grid(p-> nan2zero.(p), xs, ys, zs)
    rect = Tesselation(Rect2f(0, 0, 1, 1), size(zs))
    faces = decompose(QuadFace{Int}, rect)
    uv = map(x-> Vec2f(1f0 - x[2], 1f0 - x[1]), decompose_uv(rect))
    uvm = GeometryBasics.Mesh(GeometryBasics.meta(ps; uv=uv), faces)
    return GeometryBasics.normal_mesh(uvm)
end

################################################################################
#                                 MeshScatter                                  #
################################################################################

function draw_atomic(scene::Scene, screen::Screen, @nospecialize(primitive::Makie.MeshScatter))
    @get_attribute(primitive, (color, model, marker, markersize, rotations))

    if color isa AbstractArray{<: Number}
        color = numbers_to_colors(color, primitive)
    end

    m = convert_attribute(marker, key"marker"(), key"meshscatter"())
    pos = primitive[1][]
    # For correct z-ordering we need to be in view/camera or screen space
    model = copy(model)
    view = scene.camera.view[]

    zorder = sortperm(pos, by = p -> begin
        p4d = to_ndim(Vec4f, to_ndim(Vec3f, p, 0f0), 1f0)
        cam_pos = view * model * p4d
        cam_pos[3] / cam_pos[4]
    end, rev=false)

    submesh = Attributes(
        model=model,
        color=color,
        shading=primitive.shading, diffuse=primitive.diffuse,
        specular=primitive.specular, shininess=primitive.shininess,
        faceculling=get(primitive, :faceculling, -10)
    )

    if !(rotations isa Vector)
        R = Makie.rotationmatrix4(to_rotation(rotations))
        submesh[:model] = model * R
    end
    scales = primitive[:markersize][]

    for i in zorder
        p = pos[i]
        if color isa AbstractVector
            submesh[:color] = color[i]
        end
        if rotations isa Vector
            R = Makie.rotationmatrix4(to_rotation(rotations[i]))
            submesh[:model] = model * R
        end
        scale = markersize isa Vector ? markersize[i] : markersize

        draw_mesh3D(
            scene, screen, submesh, m, pos = p,
            scale = scale isa Real ? Vec3f(scale) : to_ndim(Vec3f, scale, 1f0)
        )
    end

    return
end
