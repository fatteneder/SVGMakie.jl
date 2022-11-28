################################################################################
#                    Poly - the not so primitive, primitive                    #
################################################################################


"""
Special method for polys so we don't fall back to atomic meshes, which are much more
complex and slower to draw than standard paths with single color.
"""
function draw_plot(scene::Scene, screen::Screen, poly::Poly)
    # dispatch on input arguments to poly to use smarter drawing methods than
    # meshes if possible
    draw_poly(scene, screen, poly, to_value.(poly.input_args)...)
end

"""
Fallback method for args without special treatment.
"""
function draw_poly(scene::Scene, screen::Screen, poly, args...)
    draw_poly_as_mesh(scene, screen, poly)
end

function draw_poly_as_mesh(scene, screen, poly)
    draw_plot(scene, screen, poly.plots[1])
    draw_plot(scene, screen, poly.plots[2])
end


# in the rare case of per-vertex colors redirect to mesh drawing
function draw_poly(scene::Scene, screen::Screen, poly, points::Vector{<:Point2},
        color::AbstractArray, model, strokecolor, strokewidth)
    println("or maybe here?")
    draw_poly_as_mesh(scene, screen, poly)
end

function draw_poly(scene::Scene, screen::Screen, poly, points::Vector{<:Point2})
    println("or here?")
    draw_poly(scene, screen, poly, points, poly.color[], poly.model[], poly.strokecolor[],
              poly.strokewidth[])
end

# # when color is a Makie.AbstractPattern, we don't need to go to Mesh
# function draw_poly(scene::Scene, screen::Screen, poly, points::Vector{<:Point2},
#         color::Union{Symbol, Colorant, Makie.AbstractPattern}, model, strokecolor, strokewidth)
#     space = to_value(get(poly, :space, :data))
#     points = project_position.(Ref(scene), space, points, Ref(model))
#     Cairo.move_to(screen.context, points[1]...)
#     for p in points[2:end]
#         Cairo.line_to(screen.context, p...)
#     end
#     Cairo.close_path(screen.context)
#     if color isa Makie.AbstractPattern
#         cairopattern = Cairo.CairoPattern(color)
#         Cairo.pattern_set_extend(cairopattern, Cairo.EXTEND_REPEAT);
#         Cairo.set_source(screen.context, cairopattern)
#     else
#         Cairo.set_source_rgba(screen.context, rgbatuple(to_color(color))...)
#     end
#
#     Cairo.fill_preserve(screen.context)
#     Cairo.set_source_rgba(screen.context, rgbatuple(to_color(strokecolor))...)
#     Cairo.set_line_width(screen.context, strokewidth)
#     Cairo.stroke(screen.context)
# end
#
function draw_poly(scene::Scene, screen::Screen, poly, points_list::Vector{<:Vector{<:Point2}})
    println("are we here?")
    broadcast_foreach(points_list, poly.color[],
        poly.strokecolor[], poly.strokewidth[]) do points, color, strokecolor, strokewidth

            draw_poly(scene, screen, poly, points, color, poly.model[], strokecolor, strokewidth)
    end
    return
end

draw_poly(scene::Scene, screen::Screen, poly, rect::Rect2) = draw_poly(scene, screen, poly, [rect])

function draw_poly(scene::Scene, screen::Screen, poly, rects::Vector{<:Rect2})
    model = poly.model[]
    space = to_value(get(poly, :space, :data))
    projected_rects = project_rect.(Ref(scene), space, rects, Ref(model))

    color = poly.color[]
    if color isa AbstractArray{<:Number}
        color = numbers_to_colors(color, poly)
    elseif color isa String
        # string is erroneously broadcasted as chars otherwise
        color = to_color(color)
    elseif color isa Makie.AbstractPattern
        error("Not implemented!")
        cairopattern = Cairo.CairoPattern(color)
        Cairo.pattern_set_extend(cairopattern, Cairo.EXTEND_REPEAT);
    end
    strokecolor = poly.strokecolor[]
    if strokecolor isa AbstractArray{<:Number}
        strokecolor = numbers_to_colors(strokecolor, poly)
    elseif strokecolor isa String
        # string is erroneously broadcasted as chars otherwise
        strokecolor = to_color(strokecolor)
    end

    svg_el = last(root(screen.svg))
    broadcast_foreach(projected_rects, color, strokecolor, poly.strokewidth[]) do r, c, sc, sw

        rect = Element("rect")
        x, y = origin(r)
        w, h = abs.(widths(r))
        ox, oy = x, y - h
        rect.x = ox
        rect.y = oy
        rect.width = w
        rect.height = h
        println(ox)
        println(oy)

        if c isa Makie.AbstractPattern
            error("Not implemented!")
            Cairo.set_source(screen.context, cairopattern)
        else
            rect.fill = svg_color(c)
            rect."fill-opacity" = svg_color_alpha(c)
        end

        rect.stroke = svg_color(sc)
        rect."stroke-opacity" = svg_color_alpha(sc)
        rect."stroke-width" = sw

        push!(svg_el, rect)
    end

    return
end

function polypath(svg_el, polygon)
    ext = decompose(Point2f, polygon.exterior)

    path = Element("path")
    path.d = "M $(join(ext[1],","))"
    for point in ext[2:end]
        path.d *= " L $(join(point,","))"
    end
    path.d *= " Z"

    interiors = decompose.(Point2f, polygon.interiors)
    for interior in interiors
        path.d *= "M $(join(interior,","))"
        for point in interior[2:end]
            path.d *= " L $(join(point,","))"
        end
        path.d *= " Z"
    end

    push!(svg_el, path)

    return
end

draw_poly(scene::Scene, screen::Screen, poly, polygon::Polygon) =
    draw_poly(scene, screen, poly, [polygon])

function draw_poly(scene::Scene, screen::Screen, poly, polygons::AbstractArray{<:Polygon})

    model = poly.model[]
    space = to_value(get(poly, :space, :data))
    projected_polys = project_polygon.(Ref(scene), space, polygons, Ref(model))

    color = poly.color[]
    if color isa AbstractArray{<:Number}
        color = numbers_to_colors(color, poly)
    elseif color isa String
        # string is erroneously broadcasted as chars otherwise
        color = to_color(color)
    end
    strokecolor = poly.strokecolor[]
    if strokecolor isa AbstractArray{<:Number}
        strokecolor = numbers_to_colors(strokecolor, poly)
    elseif strokecolor isa String
        # string is erroneously broadcasted as chars otherwise
        strokecolor = to_color(strokecolor)
    end

    svg_el = last(root(screen.svg))
    broadcast_foreach(projected_polys, color, strokecolor, poly.strokewidth[]) do po, c, sc, sw
        polypath(svg_el, po)
        path = last(svg_el)
        path.fill = svg_color(c)
        path."fill-opacity" = svg_color_alpha(c)
        path.stroke = svg_color(sc)
        path."stroke-opacity" = svg_color_alpha(sc)
        path."stroke-width" = sw
    end

    return
end

################################################################################
#                                     Band                                     #
#     Override because band is usually a polygon, but because it supports      #
#        gradients as well via `mesh` we have to intercept the poly use        #
################################################################################

function draw_plot(scene::Scene, screen::Screen,
        band::Band{<:Tuple{<:AbstractVector{<:Point2},<:AbstractVector{<:Point2}}})

    svg_el = last(root(screen.svg))

    if !(band.color[] isa AbstractArray)
        upperpoints = band[1][]
        lowerpoints = band[2][]
        points = vcat(lowerpoints, reverse(upperpoints))
        model = band.model[]
        space = to_value(get(band, :space, :data))
        points = project_position.(Ref(scene), space, points, Ref(model))

        path = Element("path")
        path.d = "M $(join(points[1],","))"
        for p in points[2:end]
            path.d *= " L $(join(p,","))"
        end
        path.d *= " Z"

        color = to_color(band.color[])
        path.fill = svg_color(color)
        path."fill-opacity" = svg_color_alpha(color)

        push!(svg_el, path)
    else
        for p in band.plots
            display(p |> typeof)
            draw_plot(scene, screen, p)
        end
    end

    return
end

#################################################################################
#                                  Tricontourf                                  #
# Tricontourf creates many disjoint polygons that are adjacent and form contour #
#  bands, however, at the gaps we see white antialiasing artifacts. Therefore   #
#               we override behavior and draw each band in one go               #
#################################################################################

function draw_plot(scene::Scene, screen::Screen, tric::Tricontourf)

    pol = only(tric.plots)::Poly
    colornumbers = pol.color[]
    colors = numbers_to_colors(colornumbers, pol)

    polygons = pol[1][]

    model = pol.model[]
    space = to_value(get(pol, :space, :data))
    projected_polys = project_polygon.(Ref(scene), space, polygons, Ref(model))

    svg_el = last(root(screen.svg))

    function draw_tripolys(polys, colornumbers, colors)
        for (i, (pol, colnum, col)) in enumerate(zip(polys, colornumbers, colors))
            polypath(svg_el, pol)
            path = last(svg_el)
            path.fill = svg_color(col)
            path."fill-opacity" = svg_color_alpha(col)
            # this is enough to get rid of the white edges between adjacent elements
            path.stroke = svg_color(col)
            path."stroke-opacity" = svg_color_alpha(col)
        end
        return
    end

    draw_tripolys(projected_polys, colornumbers, colors)

    return
end
