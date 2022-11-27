function svg_draw(screen::Screen, scene::Scene)
    # Here comes the magic

    # maybe backup previous IO in case we encounter an error?
    draw_background(screen, scene)
    allplots = get_all_plots(scene)
    zvals = Makie.zvalue2d.(allplots)
    permute!(allplots, sortperm(zvals))

    last_scene = scene

    for p in allplots
        to_value(get(p, :visible, true)) || continue
        pparent = p.parent::Scene
        pparent.visible[] || continue
        if pparent != last_scene
            prepare_for_scene!(screen, pparent)
            last_scene = pparent
        end
        draw_plot(pparent, screen, p)
    end

    return
end


function get_all_plots(scene, plots = AbstractPlot[])
    append!(plots, scene.plots)
        for c in scene.children
            get_all_plots(c, plots)
        end
    plots
end


function prepare_for_scene!(screen::Screen, scene::Scene)

    # get the root area to correct for its pixel size when translating
    root_area = Makie.root(scene).px_area[]

    root_area_height = widths(root_area)[2]
    scene_area = pixelarea(scene)[]
    scene_width, scene_height = widths(scene_area)
    scene_x_origin, scene_y_origin = scene_area.origin

    # we need to translate x by the origin, so distance from the left
    # but y by the distance from the top, which is not the origin, but can
    # be calculated using the parent's height, the scene's height and the y origin
    # this is because y goes downwards in SVG and upwards in Makie

    top_offset = root_area_height - scene_height - scene_y_origin

    # clip to the scene's area
    id = "$(string(UUIDs.uuid1()))"
    clip = Element("clipPath")
    clip.id = id
    rect = Element("rect")
    top_left_corner = coordinates(scene_area) |> first
    rect.x = top_left_corner[1] - scene_x_origin
    rect.y = top_left_corner[2] - scene_y_origin
    rect.width = scene_width
    rect.height = scene_height
    push!(clip, rect)
    push!(defs(screen.svg), clip)

    # following drawing commands are to be wrapped inside a <g> element that takes care of
    # translation and clipping of the scene
    g = Element("g")
    g.transform = "translate($scene_x_origin, $top_offset)"
    g."clip-path" = "url(#$id)"
    g."clip-rule" = "nonzero"
    push!(root(screen.svg), g)

    return
end


function draw_background(screen::Screen, scene::Scene)
    if scene.clear[]
        bg = scene.backgroundcolor[]
        # svg 1.1 cannot fill a viewBox, so just add a rectangle as the first element and color it
        rect = Element("rect")
        rect.width = "100%"
        rect.height = "100%"
        rect.fill = svg_color(bg)
        rect."fill-opacity" = svg_color_alpha(bg)
        push!(screen.svg, rect)
    end
    foreach(child_scene-> draw_background(screen, child_scene), scene.children)
end


function draw_plot(scene::Scene, screen::Screen, primitive::Combined)
    !to_value(get(primitive, :visible, true)) && return
    if isempty(primitive.plots)
        draw_atomic(scene, screen, primitive)
    else
        zvals = Makie.zvalue2d.(primitive.plots)
        for idx in sortperm(zvals)
            draw_plot(scene, screen, primitive.plots[idx])
        end
    end
    return
end


# We need to intercept Makie.Text in order to extract the :text field (if present)
# and forward it to draw_atomic.
function draw_plot(scene::Scene, screen::Screen, primitive::Makie.Text,
                   text_signal=nothing)
    !to_value(get(primitive, :visible, true)) && return
    if isempty(primitive.plots)
        draw_atomic(scene, screen, primitive, text_signal)
    else
        zvals = Makie.zvalue2d.(primitive.plots)
        if isnothing(text_signal) && :text in propertynames(primitive)
            text_signal = primitive.text
        end
        for idx in sortperm(zvals)
            p = primitive.plots[idx]
            if p isa Makie.Text
                draw_plot(scene, screen, p, text_signal)
            else
                draw_plot(scene, screen, p)
            end
        end
    end
    return
end


function draw_atomic(::Scene, ::Screen, x)
    @warn "$(typeof(x)) is not supported by SVGMakie right now"
end
