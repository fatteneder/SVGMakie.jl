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


function draw_background(screen::Screen, scene::Scene)
    if scene.clear[]
        bg = scene.backgroundcolor[]
        screen.svg.fill = svg_color(bg)
        # screen > "fill=" * svg_color(bg)
    end
    foreach(child_scene-> draw_background(screen, child_scene), scene.children)
end


function draw_plot(scene::Scene, screen::Screen, primitive::Combined)
    if to_value(get(primitive, :visible, true))
        if isempty(primitive.plots)
            draw_atomic(scene, screen, primitive)
        else
            zvals = Makie.zvalue2d.(primitive.plots)
            for idx in sortperm(zvals)
                draw_plot(scene, screen, primitive.plots[idx])
            end
        end
    end
    return
end


function draw_atomic(::Scene, ::Screen, x)
    @warn "$(typeof(x)) is not supported by SVGMakie right now"
end
