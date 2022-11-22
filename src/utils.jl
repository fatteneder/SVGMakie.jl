# recognized color keywords: https://www.w3.org/TR/SVG11/types.html#ColorKeywords
svg_color(color) = string(color)
svg_color(color::Colorant) = "#$(hex(color, :rrggbb))"

svg_color_alpha(color) = "1"
svg_color_alpha(color::TransparentColor) = "$(color.alpha)"

svg_url(url) = "url(#$url)"


#######################################################################
#       Methods below were copied from CairoMakie/src/utils.jl        #
#######################################################################


################################################################################
#                             Projection utilities                             #
################################################################################

function project_position(scene, transform_func::T, space, point, model, yflip::Bool = true) where T
    # use transform func
    point = Makie.apply_transform(transform_func, point)
    _project_position(scene, space, point, model, yflip)
end

function _project_position(scene, space, point, model, yflip)
    res = scene.camera.resolution[]
    p4d = to_ndim(Vec4f, to_ndim(Vec3f, point, 0f0), 1f0)
    clip = Makie.space_to_clip(scene.camera, space) * model * p4d
    @inbounds begin
        # between -1 and 1
        p = (clip ./ clip[4])[Vec(1, 2)]
        # flip y to match cairo
        p_yflip = Vec2f(p[1], (1f0 - 2f0 * yflip) * p[2])
        # normalize to between 0 and 1
        p_0_to_1 = (p_yflip .+ 1f0) ./ 2f0
    end
    # multiply with scene resolution for final position
    return p_0_to_1 .* res
end

function project_position(scene, space, point, model, yflip::Bool = true)
    project_position(scene, scene.transformation.transform_func[], space, point, model, yflip)
end

########################################
#          Rotation handling           #
########################################

remove_billboard(x) = x
remove_billboard(b::Makie.Billboard) = b.rotation
