
# recognized color keywords: https://www.w3.org/TR/SVG11/types.html#ColorKeywords
svg_color(color) = string(color)
svg_color(color::Colorant) = "\"rgb($(color.r),$(color.g),$(color.b))\""

svg_color_alpha(color) = ""
svg_color_alpha(color::TransparentColor) = "\"$(color.alpha)\""
