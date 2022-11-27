# SVGMakie

Very experimental. See `mwe/` for (hopefully) working examples.

# TODO
- [ ] Understand SVG.
- [x] (Roughly) Figure out what needs to be implemented from Makie.
  - IMO CairoMakie is the backend that is easiest to understand. I will try to mimick this first.
  - Mimiced a project structure similar to CairoMakie.jl
- [x] Get simple plot going through Makies interface.
- [x] Implement `svg_color`.
- [x] SVGs are strucutered xml files. It allows to add CSS sections and later refere to them
  in primitives for styling or automation. We should utilize `XML.jl` to construct the files
  and build a SVG type that is just a wrapper around an `XML.Document` object.
- [ ] Figure out if XML prologs are of any relevance for us,
  https://oreillymedia.github.io/Using_SVG/extras/ch01-XML.html
- [x] Implement `Lines, LineScatter`.
- [x] Fix opacity for `Lines, LineScatter`.
- [x] `Lines, Linescatter` seem to be not placed correctly.
  Directly compared the values for the positions with `CairoMakie.jl`.
  All values agree, but in `SVGMakie.jl` we have one extra LineSegment appearing.
  That should not be a problem, because it becomes its own `<path>` element, but it is
  still interesting where this comes from.
  Problemm was that in `CairoMakie` the coordinate system is translated before drawing
  inside `prepare_for_scene`, which I somehow had removed from `SVGMakie`.
- [x] Look into the structure of the `.svg` output from `CairoMakie.jl`.
  For text: Every glyph is rendered separately with a `<g>` section.
- [x Implement basic text support.
- [x] Fix text placement.
- [x] Add rounded edges for `Lines, LineScatter` strokes.
- [ ] Lines: stroke-linecap = "round" adds a half circle centered at the endpoint of the line.
  How do Cairo and GLMakie handle those? To get it right, we would have to shorten the line by
  half a stroke width on each side.
- [ ] Limit line lengths of svg output to 255 as suggested in section 8.3. Path in the svg 1.1 specs.
- [x] Implement `Heatmap, Image`.
- [x] Implement `Scatter`.
- [x] Implement `Mesh`.
- [x] Implement `Surface`. In `CairoMakie` this is implemented through `Mesh`.
- [x] Implement `MeshScatter`. In `CairoMakie` this is implemented through `Mesh`.
- [ ] Optimize SVG output.
  - Limit line length to 255 chars (recommended by W3).
  - Remove superficial white spaces in commands.
  - Trim nr of significant digits to a minimum.
  - Avoid duplication of embeded images and markers.
- [ ] Make `display(fig)` work.
  Atm we have to set `Makie.ALWAYS_INLINE_PLOTS[] = false` and use
  `Makie.display(f; backend=SVGMakie)`.
- [ ] Setup `save(filename, fig)`. Atm we always write to `/tmp/svgmakie.svg`.
- [x] Fix `text!` which shows nothing.
- [ ] Optimize rastered mesh gradients, e.g. reduce the rasters size.
- [ ] Convert `mwe/mwe_pts.jl` into unit tests.
- [ ] Implement `AbstractPattern` using `<pattern>`.
- [ ] Optimize `Mesh3d`: Atm we mesh all points, but it would be sufficient if we mesh only
  all points that are visible from the current projection.
  This could be done by generating one big raster image with all zeros (transparent) pixels,
  then traverse the meshes in reverse z depth (back to front) and merge the `color_matrix`
  with the one of the current layer, where the latter's values are prioritized.
  To get straight edges we fill currently the `color_matrix` also outside the triangle region
  and latter clip it in the SVG. For this approach I think we would have to already 'clip' the
  `color_matrix` before we merge them. But I fear that this might introduce zig-zag edges again.
  To fix the latter we could try to artificially extend the 'clipped' `color_matrix` by
  just duplicating the colors at the edges so that we have a fully colored area that we can clip
  again in `SVG`. This leaves the problem of finding a non-trivial clipping path.
  All these optimizations will also effect performance/quality `Surface, MeshScatter`.

# Resources

- Cairo can render to svg: https://cairographics.org/manual/cairo-SVG-Surfaces.html
- Agg extension for their svg parser: https://github.com/dov/agg-svg
- Agg: https://github.com/ghaerr/agg-2.6
- MDN tutorial: https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial
- More info on MDN: https://developer.mozilla.org/en-US/docs/Web/SVG
- SVG(1.1) standard: https://www.w3.org/Graphics/SVG/1.1/
- SVG(2) draft: https://www.w3.org/TR/SVG/

# SVG Notes

SVG 1.1 is the current standard which will be replace by SVG 2 at some point, but the latter
is still under development.

SVG coordinates are oriented like in Cairo: x increases towards right, y increases
towards bottom.

> The globally valid rule for SVG files is, that later elements are rendered atop previous
> elements. The further down an element is the more it will be visible.

SVG extension is `.svg`. Compressed SVGs have extension `.svgz` but they are more problematic
to get rendered propely on user agents. (not sure if this is only a issue for webservers...)

Text: https://www.w3.org/Graphics/SVG/1.1/text.html
SVG text is rendered the same way as other primitives. It can be entered with
`<text>, <tspan>, <tref>` elements. SVG does not compute line breaks.
Instead either split text into multiple `<text>` elements or
fill a `<text>` elements with multiple `<tspan>` childs. This requires manual positioning
of the first character of each new line.
The `<tspan>` approach has the benefit that it allows user text selection over multiple lines.

SVG allows to embed (possibly subsets of a) font or use web fonts to ensure that SVG
renders the same in different environments.
'object bounding box' coordinates can be useful when applying filters, masks, patterns etc
to text, because they can also translate to chiildren `<tspan>` elements inside a `<text>`.

# Other Notes

- `Makie.jl`: Shouldn't each backend add its defaults to `Makie.minimal_default` by itself?
- `XML.jl`: It should implement `Base.push!(el::Element, s::String)`.
