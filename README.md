# SVGMakie

# TODO
- [-] Understand SVG.
- [x] (Roughly) Figure out what needs to be implemented from Makie.
  - IMO CairoMakie is the backend that is easiest to understand. I will try to mimick this first.
  - Mimiced a project structure similar to CairoMakie.jl
- [x] Get simple plot going through Makies interface.
- [-] Implement `svg_color`.
- [ ] SVGs are strucutered xml files. It allows to add CSS sections and later refere to them
  in primitives for styling or automation. We should utilize `XML.jl` to construct the files
  and build a SVG type that is just a wrapper around an `XML.Document` object.

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

# Makie Notes

- Shouldn't each backend add its defaults to `Makie.minimal_default` by itself?
