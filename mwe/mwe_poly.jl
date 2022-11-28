using SVGMakie
using Makie.GeometryBasics
SVGMakie.activate!()

f = Figure()
ax = Axis(f[1, 1])

# ps = Point2f[(0,0), (2,0), (3,1), (1,1)]
# poly!(ax, ps, color = :red, strokecolor = :black, strokewidth = 1)
# p = Polygon(ps)
# poly!(ax, p)
# poly!(ax, [p])

r = Rect2f(1, 1, 3, 3)
# poly!(ax, r)
poly!(ax, [r])

save("mwe_poly.svg", f)
