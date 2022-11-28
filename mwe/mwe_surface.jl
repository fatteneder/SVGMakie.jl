using SVGMakie
SVGMakie.activate!()

xs = LinRange(0, 10, 100)
ys = LinRange(0, 15, 100)
zs = [cos(x) * sin(y) for x in xs, y in ys]

f = Figure()
ax = Axis3(f[1,1])
surface!(ax, xs, ys, zs)

display(f)
