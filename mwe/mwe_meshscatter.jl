using SVGMakie
SVGMakie.activate!()

xs = cos.(1:0.5:20)
ys = sin.(1:0.5:20)
zs = LinRange(0, 3, length(xs))

f = Figure()
ax = Axis3(f[1,1])
meshscatter!(ax, xs, ys, zs, markersize = 0.1, color = zs)

save("mwe_meshscatter.svg", fig)
