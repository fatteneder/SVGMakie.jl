using SVGMakie
SVGMakie.activate!()

# What does Makie.ALWAYS_INLINE_PLOTS do?
Makie.ALWAYS_INLINE_PLOTS[] = false

xs = cos.(1:0.5:20)
ys = sin.(1:0.5:20)
zs = LinRange(0, 3, length(xs))

f = Figure()
ax = Axis3(f[1,1])
meshscatter!(ax, xs, ys, zs, markersize = 0.1, color = zs)

# Why do I have to pass on the backend?
Makie.display(f; backend=SVGMakie)
