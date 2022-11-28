using SVGMakie
SVGMakie.activate!()

# f = Figure()
# Axis(f[1, 1])
# xs = 1:0.2:10
# ys_low = -0.2 .* sin.(xs) .- 0.25
# ys_high = 0.2 .* sin.(xs) .+ 0.25
# band!(xs, ys_low, ys_high)
# band!(xs, ys_low .- 1, ys_high .-1, color = :red)

f = Figure()
ax = Axis3(f[1, 1])
lower = fill(Point3f(0,0,0), 100)
upper = [Point3f(sin(x), cos(x), 1.0) for x in range(0,2pi, length=100)]
col = repeat([1:50;50:-1:1],outer=2)
band!(ax, lower, upper, color=col)

save("mwe_band.svg", f)
