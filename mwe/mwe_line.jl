using SVGMakie
SVGMakie.activate!()

f = Figure()
ax = Axis(f[1,1])
# lines!(ax, 1:3, 1:3)
x = LinRange(-1,1,100)
lines!(ax, x, x.^2)

display(f)
