using SVGMakie
SVGMakie.activate!()

# What does Makie.ALWAYS_INLINE_PLOTS do?
Makie.ALWAYS_INLINE_PLOTS[] = false

f = Figure()
ax = Axis(f[1,1])
# lines!(ax, 1:3, 1:3)
x = LinRange(-1,1,100)
lines!(ax, x, x.^2)


# Why do I have to pass on the backend?
Makie.display(f; backend=SVGMakie)
