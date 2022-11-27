using SVGMakie
SVGMakie.activate!()

# What does Makie.ALWAYS_INLINE_PLOTS do?
Makie.ALWAYS_INLINE_PLOTS[] = false

f = Figure()
ax = Axis(f[1,1])
text!(ax, Vec2f(0.1, 0.1), text="sers")

# Why do I have to pass on the backend?
Makie.display(f; backend=SVGMakie)
