using SVGMakie
SVGMakie.activate!()

# What does Makie.ALWAYS_INLINE_PLOTS do?
Makie.ALWAYS_INLINE_PLOTS[] = false

vertices = [
            0.0 0.0;
            1.0 0.0;
            1.0 1.0;
            0.0 1.0;
           ]

faces = [
         1 2 3;
         3 4 1;
        ]

colors = [:red, :green, :blue, :orange]

f = Figure()
ax = Axis(f[1,1])
mesh!(ax, vertices, faces, color = colors, shading = false)

# Why do I have to pass on the backend?
Makie.display(f; backend=SVGMakie)
