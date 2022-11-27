using FileIO
using SVGMakie
using MeshIO
SVGMakie.activate!()

# What does Makie.ALWAYS_INLINE_PLOTS do?
Makie.ALWAYS_INLINE_PLOTS[] = false

brain = load(assetpath("brain.stl"))

f = Figure()
ax = Axis3(f[1,1])
mesh!(ax, brain,
      color = [tri[1][2] for tri in brain for i in 1:3], colormap = Reverse(:Spectral))


# Why do I have to pass on the backend?
Makie.display(f; backend=SVGMakie)
