using FileIO
using SVGMakie
using MeshIO
SVGMakie.activate!()

brain = load(assetpath("brain.stl"))

f = Figure()
ax = Axis3(f[1,1])
mesh!(ax, brain,
      color = [tri[1][2] for tri in brain for i in 1:3], colormap = Reverse(:Spectral))

save("mwe_mesh3d.svg", fig)
