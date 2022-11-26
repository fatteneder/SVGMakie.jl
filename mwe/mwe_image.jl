using Makie.FileIO
using Random
using SVGMakie
SVGMakie.activate!()

# What does Makie.ALWAYS_INLINE_PLOTS do?
Makie.ALWAYS_INLINE_PLOTS[] = false

img = load(Makie.assetpath("logo.png"))

Random.seed!(123)
fig = Figure(resolution = (600, 400))
ax = Axis(fig[1, 1])
img = image!(ax, 1:200, 1:200, rotr90(img))
# rotate!(img, π/3)

# Why do I have to pass on the backend?
Makie.display(fig; backend=SVGMakie)
