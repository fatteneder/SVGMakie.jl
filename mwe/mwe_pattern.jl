using FileIO
using SVGMakie
SVGMakie.activate!()

f = Figure()
ax = Axis(f[1, 1])

pattern = Makie.LinePattern(tilesize=(100,100))
img = load(Makie.assetpath("doge.png"))[1:2:end,1:2:end]
img_pattern = Pattern(rotr90(img))
p = poly!(ax, Point2f[(0, 0), (2, 0), (3, 1), (1, 1)],
      # color = :black,
      # color = pattern,
      color = img_pattern,
      strokecolor = :black,
      strokewidth = 1)

save("mwe_pattern.svg", f)
