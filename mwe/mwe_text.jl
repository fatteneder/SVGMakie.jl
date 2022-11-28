using SVGMakie
SVGMakie.activate!()

f = Figure()
ax = Axis(f[1,1])
text!(ax, Vec2f(0.1, 0.1), text="sers")

save("mwe_text.svg", f)
