using SVGMakie
SVGMakie.activate!()
import SVGMakie.GeometryBasics: Mat2f

fig = Figure(resolution = (600, 400))
ax = Axis(fig[1, 1])

alpha = 0
rot = Mat2f(cos(alpha), sin(alpha), -sin(alpha), cos(alpha))
o = Vec2f(10, 10)

p1 = rot * Vec2f(0,0) .+ o
p3 = rot * Vec2f(1,0) .+ o
p2 = rot * Vec2f(0,1) .+ o
# p1 = rot * Vec2f(0,0)
# p2 = rot * Vec2f(1,0.5)
# p3 = rot * Vec2f(0,1)
# p1 = rot * Vec2f(0,0)
# p2 = rot * Vec2f(1,2)
# p3 = rot * Vec2f(0,1)
lines!(ax, [p1[1], p2[1]], [p1[2], p2[2]])
lines!(ax, [p1[1], p3[1]], [p1[2], p3[2]])
lines!(ax, [p2[1], p3[1]], [p2[2], p3[2]])

s1 = rot * Vec2f(0.2, 0.3) .+ o
s2 = rot * Vec2f(0.6, 0.5) .+ o
s3 = rot * Vec2f(-0.2, 0.3) .+ o
s4 = rot * Vec2f(0.6, -0.5) .+ o
s = [ s1, s2, s3, s4 ]
scatter!(ax, s)

isinside(s) = SVGMakie.is_inside_triangle(p1, p2, p3, s)

display(isinside.(s))

# text!(ax, s1, text=string(isinside(s1)))
# text!(ax, s2, text=string(isinside(s2)))
# display(SVGMakie.is_inside_triangle(p1, p2, p3, s))
# display(SVGMakie.is_inside_triangle(p1, p2, p3, s))

save("mwe_pts.svg", f)
