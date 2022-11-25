using SVGMakie
using Makie.FileIO
SVGMakie.activate!()

# What does Makie.ALWAYS_INLINE_PLOTS do?
Makie.ALWAYS_INLINE_PLOTS[] = false

batsymbol_string = "M96.84 141.998c-4.947-23.457-20.359-32.211-25.862-13.887-11.822-22.963-37.961-16.135-22.041 6.289-3.005-1.295-5.872-2.682-8.538-4.191-8.646-5.318-15.259-11.314-19.774-17.586-3.237-5.07-4.994-10.541-4.994-16.229 0-19.774 21.115-36.758 50.861-43.694.446-.078.909-.154 1.372-.231-22.657 30.039 9.386 50.985 15.258 24.645l2.528-24.367 5.086 6.52H103.205l5.07-6.52 2.543 24.367c5.842 26.278 37.746 5.502 15.414-24.429 29.777 6.951 50.891 23.936 50.891 43.709 0 15.136-12.406 28.651-31.609 37.267 14.842-21.822-10.867-28.266-22.549-5.549-5.502-18.325-21.147-9.341-26.125 13.886z"
batsymbol = BezierPath(batsymbol_string, fit = true, flipy = true)

circle_with_hole = BezierPath([
    MoveTo(Point(1, 0)),
    # This is missing a parameter.
    EllipticalArc(Point(0, 0), 1, 1, 0, 0, 2pi),
    MoveTo(Point(0.5, 0.5)),
    LineTo(Point(0.5, -0.5)),
    LineTo(Point(-0.5, -0.5)),
    LineTo(Point(-0.5, 0.5)),
    LineTo(Point(0.5, 0.5)),
    ClosePath(),
])

# img = load(Makie.assetpath("doge.png"))

f = Figure()
ax = Axis(f[1,1])
x = LinRange(-1,1,100)
s = scatterlines!(ax, x, x.^2, 
         # marker='x',
         # marker=:rect,
         # strokecolor=:grey,
         color=:limegreen,
         strokecolor=:black,
         strokewidth=1,
         # marker=batsymbol,
         marker=img,
         # marker=circle_with_hole,
         # marker=:rect,
         # marker=Circle,
         # marker=Rect,
         # markersize=15 .* (1.0, 0.5))
         markersize=5)
         # markersize=35)


# Why do I have to pass on the backend?
Makie.display(f; backend=SVGMakie)
