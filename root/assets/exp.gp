# The output "format".
set terminal svg

# The output file.
set output "root/assets/exp.svg"

# Enable gridlines.
set grid

# Where to place the lines/points/&c legend.
set key right bottom

# Legend of the XX/YY axes.
set xlabel "The passage of time..."
set ylabel "Shittiness of the web"

# Mirror or not the axes' tics -- notice the YY axis has tics on both the left
#   and right, but the XX axis has only on the bottom, not the top.
set ytics mirror
set xtics nomirror

# Use a logscale of base 7 for the XX axis -- the base is optional and defaults
#   to 10 I think.
set logscale y 7

# The actual plot: `exp(x)` is the expression to plot; `x` is "special" --
#   there are a few different variables you can use, but they seem to depend on
#   the available axes/dimensions, but I don't know details of this so RTFM.
#
# `title "..."` sets this line's legend.
plot exp(x) title "Super straight line"
