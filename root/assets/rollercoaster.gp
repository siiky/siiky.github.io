set terminal svg
set output "root/assets/rollercoaster.svg"

# The number of samples to use to plot the expression.
set samples 1000

# The ranges here specify the XX and YY ranges respectively.
plot [-50:50] [-5:5] x*sin(x)*cos(x)**x title "Rollercoaster"
