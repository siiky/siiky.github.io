$Dataset << EOD
NELEMS	RTIME-MEAN	RTIME-MIN	RTIME-MAX	TOTCYC-MEAN	TOTCYC-MIN	TOTCYC-MAX	TOTINS-MEAN	TOTINS-MIN	TOTINS-MAX	L1DCM-MEAN	L1DCM-MIN	L1DCM-MAX	L2DCM-MEAN	L2DCM-MIN	L2DCM-MAX
100	51.5	51	52	153107.0	151836	154378	49774.5	42768	56781	912.5	849	976	557.0	528	586
1000	386.4	373	396	1117484.4	1076454	1145864	652026.8	634123	680188	5681.0	5118	5990	1660.4	1300	1793
10000	29215.8	29061	29364	21425394.0	10119054	26280869	15941285.8	6979549	22139082	619685.8	169351	1140152	298639.6	17765	644415
100000	2852669.8	2845788	2859074	708231510.6	119757602	1080138950	1145341284.8	184498435	1753956316	93553900.2	13537085	144424582	72470004.8	2110199	120731566
200000	11472829.2	11426994	11502233	922765811.8	181797852	3298279336	1480628498.6	268678225	5359163617	120437869.6	19420767	444041994	87885551.2	2726575	385487784
300000	25821154.8	25729787	25883755	7291510319.0	393353142	9962527951	11904840316.2	594020363	16257913776	988685146.8	45337229	1351805549	907188629.2	12260881	1258022140
400000	45910144.6	45833530	46047114	680904937.0	670501982	689922089	1033095368.6	1029084389	1038083524	80272464.4	80011372	80610671	30805397.6	29580829	32181760
500000	71859779.2	71703444	72007099	11187695199.8	1031362427	25645713435	18329461121.4	1586757574	42172405522	1521273469.8	125041982	3509799991	1394315157.4	56848580	3340745801
600000	103237386.5	103196178	103278595	1492849041.0	1484306515	1501391567	2238479602.0	2234440484	2242518720	177991124.0	177535409	178446839	93661585.0	93119294	94203876
EOD

set terminal svg
set output "assets/errorlines.svg"

# Tell Gnuplot that fields are separated by a tab, as briefly mentioned before.
set datafile separator tab

set title "Some shitty performance right here..."
set key left top

# Ask Gnuplot to use log scales for the XX, YY, and YY2 (right side) axes.
set logscale xyy2 10

set xtics nomirror
set ytics nomirror
set y2tics nomirror

set xlabel "#Elements"
set ylabel "Time (s)"
set y2label "L1 Cache Misses"

set grid

# The `($n/1000000)` syntax asks Gnuplot to divide the values of the field `n`
#   by 1000000 (in this case, the time is in microseconds, so dividing by
#   1000000 converts to secconds).
#
# `with yerrorlines` changes the style of plot, in this case lines with error
#   bars. `yerrorbars` is the same but without the connecting lines.
# Other common styles are `points` (the default?), `lines`, & `linespoints`.
# RTFM for more: `help with`.
#
# The `yerrorlines` style requires additional values. There are some different
#   alternatives (RTFM), but in this case the columns are x:y:ymin:ymax. In
#   this dataset I've used the mean for the YY, but you may use whatever you
#   wish.
#
# `title columnheader` asks Gnuplot to automatically read the given line's
#   legend from the input dataset. Note that Gnuplot supports some LaTeX-like
#   formatting syntax for text. E.g., the text "RTIME_MEAN" would be rendered
#   as "RTIMEMEAN" with the the "M" of "MEAN" in subscript.
#
# Finally, `axis x1y1` & `axis x1y2` set the axes the data should be plotted in
#   -- x1 & x2 for bottom & top XX respectively; y1 & y2 for left & right YY
#   respectively.
plot $Dataset using 1:($2/1000000):($3/1000000):($4/1000000) with yerrorlines title columnheader axis x1y1,\
     ""       using 1:11:12:13                               with yerrorlines title columnheader axis x1y2
