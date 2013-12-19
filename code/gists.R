colcode <- findColours(classIntervals(angles %% pi, 8, style="kmeans"), c("wheat", "red"))
plot(nodes, col=colcode, border='transparent')
