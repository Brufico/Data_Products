# googlevis1_1
# ============
# 

suppressPackageStartupMessages(library(googleVis))

# Fruits
# ======

M <- gvisMotionChart(Fruits, "Fruit", "Year",
                     options=list(width=600, height=400))
plot(M)

print(M,"chart")


# Maps
# ===

G2 <- gvisGeoChart(Exports, locationvar="Country",
                   colorvar="Profit",options=list(width=600, height=400))
plot(G2)


G2 <- gvisGeoChart(Exports, locationvar="Country",
                   colorvar="Profit",options=list(width=1000, height=700, region="150"))
plot(G2)




# combinations
# ============

G <- gvisGeoChart(Exports, "Country", "Profit",options=list(width=200, height=100))
T1 <- gvisTable(Exports,options=list(width=200, height=270))
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(width=400, height=370))

GT <- gvisMerge(G,T1, horizontal=FALSE)
GTM <- gvisMerge(GT, M, horizontal=TRUE,tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")

plot(GTM)
