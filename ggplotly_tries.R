devtools::install_github('hadley/ggplot2')

library(maps)

data("world.cities")

head(world.cities)

cp <- with(world.cities,
           world.cities[capital==1,] )

cp <- cp[order(cp$pop, decreasing = TRUE),]
cp$rank <- 1:nrow(cp)
cp$lpop <- log10(cp$pop)


begin <- 1
end <- 81

firstcap <- cp[begin:end, ]
fit <- lm(lpop ~ rank, data = firstcap)
cf <- coef(fit)

mcoef <- 10 ^ cf[2]

gpop <- ggplot(firstcap,aes(rank, lpop, color = name)) +
        geom_point() +
        geom_smooth(method = "lm", mapping = aes(group=1), se = FALSE) +
        theme(legend.position = "none") +
        labs(title = "Population of the world's largest cities",
             y = "log10(population)")
ggpoply <- ggplotly(gpop)
ggpoply


# verification of zipf's law

verif <- function(rank1, rank2, dataf = firstcap) {
        n1 <- firstcap[rank1, "name"]
        p1 <- firstcap[rank1, "pop"]
        n2 <- firstcap[rank2, "name"]
        p2 <- firstcap[rank2, "pop"]
        predp2 <- p1*mcoef ^ (rank2 - rank1)

        sprintf("Reference: %s, pop = %i ; Predicted: %s, predicted pop: %i,  real pop = %i",
                n1, p1, n2, round(predp2,0), p2)

}

verif(5,20)
