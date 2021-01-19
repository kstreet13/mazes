
# some interesting looks
source('utils.R')


m <- matrix(-1, 8, 24)
m <- fill_maze(m)

plot(c(-1,ncol(m)),c(-1,nrow(m)+2), col='white', axes=FALSE,
     xlab = '', ylab = '', asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  "grey70")
for(i in 1:7){
    mlines(m, xadj = -.1*(i-1), yadj = .1*(i-1), col = brewer.pal(9,'YlOrRd')[10-i],
           lwd = 15-i)
}


m <- matrix(-1, 9, 9)
m <- fill_maze(m)

plot(c(-2,ncol(m)),c(-2,nrow(m)+2), col='white', axes=FALSE,
     xlab = '', ylab = '', asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  "grey95")
mlines(m, lwd=4)
mlines(m, xadj = -.2, yadj = .2, col = 'pink')
mlines(m, xadj = -.4, yadj = .4, col = 'turquoise')



m <- matrix(-1, 10, 12)
m[2:10,1] <- -5
m[1:9,12] <- -5
m <- fill_maze(m)

plot(c(-1,ncol(m)),c(-1,nrow(m)+2), col='white', axes=FALSE,
     xlab = '', ylab = '', asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  "grey96")
mlines(m)
points(c(1,12), c(1,10), col = 3:2)

plot(c(-1,ncol(m)),c(-1,nrow(m)+2), col='white', axes=FALSE,
     xlab = '', ylab = '', asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  "grey96")
mlines(m)
points(c(.5,12.5), c(1,10), cex = 3)
points(c(.5,12.5), c(1,10), col = 3:2)




m <- matrix(-1, 30, 30)
for(i in 1:nrow(m)){
    for(j in 1:ncol(m)){
        if((i-15.5)^2+(j-15.5)^2 > 220){
            m[i,j] <- -5
        }
    }
}
m <- fill_maze(m)



plot(c(-2,ncol(m)),c(-2,nrow(m)+2), col='white', axes=FALSE,
     xlab = '', ylab = '', asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = alpha('darkblue',alpha=.2))
polygon(15.5 + 16*cos(seq(0,2*pi, length.out = 1000)),
        15.5 + 16*sin(seq(0,2*pi, length.out = 1000)), col='grey2')
mlines(m, col='orange')







m <- matrix(-1, 90, 150)
m <- fill_maze(m)

plot(c(-1,ncol(m)),c(-1,nrow(m)+2), col='white', axes=FALSE,
     xlab = '', ylab = '', asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  "grey96")
mlines(m, lwd=4)






