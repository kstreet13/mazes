source('utils.R')

m <- matrix(-1, 8, 24)
m <- fill_maze(m)
m <- toThick(m)
m[2,1] <- m[nrow(m)-1, ncol(m)] <- 1


plot(c(-1,ncol(m)),c(-1,nrow(m)+2), col='white', axes=FALSE,
     xlab = '', ylab = '', asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  "grey60")
for(i in 1:7){
    elines(m, xadj = -.1*(i-1), yadj = .1*(i-1), col = brewer.pal(9,'YlOrRd')[10-i],
           lwd = 15-i)
}


# maze-ception
m <- matrix(-1, 10, 10)
m <- fill_maze(m)
m <- toThick(m)

expand <- function(m){
    m2 <- matrix(NA, nrow = 2*nrow(m), ncol = 2*ncol(m))
    for(i in 1:ncol(m)){
        m2[,2*i-1] <- m2[,2*i] <- rep(m[,i], each = 2)
    }
    return(m2)
}
seep <- function(m, what = 1){
    m2 <- m
    for(i in 2:nrow(m)){
        m2[i, ][m[i-1, ]==what] <- what
    }
    for(i in 1:(nrow(m)-1)){
        m2[i, ][m[i+1, ]==what] <- what
    }
    for(j in 2:ncol(m)){
        m2[,j][m[,j-1]==what] <- what
    }
    for(j in 1:(ncol(m)-1)){
        m2[,j][m[,j+1]==what] <- what
    }
    return(m2)
}

m <- seep(seep(seep(expand(expand(expand(m))))))
m[m==1] <- -1
m <- fill_maze(m)


# plot(c(-1,ncol(m)),c(-1,nrow(m)+2), col='white', axes=FALSE,
#      xlab = '', ylab = '', asp=1)
# rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  "grey60")
# mlines(m, lwd = 1)


m <- toThick(m)
m[16, 1:13] <- 1
m[322, (ncol(m)-12):ncol(m)] <- 1


plot(c(-1,ncol(m)),c(-1,nrow(m)+2), col='white', axes=FALSE,
     xlab = '', ylab = '', asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  "grey80")
elines(m, lwd = 2, lend=2)





