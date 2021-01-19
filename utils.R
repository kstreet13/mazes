
# thin-walled mazes

# where did it come from?
# 0 = origin, 1 = down, 2 = left, 3 = up, 4 = right

adjacent <- function(coords, maze, return.coords = FALSE){
    out.coords <- NULL
    val <- rep(NA, 4)
    up <- coords + c(1,0)
    if(up[1] <= nrow(maze)){
        out.coords <- rbind(out.coords, up)
        val[3] <- maze[up[1],up[2]]
    }
    dn <- coords + c(-1,0)
    if(dn[1] > 0){
        out.coords <- rbind(out.coords, dn)        
        val[1] <- maze[dn[1],dn[2]]
    }
    lf <- coords + c(0,-1)
    if(lf[2] > 0){
        out.coords <- rbind(out.coords, lf)
        val[2] <- maze[lf[1],lf[2]]
    }
    rt <- coords + c(0,1)
    if(rt[2] <= ncol(maze)){
        out.coords <- rbind(out.coords, rt)
        val[4] <- maze[rt[1],rt[2]]
    }
    if(return.coords){
        return(out.coords)
    }
    return(val)
}
previous <- function(coords, maze){
    dir <- maze[coords[1],coords[2]]
    if(dir == 0){
        return(c(NA, NA))
    }
    # reverse the move that was taken to get here
    prev <- switch(dir,
                   '1' = coords + c(1,0),
                   '2' = coords + c(0,1),
                   '3' = coords + c(-1,0),
                   '4' = coords + c(0,-1))
    return(prev)
}
fill_maze <- function(maze, start = NULL){
    if(is.null(start)){
        # pick a not-exactly-random start
        s1 <- sample((1:nrow(maze))[rowSums(maze==-1) > 0], 1)
        s2 <- sample(which(maze[s1,]==-1), 1)
        start <- c(s1,s2)
    }
    maze[start[1],start[2]] <- 0
    last <- start
    while(sum(maze == -1) > 0){
        # select next space from neighbors of 'last'
        adj <- adjacent(last, maze)
        # if no valid options, back up one step and try again
        poss <- which(adj == -1)
        if(length(poss) == 0){
            curr <- previous(last, maze)
        }else{
            dir <- poss[sample(length(poss), 1)] # coding around sample's "convenience" feature
            curr <- switch(dir,
                           '1' = last + c(-1,0), # technically, the names are 
                           '2' = last + c(0,-1), # unnecessary. It's picking
                           '3' = last + c(1,0),  # the correct case based on 
                           '4' = last + c(0,1))  # position, not name
            maze[curr[1],curr[2]] <- dir
        }
        last <- curr
    }
    return(maze)
}

draw1 <- function(maze, ...){
    plot(c(.5,ncol(maze)+.5),c(.5,nrow(maze)+.5), col='white', axes=FALSE,
         xlab = '', ylab = '', asp=1)
    # abline(v = seq(0.5, ncol(maze)+.5, by = 1))
    # abline(h = seq(0.5, nrow(maze)+.5, by = 1))
    rect(.5, .5, ncol(maze)+.5, nrow(maze)+.5, border = 1, lwd =2)
    for(i in 2:nrow(maze)){
        lines(c(.5,ncol(maze)+.5), c(i-.5,i-.5), lwd =2)
    }
    for(j in 2:ncol(maze)){
        lines(c(j-.5,j-.5), c(.5,nrow(maze)+.5), lwd =2)
    }
    for(i in 1:nrow(maze)){
        for(j in 1:ncol(maze)){
            if(maze[i,j] != 0){
                dir <- maze[i,j]
                # reverse the move that was taken to get here
                prev <- switch(dir,
                               '1' = c(i,j) + c(1,0),
                               '2' = c(i,j) + c(0,1),
                               '3' = c(i,j) + c(-1,0),
                               '4' = c(i,j) + c(0,-1))
                center <- (c(i,j) + prev) / 2
                rect(center[2]-.47, center[1]-.47,
                     center[2]+.47, center[1]+.47,
                     col = 'white', border = NA)
            }
        }
    }
}
draw2 <- function(maze, ...){
    plot(c(1,ncol(maze)),c(1,nrow(maze)), col='white', axes=FALSE,
         xlab = '', ylab = '', ...)
    #abline(v = seq(0.5, ncol(maze)+.5, by = 1))
    #abline(h = seq(0.5, nrow(maze)+.5, by = 1))
    
    for(i in 1:nrow(maze)){
        for(j in 1:ncol(maze)){
            if(maze[i,j] != 0){
                dir <- maze[i,j]
                # reverse the move that was taken to get here
                prev <- switch(dir,
                               '1' = c(i,j) + c(1,0),
                               '2' = c(i,j) + c(0,1),
                               '3' = c(i,j) + c(-1,0),
                               '4' = c(i,j) + c(0,-1))
                lines(c(j,prev[2])-.4, c(i,prev[1])+.4, col='turquoise', lwd=10)
            }
        }
    }
}
mlines <- function(maze, xadj = 0, yadj = 0, lwd = 10, ...){
    for(i in 1:nrow(maze)){
        for(j in 1:ncol(maze)){
            if(maze[i,j] != 0){
                dir <- maze[i,j]
                # reverse the move that was taken to get here
                prev <- switch(dir,
                               '1' = c(i,j) + c(1,0),
                               '2' = c(i,j) + c(0,1),
                               '3' = c(i,j) + c(-1,0),
                               '4' = c(i,j) + c(0,-1))
                lines(c(j,prev[2])+xadj, c(i,prev[1])+yadj, lwd = lwd, ...)
            }
        }
    }
    
}


# convert to think-walled

toThick <- function(m){
    m2 <- matrix(NA, 2*nrow(m)+1, 2*ncol(m)+1)
    m2[,1] <- m2[,ncol(m2)] <- -5
    m2[1,] <- m2[nrow(m2),] <- -5
    for(i in 1:nrow(m)){
        for(j in 1:ncol(m)){
            dir <- m[i,j]
            m2[2*i, 2*j] <- dir
            if(dir != 0){
                # reverse the move that was taken to get here
                prev <- switch(dir,
                               '1' = 2*c(i,j) + c(1,0),
                               '2' = 2*c(i,j) + c(0,1),
                               '3' = 2*c(i,j) + c(-1,0),
                               '4' = 2*c(i,j) + c(0,-1))
                m2[prev[1], prev[2]] <- dir
            }
        }
    }
    m2[is.na(m2)] <- -5
    m2[m2 >= 0] <- 1
    return(m2)
}


plotThick <- function(m, lwd = 10, ...){
    plot(c(0,ncol(m)+1),c(0,nrow(m)+1), col='white', axes=FALSE, xlab = '', ylab = '', asp=1)
    mcopy <- m
    for(i in 1:nrow(m)){
        for(j in 1:ncol(m)){
            if(m[i,j] == -5){
                adj <- adjacent(c(i,j), mcopy)
                draw <- which(adj == -5)
                for(dir in draw){
                    trgt <- switch(dir,
                                   '1' = c(i,j) + c(-1,0), # technically, the names are 
                                   '2' = c(i,j) + c(0,-1), # unnecessary. It's picking
                                   '3' = c(i,j) + c(1,0),  # the correct case based on 
                                   '4' = c(i,j) + c(0,1))  # position, not name
                    lines(c(j,trgt[2]), c(i,trgt[1]), lwd=lwd, ...)
                }
                mcopy[i,j] <- 0
            }
        }
    }
}

elines <- function(m, xadj = 0, yadj = 0, lwd = 10, ...){
    mcopy <- m
    for(i in 1:nrow(m)){
        for(j in 1:ncol(m)){
            if(m[i,j] == -5){
                adj <- adjacent(c(i,j), mcopy)
                draw <- which(adj == -5)
                for(dir in draw){
                    trgt <- switch(dir,
                                   '1' = c(i,j) + c(-1,0), # technically, the names are 
                                   '2' = c(i,j) + c(0,-1), # unnecessary. It's picking
                                   '3' = c(i,j) + c(1,0),  # the correct case based on 
                                   '4' = c(i,j) + c(0,1))  # position, not name
                    lines(c(j,trgt[2])+xadj, c(i,trgt[1])+yadj, lwd = lwd, ...)
                }
                mcopy[i,j] <- 0
            }
        }
    }
}


