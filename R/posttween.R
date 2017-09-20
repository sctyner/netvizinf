#' A function to merge a data frame of \code{tween}'d node placements with their corresponding edgelists.
#'
#' @param tweened \code{data.frame} Node placements that have been created using \code{\link[tweenr]{tween_states}}
#' @param pte \code{data.frame} \code{pte} stands for pretween'd edges. Output from \code{\link{pretween_edges()}}.
#' @param ecolor \code{character} Length 2 vector of edge colors. First color should be the desired color for all edges, while the second is the emphasized edge color.
#' @param vcolor \code{character} Length 2 vector of node colors. First color should be the desired color for all nodes, while the second is the emphasized ego node color.
#'
#'
#' @export
#'
posttween <- function(tweened, pte, ecolor, vcolor){
 #  msids <- unique(pte$microstep)
 #  addededges <- pte[which(pte$addedge), c("from", "to", "microstep")]
 #  removededges <- pte$microstep[which(pte$rmvedge)]
 #
 #  # first, split according to microstep.
 #  listTweened <- split(tweened, as.factor(floor(tweened$ms)))
 #  listpte <- split(pte, as.factor(pte$microstep))
 #  listPostTween <- list()
 #  # then, join edges
 #  for (i in 1:length(listTweened)){
 #    if(sum(listTweened[[i]]$addedge) == 1) {
 #      step1 <- merge(listpte[[i]], listTweened[[i]],
 #                     by.x = c("from", "microstep", "addedge", "rmvedge"),
 #                     by.y = c("id", "ms", "addedge", "rmvedge"), all = TRUE)
 #      names(step1)[names(step1) %in% c("x", "y")] <- c("x.from", "y.from")
 #      step2 <- merge(step1, listTweened[[i]],
 #                     by.x = c("to", "microstep", "addedge", "rmvedge",".frame"),
 #                     by.y = c("id", "ms","addedge", "rmvedge",".frame"),
 #                     all.x = TRUE)
 #      names(step2)[names(step2) %in% c("x", "y")]  <- c("x.to", "y.to")
 #      listPostTween[[i]] <- merge
 #    }
 #
 #  }
 #  # merge
 #  step1 <- merge(pte, tweened, by.x = c("from", "microstep", "addedge", "rmvedge"), by.y = c("id", "ms", "addedge", "rmvedge"), all = TRUE)
 #  names(step1)[names(step1) %in% c("x", "y")] <- c("x.from", "y.from")
 #  step2 <- merge(step1, tweened, by.x = c("to", "microstep", "addedge", "rmvedge",".frame"), by.y = c("id", "ms","addedge", "rmvedge",".frame"), all.x = TRUE)
 #  names(step2)[names(step2) %in% c("x", "y")]  <- c("x.to", "y.to")
 #
 #  # add coloring variables
 #  for (i in msids){
 #    if (i %in% addededges){
 #      lowidx <- which(step2$microstep == (i-1) & step2$from == addededges$from[addededges$microstep==i] & step2$to == addededges$to[addededges$microstep==i])
 #      mididx <- which(dplyr:::between(step2$microstep, i-1,i) & step2$from == addededges$from[addededges$microstep==i] & step2$to == addededges$to[addededges$microstep[i]])
 #      highidx <- which(step2$microstep == i & step2$from == addededges$from & step2$to == addededges$to)
 #      step2$.color[] <- ecolor[1]
 #      step2$.color <- tween_color(data = rev(ecolor), n = length(idxforcolor), ease = 'quintic-in')
 #      step2$.alpha <- tween_numeric(data = c(0,1), n = length(idxforcolor), ease = 'quintic-in')
 #    } else if (i %in% removededges) {
 #      lowidx <- which(step2$microstep == i)
 #      mididx <- which(dplyr:::between(step2$microstep, i,i+1))
 #      highidx <- which(step2$microstep == i+1)
 #      idxforcolor <- c(lowidx, mididx, highidx)
 #      step2$.color <- tween_color(data = rev(ecolor), n = length(idxforcolor), ease = 'quintic-out')
 #      step2$.alpha <- tween_numeric(data = c(1,0), n = length(idxforcolor), ease = 'quintic-in')
 #    }
 #  }
 # tween_numeric(data = c(0,1), n = grpn, ease = 'quintic-out'),1))
}
