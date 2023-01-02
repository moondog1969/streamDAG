### R code from vignette source 'streamDAG_examples'

###################################################
### code chunk number 1: closeConnetions
###################################################
allCon <- showConnections()
socketCon <- as.integer(rownames(allCon)[allCon[, "class"] == "sockconn"])
sapply(socketCon, function(ii) close.connection(getConnection(ii)) )


