#' @export
mclapply2 <- function (X, FUN, ..., mc.preschedule = TRUE,
                      mc.set.seed = TRUE, mc.silent = FALSE,
                      mc.cores = getOption("mc.cores", 2L), 
                      mc.cleanup = TRUE, mc.allow.recursive = TRUE,
                      affinity.list = NULL, stop.on.error=TRUE){

	require("parallel")

    stop.on.error <- stop.on.error[1]        #!!
    stopifnot(is.logical(stop.on.error))     #!!
    cores <- as.integer(mc.cores)
    if ((is.na(cores) || cores < 1L) && is.null(affinity.list)) 
        stop("'mc.cores' must be >= 1")
    parallel:::.check_ncores(cores)
    if (parallel:::isChild() && !isTRUE(mc.allow.recursive)) 
        return(lapply(X = X, FUN = FUN, ...))
    if (!is.vector(X) || is.object(X)) 
        X <- as.list(X)
    if (!is.null(affinity.list) && length(affinity.list) < length(X)) 
        stop("affinity.list and X must have the same length")
    if (mc.set.seed) 
        mc.reset.stream()
    if (length(X) < 2) {
        old.aff <- mcaffinity()
        mcaffinity(affinity.list[[1]])
        res <- lapply(X = X, FUN = FUN, ...)
        mcaffinity(old.aff)
        return(res)
    }
    if (length(X) < cores) 
        cores <- length(X)
    if (cores < 2L && is.null(affinity.list)) 
        return(lapply(X = X, FUN = FUN, ...))
    jobs <- list()
    parallel:::prepareCleanup()
    on.exit(parallel:::cleanup(mc.cleanup))
    if (!mc.preschedule) {
        FUN <- match.fun(FUN)
        if (length(X) <= cores && is.null(affinity.list)) {
            jobs <- lapply(seq_along(X), function(i) mcparallel(FUN(X[[i]], 
                ...), name = names(X)[i], mc.set.seed = mc.set.seed, 
                silent = mc.silent))
            res <- mccollect(jobs)
            if (length(res) == length(X)) 
                names(res) <- names(X)
            has.errors <- sum(sapply(res, inherits, "try-error"))
        }
        else {
            sx <- seq_along(X)
            res <- vector("list", length(sx))
            names(res) <- names(X)
            fin <- rep(FALSE, length(X))
            if (!is.null(affinity.list)) {
                cores <- max(unlist(x = affinity.list, recursive = TRUE))
                d0 <- logical(cores)
                cpu.map <- lapply(sx, function(i) {
                  data <- d0
                  data[as.vector(affinity.list[[i]])] <- TRUE
                  data
                })
                ava <- do.call(rbind, cpu.map)
            }
            else {
                ava <- matrix(TRUE, nrow = length(X), ncol = cores)
            }
            jobid <- integer(cores)
            for (i in 1:cores) {
                jobid[i] <- match(TRUE, ava[, i])
                ava[jobid[i], ] <- FALSE
            }
            if (anyNA(jobid)) {
                unused <- which(is.na(jobid))
                jobid <- jobid[-unused]
                ava <- ava[, -unused, drop = FALSE]
            }
            jobs <- lapply(jobid, function(i) mcparallel(FUN(X[[i]], 
                ...), mc.set.seed = mc.set.seed, silent = mc.silent, 
                mc.affinity = affinity.list[[i]]))
            jobsp <- parallel:::processID(jobs)
            has.errors <- 0L
            delivered.result <- 0L
            while (!all(fin)) {
                s <- parallel:::selectChildren(jobs[!is.na(jobsp)], -1)
                if (is.null(s)) 
                  break
                if (is.integer(s)) 
                  for (ch in s) {
                    ji <- match(TRUE, jobsp == ch)
                    ci <- jobid[ji]
                    r <- parallel:::readChild(ch)
                    if (is.raw(r)) {
                      child.res <- unserialize(r)
                      if (inherits(child.res, "try-error")){
                          if(stop.on.error)                     #!!
                              stop("error in process X = ", ci, "\n", attr(child.res, "condition")$message) #!!
                          has.errors <- has.errors + 1L
                      }
                      if (!is.null(child.res)) 
                        res[[ci]] <- child.res
                      delivered.result <- delivered.result + 
                        1L
                    }
                    else {
                      fin[ci] <- TRUE
                      jobsp[ji] <- jobid[ji] <- NA
                      if (any(ava)) {
                        nexti <- which.max(ava[, ji])
                        if (!is.na(nexti)) {
                          jobid[ji] <- nexti
                          jobs[[ji]] <- mcparallel(FUN(X[[nexti]], 
                            ...), mc.set.seed = mc.set.seed, 
                            silent = mc.silent, mc.affinity = affinity.list[[nexti]])
                          jobsp[ji] <- parallel:::processID(jobs[[ji]])
                          ava[nexti, ] <- FALSE
                        }
                      }
                    }
                  }
            }
            nores <- length(X) - delivered.result
            if (nores > 0) 
                warning(sprintf(ngettext(nores, "%d parallel function call did not deliver a result", 
                  "%d parallel function calls did not deliver results"), 
                  nores), domain = NA)
        }
        if (has.errors) 
            warning(gettextf("%d function calls resulted in an error", 
                has.errors), domain = NA)
        return(res)
    }
    if (!is.null(affinity.list)) 
        warning("'mc.preschedule' must be false if 'affinity.list' is used")
    sindex <- lapply(seq_len(cores), function(i) seq(i, length(X), 
        by = cores))
    schedule <- lapply(seq_len(cores), function(i) X[seq(i, length(X), 
        by = cores)])
    ch <- list()
    res <- vector("list", length(X))
    names(res) <- names(X)
    cp <- rep(0L, cores)
    fin <- rep(FALSE, cores)
    dr <- rep(FALSE, cores)
    inner.do <- function(core) {
        S <- schedule[[core]]
        f <- parallel:::mcfork()
        if (isTRUE(mc.set.seed)) 
            parallel:::mc.advance.stream()
        if (inherits(f, "masterProcess")) {
            on.exit(mcexit(1L, structure("fatal error in wrapper code", 
                class = "try-error")))
            if (isTRUE(mc.set.seed)) 
                parallel:::mc.set.stream()
            if (isTRUE(mc.silent)) 
                closeStdout(TRUE)
            parallel:::sendMaster(try(lapply(X = S, FUN = FUN, ...), silent = TRUE))
            parallel:::mcexit(0L)
        }
        jobs[[core]] <<- ch[[core]] <<- f
        cp[core] <<- parallel:::processID(f)
        NULL
    }
    job.res <- lapply(seq_len(cores), inner.do)
    ac <- cp[cp > 0]
    has.errors <- integer(0)
    while (!all(fin)) {
        s <- parallel:::selectChildren(ac[!fin], -1)
        if (is.null(s)) 
            break
        if (is.integer(s)) 
            for (ch in s) {
                a <- parallel:::readChild(ch)
                if (is.integer(a)) {
                  core <- which(cp == a)
                  fin[core] <- TRUE
                }
                else if (is.raw(a)) {
                  core <- which(cp == attr(a, "pid"))
                  job.res[[core]] <- ijr <- unserialize(a)
                  if (inherits(ijr, "try-error")){ 
                    has.errors <- c(has.errors, core)
                    if(stop.on.error)  #!!
                        stop("error in one of X = ", paste(schedule[[core]], collapse=", "), "\n", attr(ijr, "condition")$message) #!!
                  }
                  dr[core] <- TRUE
                }
                else if (is.null(a)) {
                  core <- which(cp == ch)
                  fin[core] <- TRUE
                }
            }
    }
    for (i in seq_len(cores)) {
        this <- job.res[[i]]
        if (inherits(this, "try-error")) {
            for (j in sindex[[i]]) res[[j]] <- this
        }
        else if (!is.null(this)) 
            res[sindex[[i]]] <- this
    }
    nores <- cores - sum(dr)
    if (nores > 0) 
        warning(sprintf(ngettext(nores, "scheduled core %s did not deliver a result, all values of the job will be affected", 
            "scheduled cores %s did not deliver results, all values of the jobs will be affected"), 
            paste(which(dr == FALSE), collapse = ", ")), domain = NA)
    if (length(has.errors)) {
        if (length(has.errors) == cores) 
            warning("all scheduled cores encountered errors in user code")
        else warning(sprintf(ngettext(has.errors, "scheduled core %s encountered error in user code, all values of the job will be affected", 
            "scheduled cores %s encountered errors in user code, all values of the jobs will be affected"), 
            paste(has.errors, collapse = ", ")), domain = NA)
    }
    res
}