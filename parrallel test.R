

library("listenv")
library("future")
plan(multisession, workers = 8)

res <- listenv()

ii <- 1
while (ii <=8) {
  ## This is evaluated in parallel and will only block
  ## if all workers are busy.
  res[[ii]] %<-% {
    p_name <- paste0("archive",ii,"/olddata.csv",sep="")
    write.csv(old_data,p_name, row.names = FALSE)
  }
  ii <- ii + 1
}

## Resolve all futures (blocks if not already finished)
res <- as.list(res)