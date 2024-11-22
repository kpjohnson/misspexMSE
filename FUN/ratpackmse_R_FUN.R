# Functions for running ratpackmse via R

clean_outputs <- function(parent_dir) {
  old_outputs <- list.files(parent_dir, include.dirs = F, full.names = T, recursive = T)
  file.remove(old_outputs)
  check <- list.files(parent_dir, include.dirs = F, full.names = T, recursive = T)
  
  return(length(check))
}

run_ratpackmse <- function(parent_dir, 
                           rat_parent="E:\\Program_Files\\msys64\\home\\Kristin\\ratpack\\ratpackmse", 
                           rat_number=NULL, batch_file) {
  # Remove old results
  clean_outputs(parent_dir=parent_dir)
  
  # Pick a ratpack folder
  # rat_number <- NULL
  rat_dir <- paste(rat_parent, rat_number, "\\data\\", sep="")
  
  # Run ratpackmse
  setwd(rat_dir)
  Out <- shell(cmd=paste(batch_file), intern=TRUE)
  
  # Reset directory
  setwd(here())
  
  return(Out)
}

cleanDepl <- function(OM_Out, EM_Out) {
  
  nsim <- length(unique(OM_Out$Sim))
  simremove <- c()
  
  for (s in 1:nsim) {
    temp_Out <- OM_Out[OM_Out$Sim==s,]
    projindex <- which(temp_Out$Year == temp_Out[temp_Out$Period=="Sim", "Year"][1])
    
    if (sum(temp_Out[projindex,10:12])==0) {
      simremove <- c(simremove, s)
    }
  }
  
  rowremOM <- OM_Out$Sim %in% simremove
  rindexOM <- which(rowremOM == TRUE)
  
  rowremEM <- EM_Out$sim %in% paste(simremove, ":", sep="")
  rindexEM <- which(rowremEM == TRUE)
  
  OM_Out2 <- OM_Out[-rindexOM,]
  EM_Out2 <- EM_Out[-rindexEM,]
  
  Outs <- list()
  Outs$OM_Out <- OM_Out2
  Outs$EM_Out  <- EM_Out2
  
  
  return(Outs)
}

tidyFormat <- function(OM_Files, EM_Files, FactorColNames, OutDir) {
  for (f in 1:length(OM_Files)) {
    OM_Out <- read.table(OM_Files[f],
                         header=TRUE,
                         fill=TRUE)
    EM_Out <- read.table(EM_Files[f],
                         header=TRUE,
                         fill=TRUE)
    
    Outs <- cleanDepl(OM_Out = OM_Out,
                      EM_Out = EM_Out)
    
    tidyFactors <- unlist(strsplit(OM_Files[f], split="/"))
    tidyFactors <- tidyFactors[c(-1, -length(tidyFactors))]
    
    tempOMOut <- data.frame(matrix(ncol=length(tidyFactors), nrow=nrow(Outs$OM_Out)))
    colnames(tempOMOut) <- FactorColNames
    
    tempEMOut <- data.frame(matrix(ncol=length(tidyFactors), nrow=nrow(Outs$EM_Out)))
    colnames(tempEMOut) <- FactorColNames
    
    for (c in 1:length(FactorColNames)) {
      tempOMOut[,c] <- factor(tidyFactors[c])
      tempEMOut[,c] <- factor(tidyFactors[c])
    }
    
    tempOM_Out <- cbind(tempOMOut, Outs$OM_Out)
    tempEM_Out <- cbind(tempEMOut, Outs$EM_Out)
    
    if (f==1) {
      tidyOM_Out <- tempOM_Out
      tidyEM_Out <- tempEM_Out
    } else {
      tidyOM_Out <- rbind(tidyOM_Out, tempOM_Out)
      tidyEM_Out <- rbind(tidyEM_Out, tempEM_Out)
    }
  }
  
  saveRDS(tidyOM_Out, file=paste(OutDir, "tidy_OM_Out.rds", sep=""))
  saveRDS(tidyEM_Out, file=paste(OutDir, "tidy_EM_Out.rds", sep=""))
  
  return(list(OM=tidyOM_Out, EM=tidyEM_Out))
}