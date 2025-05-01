
  library(seewave)
library(tuneR)


audio <- readWave("p1_23_4.wav") # this is an S4 class object


# the frequency of your audio file
freq <- 44100
# the length and duration of your audio file
totlen <- length(audio)
totsec <- totlen/freq

# the duration that you want to chop the file into (in seconds)
seglen <- 60

# defining the break points
breaks <- unique(c(seq(0, totsec, seglen), totsec))
breaks
index <- 1:(length(breaks)-1)
index
# the split
leftmat<-matrix(audio@left, ncol=(length(breaks)-1), nrow=seglen*freq)
leftmat
rightmat<-matrix(audio@right, ncol=(length(breaks)-2), nrow=seglen*freq)
# the warnings are nothing to worry about here... 

# convert to list of Wave objects.
subsamps <- lapply(1:ncol(leftmat), function(x)Wave(left=leftmat[,x],
                                           samp.rate=audio@samp.rate,bit=audio@bit)) 

       
       # I had some memory management issues on my computer when doing this
       # process with large (~ 130-150 MB) audio files so I used rm() and gc(),
       # which seemed to resolve the problems I had with allocating memory.
       rm("breaks","audio","freq","index","lastbitleft","lastbitright","leftmat",
          "rightmat","seglen","totlen","totsec")
       
       gc()
       
       filenames <- paste("audio","_split",1:(length(breaks)),".wav",sep="")
       
       # Save the files
       sapply(1:length(subsamps),
              function(x)writeWave(subsamps[[x]], 
                                   filename=filenames[x]))
       