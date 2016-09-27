rnaseq2g.validate.result <- function(res) {
  
  if (!is.list(res)) msg <- 'Uploaded R object is not a list' else
    if (!('input' %in% names(res))) msg <- 'Uploaded object has no inputs of DE analysis.' else
      if (!('input' %in% names(res))) msg <- 'Uploaded object has no outputs of DE analysis.' else {
        # validate input data
        iput <- res$input;        
        if (!('original' %in% names(iput))) msg <- 'Original read count matrix is not part of the uploaded object.' else 
          if (!('filtered' %in% names(iput))) msg <- 'Filtered read count matrix is not part of the uploaded object.' else 
            if (!('normalized' %in% names(iput))) msg <- 'Normalized data is not part of the uploaded object.' else 
              if (!('count' %in% names(iput$normalized))) msg <- 'Normalized read count matrix is not part of the uploaded object.' else 
                if (!('logged' %in% names(iput$normalized))) msg <- 'Normalized log-transformed data is not part of the uploaded object.' else {
                  # validate output data
                  oput <- res$output;        
                  if (length(oput)==0) msg <- 'There is no result matrix in the uploaded object.' else {
                    cl <- sapply(oput, class); 
                    if (length(cl[cl!='matrix']) > 0) msg <- 'Some outputs are not matrix in the uploaded object.' else {
                      rnm <- rownames(iput$filter); 
                      eql <- sapply(oput, function(x) identical(rownames(x), rnm));
                      if (length(eql[!eql])>0) msg <- 'Row names of input and output matrixes not match in the uploaded object.' else
                        msg <- NA;
                    }
                  }
                }
      }
  
  msg;
}