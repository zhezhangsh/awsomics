makeScatterPlot <- function(input, output, session, session.data) {
  dd <- session.data$data;
  md <- session.data$metadata;
  a1 <- input$axis.x;
  a2 <- input$axis.y;
  s1 <- input$primary;
  s2 <- input$secondary; 
  h1 <- input$highlight.primary;
  h2 <- input$highlight.secondary; 
  o1 <- input$only.primary;
  o2 <- input$only.secondary;

  if (!is.null(dd) & !is.null(md) & !is.null(s1) & !is.null(h1) & !is.null(s2) & !is.null(h2) & !is.na(a1) & !is.na(a2)) {
    data <- dd;
    anno <- md;
    
    ind1 <- as.integer(a1);
    ind2 <- as.integer(a2);
    data <- data[, c(ind1, ind2)];
    
    if (o1 & s1!='0' & h1!='0') {
      a <- as.character(as.vector(anno[, as.integer(s1)]));
      a[is.na(a)] <- 'N/A';
      u <- unique(a);
      i <- which(a == u[as.integer(h1)]); 
      data <- data[i, , drop=FALSE];
      anno <- anno[i, , drop=FALSE];
    };
    
    if (o2 & s2!='0' & h2!='0') {
      b <- as.character(as.vector(anno[, as.integer(s2)]));
      b[is.na(b)] <- 'N/A';
      v <- unique(b);
      j <- which(b == v[as.integer(h2)]); 
      data <- data[j, , drop=FALSE];
      anno <- anno[j, , drop=FALSE];
    };
    
    par(mar=c(0,0,0,0));
    
    hl <- c();
    col <- rep('#88888888', nrow(data));
    pch <- rep(20, nrow(data));
    
    # Color
    if (s1 != '0') {
      a <- as.character(as.vector(anno[, as.integer(s1)]));
      a[is.na(a)] <- 'N/A';
      u <- unique(as.character(as.vector(md[, as.integer(s1)])));
      c <- rainbow(length(u));
      names(c) <- u;
      col <- c[a];
      if (h1 != '0') {
        hl <- which(a == u[as.integer(h1)]); 
      }
    }
    
    # Shape
    if (s2 != '0') {
      b <- as.character(as.vector(anno[, as.integer(s2)]));
      b[is.na(b)] <- 'N/A';
      v <- unique(as.character(as.vector(md[, as.integer(s2)])));
      p <- (1:25)[1:min(25, length(v))];
      if (length(p) < length(v)) p <- c(p, 1:(length(v) - length(p)));
      names(p) <- v;
      pch <- p[b];
      if (h2 != '0') {
        hl <- intersect(hl, which(b == v[as.integer(h2)])); print(length(hl)); 
      }
    };
    
    xl <- c(min(data[, 1]), max(data[, 1]) + 0.5*(max(data[, 1]) - min(data[, 1])));
    plot(data[, 1], data[, 2], pch=pch, col=col, axes=FALSE, xlab='', ylab='', xlim=xl);
    if (length(hl) > 0) {
      if (!o1 | !o2) { print(length(hl)); 
        points(data[hl, 1], data[hl, 2], pch=13, cex=1.8, col='#111111EE');
        points(data[hl, 1], data[hl, 2], pch=pch[hl], col=col[hl]);        
      }
    }
    if (s1 != '0') {
      if (s2 == '0') {
        legend(max(data[, 1]), max(data[, 2]), bty='n', legend=u, col=c, cex=1.5, pch=19, text.col=c);
      } else {
        df <- data.frame(col, pch, stringsAsFactors = FALSE);
        df <- df[!duplicated(df), ];
        l1 <- as.vector(sapply(df[, 1], function(x) names(c)[c==x]));
        l2 <- as.vector(sapply(df[, 2], function(x) names(p)[p==x]));
        legend(max(data[, 1]), max(data[, 2]), bty='n', legend=paste(l1, l2, sep=':::'),
               col=df[, 1], cex=1.5, pch=df[, 2], text.col=df[, 1]);
      }
    }
  } else plot(0, axes=FALSE, xlab='', ylab='', type='n'); 
}