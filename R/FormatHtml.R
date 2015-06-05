# Helper functions used for specific types of HTML-like text

##########################################################################################################################################################
# Add href tags to a character string
AddHref<-function(v, url) {
    paste('<a href="', as.vector(url), '" target="_blank">', as.vector(v), '</a>', sep='');
}

##########################################################################################################################################################
# Write out HTML-like text in key-value pairs. Can be used by readerUI function to render tagged text
HtmlKeyValue<-function(values, keys=names(values), n.br=1, separator1=':', separator2='\\.', title='', title.size=1, bold.key=TRUE, add.url=TRUE) {
    # values, key           To be rendered in pairs, such as "gene_type: coding"
    # n.br                  Number of break lines between each key-value pair
    # separator1            Character between keys and values
    # separator2            When <values> is a list, collapse each element with this character
    # title                 Optional title at the top
    # title.size            The number is h1, h2, etc. Used to specify title size
    # bold.key              Whether to show key in boldface
    # add.url               Whether to add URL tag if the key equals 'URL'
    
    library(shiny);
    
    # same length and remove NA
    keys<-keys[1:length(values)];
    keys[is.na(keys)]<-'';
    names(values)<-keys;
    
    # separator between key and value
    separator1<-separator1[1];
    if (is.na(separator1)) separator1<-': ';
    
    # separator between elements when value is a vector
    separator2<-separator2[1];
    if (is.na(separator2)) separator2<-'\\. ';
    
    # collapse
    if(is.list(values)) values<-sapply(values, function(v) paste(v, collapse=separator2));
    values[is.na(values)]<-'';

    #url.tag<-function(url) paste('<a href="', url, '" target="_blank">Click</a>', sep='');

    # At least one line break between key-value pairs
    if(n.br<1 | identical(n.br, NA)) n.br<-1;
    
    # create a title and list of outputs
    if (!identical('', title) & !is.na(title)) {
        if (title.size<1 | title.size>6) title.size<-2;
        tg<-paste('h', title.size, sep='');
        out<-list(do.call(tg, list(title)));
    } else out<-c();
    
    # key-value pairs
    out1<-lapply(1:length(keys), function(i) {
        key<-paste(keys[i], separator1, sep='');
        if (bold.key) key.tag<-strong(key) else key.tag<-key; # show keys in boldface
        value<-as.vector(values[i]);
        
        # add URL tag
        if (add.url & grepl('^URL', key, ignore.case=TRUE)) value.tag<-tags$a(href=value, target='_blank', value) else value.tag<-value;
        
        list(lapply(1:n.br, function(i) br()), key.tag, value.tag);
    });
    
    c(out, do.call('c', out1));
}

##########################################################################################################################################################
# Show gene info as a list of factoids, mostly deal with gene location of different genome assembly
HtmlGeneFactoid<-function(gn, genome='GRCh37') {
    # gn        A list of gene factoids, such as symbol and description
    # genome    Genome assebly version
    
    ind<-grep('Location', names(gn));
    if (length(ind) == 0) gn else { # return the same list if no location information
        loc<-gn[[ind[1]]];
        
        ind1<-grep(paste('^', genome, '$', sep=''), names(loc), ignore.case=TRUE);
        if (length(ind1) == 0) gn[-ind] else {
            loc<-loc[[ind1[1]]];
            append(gn, loc, after=ind)[-ind];
        }
    }
}
##########################################################################################################################################################





