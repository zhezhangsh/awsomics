# UI elements

################################################################################################   
# Introduction of App
intro.paragraph<-list(
  h1("Introduction"),
  p("This App provides an access to previous GWAS results from published or local studies. 
      The back-end of this App is a SQLite database storing GWAS p values collected from various resources, such as dbGaP and GWAScatalog.
      Users can use web browsers to query the database by specifying a SNP, gene, or genomic region.
      The results will be illustrated in various types of tables and publishable plots."
    )
)

################################################################################################   
# Data
data.paragraph<-list(
  h3("Searchable data through this App"),
  p("The backend of this App is a collection of GWAS results from dbGaP (", a("ftp://ftp.ncbi.nlm.nih.gov/dbgap"), ") and ",
    "GWAScatalog (", a("http://www.genome.gov/admin/gwascatalog.txt"), "). ",
    "Extra results from local data can also be included when the App is running on a local computer. ",
    "All GWAS results are stored in a SQLite database as Phred scores. ",
    "Each entry in the database corresponds to a unique SNP with a dbSNP ID. ",
    "Most queries takes a few seconds to retrieve data from the database. ",
    "The data is structured in 4 levels as described below."),
  
  tags$ol(
    tags$li(strong("Phred score: "), 
            "Each phred score corresponds to the p value of a SNP obtained from a GWAS analysis. ",
            "P values were converted to Phred scores by ", strong("-10*Log10(p)"), ", so a Phred score of 20 equals to a p value of 0.01. ",
            "The phred scores are searchable through dbSNP IDs or genomic loci (details below). "),
  
    tags$li(strong("Analysis: "),
            "Each analysis includes a set of Phred scores obtained from a GWAS that applied the same statistical test on multiple SNPs. ", 
            "The Phred scores of the same analysis are stored as a column in the SQLite database. ",
            "Most GWAS compared the allele frequency of SNPs between two sample groups. ",
            "Each analyses from dbGaP have a unique ID as ", strong("phaXXXXXX"), " that can be searched in dbGaP for full description. ", 
            "Each analysis from GWAScatalog have a unique as ", strong("pmidXXXXXXXX_Y"), "where XXXXXXXX is a Pubmed ID and Y is an assigned order number. ", 
            "Some analyses, including all of them from GWAScatalog, only provided significant Phred scores above a cutoff (e.g. 50). ",
            "The others provides Phred scores of all SNPs tested by the GWAS, including most dbGaP analyses."
    ),
    
    tags$li(strong("Study: "),
            "Each study can include one or multiple analyses that were based on the same study data. ",
            "For example, 1,974 analyses have been applied to the Framingham Cohort (",
            a("http://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=phs000007.v24.p9"), "). ",
            "The studies from dbGaP have unque ID like ", strong("phsXXXXXX"), " while the GWAScatalog studies have ID like ", strong("pmidXXXXXXXX"), " which is the Pubmed ID of the paper reporting this study."
    ),
    
    tags$li(strong("Database table: "), 
            "Each database table includes analyses of one or multiple studies as columns and SNPs as rows. ",
            "The initial contents of the backend SQLite database includes ~3.5 million unique SNPs in ~6,600 analyses and takes about 3Gb disk space. ",
            "To save disk space and improve query efficiency, studies sharing the similar SNPs were grouped into the same database table. ",
            "A table named after a study ID includes exclusively the analyses of that study, such as table ", strong("phs000007"), ". ",
            "The table ", strong("phs000000"), " includes miscellaneous dbGaP analyses of multiple studies, and the ", strong("genome_gov"), " table includes all analyses in GWAScatalog.",
            "Users have the options to exclude one or more tables in the search results (details below)."
    )
            
  ) # end of tags$ol list
) # end of data.paragraph

################################################################################################   
# Search options
search.paragraph<-list(
  h3("Search options"),
  p("The collected GWAS data can be searched using the searching panel located at the right side of this screen. ",
    "Users can first use the pull-down menu near  the top to select a search type. "),
  tags$ol(strong("Basic search options:"),
          tags$li(strong("Search by dbSNP ID: ")), 
          tags$li(strong("Search by genomic loci: ")), 
          tags$li(strong("Search by Entrez gene ID: ")), 
          tags$li(strong("Search by official gene symbol: "))
  ), 
  
  tags$ol(strong("Extra search options:"),
          tags$li(strong("Mask p values: ")), 
          tags$li(strong("Extend search region: ")), 
          tags$li(strong("Limit search to: ")), 
          tags$li(strong("Exclude database table(s): "))
  )
) # end of search.paragraph

