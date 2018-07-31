to_pdf <- function(expr, filename, ..., verbose=TRUE) {
  if(!file.exists(dirname(filename)))
    dir.create(dirname(filename), recursive=TRUE)
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

source("C:/R-projects/EucFACE_gmes/masterscripts/functions.R")

fn <- function(...)file.path("master_scripts",...)

to_pdf(source("masterscripts/phys_boxplots.R"),fn("Figure1.pdf"), width=12, height=6)
to_pdf(source("masterscripts/cicc_boxplots.R"),fn("Figure2.pdf"), width=12, height=6)
to_pdf(source("masterscripts/photo_gmgs.R"),fn("Figure3.pdf"), width=10, height=6)
to_pdf(source("masterscripts/cond_stom_anatomy.R"),fn("Figure4.pdf"), width=10, height=6)
to_pdf(source("masterscripts/gmes_anatomy.R"),fn("Figure5.pdf"), width=10, height=6)
to_pdf(source("masterscripts/lma_anatomy.R"),fn("Figure6.pdf"), width=10, height=6)