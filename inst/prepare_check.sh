## Setup docker container ##
apt-get update -y && apt-get upgrade -y
apt-get install -y pandoc pandoc-citeproc apt-utils curl git gpg gnupg
apt-get install -y r-cran-devtools r-cran-markdown r-cran-dplyr r-cran-ggplot2
apt-get install -y r-cran-sf r-cran-terra r-cran-lwgeom
apt-get install -y r-cran-curl r-cran-covr r-cran-openeo r-cran-qpdf

# Install rOPTRAM (prepare tarball for rhub)
cd /home/docker/
git clone https://gitlab.com/rsl-bidr/roptram.git
R CMD build ./roptram
tarball=`ls rOPTRAM*.tar.gz`
R CMD INSTALL $tarball
echo "Built: ${tarball} and installed rOPTRAM "
R -e "install.packages(c('CDSE'), dependencies = TRUE)"
R -e "rOPTRAM::store_cdse_credentials(clientid = '$OAUTH_CLIENTID',
                                      secret = '$OAUTH_SECRET')"

# Change to package directory
cd /home/docker/roptram

# Now run pipeline:
# Rscript rhubcheck.R
# R -e "covr::package_coverage()"
