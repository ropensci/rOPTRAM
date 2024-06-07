## Setup docker container ##
apt-get update -y && apt-get upgrade -y
apt-get install -y pandoc pandoc-citeproc apt-utils curl git gpg gnupg qpdf
apt-get install -y r-cran-devtools r-cran-markdown r-cran-dplyr r-cran-ggplot2
apt-get install -y r-cran-sf r-cran-terra r-cran-lwgeom
apt-get install -y r-cran-curl r-cran-covr r-cran-jsonlite
apt-get install -y r-cran-curl r-cran-covr r-cran-openeo r-cran-qpdf
# Other dependencies
R -e "install.packages(c('geojsonio'), dependencies = TRUE)"

# Install rOPTRAM (prepare tarball for rhub)
cd /home/docker/
git clone https://github.com/ropensci/rOPTRAM.git
R CMD build ./roptram
tarball=`ls rOPTRAM*.tar.gz`
R CMD INSTALL $tarball
echo "Built: ${tarball} and installed rOPTRAM "
R -e "install.packages(c('CDSE'), dependencies = TRUE)"
R -e "rOPTRAM::store_cdse_credentials(clientid = '$OAUTH_CLIENTID',
                                      secret = '$OAUTH_SECRET')"
# Keep creds in env variables for rhub
export OAUTH_CLIENTID='$OAUTH_CLIENTID'
export OAUTH_SECRET='$OAUTH_SECRET'

# Validate email for rhub:
R -e "install.packages(c('rhub'), dependencies = TRUE)"
# Get the RHUB_EMAIL and TOKEN from gitlab custom variables
R -e "rhub::validate_email(email = '$RHUB_EMAIL', token = '$RHUB_TOKEN')"
# Authentication for CDSE
R -e "install.packages(c('CDSE'), dependencies = TRUE)"
R -e "rOPTRAM::store_cdse_credentials(clientid = '$OAUTH_CLIENTID',
                                      secret = '$OAUTH_SECRET')"

# Change to package directory and start rhub checks (in .gitlab-ci.yml)
cd /home/docker/roptram

# Now run pipeline:
# Rscript rhubcheck.R
# R -e "covr::package_coverage()"
