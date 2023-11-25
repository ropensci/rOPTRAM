## Setup docker container ##
apt update -y && apt upgrade -y
apt install -y pandoc pandoc-citeproc apt-utils curl git
apt install -y r-cran-devtools r-cran-markdown r-cran-dplyr r-cran-ggplot2
apt install -y r-cran-sf r-cran-terra r-cran-curl r-cran-covr r-cran-sen2r

# Install rOPTRAM (prepare tarball for rhub)
cd /home/docker/
git clone https://gitlab.com/rsl-bidr/roptram.git
R CMD build ./roptram
tarball=`ls rOPTRAM*.tar.gz`
R CMD INSTALL $tarball
echo "Built and installed rOPTRAM in tarball: ${tarball}"

# Install and then run validate_email()
R -e "install.packages(c('rhub'), dependencies = TRUE)"
# Get the RHUB_EMAIL and TOKEN from gitlab custom variables
R -e "rhub::validate_email(email = '$RHUB_EMAIL', token = '$RHUB_TOKEN')"

# Install Google cloud
curl -O https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-cli-455.0.0-linux-x86_64.tar.gz
tar xvzf google-cloud-cli-455.0.0-linux-x86_64.tar.gz
./google-cloud-sdk/install.sh -q
#- ./google-cloud-sdk/bin/gcloud init
export PATH=$PATH:`pwd`/google-cloud-sdk/bin

# Now run pipeline:
# Rscript rhubcheck.R
# R -e "covr::package_coverage()"
