## Setup docker container ##
apt-get update -y && apt-get upgrade -y
apt-get install -y pandoc pandoc-citeproc apt-utils curl git gpg gnupg
apt-get install -y r-cran-devtools r-cran-markdown r-cran-dplyr r-cran-ggplot2
apt-get install -y r-cran-sf r-cran-terra r-cran-curl r-cran-covr r-cran-sen2r

# Install rOPTRAM (prepare tarball for rhub)
cd /home/docker/
git clone https://gitlab.com/rsl-bidr/roptram.git
R CMD build ./roptram
tarball=`ls rOPTRAM*.tar.gz`
R CMD INSTALL $tarball
echo "Built: ${tarball} and installed rOPTRAM "

# Install and then run validate_email()
R -e "install.packages(c('rhub'), dependencies = TRUE)"
# Get the RHUB_EMAIL and TOKEN from gitlab custom variables
R -e "rhub::validate_email(email = '$RHUB_EMAIL', token = '$RHUB_TOKEN')"

# Install Google cloud
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | gpg --dearmor -o /usr/share/keyrings/cloud.google.gpg
echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
apt-get -y update && apt-get -y install google-cloud-cli

# curl -O https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-cli-455.0.0-linux-x86_64.tar.gz
# tar xvzf google-cloud-cli-455.0.0-linux-x86_64.tar.gz
#./google-cloud-sdk/install.sh -q
#- ./google-cloud-sdk/bin/gcloud init
# export PATH=$PATH:`pwd`/google-cloud-sdk/bin

# Change to package directory
cd /home/docker/roptram

# Now run pipeline:
# Rscript rhubcheck.R
# R -e "covr::package_coverage()"
