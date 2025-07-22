FROM rocker/tidyverse
LABEL Name=friendstest Version=0.99.18
RUN <<EOF
Rscript -e "install.packages('remotes')"
Rscript -e "remotes::install_github('favorov/friends.test')"
EOF
