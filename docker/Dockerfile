FROM debian:sid

# setup user
USER root
ENV HOME /root
WORKDIR /juloo

# basic packages
RUN apt-get update
RUN apt-get -y install build-essential curl

# aliases and scripts
RUN TMP_ENV="/tmp/_env.tar.gz" ; ENV_DIR="$HOME/_env" ; \
	curl -L -o $TMP_ENV https://github.com/Julow/env/archive/master.tar.gz ; \
	mkdir $ENV_DIR ; \
	tar -xf $TMP_ENV -C $ENV_DIR --strip-components=1 ; \
	cd $ENV_DIR && bash install.sh

# ocaml
RUN apt-get -y install ocaml opam m4 pkg-config
RUN opam init -a --dot-profile=/root/.bashrc
RUN opam install -y js_of_ocaml
