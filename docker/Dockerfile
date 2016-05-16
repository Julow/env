FROM debian

RUN echo "root:root" | chpasswd

#
RUN apt-get update && apt-get -y upgrade
RUN apt-get -y install build-essential openssh-server curl git python python-setuptools

WORKDIR /root
ENV HOME /root

# brew
RUN apt-get -y install ruby
RUN git clone https://github.com/Linuxbrew/linuxbrew.git $HOME/.brew

# aliases and scripts
RUN git clone https://github.com/Julow/env.git $HOME/_env
RUN cd $HOME/_env && bash install.sh

# ocaml
RUN apt-get -y install ocaml opam m4 pkg-config
RUN opam init -a
RUN opam install -y js_of_ocaml

# ssh
RUN mkdir /var/run/sshd
RUN sed -i 's/PermitRootLogin without-password/PermitRootLogin yes/' /etc/ssh/sshd_config
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd

EXPOSE 22
CMD ["/usr/sbin/sshd", "-D"]
