FROM ubuntu:latest

Label version="1.0" maintainer="Y.Kitamu <ymyk6602@gmail.com>"

# add user
ARG user
RUN useradd -ms /bin/bash ${user}

# install dependent libraries
RUN apt-get update && apt-get upgrade -y && apt-get install git curl gcc -y

# setup rust
USER ${user}
SHELL ["/bin/bash", "-c"]
RUN cd /home/${user} &&\
    curl https://sh.rustup.rs -sSf > ./rust.sh &&\
    chmod +x ./rust.sh &&\
    ./rust.sh -y &&\
    rm -rf ./rust.sh &&\
    chmod +x /home/${user}/.cargo/env && source /home/${user}/.cargo/env \
    rustup update
ENV PATH /home/${user}/.cargo/bin:$PATH

# setup rust-analyzer
RUN mkdir /home/${user}/opt
WORKDIR /home/${user}/opt
RUN git clone https://github.com/rust-analyzer/rust-analyzer.git &&\
    cd /home/${user}/opt/rust-analyzer && \
    rustup update && cargo xtask install --server
