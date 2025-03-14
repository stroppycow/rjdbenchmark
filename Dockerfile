FROM r-base:4.4.3
COPY . /home/rjdbenchmark
RUN apt-get update && \
    apt-get install -y default-jdk wget curl git-all unzip libprotobuf-dev libgit2-dev libcurl4-openssl-dev && \
    mkdir -p /home/cruncher && \
    mkdir -p /home/rjdbenchmark && \
    wget https://github.com/jdemetra/jdplus-main/releases/download/v3.4.0/jwsacruncher-3.4.0.zip -O /home/cruncher/jwsacruncher-3.4.0-bin.zip && \
    wget https://github.com/jdemetra/jdplus-main/releases/download/v3.3.0/jwsacruncher-3.3.0.zip -O /home/cruncher/jwsacruncher-3.3.0-bin.zip && \
    wget https://github.com/jdemetra/jdplus-main/releases/download/v3.2.4/jwsacruncher-3.2.4.zip -O /home/cruncher/jwsacruncher-3.2.4-bin.zip && \
    wget https://github.com/jdemetra/jwsacruncher/releases/download/v2.2.6/jwsacruncher-2.2.6-bin.zip -O /home/cruncher/jwsacruncher-2.2.6-bin.zip && \
    wget https://github.com/jdemetra/jwsacruncher/releases/download/v2.2.6/jwsacruncher-2.2.6-bin.zip -O /home/cruncher/jwsacruncher-2.2.6-bin.zip && \
    wget https://github.com/jdemetra/jwsacruncher/releases/download/v2.2.5/jwsacruncher-2.2.5-bin.zip -O /home/cruncher/jwsacruncher-2.2.5-bin.zip && \
    wget https://github.com/jdemetra/jwsacruncher/releases/download/v2.2.4/jwsacruncher-2.2.4-bin.zip -O /home/cruncher/jwsacruncher-2.2.4-bin.zip && \
    wget https://github.com/jdemetra/jwsacruncher/releases/download/v2.2.3/jwsacruncher-2.2.3-bin.zip -O /home/cruncher/jwsacruncher-2.2.3-bin.zip && \
    wget https://github.com/jdemetra/jwsacruncher/releases/download/v2.2.2/jwsacruncher-2.2.2-bin.zip -O /home/cruncher/jwsacruncher-2.2.2-bin.zip && \ 
    cd /home/cruncher && \
    unzip "*.zip" && \
    rm *.zip && \
    cd /home && \
    wget -O quarto.deb "https://github.com/quarto-dev/quarto-cli/releases/download/v1.6.42/quarto-1.6.42-linux-amd64.deb" && \
    apt-get install -y ./quarto.deb && \
    rm quarto.deb && \
    cd /home/rjdbenchmark && \
    R --no-restore --no-save -e 'options(renv.config.repos.override = "https://packagemanager.posit.co/cran/__linux__/bookworm/latest")'  -e 'renv::restore()'  && \
    R --no-restore --no-save -e 'options(renv.config.repos.override = "https://packagemanager.posit.co/cran/__linux__/bookworm/latest")' -e 'renv::install("gert", type = "source")'  -e 'renv::install("devtools")'  -e 'devtools::install(".")'
WORKDIR /home/rjdbenchmark