FROM r-base:4.4.2
COPY . /home/jdbenchmark
RUN apt-get update &&
    apt-get install -y default-jdk wget unzip &&
    mkdir -p /home/cruncher &&
    mkdir -p /home/jdbenchmark &&
    wget https://github.com/jdemetra/jdplus-main/releases/download/v3.4.0/jwsacruncher-3.4.0.zip -O /home/cruncher/jwsacruncher-3.4.0-bin.zip &&
    wget https://github.com/jdemetra/jdplus-main/releases/download/v3.3.0/jwsacruncher-3.3.0.zip -O /home/cruncher/jwsacruncher-3.3.0-bin.zip &&
    wget https://github.com/jdemetra/jdplus-main/releases/download/v3.2.4/jwsacruncher-3.2.4.zip -O /home/cruncher/jwsacruncher-3.2.4-bin.zip &&
    wget https://github.com/jdemetra/jwsacruncher/releases/download/v2.2.6/jwsacruncher-2.2.6-bin.zip -O /home/cruncher/jwsacruncher-2.2.6-bin.zip &&
    wget https://github.com/jdemetra/jwsacruncher/releases/download/v2.2.6/jwsacruncher-2.2.6-bin.zip -O /home/cruncher/jwsacruncher-2.2.6-bin.zip &&
    wget https://github.com/jdemetra/jwsacruncher/releases/download/v2.2.5/jwsacruncher-2.2.5-bin.zip -O /home/cruncher/jwsacruncher-2.2.5-bin.zip &&
    wget https://github.com/jdemetra/jwsacruncher/releases/download/v2.2.4/jwsacruncher-2.2.4-bin.zip -O /home/cruncher/jwsacruncher-2.2.4-bin.zip &&
    wget https://github.com/jdemetra/jwsacruncher/releases/download/v2.2.3/jwsacruncher-2.2.3-bin.zip -O /home/cruncher/jwsacruncher-2.2.3-bin.zip &&
    wget https://github.com/jdemetra/jwsacruncher/releases/download/v2.2.2/jwsacruncher-2.2.2-bin.zip -O /home/cruncher/jwsacruncher-2.2.2-bin.zip &&
    cd /home/cruncher &&
    unzip "*.zip" &&
    rm *.zip
    cd /home/jdbenchmark &&
    R --no-restore --no-save -e 'renv::activate()' -e 'devtools::install(".")'
WORKDIR /home/jdbenchmark