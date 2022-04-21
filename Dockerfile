FROM racket/racket:8.3-full
RUN apt-get update -y
RUN apt-get install -y apt-utils build-essential vim libedit-dev python3.9
