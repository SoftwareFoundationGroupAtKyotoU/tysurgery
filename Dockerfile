FROM rust:1.77

WORKDIR /usr/src/tysurgery
COPY . .

RUN cargo install --path .

CMD ["bash"]
