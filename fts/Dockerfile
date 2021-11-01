ARG NODE_OPTIONS
FROM debian:stable-slim
WORKDIR /app
COPY .crdb/certs .crdb/certs
COPY target/release/fertile-earth target/release/fertile-earth
ENV FTS_PORT=7070
EXPOSE 7070
CMD ["target/release/fertile-earth"]
