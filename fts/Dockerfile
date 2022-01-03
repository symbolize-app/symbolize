FROM debian:stable-slim
WORKDIR /app
COPY .crdb/certs .crdb/certs
COPY target/release/fertile-earth target/release/fertile-earth
ENV FTS_HOST=::
ENV FTS_PORT=7070
# FLY_SECRET_ENV DATABASE_URL_API_READ
# FLY_SECRET_ENV FTS_PASSWORD
EXPOSE 7070
CMD ["target/release/fertile-earth"]
