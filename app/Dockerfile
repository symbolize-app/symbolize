ARG NODE_OPTIONS
FROM node:15-slim
WORKDIR /app
COPY .crdb/certs .crdb/certs
COPY build build
COPY node_modules node_modules
ENV NODE_ENV=production
ENV NODE_OPTIONS=${NODE_OPTIONS}
ENV PORT=8080
EXPOSE 8080
CMD ["node", "./build/node/app/src/api/index.ts.mjs"]
