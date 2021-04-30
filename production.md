# Production setup

## Fly.io

1. Run `flyctl launch`
1. Save `flyctl auth token` to GitHub Secret `FLY_ACCESS_TOKEN`

## CockroachCloud

1. Create a [new cluster](https://cockroachlabs.cloud/clusters)
1. Save `cc-ca.crt` to GitHub Secret `DATABASE_CA_CRT`
1. Create a new `migrate` SQL user (admin by default)
1. Build URL and save to GitHub Secret `DATABASE_URL_ADMIN`
1. Run `CREATE ROLE 'api_read'; ALTER ROLE 'api_read' WITH LOGIN PASSWORD 'xxx';`
1. Build URL and save to `cat | flyctl secrets set --detach DATABASE_URL_API_READ=-`
1. Run `CREATE ROLE 'api_write'; ALTER ROLE 'api_write' WITH LOGIN PASSWORD 'xxx';`
1. Build URL and save to `cat | flyctl secrets set --detach DATABASE_URL_API_READ=-`
