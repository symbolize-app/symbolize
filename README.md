# Setup

## Nix package manager

1. Install Nix 2.15.0
1. Install init profile flake

```
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix/tag/v0.9.1 | sh -s -- install
nix profile install ./dev/nix/init
```

### Upgrade

1. Nix: uninstall, then reinstall
1. Init profile flake: find and upgrade the profile

```
flake profile list
flake profile upgrade <YOUR_PROFILE_ENTRY_INDEX>
```

## direnv

1. Installed by Nix
1. [Hook into your shell](https://direnv.net/docs/hook.html)
1. Allow this repo
1. Use nix-direnv library

```
direnv allow
mkdir -p $HOME/.config/direnv
echo 'source $HOME/.nix-profile/share/nix-direnv/direnvrc' >> $HOME/.config/direnv/direnvrc
```

## pnpm

1. Installed by Nix

## Rust

1. Installed by Nix

## Visual Studio Code

1. Install [Visual Studio Code](https://code.visualstudio.com/docs/setup/setup-overview)
1. Run the command `Tasks: Allow Automatic Tasks in Folder`

## CockroachDB

1. Install [CockroachDB](https://www.cockroachlabs.com/docs/stable/install-cockroachdb.html)
1. Run `mkdir -p .crdb/certs && mkdir -p .crdb/ca`
1. Run `cockroach cert create-ca --certs-dir=.crdb/certs --ca-key=.crdb/ca/ca.key`
1. Run `cockroach cert create-node localhost --certs-dir=.crdb/certs --ca-key=.crdb/ca/ca.key`
1. Run `cockroach cert create-client root --certs-dir=.crdb/certs --ca-key=.crdb/ca/ca.key`
1. Run `cockroach cert create-client api_read --certs-dir=.crdb/certs --ca-key=.crdb/ca/ca.key`
1. Run `cockroach cert create-client api_write --certs-dir=.crdb/certs --ca-key=.crdb/ca/ca.key`
1. Run `curl -fsSL -o .crdb/certs/cc-ca.crt -O https://cockroachlabs.cloud/clusters/CLUSTER_ID/cert`

# App development

## `pnpm db up`

## `pnpm import-basic`

## `pnpm dev`

Runs the development server

## `pnpm fmt`

Formats all source code

## `pnpm build`

Compiles all source code

## `pnpm sql`

Open a CockroachDB SQL prompt

- Use `select id::string from member` to view hex
- Use `'\xabc123'` to write hex

# FTS development

## `cargo fetch`

## `cargo watch-run`

## `cargo fix-fmt`

## `cargo fix-clippy`

## `curl -i -G -u :$FTS_PASSWORD 'localhost:7070/query' --data-urlencode 'language=en' --data-urlencode 'query=flower'`
