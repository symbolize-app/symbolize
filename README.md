# Symbolize <img width="30" height="30" src="art/favicon.svg?raw=true"/>

[Vision](vision.md)

## Setup

### Nix package manager

1. Install Nix 2.15.0 (pinned via Determinate Systems 0.9.1 installer)
1. Install Devenv 2.2.2 in the default user profile

```sh
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix/tag/v0.9.1 | sh -s -- install
nix profile install github:NixOS/nixpkgs/c27cdad491a991b11ed731760aa2ef8db0cb0410#devenv
```

#### Upgrade

1. Nix: uninstall, then reinstall
1. Devenv: replace the pinned Devenv version and corresponding Nixpkgs revision below, then reinstall the profile entry

```sh
nix profile remove devenv
nix profile install github:NixOS/nixpkgs/c27cdad491a991b11ed731760aa2ef8db0cb0410#devenv
```

### Devenv

1. Hook Devenv into Zsh
1. Allow this repo

```sh
echo 'eval "$(devenv hook zsh)"' >> $HOME/.zshrc
devenv allow
```

Devenv auto-reload is disabled for this project so Watchman remains the sole file watcher.

### Easy-RSA

General:

```sh
task easyrsa:server:build
```

Ubuntu:

```sh
sudo cp .pki/ca.crt /usr/local/share/ca-certificates
sudo update-ca-certificates
```

Arch Linux:

```sh
sudo trust anchor .pki/ca.crt
```

Chrome & Firefox:

- Navigate settings UI to manually import `.pki/ca.crt`

### Manual install

```
task i
```

## Visual Studio Code

1. Install [Visual Studio Code](https://code.visualstudio.com/docs/setup/setup-overview)
1. Run the command `Tasks: Allow Automatic Tasks in Folder`

## Commands

- `task c`

## License

This project is licensed under the terms of the [MIT license](LICENSE-MIT) and the [Apache License (Version 2.0)](LICENSE-APACHE), at your option.
