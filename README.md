# Symbolize <img width="30" height="30" src="art/favicon.svg?raw=true"/>

## Setup

### Nix package manager

1. Install Nix 2.15.0 (pinned via Determinate Systems 0.9.1 installer)
1. Install init profile flake

```
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix/tag/v0.9.1 | sh -s -- install
nix profile install ./dev-nix-init
```

#### Upgrade

1. Nix: uninstall, then reinstall
1. Init profile flake: find and upgrade the profile

```
nix profile list
nix profile upgrade <YOUR_PROFILE_ENTRY_INDEX>
```

### direnv

1. Installed by Nix
1. [Hook into your shell](https://direnv.net/docs/hook.html)
1. Allow this repo
1. Use nix-direnv library

```
direnv allow
mkdir -p $HOME/.config/direnv
echo 'source $HOME/.nix-profile/share/nix-direnv/direnvrc' >> $HOME/.config/direnv/direnvrc
```

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
