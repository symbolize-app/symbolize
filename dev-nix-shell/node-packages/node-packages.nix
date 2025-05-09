# This file has been generated by node2nix 1.11.1. Do not edit!

{nodeEnv, fetchurl, fetchgit, nix-gitignore, stdenv, lib, globalBuildInputs ? []}:

let
  sources = {
    "semver-7.7.1" = {
      name = "semver";
      packageName = "semver";
      version = "7.7.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/semver/-/semver-7.7.1.tgz";
        sha512 = "hlq8tAfn0m/61p4BVRcPzIGr6LKiMwo4VM6dGi6pt4qcRkmNzTcWq6eCEjEh+qXjkMDvPlOFFSGwQjoEa6gyMA==";
      };
    };
  };
in
{
  "@withgraphite/graphite-cli" = nodeEnv.buildNodePackage {
    name = "_at_withgraphite_slash_graphite-cli";
    packageName = "@withgraphite/graphite-cli";
    version = "1.6.1";
    src = fetchurl {
      url = "https://registry.npmjs.org/@withgraphite/graphite-cli/-/graphite-cli-1.6.1.tgz";
      sha512 = "paRVY86QKHxEG0+BKBrJr5W1Jz9z+tRppGB4FpJPSCfyX7HKINkc314mgGgswvbOW5y/UtCgiI7rgGc260ZpJQ==";
    };
    dependencies = [
      sources."semver-7.7.1"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      homepage = "https://github.com/withgraphite/graphite-cli";
      license = "None";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
  pnpm = nodeEnv.buildNodePackage {
    name = "pnpm";
    packageName = "pnpm";
    version = "10.10.0";
    src = fetchurl {
      url = "https://registry.npmjs.org/pnpm/-/pnpm-10.10.0.tgz";
      sha512 = "1hXbJG/nDyXc/qbY1z3ueCziPiJF48T2+Igkn7VoFJMYY33Kc8LFyO8qTKDVZX+5VnGIv6tH9WbR7mzph4FcOQ==";
    };
    buildInputs = globalBuildInputs;
    meta = {
      description = "Fast, disk space efficient package manager";
      homepage = "https://pnpm.io";
      license = "MIT";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
}
