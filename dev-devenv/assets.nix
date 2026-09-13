final: prev:

{
  symbolize-relude-hlint = final.fetchurl {
    url = "https://raw.githubusercontent.com/kowainik/relude/v1.2.2.0/.hlint.yaml";
    sha256 = "sha256-6Hnam1jdPALSXPTLUhWZflsIOFsk81ajcCraLj6Npl8=";
  };

  symbolize-font-literata = final.stdenv.mkDerivation {
    pname = "symbolize-font-literata";
    version = "3.103";

    src = final.fetchurl {
      url = "https://github.com/googlefonts/literata/releases/download/3.103/3.103.zip";
      sha256 = "sha256-9/uXPK+ybPeFy+uur1HBj4fBWjvPTYKn1IV1ZNtbBW0=";
    };

    nativeBuildInputs = [
      final.unzip
      final.woff2
    ];

    dontUnpack = true;

    installPhase = ''
      mkdir -p $out
      unzip -p $src 'fonts/variable/Literata\[opsz,wght\].ttf' > $out/literata.ttf
      woff2_compress $out/literata.ttf
      unzip -p $src 'fonts/variable/Literata-Italic\[opsz,wght\].ttf' > $out/literata-italic.ttf
      woff2_compress $out/literata-italic.ttf
    '';
  };

  symbolize-relude-hlint-path = final.writeShellScriptBin "symbolize-relude-hlint-path" ''
    echo "${final.symbolize-relude-hlint}"
  '';

  symbolize-font-literata-path = final.writeShellScriptBin "symbolize-font-literata-path" ''
    echo "${final.symbolize-font-literata}"
  '';

  symbolize-font-path = final.writeShellScriptBin "symbolize-font-path" ''
    echo "${final.symbolize-font-literata}"
  '';
}
