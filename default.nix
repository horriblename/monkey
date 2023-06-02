{
  lib,
  rustPlatform,
  cargo,
  rustc,
}: let
  cargoToml = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in
  rustPlatform.buildRustPackage {
    src = ./.;
    cargoLock = {
      lockFile = ./Cargo.lock;
    };
    checkInputs = [cargo rustc];
    nativeBuildInputs = [
      rustc
      cargo
    ];

    CARGO_BUILD_INCREMENTAL = "false";
    RUST_BACKTRACE = "full";
    copyLibs = true;

    name = cargoToml.package.name;
    version = cargoToml.package.version;

    meta = with lib; {
      description = "Implementation of the Monkey language from \"Writing an Interpreter in go\"";
      homepage = "https://www.interpreterbook.com";
      license = licenses.gpl3;
    };
  }
