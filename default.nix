{ mkDerivation, async, base, directory, filepath, process, stdenv
, stm, unix
}:
mkDerivation {
  pname = "shlevy-backup";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base directory filepath process stm unix
  ];
  homepage = "https://github.com/shlevy/shlevy-backup";
  description = "Personal backup tools";
  license = stdenv.lib.licenses.mit;
}
