let
  e = (import ./default.nix {}).env;
in
  e.overrideAttrs (drv: {
    shellHook = ''
      function ghci_shared () {
        mkdir -p lib/
        gcc -shared cbits/haversine.c -olib/haversine.dylib
        ghci -Icbits/ -Llib -isrc
      }
    '';
  })
