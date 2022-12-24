{
  description = "Very simple status monitor written in Haskell";
  outputs = { self }: {
    overlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = final.lib.composeExtensions
          prev.haskell.packageOverrides
          (self: super: {
            hsblocks = self.callCabal2nix "hsblocks" ./. {};
          });
      };
    };
    nixosModule = { ... }: {
      nixpkgs.overlays = [ self.overlay ];
    };
  };
}
