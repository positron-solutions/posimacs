{
  description = "Emacs installation with home manager dotfiles integration and
  3rd party deps defined in nix flakes.";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    rust.url = "path:./rust";
    emacs-vterm.url = "path:./vterm/emacs-vterm";
  };

  # "properly defined inputs" are just passed through for consumption in modules
  outputs = inputs: inputs;
}
