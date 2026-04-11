{
  description = "Headless Emacs runner for init.el logging";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      forSystem = system:
        let
          pkgs = import nixpkgs { inherit system; };

          emacs-headless = pkgs.writeShellApplication {
            name = "emacs-headless";
            runtimeInputs = [ pkgs.emacs-nox pkgs.coreutils ];
            text = ''
              set -euo pipefail

              repo_root="''${EMACS_HEADLESS_ROOT:-$(pwd)}"
              init_file="''${EMACS_HEADLESS_INIT:-$repo_root/init.el}"
              log_dir="''${EMACS_HEADLESS_LOG_DIR:-$repo_root/.agent-shell/emacs}"
              run_id="$(date +%Y%m%d-%H%M%S)"
              log_file="$log_dir/emacs-headless-$run_id.log"

              mkdir -p "$log_dir"

              export EMACS_HEADLESS_ROOT="$repo_root"

              echo "Logging to $log_file"
              set +e
              emacs --batch --debug-init \
                --eval '(setq user-emacs-directory (file-name-as-directory (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
                -l "$init_file" \
                "$@" 2>&1 | tee "$log_file"

              status="$?"
              set -e
              exit "$status"
            '';
          };
        in
        {
          packages = {
            inherit emacs-headless;
            default = emacs-headless;
          };

          apps = {
            emacs-headless = {
              type = "app";
              program = "${emacs-headless}/bin/emacs-headless";
            };
            default = {
              type = "app";
              program = "${emacs-headless}/bin/emacs-headless";
            };
          };

          devShells.default = pkgs.mkShell {
            packages = [ pkgs.emacs-nox ];
          };
        };
    in
    {
      packages = nixpkgs.lib.genAttrs systems (system: (forSystem system).packages);
      apps = nixpkgs.lib.genAttrs systems (system: (forSystem system).apps);
      devShells = nixpkgs.lib.genAttrs systems (system: (forSystem system).devShells);
    };
}
