{
  description = "Headless Emacs runner for init.el logging";

  nixConfig = {
    warn-dirty = false;
    extra-experimental-features = [ "nix-command" "flakes" ];
  };

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

  outputs = { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      forSystem = system:
        let
          pkgs = import nixpkgs { inherit system; };
          emacsBin = "${pkgs.emacs-nox}/bin/emacs";

          cleanPath = ''
            clean_path=""
            old_ifs=$IFS
            IFS=':'
            for p in $PATH; do
              case "$p" in
                *nix-ld*) ;;
                *) clean_path="''${clean_path:+$clean_path:}$p" ;;
              esac
            done
            IFS=$old_ifs
            export PATH="$clean_path"
          '';

          emacsPkg = pkgs.emacs.pkgs.withPackages (epkgs: [
            epkgs.gptel
            epkgs.use-package
            epkgs.exwm
            epkgs.xelb
            epkgs.xclip
            epkgs.avy
            epkgs.multiple-cursors
            epkgs.expand-region
            epkgs.whole-line-or-region
            epkgs.shift-text
            epkgs.reverse-im
          ]);

          emacs-headless = pkgs.writeShellApplication {
            name = "emacs-headless";
            runtimeInputs = [ emacsPkg pkgs.coreutils ];
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
              "${emacsPkg}/bin/emacs" --batch --debug-init \
                --eval '(setq user-emacs-directory (file-name-as-directory (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
                -l "$init_file" \
                "$@" 2>&1 | tee "$log_file"

              status="$?"
              set -e
              exit "$status"
            '';
          };

          emacs-headless-test = pkgs.writeShellApplication {
            name = "emacs-headless-test";
            runtimeInputs = [ emacsPkg pkgs.coreutils ];
            text = ''
              set -euo pipefail

              repo_root="''${EMACS_HEADLESS_ROOT:-$(pwd)}"
              test_file="''${1:-$repo_root/tests/unit/ai-openrouter-api.el}"

              export EMACS_HEADLESS_ROOT="$repo_root"
              unset NIX_LD LD_LIBRARY_PATH
              ${cleanPath}

              "${emacsPkg}/bin/emacs" --batch -Q \
                --eval '(setq user-emacs-directory (file-name-as-directory (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
                --eval "(add-to-list 'load-path (expand-file-name \"интеграция\" (or (getenv \"EMACS_HEADLESS_ROOT\") default-directory)))" \
                -l "$test_file"
            '';
          };
        in
        {
          packages = {
            inherit emacs-headless;
            inherit emacs-headless-test;
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
            packages = [ pkgs.emacs-nox pkgs.coreutils ];
            shellHook = ''
              unset NIX_LD LD_LIBRARY_PATH
              ${cleanPath}
              export EMACS_HEADLESS_ROOT="$(pwd)"
              echo "=== Nix devShell: emacs ready (nix-ld bypassed) ==="
            '';
          };

          devShells.test = pkgs.mkShell {
            packages = [ pkgs.emacs-nox pkgs.git pkgs.coreutils ];
            buildInputs = [ pkgs.emacs-nox ];
            shellHook = ''
              unset NIX_LD LD_LIBRARY_PATH
              ${cleanPath}
              export EMACS_HEADLESS_ROOT="$(pwd)"
              echo "=== Test devShell: ready for e2e tests ==="
              echo "Run: emacs --batch -Q -l tests/e2e/<name>.el"
            '';
          };

          checks.smoke = pkgs.runCommand "emacs-headless-smoke" {
            nativeBuildInputs = [ pkgs.emacs-nox pkgs.coreutils ];
          } ''
            export EMACS_HEADLESS_ROOT=${self}
            unset NIX_LD LD_LIBRARY_PATH
            ${cleanPath}
            "${emacsBin}" --batch -Q --eval '(princ "ok")' > "$out"
          '';

          checks.byteCompileConfig = pkgs.runCommand "emacs-config-byte-compile" {
            nativeBuildInputs = [ emacsPkg pkgs.coreutils ];
          } ''
            set -euo pipefail

            export EMACS_HEADLESS_ROOT=${self}
            unset NIX_LD LD_LIBRARY_PATH
            ${cleanPath}

            workdir="$(mktemp -d)"
            trap 'chmod -R u+w "$workdir" >/dev/null 2>&1 || true; rm -rf "$workdir" >/dev/null 2>&1 || true' EXIT

            mkdir -p "$workdir/repo"
            install -m 644 ${self}/среда/про-графическую-среду.el "$workdir/repo/config.el"
            cd "$workdir/repo"

            "${emacsPkg}/bin/emacs" --batch -Q \
              --eval '(setq user-emacs-directory (file-name-as-directory (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
              --eval '(require (quote package))' \
              --eval '(package-initialize)' \
              --eval '(dolist (dir (directory-files (getenv "EMACS_HEADLESS_ROOT") t "^[^.].*")) (when (file-directory-p dir) (add-to-list (quote load-path) dir)))' \
              --eval '(setq byte-compile-error-on-warn nil)' \
              --eval '(byte-compile-file "config.el")'

            test -f "config.elc"
            printf 'ok\n' > "$out"
          '';
        };
    in
    {
      packages = nixpkgs.lib.genAttrs systems (system: (forSystem system).packages);
      apps = nixpkgs.lib.genAttrs systems (system: (forSystem system).apps);
      devShells = nixpkgs.lib.genAttrs systems (system: (forSystem system).devShells);
      checks = nixpkgs.lib.genAttrs systems (system: (forSystem system).checks);
    };
}
