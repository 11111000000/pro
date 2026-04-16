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
              init_file="''${EMACS_HEADLESS_INIT:-$repo_root/sample-init.el}"
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

          emacs-system-headless = pkgs.writeShellApplication {
            name = "emacs-system-headless";
            runtimeInputs = [ emacsPkg pkgs.coreutils ];
            text = ''
              set -euo pipefail

              repo_root="''${EMACS_HEADLESS_ROOT:-$(pwd)}"
              init_file="''${EMACS_HEADLESS_INIT:-$HOME/.emacs.d/init.el}"
              log_dir="''${EMACS_HEADLESS_LOG_DIR:-$repo_root/.agent-shell/emacs}"
              run_id="$(date +%Y%m%d-%H%M%S)"
              log_file="$log_dir/emacs-system-headless-$run_id.log"

              mkdir -p "$log_dir"

              export EMACS_HEADLESS_ROOT="$repo_root"

              echo "Logging to $log_file"
              set +e
              "${emacsPkg}/bin/emacs" --batch --debug-init -Q \
                --eval '(setq user-emacs-directory (file-name-as-directory (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
                -l "$init_file" \
                "$@" 2>&1 | tee "$log_file"

              status="$?"
              set -e
              exit "$status"
            '';
          };

          emacs-system-check = pkgs.writeShellApplication {
            name = "emacs-system-check";
            runtimeInputs = [ emacsPkg pkgs.coreutils ];
            text = ''
              set -euo pipefail

              workdir="$(mktemp -d)"
              trap 'chmod -R u+w "$workdir" >/dev/null 2>&1 || true; rm -rf "$workdir" >/dev/null 2>&1 || true' EXIT

              mkdir -p "$workdir/home/.emacs.d"
              cp -R /home/zoya/.emacs.d/. "$workdir/home/.emacs.d/"
              ln -s ${self} "$workdir/home/pro"

              export HOME="$workdir/home"
              export EMACS_HEADLESS_ROOT="${self}"
              unset NIX_LD LD_LIBRARY_PATH
              ${cleanPath}

              "${emacsPkg}/bin/emacs" --batch --debug-init -Q \
                --eval '(setq user-emacs-directory (file-name-as-directory (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
                -l "$HOME/.emacs.d/init.el" \
                --eval '(princ "system-init-ok")'
            '';
          };

          emacs-tty-diagnose = pkgs.writeShellApplication {
            name = "emacs-tty-diagnose";
            runtimeInputs = [ emacsPkg pkgs.coreutils ];
            text = ''
              set -euo pipefail

              workdir="$(mktemp -d)"
              trap 'chmod -R u+w "$workdir" >/dev/null 2>&1 || true; rm -rf "$workdir" >/dev/null 2>&1 || true' EXIT

              mkdir -p "$workdir/home/.emacs.d"
              cp -R /home/zoya/.emacs.d/. "$workdir/home/.emacs.d/"
              ln -s ${self} "$workdir/home/pro"

              export HOME="$workdir/home"
              export EMACS_HEADLESS_ROOT="${self}"
              unset NIX_LD LD_LIBRARY_PATH
              ${cleanPath}

              "${emacsPkg}/bin/emacs" --batch --debug-init -Q \
                --eval '(setq user-emacs-directory (file-name-as-directory (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
                --eval "(let ((mods (getenv \"EMACS_TTY_MODULES\"))) (when (and mods (> (length mods) 0)) (setq pro/startup-modules (mapcar #'intern (split-string mods \",\" t \"[[:space:]]+\")))))" \
                -l "$HOME/.emacs.d/init.el" \
                --eval "(let ((vars '(blink-cursor-mode show-paren-mode display-line-numbers-mode column-number-mode xterm-mouse-mode which-key-mode corfu-terminal-mode window-divider-mode display-time-mode tooltip-mode help-window-select fast-but-imprecise-scrolling redisplay-skip-fontification-on-input jit-lock-defer-time))) (princ \"[tty-diagnose] variables\\n\") (dolist (v vars) (princ (format \"%s=%S\\n\" v (if (boundp v) (symbol-value v) :unbound)))))" \
                --eval "(let ((hooks '(post-command-hook buffer-list-update-hook window-configuration-change-hook window-state-change-hook focus-in-hook focus-out-hook minibuffer-setup-hook minibuffer-exit-hook))) (princ \"[tty-diagnose] hooks\\n\") (dolist (h hooks) (princ (format \"%s=%d\\n\" h (length (symbol-value h))))))"
            '';
          };

          emacs-xvfb-smoke = pkgs.writeShellApplication {
            name = "emacs-xvfb-smoke";
            runtimeInputs = [ emacsPkg pkgs.coreutils pkgs.xvfb-run pkgs.xorg.xauth pkgs.xorg.xorgserver ];
            text = ''
              set -euo pipefail

              repo_root="''${EMACS_HEADLESS_ROOT:-$(pwd)}"
              log_dir="''${EMACS_STARTUP_LOG_DIR:-$repo_root/.agent-shell/emacs}"
              run_id="$(date +%Y%m%d-%H%M%S)"
              log_file="$log_dir/emacs-xvfb-$run_id.log"

              mkdir -p "$log_dir"

              export EMACS_HEADLESS_ROOT="$repo_root"
              export EMACS_STARTUP_LOG_DIR="$log_dir"
              export EMACS_STARTUP_LOG_FILE="$log_file"

              xvfb-run -a -s "-screen 0 1280x720x24 -nolisten tcp" \
                timeout 60s "${emacsPkg}/bin/emacs" -Q \
                --eval '(setq user-emacs-directory (file-name-as-directory (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
                --eval '(dolist (dir (directory-files (or (getenv "EMACS_HEADLESS_ROOT") default-directory) t "^[^.].*")) (when (file-directory-p dir) (add-to-list (quote load-path) dir)))' \
                --eval '(setq use-package-always-ensure nil)' \
                --eval '(setq package-enable-at-startup nil)' \
                --eval '(require (quote package))' \
                --eval '(package-initialize)' \
                --eval '(require (quote use-package))' \
                --eval '(load-file (expand-file-name "sample-early-init.el" (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
                --eval '(pro/early-log "xvfb" "after early-init")' \
                --eval '(pro/early-log "xvfb" "loading pro-otladku begin")' \
                --eval '(load-file (expand-file-name "разработка/про-отладку.el" (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
                --eval '(pro/early-log "xvfb" "loading pro-otladku end")' \
                --eval '(pro/early-log "xvfb" "loading zagrusit begin")' \
                --eval '(load-file (expand-file-name "инфраструктура/загрузить.el" (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
                --eval '(pro/early-log "xvfb" "loading zagrusit end")' \
                --eval '(pro/early-log "xvfb" "loading install-from begin")' \
                --eval '(load-file (expand-file-name "инфраструктура/установить-из.el" (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
                --eval '(pro/early-log "xvfb" "loading install-from end")' \
                --eval '(pro/early-log "xvfb" "loading monitors begin")' \
                --eval '(load-file (expand-file-name "среда/про-мониторы.el" (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
                --eval '(pro/early-log "xvfb" "loading monitors end")' \
                --eval '(pro/early-log "xvfb" "loading graphics begin")' \
                --eval '(load-file (expand-file-name "среда/про-графическую-среду.el" (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
                --eval '(pro/early-log "xvfb" "loading graphics end")' \
                --eval '(pro/log-startup-stage "xvfb-smoke" (format "window-system=%s" window-system))' \
                --eval '(pro/early-log "xvfb" "before exwm start call")' \
                --eval '(run-at-time 25 nil (lambda () (pro/log-startup-stage "xvfb-smoke" (format "booted=%s" (if (boundp (quote pro/exwm-booted)) pro/exwm-booted nil))) (kill-emacs 0)))' \
                --eval '(condition-case err (pro/старт-графической-среды) (error (pro/early-log "xvfb" (format "exwm start error %S" err)) (kill-emacs 1)))' \
                --eval '(pro/early-log "xvfb" "after exwm start call")' \
                --eval '(let ((deadline (+ (float-time) 55))) (while (and (not (bound-and-true-p pro/exwm-booted)) (< (float-time) deadline)) (sleep-for 1)) (unless (bound-and-true-p pro/exwm-booted) (pro/log-startup-stage "xvfb-smoke" "timeout waiting for exwm boot") (kill-emacs 1)))'
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
            inherit emacs-system-headless;
            inherit emacs-system-check;
            inherit emacs-tty-diagnose;
            inherit emacs-xvfb-smoke;
            inherit emacs-headless-test;
            default = emacs-headless;
          };

          apps = {
            emacs-headless = {
              type = "app";
              program = "${emacs-headless}/bin/emacs-headless";
            };
            emacs-system-headless = {
              type = "app";
              program = "${emacs-system-headless}/bin/emacs-system-headless";
            };
            emacs-system-check = {
              type = "app";
              program = "${emacs-system-check}/bin/emacs-system-check";
            };
            emacs-tty-diagnose = {
              type = "app";
              program = "${emacs-tty-diagnose}/bin/emacs-tty-diagnose";
            };
            emacs-xvfb-smoke = {
              type = "app";
              program = "${emacs-xvfb-smoke}/bin/emacs-xvfb-smoke";
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
            export HOME=/home/zoya
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
            cat > "$workdir/repo/config.el" <<'EOF'
            (load-file "${self}/среда/про-графическую-среду-старт.el")
            EOF
            cd "$workdir/repo"

            "${emacsPkg}/bin/emacs" --batch -Q \
              --eval '(setq user-emacs-directory (file-name-as-directory (or (getenv "EMACS_HEADLESS_ROOT") default-directory)))' \
              --eval '(require (quote package))' \
              --eval '(package-initialize)' \
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
