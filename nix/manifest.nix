# Совместимый с Guix минимальный Nix-выражение окружения для ПРО Дао
{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  name = "pro-dao";
  packages = with pkgs; [
    emacs29      # или emacs-gtk, если нужно GUI
    git
    coreutils
    fontconfig
    findutils
    # Если нужно, доп. плагины Emacs через emacsPackages
    emacsPackages.use-package
    emacsPackages.magit
    emacsPackages.exwm
    emacsPackages.vertico
    emacsPackages.marginalia
    emacsPackages.consult
    emacsPackages.orderless
    emacsPackages.corfu
    emacsPackages.cape
    emacsPackages.undo-tree
    emacsPackages.projectile
    emacsPackages.avy
    emacsPackages.expand-region
    emacsPackages.org
    emacsPackages.rust-mode
    emacsPackages.cargo
    emacsPackages.auctex
    emacsPackages.pipenv
    emacsPackages.blacken
    emacsPackages.org
    emacsPackages.haskell-mode
    texlive.combined.scheme-basic
    python3
    rustc
    cargo
    ghc
    stack
    emacsPackages.yasnippet
    emacsPackages.pandoc-mode
    emacsPackages.lsp-mode
    pandoc
    emacsPackages.clojure-mode
    emacsPackages.cider
    emacsPackages.web-mode
    emacsPackages.emmet-mode
    emacsPackages.typescript-mode
    emacsPackages.org-roam
    emacsPackages.elfeed
    mu
  ];
  shellHook = ''
    export LANG=ru_RU.UTF-8
  '';
}
