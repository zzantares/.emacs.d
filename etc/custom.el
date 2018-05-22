(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#dddddd" "#222222" "#555555" "#000000" "#000000" "#222222" "#000000" "#000000"])
 '(ansi-term-color-vector
   [unspecified "#2c3e50" "#e74c3c" "#2ecc71" "#f1c40f" "#3498db" "#9b59b6" "#3498db" "#e0e0e0"])
 '(company-quickhelp-color-background "#b0b0b0")
 '(company-quickhelp-color-foreground "#232333")
 '(custom-safe-themes
   (quote
    ("4ba6aa8a2776688ef7fbf3eb2b5addfd86d6e8516a701e69720b705d0fbe7f08" "125744c0b04c7addbe25d238a4053740fad2ad5e18211662f637dbacb3802331" "a75137dcd452a69cd404b4d44f0e50986196bcd9cf92bae49f8aa01038228221" "26a8f9baf6a7887110c25e0bc5aff033e54555251bd80233ad99e33b32e5f3f6" "9b39b25c3a23b1be6e99a3648b91ebaf2a7efdde236e3472aa95f1708ec61d4f" "7e1fa2fd97e792390d0c2347f0eefa2d1679c68da56e6baf983b057cefa400b4" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "e93c4567f5d30365064747972b179e80939cee875627034dc76cd50477c6b998" "00a9bb90413c4e218f02287db09e5dae9f6080890dd641ec0d6ff83d28d1335f" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "b273cc6a1d492660fff886a3cae1f00d5fd2d53b55fb374a21a14afd74fdec92" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "25c06a000382b6239999582dfa2b81cc0649f3897b394a75ad5a670329600b45" "f8cf128fa0ef7e61b5546d12bb8ea1584c80ac313db38867b6e774d1d38c73db" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "5c9bd73de767fa0d0ea71ee2f3ca6fe77261d931c3d4f7cca0734e2a3282f439" "bf7f4fb05a45eae1a6bc1a009b7731b09260d945ec4c3c4ed7f5da06647a7946" "5dc8ea509d300f9b751fce3d94d9885d264c5bda240e90e884d48601955b3f8c" "1ba61848d0d8c78e037867c26f118875705c20f5ad64949a8cee8c8059e5c50f" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#000000" :underline
       (:style wave :color "yellow"))
     (val :foreground "#000000")
     (varField :foreground "#600e7a" :slant italic)
     (valField :foreground "#600e7a" :slant italic)
     (functionCall :foreground "#000000" :slant italic)
     (implicitConversion :underline
       (:color "#c0c0c0"))
     (implicitParams :underline
       (:color "#c0c0c0"))
     (operator :foreground "#000080")
     (param :foreground "#000000")
     (class :foreground "#20999d")
     (trait :foreground "#20999d" :slant italic)
     (object :foreground "#5974ab" :slant italic)
     (package :foreground "#000000")
     (deprecated :strike-through "#000000"))))
 '(evil-want-C-u-scroll t)
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#eeeeee")
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(linum-format " %5i ")
 '(nrepl-message-colors
   (quote
    ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(pdf-view-midnight-colors (quote ("#eeeeee" . "#000000")))
 '(safe-local-variable-values
   (quote
    ((eval pyvenv-activate
           (concat
            (projectile-project-root)
            ".venv")))))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(vc-annotate-background "#eeeeee")
 '(vc-annotate-color-map
   (quote
    ((20 . "#437c7c")
     (40 . "#336c6c")
     (60 . "#205070")
     (80 . "#2f4070")
     (100 . "#1f3060")
     (120 . "#0f2050")
     (140 . "#a080a0")
     (160 . "#806080")
     (180 . "#704d70")
     (200 . "#603a60")
     (220 . "#502750")
     (240 . "#401440")
     (260 . "#6c1f1c")
     (280 . "#935f5c")
     (300 . "#834744")
     (320 . "#732f2c")
     (340 . "#6b400c")
     (360 . "#23733c"))))
 '(vc-annotate-very-old-color "#23733c"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
