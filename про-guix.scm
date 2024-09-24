
(simple-service 'guixrus-service
                home-channels-service-type
                (list
                 (channel
                  (name 'guixrus)
                  (url "https://git.sr.ht/~whereiseveryone/guixrus"))))

(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells))

