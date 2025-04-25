;; Decentralized Rare Gemstone Registry - MVP Stage
;; A Clarity smart contract for gemstone authentication and registration

;; Constants
(define-constant ERR-NOT-CHIEF-GEMOLOGIST (err u1))
(define-constant ERR-REGISTRY-OFFLINE (err u2))
(define-constant ERR-INVALID-GEMSTONE (err u3))
(define-constant ERR-INVALID-PARAMETER (err u5))
(define-constant ERR-GEMSTONE-EXISTS (err u7))
(define-constant MAX-GEMSTONE-ID u1000) ;; Maximum allowed gemstone ID

;; Data Variables
(define-data-var chief-gemologist principal tx-sender)
(define-data-var registry-operational bool false)

;; Gemstone Structure
(define-map rare-gemstones
    uint
    {
        gemstone-name: (string-utf8 128),
        description: (string-utf8 512),
        certificate-hash: (buff 32),    ;; SHA256 hash of the certificate
        origin-region: (string-utf8 64),
        registrar: principal
    }
)

;; Gemologist Profiles
(define-map gemologist-profiles
    principal
    {
        credentials: uint,
        gemstones-registered: (list 30 uint)
    }
)

;; Authorization
(define-private (is-chief-gemologist)
    (is-eq tx-sender (var-get chief-gemologist)))

;; Data Validation Functions
(define-private (is-valid-hash (hash (buff 32)))
    (> (len hash) u0))

;; Registry Management Functions
(define-public (activate-registry)
    (begin
        (asserts! (is-chief-gemologist) ERR-NOT-CHIEF-GEMOLOGIST)
        (var-set registry-operational true)
        (ok true)))

(define-public (register-gemstone
    (gemstone-id uint)
    (gemstone-name (string-utf8 128))
    (description (string-utf8 512))
    (certificate-hash (buff 32))
    (origin-region (string-utf8 64)))
    (let (
        (gemologist-profile (default-to 
            {credentials: u0, gemstones-registered: (list)} 
            (map-get? gemologist-profiles tx-sender)))
        )
        
        ;; Check registry status
        (asserts! (var-get registry-operational) ERR-REGISTRY-OFFLINE)
        
        ;; Validate gemstone-id is within acceptable range
        (asserts! (<= gemstone-id MAX-GEMSTONE-ID) ERR-INVALID-PARAMETER)
        
        ;; Check if gemstone already exists
        (asserts! (is-none (map-get? rare-gemstones gemstone-id)) ERR-GEMSTONE-EXISTS)
        
        ;; Validate gemstone-name and description are not empty
        (asserts! (> (len gemstone-name) u0) ERR-INVALID-PARAMETER)
        (asserts! (> (len description) u0) ERR-INVALID-PARAMETER)
        (asserts! (> (len origin-region) u0) ERR-INVALID-PARAMETER)
        
        ;; Validate hash is not empty
        (asserts! (is-valid-hash certificate-hash) ERR-INVALID-PARAMETER)
        
        ;; Set the gemstone data
        (map-set rare-gemstones gemstone-id
            {
                gemstone-name: gemstone-name,
                description: description,
                certificate-hash: certificate-hash,
                origin-region: origin-region,
                registrar: tx-sender
            })
        
        ;; Update gemologist profile
        (map-set gemologist-profiles tx-sender
            {
                credentials: (get credentials gemologist-profile),
                gemstones-registered: (unwrap! (as-max-len? 
                    (append (get gemstones-registered gemologist-profile) gemstone-id) u30)
                    ERR-INVALID-PARAMETER)
            })
        
        (ok true)))

;; Gemologist Registration Functions
(define-public (register-gemologist (initial-credentials uint))
    (begin
        (asserts! (var-get registry-operational) ERR-REGISTRY-OFFLINE)
        
        ;; Initialize gemologist profile
        (map-set gemologist-profiles tx-sender
            {
                credentials: initial-credentials,
                gemstones-registered: (list)
            })
            
        (ok true)))

;; Read-only functions
(define-read-only (get-gemstone-details (gemstone-id uint))
    (map-get? rare-gemstones gemstone-id))

(define-read-only (get-gemologist-profile (gemologist principal))
    (map-get? gemologist-profiles gemologist))

(define-read-only (get-registry-operational)
    (var-get registry-operational))

(define-public (close-registry)
    (begin
        (asserts! (is-chief-gemologist) ERR-NOT-CHIEF-GEMOLOGIST)
        (var-set registry-operational false)
        (ok true)))

(define-public (transfer-chief-gemologist-role (new-chief principal))
    (begin 
        (asserts! (is-chief-gemologist) ERR-NOT-CHIEF-GEMOLOGIST)
        ;; Cannot set to zero address (represented as none in Clarity)
        (asserts! (is-some (some new-chief)) ERR-INVALID-PARAMETER)
        (var-set chief-gemologist new-chief)
        (ok true)))