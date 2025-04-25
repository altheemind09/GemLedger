;; Decentralized Rare Gemstone Registry - Assessment
;; A Clarity smart contract for gemstone authentication, valuation, and assessment

;; Constants
(define-constant ERR-NOT-CHIEF-GEMOLOGIST (err u1))
(define-constant ERR-REGISTRY-OFFLINE (err u2))
(define-constant ERR-INVALID-GEMSTONE (err u3))
(define-constant ERR-GEMSTONE-LOCKED (err u4))
(define-constant ERR-INVALID-PARAMETER (err u5))
(define-constant ERR-INSUFFICIENT-CREDENTIALS (err u6))
(define-constant ERR-GEMSTONE-EXISTS (err u7))
(define-constant ERR-ALREADY-EXAMINED (err u8))
(define-constant ERR-NOT-AUTHORIZED (err u9))
(define-constant ERR-ASSESSMENT-NOT-FOUND (err u10))
(define-constant MAX-GEMSTONE-ID u1000) ;; Maximum allowed gemstone ID
(define-constant MIN-CREDENTIALS-REQUIRED u10) ;; Minimum credentials to register

;; Data Variables
(define-data-var chief-gemologist principal tx-sender)
(define-data-var registry-operational bool false)
(define-data-var minimum-credentials-threshold uint u100) ;; 100 credentials points minimum

;; Gemstone Structure
(define-map rare-gemstones
    uint
    {
        gemstone-name: (string-utf8 128),
        description: (string-utf8 512),
        certificate-hash: (buff 32),    ;; SHA256 hash of the certificate
        origin-region: (string-utf8 64),
        open-for-assessments: bool,
        registrar: principal,
        verified-assessments: uint      ;; Counter of accepted assessments
    }
)

;; Gemologist Profiles
(define-map gemologist-profiles
    principal
    {
        credentials: uint,
        gemstones-registered: (list 30 uint),
        assessments-submitted: (list 30 uint)
    }
)

;; Assessment Structure
(define-map gemstone-assessments
    uint  ;; assessment-id
    {
        description: (string-utf8 256),
        analysis-hash: (buff 32),
        gemologist: principal,
        target-gemstone: uint,
        verified: bool
    }
)

;; Authorization
(define-private (is-chief-gemologist)
    (is-eq tx-sender (var-get chief-gemologist)))

;; Data Validation Functions
(define-private (is-valid-hash (hash (buff 32)))
    (> (len hash) u0))

(define-private (is-valid-description (desc (string-utf8 256)))
    (> (len desc) u0))

(define-private (is-valid-credentials (cred uint))
    (>= cred MIN-CREDENTIALS-REQUIRED))

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
        (gemologist-profile (unwrap! (map-get? gemologist-profiles tx-sender) ERR-INSUFFICIENT-CREDENTIALS))
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
        
        ;; Check gemologist has enough credentials to register gemstone
        (asserts! (>= (get credentials gemologist-profile) (var-get minimum-credentials-threshold)) ERR-INSUFFICIENT-CREDENTIALS)
        
        ;; Set the gemstone data
        (map-set rare-gemstones gemstone-id
            {
                gemstone-name: gemstone-name,
                description: description,
                certificate-hash: certificate-hash,
                origin-region: origin-region,
                open-for-assessments: true,
                registrar: tx-sender,
                verified-assessments: u0
            })
        
        ;; Update gemologist profile
        (map-set gemologist-profiles tx-sender
            (merge gemologist-profile {
                gemstones-registered: (unwrap! (as-max-len? 
                    (append (get gemstones-registered gemologist-profile) gemstone-id) u30)
                    ERR-INVALID-PARAMETER)
            }))
        
        (ok true)))

;; Gemologist Registration Functions
(define-public (register-gemologist (initial-credentials uint))
    (begin
        (asserts! (var-get registry-operational) ERR-REGISTRY-OFFLINE)
        
        ;; Validate credentials input
        (asserts! (is-valid-credentials initial-credentials) ERR-INVALID-PARAMETER)
        
        ;; Require some credentials token transfer (simplified for demonstration)
        (try! (stx-transfer? initial-credentials tx-sender (var-get chief-gemologist)))
        
        ;; Initialize gemologist profile with validated credentials
        (map-set gemologist-profiles tx-sender
            {
                credentials: initial-credentials,
                gemstones-registered: (list),
                assessments-submitted: (list)
            })
            
        (ok true)))

;; Assessment Submission
(define-public (submit-assessment
    (assessment-id uint)
    (target-gemstone-id uint)
    (description (string-utf8 256))
    (analysis-hash (buff 32)))
    (let (
        (gemstone (unwrap! (map-get? rare-gemstones target-gemstone-id) ERR-INVALID-GEMSTONE))
        (gemologist (unwrap! (map-get? gemologist-profiles tx-sender) ERR-INSUFFICIENT-CREDENTIALS))
        )
        
        ;; Check registry status
        (asserts! (var-get registry-operational) ERR-REGISTRY-OFFLINE)
        
        ;; Check if gemstone is accepting assessments
        (asserts! (get open-for-assessments gemstone) ERR-GEMSTONE-LOCKED)
        
        ;; Check that assessment ID doesn't already exist
        (asserts! (is-none (map-get? gemstone-assessments assessment-id)) ERR-INVALID-PARAMETER)
        
        ;; Validate description and hash
        (asserts! (is-valid-description description) ERR-INVALID-PARAMETER)
        (asserts! (is-valid-hash analysis-hash) ERR-INVALID-PARAMETER)
        
        ;; Record the assessment
        (map-set gemstone-assessments assessment-id
            {
                description: description,
                analysis-hash: analysis-hash,
                gemologist: tx-sender,
                target-gemstone: target-gemstone-id,
                verified: false
            })
        
        ;; Update gemologist's submitted assessments
        (map-set gemologist-profiles tx-sender
            (merge gemologist {
                assessments-submitted: (unwrap! (as-max-len? 
                    (append (get assessments-submitted gemologist) assessment-id) u30)
                    ERR-INVALID-PARAMETER)
            }))
        
        (ok true)))

;; Chief Gemologist Approve Assessment
(define-public (approve-assessment (assessment-id uint))
    (let (
        (assessment (unwrap! (map-get? gemstone-assessments assessment-id) ERR-ASSESSMENT-NOT-FOUND))
        (gemstone (unwrap! (map-get? rare-gemstones (get target-gemstone assessment)) ERR-INVALID-GEMSTONE))
        )
        
        ;; Only chief gemologist can approve assessments
        (asserts! (is-chief-gemologist) ERR-NOT-AUTHORIZED)
        (asserts! (var-get registry-operational) ERR-REGISTRY-OFFLINE)
        
        ;; Ensure assessment hasn't already been verified
        (asserts! (not (get verified assessment)) ERR-GEMSTONE-LOCKED)
        
        ;; Update assessment status
        (map-set gemstone-assessments assessment-id
            (merge assessment {verified: true}))
        
        ;; Update gemstone data
        (map-set rare-gemstones (get target-gemstone assessment)
            (merge gemstone {
                certificate-hash: (get analysis-hash assessment),
                verified-assessments: (+ (get verified-assessments gemstone) u1)
            }))
        
        ;; Reward gemologist with credentials (simplified)
        (let ((gemologist-profile (unwrap! (map-get? gemologist-profiles (get gemologist assessment)) ERR-INVALID-PARAMETER)))
            (map-set gemologist-profiles (get gemologist assessment)
                (merge gemologist-profile {
                    credentials: (+ (get credentials gemologist-profile) u25)
                })))
        
        (ok true)))

;; Change Gemstone Assessment Status
(define-public (set-gemstone-assessment-status (gemstone-id uint) (open bool))
    (let (
        (gemstone (unwrap! (map-get? rare-gemstones gemstone-id) ERR-INVALID-GEMSTONE))
        )
        
        ;; Check registry status
        (asserts! (var-get registry-operational) ERR-REGISTRY-OFFLINE)
        
        ;; Only registrar or chief gemologist can change status
        (asserts! (or (is-eq tx-sender (get registrar gemstone)) (is-chief-gemologist)) ERR-NOT-AUTHORIZED)
        
        ;; Update gemstone status
        (map-set rare-gemstones gemstone-id
            (merge gemstone {open-for-assessments: open}))
        
        (ok true)))

;; Read-only functions
(define-read-only (get-gemstone-details (gemstone-id uint))
    (map-get? rare-gemstones gemstone-id))

(define-read-only (get-gemologist-profile (gemologist principal))
    (map-get? gemologist-profiles gemologist))

(define-read-only (get-assessment-details (assessment-id uint))
    (map-get? gemstone-assessments assessment-id))

(define-read-only (get-registry-metrics)
    {
        operational: (var-get registry-operational),
        minimum-credentials: (var-get minimum-credentials-threshold)
    })

(define-public (update-minimum-credentials (new-minimum uint))
    (begin
        (asserts! (is-chief-gemologist) ERR-NOT-CHIEF-GEMOLOGIST)
        ;; Validate new threshold is within acceptable range
        (asserts! (>= new-minimum MIN-CREDENTIALS-REQUIRED) ERR-INVALID-PARAMETER)
        (var-set minimum-credentials-threshold new-minimum)
        (ok true)))

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