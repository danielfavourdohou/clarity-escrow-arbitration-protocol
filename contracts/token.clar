;; token.clar
;; ARB fungible token for staking & fees

;; SIP-010 interface for fungible tokens
(impl-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED u4001)
(define-constant ERR-NOT-FOUND u4002)
(define-constant ERR-MINT-FAILED u4003)
(define-constant ERR-TRANSFER-FAILED u4004)
(define-constant ERR-BURN-FAILED u4005)
(define-constant ERR-INSUFFICIENT-BALANCE u4006)

;; Data definitions
(define-data-var contract-owner principal tx-sender)
(define-data-var token-name (string-ascii 32) "Arbiter Token")
(define-data-var token-symbol (string-ascii 10) "ARB")
(define-data-var token-decimals uint u6)
(define-data-var token-uri (optional (string-utf8 256)) none)
(define-data-var total-supply uint u0)
(define-data-var arbiter-reward uint u100000000) ;; 100 ARB tokens per vote

;; Token balances
(define-map token-balances principal uint)

;; Token allowances
(define-map token-allowances
  { owner: principal, spender: principal }
  uint
)

;; Authorization check
(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; Get token balance
(define-private (get-balance (owner principal))
  (default-to u0 (map-get? token-balances owner))
)

;; SIP-010 implementation
(define-read-only (get-name)
  (var-get token-name)
)

(define-read-only (get-symbol)
  (var-get token-symbol)
)

(define-read-only (get-decimals)
  (var-get token-decimals)
)

(define-read-only (get-total-supply)
  (var-get total-supply)
)

(define-read-only (get-token-uri)
  (var-get token-uri)
)

(define-read-only (get-balance-of (owner principal))
  (ok (get-balance owner))
)

(define-read-only (get-allowance (owner principal) (spender principal))
  (ok (default-to u0 (map-get? token-allowances { owner: owner, spender: spender })))
)

;; Transfer tokens
(define-public (transfer (amount uint) (sender principal) (recipient principal))
  (let
    (
      (sender-balance (get-balance sender))
    )
    ;; Check authorization
    (asserts! (or (is-eq tx-sender sender) 
                 (is-eq tx-sender (as-contract tx-sender)))
            (err ERR-NOT-AUTHORIZED))
    
    ;; Check for sufficient balance
    (asserts! (>= sender-balance amount) (err ERR-INSUFFICIENT-BALANCE))
    
    ;; Update balances
    (map-set token-balances sender (- sender-balance amount))
    (map-set token-balances recipient (+ (get-balance recipient) amount))
    
    ;; Log transfer event
    (print { event: "token-transfer", sender: sender, recipient: recipient, amount: amount })
    
    (ok true)
  )
)

;; Transfer from another account
(define-public (transfer-from (amount uint) (owner principal) (recipient principal))
  (let
    (
      (owner-balance (get-balance owner))
      (allowance-key { owner: owner, spender: tx-sender })
      (allowance (default-to u0 (map-get? token-allowances allowance-key)))
    )
    
    ;; Check for sufficient allowance
    (asserts! (>= allowance amount) (err ERR-NOT-AUTHORIZED))
    
    ;; Check for sufficient balance
    (asserts! (>= owner-balance amount) (err ERR-INSUFFICIENT-BALANCE))
    
    ;; Update balances
    (map-set token-balances owner (- owner-balance amount))
    (map-set token-balances recipient (+ (get-balance recipient) amount))
    
    ;; Update allowance
    (map-set token-allowances allowance-key (- allowance amount))
    
    ;; Log transfer-from event
    (print { event: "token-transfer-from", owner: owner, spender: tx-sender, recipient: recipient, amount: amount })
    
    (ok true)
  )
)

;; Set allowance for a spender
(define-public (set-allowance (spender principal) (amount uint))
  (let
    (
      (allowance-key { owner: tx-sender, spender: spender })
    )
    
    ;; Set allowance
    (map-set token-allowances allowance-key amount)
    
    ;; Log allowance event
    (print { event: "token-approve", owner: tx-sender, spender: spender, amount: amount })
    
    (ok true)
  )
)

;; Mint new tokens
(define-public (mint (amount uint) (recipient principal))
  (begin
    ;; Check authorization - only contract owner can mint
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    
    ;; Update total supply
    (var-set total-supply (+ (var-get total-supply) amount))
    
    ;; Update recipient balance
    (map-set token-balances recipient (+ (get-balance recipient) amount))
    
    ;; Log mint event
    (print { event: "token-mint", recipient: recipient, amount: amount })
    
    (ok true)
  )
)

;; Mint reward tokens for arbiter participation
(define-public (mint-reward (arbiter principal))
  (let
    (
      (reward-amount (var-get arbiter-reward))
    )
    
    ;; Check authorization - only arbitration contract can give rewards
    (asserts! (is-eq contract-caller .arbitration) (err ERR-NOT-AUTHORIZED))
    
    ;; Update total supply
    (var-set total-supply (+ (var-get total-supply) reward-amount))
    
    ;; Update arbiter balance
    (map-set token-balances arbiter (+ (get-balance arbiter) reward-amount))
    
    ;; Log reward event
    (print { event: "arbiter-reward", arbiter: arbiter, amount: reward-amount })
    
    (ok true)
  )
)

;; Burn tokens
(define-public (burn (amount uint))
  (let
    (
      (sender-balance (get-balance tx-sender))
    )
    
    ;; Check for sufficient balance
    (asserts! (>= sender-balance amount) (err ERR-INSUFFICIENT-BALANCE))
    
    ;; Update total supply
    (var-set total-supply (- (var-get total-supply) amount))
    
    ;; Update sender balance
    (map-set token-balances tx-sender (- sender-balance amount))
    
    ;; Log burn event
    (print { event: "token-burn", sender: tx-sender, amount: amount })
    
    (ok true)
  )
)

;; Admin functions
(define-public (set-token-uri (new-uri (optional (string-utf8 256))))
  (begin
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (var-set token-uri new-uri)
    (ok true)
  )
)

(define-public (set-arbiter-reward (new-reward uint))
  (begin
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (var-set arbiter-reward new-reward)
    (ok true)
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (var-set contract-owner new-owner)
    (ok true)
  )
)

;; Initial token supply for testing and bootstrapping
(define-public (initial-mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (asserts! (is-eq (var-get total-supply) u0) (err ERR-NOT-AUTHORIZED))
    (var-set total-supply amount)
    (map-set token-balances recipient amount)
    (print { event: "initial-mint", recipient: recipient, amount: amount })
    (ok true)
  )
)