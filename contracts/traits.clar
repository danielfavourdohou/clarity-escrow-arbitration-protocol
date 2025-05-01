;; traits.clar
;; Shared traits for escrow & arbitration protocol

;; Governance trait for DAO interaction
(define-trait governance-trait
  (
    ;; Execute a proposal that has been approved
    (execute-proposal (uint uint principal) (response bool uint))
  )
)

;; Escrow trait for arbitration interaction
(define-trait escrow-trait
  (
    ;; Get escrow details
    (get-escrow (uint) (response {
      sender: principal,
      receiver: principal,
      amount: uint,
      state: uint,
      created-at: uint,
      timeout: uint,
      description: (string-ascii 256),
      protocol-fee: uint
    } uint))
    
    ;; Execute a proposal to resolve a dispute
    (execute-proposal (uint uint principal) (response bool uint))
  )
)

;; Arbitration trait for escrow interaction
(define-trait arbitration-trait
  (
    ;; Register a new dispute
    (register-dispute (uint) (response bool uint))
  )
)

;; DAO trait for arbitration interaction
(define-trait dao-trait
  (
    ;; Check if an address is a staked arbiter
    (is-staked-arbiter (principal) (response bool uint))
    
    ;; Create a proposal for dispute resolution
    (create-proposal (uint principal) (response uint uint))
  )
)

;; Token trait for rewards and staking
(define-trait token-trait
  (
    ;; Transfer tokens
    (transfer (uint principal principal) (response bool uint))
    
    ;; Mint reward tokens for arbiter participation
    (mint-reward (principal) (response bool uint))
  )
)
