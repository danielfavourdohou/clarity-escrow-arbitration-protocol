;; dao.clar
;; Arbiter staking and governance for decentralized escrow & arbitration protocol

;; Import traits
(use-trait escrow-trait .traits.escrow-trait)
(use-trait token-trait .traits.token-trait)

;; Implement DAO trait
(impl-trait .traits.dao-trait)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED u3001)
(define-constant ERR-INSUFFICIENT-STAKE u3002)
(define-constant ERR-ALREADY-STAKED u3003)
(define-constant ERR-NOT-STAKED u3004)
(define-constant ERR-LOCKUP-ACTIVE u3005)
(define-constant ERR-PROPOSAL-NOT-FOUND u3006)
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED u3007)
(define-constant ERR-MINIMUM-STAKE-TOO-LOW u3008)
(define-constant ERR-INVALID-AMOUNT u3009)
(define-constant ERR-SLASHING-FAILED u3010)

;; Data definitions
(define-data-var contract-owner principal tx-sender)
(define-data-var minimum-stake uint u1000000000) ;; 1000 STX
(define-data-var lockup-period uint u1008) ;; ~7 days in blocks (144 blocks/day)
(define-data-var next-proposal-id uint u1)
(define-data-var slashing-percent uint u100) ;; 10% represented as basis points
(define-data-var treasury-address principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)

;; Proposal states
(define-constant PROPOSAL-PENDING u1)
(define-constant PROPOSAL-EXECUTED u2)

;; Arbiter staking data
(define-map staked-arbiters
  principal
  {
    stake-amount: uint,
    staked-at: uint,
    lockup-until: uint
  }
)

;; Proposal data
(define-map proposals
  uint
  {
    escrow-id: uint,
    winner: principal,
    state: uint,
    created-at: uint,
    executed-at: uint
  }
)

;; Public getters
(define-read-only (get-minimum-stake)
  (var-get minimum-stake)
)

(define-read-only (get-lockup-period)
  (var-get lockup-period)
)

(define-read-only (get-arbiter-info (arbiter principal))
  (map-get? staked-arbiters arbiter)
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-slashing-percent)
  (var-get slashing-percent)
)

(define-read-only (is-staked-arbiter (address principal))
  (ok (is-some (map-get? staked-arbiters address)))
)

;; Authorization check
(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; Stake tokens to become an arbiter
(define-public (stake-tokens (token-contract <token-trait>) (amount uint))
  (let
    (
      (current-time (contract-call? .utils get-current-block-height))
      (lockup-end (+ current-time (var-get lockup-period)))
      (existing-stake (map-get? staked-arbiters tx-sender))
    )

    ;; Check amount meets minimum
    (asserts! (>= amount (var-get minimum-stake)) (err ERR-INSUFFICIENT-STAKE))

    ;; Check not already staked
    (asserts! (is-none existing-stake) (err ERR-ALREADY-STAKED))

    ;; Transfer ARB tokens from user to contract
    (try! (contract-call? token-contract transfer amount tx-sender (as-contract tx-sender)))

    ;; Record stake
    (map-set staked-arbiters tx-sender {
      stake-amount: amount,
      staked-at: current-time,
      lockup-until: lockup-end
    })

    ;; Log event
    (print { event: "arbiter-staked", arbiter: tx-sender, amount: amount, lockup-until: lockup-end })

    (ok true)
  )
)

;; Unstake tokens (after lockup period)
(define-public (unstake-tokens (token-contract <token-trait>))
  (let
    (
      (stake-info (unwrap! (map-get? staked-arbiters tx-sender) (err ERR-NOT-STAKED)))
      (current-time (contract-call? .utils get-current-block-height))
    )

    ;; Check lockup period has ended
    (asserts! (>= current-time (get lockup-until stake-info)) (err ERR-LOCKUP-ACTIVE))

    ;; Transfer ARB tokens back to user
    (try! (as-contract (contract-call? token-contract transfer
                                      (get stake-amount stake-info)
                                      tx-sender
                                      tx-sender)))

    ;; Remove staking record
    (map-delete staked-arbiters tx-sender)

    ;; Log event
    (print { event: "arbiter-unstaked", arbiter: tx-sender, amount: (get stake-amount stake-info) })

    (ok true)
  )
)

;; Slash tokens from a malicious arbiter
(define-public (slash-arbiter (token-contract <token-trait>) (arbiter principal))
  (let
    (
      (stake-info (unwrap! (map-get? staked-arbiters arbiter) (err ERR-NOT-STAKED)))
      (slash-amount (/ (* (get stake-amount stake-info) (var-get slashing-percent)) u1000))
      (remaining-stake (- (get stake-amount stake-info) slash-amount))
    )

    ;; Check authorization - only contract owner can slash
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))

    ;; Update stake amount
    (map-set staked-arbiters arbiter (merge stake-info { stake-amount: remaining-stake }))

    ;; Transfer slashed tokens to treasury
    (try! (as-contract (contract-call? token-contract transfer
                                      slash-amount
                                      tx-sender
                                      (var-get treasury-address))))

    ;; Log event
    (print {
      event: "arbiter-slashed",
      arbiter: arbiter,
      slash-amount: slash-amount,
      remaining-stake: remaining-stake
    })

    (ok true)
  )
)

;; Create a proposal for dispute resolution
(define-public (create-proposal (escrow-id uint) (winner principal))
  (let
    (
      (proposal-id (var-get next-proposal-id))
      (current-time (contract-call? .utils get-current-block-height))
    )

    ;; Create proposal record
    (map-set proposals proposal-id {
      escrow-id: escrow-id,
      winner: winner,
      state: PROPOSAL-PENDING,
      created-at: current-time,
      executed-at: u0
    })

    ;; Increment next proposal ID
    (var-set next-proposal-id (+ proposal-id u1))

    ;; Execute proposal immediately - in more complex DAO implementations, this might wait for voting
    (try! (execute-proposal proposal-id))

    ;; Log event
    (print { event: "proposal-created", proposal-id: proposal-id, escrow-id: escrow-id, winner: winner })

    (ok proposal-id)
  )
)

;; Execute a proposal
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) (err ERR-PROPOSAL-NOT-FOUND)))
      (current-time (contract-call? .utils get-current-block-height))
    )

    ;; Check proposal state
    (asserts! (is-eq (get state proposal) PROPOSAL-PENDING) (err ERR-PROPOSAL-ALREADY-EXECUTED))

    ;; Update proposal state
    (map-set proposals proposal-id (merge proposal {
      state: PROPOSAL-EXECUTED,
      executed-at: current-time
    }))

    ;; Log event
    (print { event: "proposal-executed", proposal-id: proposal-id })

    (ok true)
  )
)

;; Execute a proposal with escrow contract
(define-public (execute-proposal-with-escrow (escrow-contract <escrow-trait>) (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) (err ERR-PROPOSAL-NOT-FOUND)))
    )

    ;; Execute via escrow contract
    (try! (contract-call? escrow-contract execute-proposal
                         proposal-id
                         (get escrow-id proposal)
                         (get winner proposal)))

    (ok true)
  )
)

;; Admin functions
(define-public (set-minimum-stake (new-minimum uint))
  (begin
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (asserts! (> new-minimum u0) (err ERR-MINIMUM-STAKE-TOO-LOW))
    (var-set minimum-stake new-minimum)
    (ok true)
  )
)

(define-public (set-lockup-period (new-period uint))
  (begin
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (var-set lockup-period new-period)
    (ok true)
  )
)

(define-public (set-slashing-percent (new-percent uint))
  (begin
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (asserts! (<= new-percent u1000) (err ERR-INVALID-AMOUNT))
    (var-set slashing-percent new-percent)
    (ok true)
  )
)

(define-public (set-treasury (new-treasury principal))
  (begin
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (var-set treasury-address new-treasury)
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