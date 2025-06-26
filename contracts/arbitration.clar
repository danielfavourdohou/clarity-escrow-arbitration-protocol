;; arbitration.clar
;; Dispute escalation & vote resolution for decentralized escrow & arbitration protocol

;; Import traits
(use-trait dao-trait .traits.dao-trait)
(use-trait escrow-trait .traits.escrow-trait)
(use-trait token-trait .traits.token-trait)

;; Implement traits
(impl-trait .traits.arbitration-trait)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED u2001)
(define-constant ERR-DISPUTE-NOT-FOUND u2002)
(define-constant ERR-DISPUTE-ALREADY-EXISTS u2003)
(define-constant ERR-DISPUTE-ALREADY-RESOLVED u2004)
(define-constant ERR-NOT-ENOUGH-ARBITERS u2005)
(define-constant ERR-VOTING-PERIOD-ACTIVE u2006)
(define-constant ERR-VOTING-PERIOD-EXPIRED u2007)
(define-constant ERR-ALREADY-VOTED u2008)
(define-constant ERR-NOT-ELIGIBLE-ARBITER u2009)
(define-constant ERR-INVALID-VOTE u2010)
(define-constant ERR-NO-DISPUTE-FOUND u2011)

;; Data definitions
(define-data-var contract-owner principal tx-sender)
(define-data-var voting-period uint u144) ;; ~24 hours in blocks (approx. 6 blocks/hour)
(define-data-var min-arbiters-required uint u3) ;; Minimum arbiters needed for valid resolution
(define-data-var arbiter-reward-percent uint u50) ;; 5% reward for arbiters (basis points)

;; Vote options
(define-constant VOTE-SENDER u1)
(define-constant VOTE-RECEIVER u2)

;; Dispute states
(define-constant DISPUTE-ACTIVE u1)
(define-constant DISPUTE-RESOLVED u2)

;; Dispute data structure
(define-map disputes
  uint ;; escrow ID
  {
    state: uint,
    created-at: uint,
    voting-ends-at: uint,
    sender-votes: uint,
    receiver-votes: uint,
    total-votes: uint,
    resolved-at: uint,
    winner: (optional principal)
  }
)

;; Track arbiter votes to prevent double voting
(define-map arbiter-votes
  {dispute-id: uint, arbiter: principal}
  {voted: bool, vote: uint}
)

;; Public getters
(define-read-only (get-dispute (escrow-id uint))
  (map-get? disputes escrow-id)
)

(define-read-only (get-arbiter-vote (escrow-id uint) (arbiter principal))
  (map-get? arbiter-votes {dispute-id: escrow-id, arbiter: arbiter})
)

(define-read-only (get-voting-period)
  (var-get voting-period)
)

(define-read-only (get-min-arbiters-required)
  (var-get min-arbiters-required)
)

(define-read-only (get-arbiter-reward-percent)
  (var-get arbiter-reward-percent)
)

;; Authorization check
(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; Register a new dispute
(define-public (register-dispute (escrow-id uint))
  (let
    (
      (current-time (contract-call? .utils get-current-block-height))
      (voting-end-time (+ current-time (var-get voting-period)))
      (existing-dispute (map-get? disputes escrow-id))
    )

    ;; Check caller is escrow contract
    (asserts! (is-eq contract-caller .escrow) (err ERR-NOT-AUTHORIZED))

    ;; Check dispute doesn't already exist
    (asserts! (is-none existing-dispute) (err ERR-DISPUTE-ALREADY-EXISTS))

    ;; Create dispute record
    (map-set disputes escrow-id {
      state: DISPUTE-ACTIVE,
      created-at: current-time,
      voting-ends-at: voting-end-time,
      sender-votes: u0,
      receiver-votes: u0,
      total-votes: u0,
      resolved-at: u0,
      winner: none
    })

    ;; Log event
    (print { event: "dispute-registered", escrow-id: escrow-id, voting-ends-at: voting-end-time })

    (ok true)
  )
)

;; Vote on a dispute
(define-public (vote-on-dispute (dao-contract <dao-trait>) (token-contract <token-trait>) (escrow-contract <escrow-trait>) (escrow-id uint) (vote-for uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes escrow-id) (err ERR-DISPUTE-NOT-FOUND)))
      (current-time (contract-call? .utils get-current-block-height))
      (escrow-data (unwrap! (contract-call? escrow-contract get-escrow escrow-id) (err ERR-NO-DISPUTE-FOUND)))
      (voter-info {dispute-id: escrow-id, arbiter: tx-sender})
      (previous-vote (map-get? arbiter-votes voter-info))
    )

    ;; Check if dispute is active
    (asserts! (is-eq (get state dispute) DISPUTE-ACTIVE) (err ERR-DISPUTE-ALREADY-RESOLVED))

    ;; Check if voting period is valid
    (asserts! (>= (get voting-ends-at dispute) current-time) (err ERR-VOTING-PERIOD-EXPIRED))

    ;; Check if voter is staked arbiter
    (asserts! (unwrap! (contract-call? dao-contract is-staked-arbiter tx-sender) (err ERR-NOT-ELIGIBLE-ARBITER)) (err ERR-NOT-ELIGIBLE-ARBITER))

    ;; Check if arbiter has not voted already
    (asserts! (is-none previous-vote) (err ERR-ALREADY-VOTED))

    ;; Check vote is valid
    (asserts! (or (is-eq vote-for VOTE-SENDER) (is-eq vote-for VOTE-RECEIVER)) (err ERR-INVALID-VOTE))

    ;; Record vote
    (map-set arbiter-votes voter-info {voted: true, vote: vote-for})

    ;; Update dispute vote counts
    (if (is-eq vote-for VOTE-SENDER)
      (map-set disputes escrow-id (merge dispute {
        sender-votes: (+ (get sender-votes dispute) u1),
        total-votes: (+ (get total-votes dispute) u1)
      }))
      (map-set disputes escrow-id (merge dispute {
        receiver-votes: (+ (get receiver-votes dispute) u1),
        total-votes: (+ (get total-votes dispute) u1)
      }))
    )

    ;; Log event
    (print { event: "dispute-vote-cast", escrow-id: escrow-id, arbiter: tx-sender, vote: vote-for })

    ;; Reward arbiter for participation
    (try! (contract-call? token-contract mint-reward tx-sender))

    (ok true)
  )
)

;; Finalize dispute - can be called by any arbiter after voting period
(define-public (finalize-dispute (dao-contract <dao-trait>) (escrow-contract <escrow-trait>) (escrow-id uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes escrow-id) (err ERR-DISPUTE-NOT-FOUND)))
      (current-time (contract-call? .utils get-current-block-height))
      (escrow-data (unwrap! (contract-call? escrow-contract get-escrow escrow-id) (err ERR-NO-DISPUTE-FOUND)))
    )

    ;; Check if dispute is active
    (asserts! (is-eq (get state dispute) DISPUTE-ACTIVE) (err ERR-DISPUTE-ALREADY-RESOLVED))

    ;; Check if voting period has ended
    (asserts! (< (get voting-ends-at dispute) current-time) (err ERR-VOTING-PERIOD-ACTIVE))

    ;; Check minimum number of votes
    (asserts! (>= (get total-votes dispute) (var-get min-arbiters-required)) (err ERR-NOT-ENOUGH-ARBITERS))

    ;; Determine winner
    (let
      (
        (sender (get sender escrow-data))
        (receiver (get receiver escrow-data))
        (sender-votes (get sender-votes dispute))
        (receiver-votes (get receiver-votes dispute))
        (winning-principal (if (>= sender-votes receiver-votes) sender receiver))
      )

      ;; Update dispute as resolved
      (map-set disputes escrow-id (merge dispute {
        state: DISPUTE-RESOLVED,
        resolved-at: current-time,
        winner: (some winning-principal)
      }))

      ;; Execute DAO proposal to resolve the dispute
      (try! (contract-call? dao-contract create-proposal escrow-id winning-principal))

      ;; Log event
      (print {
        event: "dispute-finalized",
        escrow-id: escrow-id,
        winner: winning-principal,
        sender-votes: sender-votes,
        receiver-votes: receiver-votes
      })

      (ok winning-principal)
    )
  )
)

;; Admin functions
(define-public (set-voting-period (new-period uint))
  (begin
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (var-set voting-period new-period)
    (ok true)
  )
)

(define-public (set-min-arbiters (new-min uint))
  (begin
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (var-set min-arbiters-required new-min)
    (ok true)
  )
)

(define-public (set-arbiter-reward (new-percent uint))
  (begin
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (var-set arbiter-reward-percent new-percent)
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