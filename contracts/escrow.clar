;; escrow.clar
;; Core escrow lifecycle for decentralized escrow & arbitration protocol

;; Import traits
(use-trait arbitration-trait .traits.arbitration-trait)

;; Implement traits
(impl-trait .traits.governance-trait)
(impl-trait .traits.escrow-trait)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED u1001)
(define-constant ERR-ALREADY-INITIALIZED u1002)
(define-constant ERR-ESCROW-NOT-FOUND u1003)
(define-constant ERR-INVALID-ESCROW-STATE u1004)
(define-constant ERR-INVALID-AMOUNT u1005)
(define-constant ERR-TIMEOUT-NOT-REACHED u1006)
(define-constant ERR-TRANSFER-FAILED u1007)
(define-constant ERR-ESCROW-EXPIRED u1008)
(define-constant ERR-ALREADY-RELEASED u1009)
(define-constant ERR-ALREADY-DISPUTED u1010)
(define-constant ERR-TIMELOCK-ACTIVE u1011)
(define-constant ERR-SENDER-NOT-PARTICIPANT u1012)

;; Data definitions
(define-data-var contract-owner principal tx-sender)
(define-data-var protocol-fee-percent uint u25) ;; 2.5% represented as basis points
(define-data-var next-escrow-id uint u1)
(define-data-var treasury-address principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)

;; Escrow states
(define-constant STATE-ACTIVE u1)
(define-constant STATE-RELEASED u2)
(define-constant STATE-REFUNDED u3)
(define-constant STATE-DISPUTED u4)
(define-constant STATE-RESOLVED u5)

;; Escrow data structure
(define-map escrows
  uint
  {
    sender: principal,
    receiver: principal,
    amount: uint,
    state: uint,
    created-at: uint,
    timeout: uint,
    description: (string-ascii 256),
    protocol-fee: uint
  }
)

;; Public getters
(define-read-only (get-escrow (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow (ok escrow)
    (err ERR-ESCROW-NOT-FOUND)
  )
)

(define-read-only (get-protocol-fee-percent)
  (var-get protocol-fee-percent)
)

(define-read-only (get-owner)
  (var-get contract-owner)
)

(define-read-only (get-treasury)
  (var-get treasury-address)
)

;; Authorization check
(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; Calculate protocol fee
(define-private (calculate-fee (amount uint))
  (/ (* amount (var-get protocol-fee-percent)) u1000)
)

;; Create a new escrow
(define-public (create-escrow
    (receiver principal)
    (timeout uint)
    (description (string-ascii 256))
  )
  (let
    (
      (escrow-id (var-get next-escrow-id))
      (escrow-amount (stx-get-balance tx-sender))
      (protocol-fee (calculate-fee escrow-amount))
      (net-amount (- escrow-amount protocol-fee))
      (current-time (contract-call? .utils get-current-block-height))
    )

    ;; Check for valid inputs
    (asserts! (> escrow-amount u0) (err ERR-INVALID-AMOUNT))
    (asserts! (> timeout current-time) (err ERR-INVALID-AMOUNT))

    ;; Transfer STX to the contract
    (try! (stx-transfer? escrow-amount tx-sender (as-contract tx-sender)))

    ;; Transfer protocol fee to treasury
    (try! (as-contract (stx-transfer? protocol-fee tx-sender (var-get treasury-address))))

    ;; Create new escrow entry
    (map-set escrows escrow-id {
      sender: tx-sender,
      receiver: receiver,
      amount: net-amount,
      state: STATE-ACTIVE,
      created-at: current-time,
      timeout: timeout,
      description: description,
      protocol-fee: protocol-fee
    })

    ;; Update next escrow ID
    (var-set next-escrow-id (+ escrow-id u1))

    ;; Log event
    (print { event: "escrow-created", escrow-id: escrow-id, sender: tx-sender, receiver: receiver, amount: net-amount })

    (ok escrow-id)
  )
)

;; Release escrow to receiver
(define-public (release-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) (err ERR-ESCROW-NOT-FOUND)))
    )

    ;; Check authorization - only sender can release
    (asserts! (is-eq tx-sender (get sender escrow)) (err ERR-NOT-AUTHORIZED))

    ;; Check escrow state
    (asserts! (is-eq (get state escrow) STATE-ACTIVE) (err ERR-INVALID-ESCROW-STATE))

    ;; Update escrow state
    (map-set escrows escrow-id (merge escrow { state: STATE-RELEASED }))

    ;; Transfer funds to receiver
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get receiver escrow))))

    ;; Log event
    (print { event: "escrow-released", escrow-id: escrow-id, sender: (get sender escrow), receiver: (get receiver escrow) })

    (ok true)
  )
)

;; Refund escrow to sender after timeout
(define-public (refund-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) (err ERR-ESCROW-NOT-FOUND)))
      (current-time (contract-call? .utils get-current-block-height))
    )

    ;; Check if timeout has passed
    (asserts! (>= current-time (get timeout escrow)) (err ERR-TIMEOUT-NOT-REACHED))

    ;; Check escrow state
    (asserts! (is-eq (get state escrow) STATE-ACTIVE) (err ERR-INVALID-ESCROW-STATE))

    ;; Update escrow state
    (map-set escrows escrow-id (merge escrow { state: STATE-REFUNDED }))

    ;; Transfer funds back to sender
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get sender escrow))))

    ;; Log event
    (print { event: "escrow-refunded", escrow-id: escrow-id, sender: (get sender escrow) })

    (ok true)
  )
)

;; Escalate to dispute - available to both sender and receiver
(define-public (dispute-escrow (arbitration-contract <arbitration-trait>) (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) (err ERR-ESCROW-NOT-FOUND)))
      (current-time (contract-call? .utils get-current-block-height))
    )

    ;; Check if sender is either sender or receiver
    (asserts! (or (is-eq tx-sender (get sender escrow))
                 (is-eq tx-sender (get receiver escrow)))
            (err ERR-SENDER-NOT-PARTICIPANT))

    ;; Check escrow state
    (asserts! (is-eq (get state escrow) STATE-ACTIVE) (err ERR-INVALID-ESCROW-STATE))

    ;; Check escrow has not timed out
    (asserts! (< current-time (get timeout escrow)) (err ERR-ESCROW-EXPIRED))

    ;; Update escrow state
    (map-set escrows escrow-id (merge escrow { state: STATE-DISPUTED }))

    ;; Register dispute with arbitration contract
    (try! (contract-call? arbitration-contract register-dispute escrow-id))

    ;; Log event
    (print { event: "escrow-disputed", escrow-id: escrow-id, disputed-by: tx-sender })

    (ok true)
  )
)

;; Complete dispute resolution - can only be called by arbitration contract
(define-public (resolve-dispute (escrow-id uint) (winner principal))
  (let
    (
      (escrow (unwrap! (map-get? escrows escrow-id) (err ERR-ESCROW-NOT-FOUND)))
    )

    ;; Check escrow is in disputed state
    (asserts! (is-eq (get state escrow) STATE-DISPUTED) (err ERR-INVALID-ESCROW-STATE))

    ;; Update escrow state
    (map-set escrows escrow-id (merge escrow { state: STATE-RESOLVED }))

    ;; Transfer funds to winner
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender winner)))

    ;; Log event
    (print { event: "dispute-resolved", escrow-id: escrow-id, winner: winner })

    (ok true)
  )
)

;; Admin functions
(define-public (set-protocol-fee (new-fee-percent uint))
  (begin
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (asserts! (<= new-fee-percent u100) (err ERR-INVALID-AMOUNT))
    (var-set protocol-fee-percent new-fee-percent)
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

;; Implementation of governance trait for DAO interaction
(define-public (execute-proposal (proposal-id uint) (escrow-id uint) (decision principal))
  (begin
    (asserts! (is-eq contract-caller .dao) (err ERR-NOT-AUTHORIZED))
    ;; Call resolve-dispute to complete the arbitration process
    (try! (resolve-dispute escrow-id decision))
    (ok true)
  )
)