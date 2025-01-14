
;; title: reputation-system
;; version:
;; summary:
;; description:

;; Reputation System Smart Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))

;; Define data maps
(define-map UserReputation
  { user: principal }
  {
    reputation-score: uint,
    verification-status: bool,
    trust-metric: uint,
    last-updated: uint
  }
)

;; Define read-only functions

(define-read-only (get-user-reputation (user principal))
  (match (map-get? UserReputation { user: user })
    entry (ok entry)
    err-not-found
  )
)

(define-read-only (get-community-standing (user principal))
  (match (map-get? UserReputation { user: user })
    entry (ok (+ 
                (get reputation-score entry)
                (if (get verification-status entry) u10 u0)
                (get trust-metric entry)
              ))
    err-not-found
  )
)

;; Define public functions

(define-public (initialize-user (user principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set UserReputation
      { user: user }
      {
        reputation-score: u0,
        verification-status: false,
        trust-metric: u0,
        last-updated: block-height
      }
    ))
  )
)

(define-public (update-reputation-score (user principal) (new-score uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (match (map-get? UserReputation { user: user })
      entry (ok (map-set UserReputation
        { user: user }
        (merge entry { 
          reputation-score: new-score,
          last-updated: block-height
        })
      ))
      err-not-found
    )
  )
)

(define-public (update-verification-status (user principal) (status bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (match (map-get? UserReputation { user: user })
      entry (ok (map-set UserReputation
        { user: user }
        (merge entry { 
          verification-status: status,
          last-updated: block-height
        })
      ))
      err-not-found
    )
  )
)

(define-public (update-trust-metric (user principal) (new-metric uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (match (map-get? UserReputation { user: user })
      entry (ok (map-set UserReputation
        { user: user }
        (merge entry { 
          trust-metric: new-metric,
          last-updated: block-height
        })
      ))
      err-not-found
    )
  )
)