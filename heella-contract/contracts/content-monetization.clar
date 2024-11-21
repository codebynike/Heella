
;; title: content-monetization
;; version:
;; summary:
;; description:

;; Define the contract owner
(define-constant contract-owner tx-sender)

;; Define the fungible token for content purchases
(define-fungible-token content-token)

;; Define the non-fungible token for content licenses
(define-non-fungible-token content-license {content-id: uint, user: principal})

;; Counter for content IDs
(define-data-var content-id-counter uint u0)

;; Map to store content information
(define-map content-info
  { content-id: uint }
  { creator: principal, price: uint, revenue: uint, is-premium: bool })

;; Map to store user subscriptions
(define-map user-subscriptions
  { user: principal }
  { expiration: uint })

;; Map to store creator revenue shares
(define-map creator-revenue-shares
  { creator: principal }
  { share: uint })

;; Constants for revenue sharing
(define-constant platform-fee-percent u20)
(define-constant creator-fee-percent u80)

;; Function to add new content
(define-public (add-content (price uint) (is-premium bool))
  (let
    ((new-content-id (+ (var-get content-id-counter) u1)))
    (begin
      (asserts! (is-eq tx-sender contract-owner) (err u100))
      (map-set content-info
        { content-id: new-content-id }
        { creator: tx-sender, price: price, revenue: u0, is-premium: is-premium }
      )
      (var-set content-id-counter new-content-id)
      (ok new-content-id)
    )
  )
)

;; Function to purchase content
(define-public (purchase-content (content-id uint))
  (let
    (
      (buyer tx-sender)
      (content (unwrap! (map-get? content-info { content-id: content-id }) (err u101)))
      (price (get price content))
      (creator (get creator content))
      (is-premium (get is-premium content))
    )
    (begin
      ;; Check if the content is premium and if the user has an active subscription
      (asserts! (or (not is-premium) (is-some (get-subscription-status buyer))) (err u102))
      
      ;; Transfer tokens from buyer to contract
      (try! (ft-transfer? content-token price buyer (as-contract tx-sender)))
      
      ;; Update content revenue
      (map-set content-info
        { content-id: content-id }
        (merge content { revenue: (+ (get revenue content) price) })
      )
      
      ;; Distribute revenue
      (let
        (
          (platform-fee (* price platform-fee-percent))
          (creator-fee (* price creator-fee-percent))
        )
        (begin
          ;; Transfer platform fee
          (try! (as-contract (ft-transfer? content-token platform-fee tx-sender contract-owner)))
          
          ;; Transfer creator fee
          (try! (as-contract (ft-transfer? content-token creator-fee tx-sender creator)))
          
          ;; Mint content license NFT
          (try! (nft-mint? content-license {content-id: content-id, user: buyer} buyer))
          
          (ok true)
        )
      )
    )
  )
)

;; Function to subscribe for premium content
(define-public (subscribe (duration uint))
  (let
    (
      (user tx-sender)
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (subscription-price (* duration u10)) ;; 10 tokens per time unit
      (expiration (+ current-time (* duration u144))) ;; Assuming 144 blocks per day
    )
    (begin
      ;; Transfer subscription fee
      (try! (ft-transfer? content-token subscription-price user (as-contract tx-sender)))
      
      ;; Update user subscription
      (map-set user-subscriptions
        { user: user }
        { expiration: expiration }
      )
      
      (ok expiration)
    )
  )
)

;; Function to get subscription status
(define-read-only (get-subscription-status (user principal))
  (let
    (
      (subscription (map-get? user-subscriptions { user: user }))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (if (and (is-some subscription) (> (get expiration (unwrap-panic subscription)) current-time))
      (some (get expiration (unwrap-panic subscription)))
      none
    )
  )
)

;; Function to check if a user has a license for specific content
(define-read-only (has-content-license (user principal) (content-id uint))
  (is-some (nft-get-owner? content-license {content-id: content-id, user: user}))
)

;; Function to get content information
(define-read-only (get-content-info (content-id uint))
  (map-get? content-info { content-id: content-id })
)

;; Function to set creator revenue share
(define-public (set-creator-revenue-share (creator principal) (share uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err u103))
    (asserts! (<= share u100) (err u104))
    (map-set creator-revenue-shares
      { creator: creator }
      { share: share }
    )
    (ok true)
  )
)

;; Function to get creator revenue share
(define-read-only (get-creator-revenue-share (creator principal))
  (default-to
    { share: creator-fee-percent }
    (map-get? creator-revenue-shares { creator: creator })
  )
)

;; Initialize the contract
(begin
  ;; Mint initial supply of content tokens to contract owner
  (try! (ft-mint? content-token u1000000000 contract-owner))
)