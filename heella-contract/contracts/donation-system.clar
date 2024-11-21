
;; title: donation-system
;; version:
;; summary:
;; description:

;; Define the contract owner
(define-constant contract-owner tx-sender)

;; Define the fungible token for donations
(define-fungible-token donation-token)

;; Define the non-fungible token for donor recognition
(define-non-fungible-token donor-nft uint)

;; Map to store donation records
(define-map donation-records
  { donor: principal }
  { total-donated: uint, last-donation: uint, donation-count: uint })

;; Counter for NFT IDs
(define-data-var nft-id-counter uint u0)

;; Function to make a donation
(define-public (make-donation (amount uint))
  (let
    (
      (donor tx-sender)
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (begin
      ;; Transfer donation tokens from donor to contract
      (try! (ft-transfer? donation-token amount donor (as-contract tx-sender)))
      
      ;; Update donation records
      (map-set donation-records
        { donor: donor }
        (merge
          (default-to
            { total-donated: u0, last-donation: u0, donation-count: u0 }
            (map-get? donation-records { donor: donor })
          )
          {
            total-donated: (+ (default-to u0 (get total-donated (map-get? donation-records { donor: donor }))) amount),
            last-donation: current-time,
            donation-count: (+ (default-to u0 (get donation-count (map-get? donation-records { donor: donor }))) u1)
          }
        )
      )
      
      ;; Mint donor recognition NFT if it's their first donation
      (if (is-eq (default-to u0 (get donation-count (map-get? donation-records { donor: donor }))) u1)
        (mint-donor-nft donor)
        (ok true)
      )
    
    )
  )
)

;; Function to mint donor recognition NFT
(define-private (mint-donor-nft (donor principal))
  (let
    ((new-id (+ (var-get nft-id-counter) u1)))
    (begin
      (try! (nft-mint? donor-nft new-id donor))
      (var-set nft-id-counter new-id)
      (ok true)
    )
  )
)

;; Function to get donation record for a donor
(define-read-only (get-donation-record (donor principal))
  (map-get? donation-records { donor: donor })
)

;; Function to get total donations received
(define-read-only (get-total-donations)
  (ft-get-balance donation-token (as-contract tx-sender))
)

;; Function to check if a donor has an NFT
(define-read-only (has-donor-nft (donor principal))
  (is-some (nft-get-owner? donor-nft (unwrap-panic (get donation-count (map-get? donation-records { donor: donor })))))
)

;; Initialize the contract
(begin
  ;; Mint initial supply of donation tokens to contract owner
  (try! (ft-mint? donation-token u1000000000 contract-owner))
)