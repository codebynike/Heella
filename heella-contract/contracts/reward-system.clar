;; Title: Charity Rewards Smart Contract
;; Version: 1.0.0
;; Description: A smart contract that manages token rewards and NFT badges for community participation
;; and charitable donations. Users can earn tokens and badges for various actions.

;; Define traits locally
(define-trait ft-trait
    (
        (transfer (uint principal principal (optional (buff 34))) (response bool uint))
        (get-balance (principal) (response uint uint))
        (get-decimals () (response uint uint))
        (get-name () (response (string-ascii 32) uint))
        (get-symbol () (response (string-ascii 32) uint))
        (get-total-supply () (response uint uint))
    ))

(define-trait badge-trait
    (
        (mint-nft (principal uint (string-ascii 256)) (response bool uint))
        (get-token-uri (uint) (response (string-ascii 256) uint))
        (get-owner (uint) (response principal uint))
        (transfer (uint principal principal) (response bool uint))
    ))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-TRANSFER-FAILED (err u2))
(define-constant ERR-INVALID-AMOUNT (err u3))
(define-constant ERR-INVALID-ACTION (err u4))
(define-constant ERR-BADGE-EXISTS (err u5))
(define-constant ERR-INSUFFICIENT-POINTS (err u6))
(define-constant ERR-NO-BADGE-CONTRACT (err u7))

;; Data Variables
(define-data-var charity-address principal CONTRACT-OWNER)
(define-data-var total-donations uint u0)
(define-data-var token-contract (optional principal) none)
(define-data-var badge-contract (optional principal) none)
(define-data-var next-badge-id uint u1)

;; Data Maps
(define-map allowed-actions {action: (string-ascii 256)}
    {
        reward-amount: uint,
        points: uint,
        badge-threshold: uint,
        badge-uri: (string-ascii 256)
    })

(define-map user-stats principal 
    {
        total-points: uint,
        actions-completed: uint,
        badges-earned: uint
    })

(define-map earned-badges {user: principal, badge-id: uint} bool)

;; Initialize allowed actions
(begin
    (map-set allowed-actions {action: "post"} 
        {
            reward-amount: u10,
            points: u5,
            badge-threshold: u100,
            badge-uri: "ipfs://QmPost100Badge"
        })
    (map-set allowed-actions {action: "comment"} 
        {
            reward-amount: u5,
            points: u2,
            badge-threshold: u50,
            badge-uri: "ipfs://QmComment50Badge"
        })
    (map-set allowed-actions {action: "attend-workshop"} 
        {
            reward-amount: u15,
            points: u10,
            badge-threshold: u150,
            badge-uri: "ipfs://QmWorkshop150Badge"
        }))

;; Private Functions

(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT-OWNER))

(define-private (get-action-details (action (string-ascii 256)))
    (map-get? allowed-actions {action: action}))

(define-private (get-user-stats (user principal))
    (default-to 
        {total-points: u0, actions-completed: u0, badges-earned: u0}
        (map-get? user-stats user)))

(define-private (update-user-stats (user principal) (points uint))
    (let (
        (current-stats (get-user-stats user))
        (new-total-points (+ (get total-points current-stats) points))
        (new-actions-completed (+ (get actions-completed current-stats) u1))
    )
    (map-set user-stats user
        {
            total-points: new-total-points,
            actions-completed: new-actions-completed,
            badges-earned: (get badges-earned current-stats)
        })))

(define-private (check-and-mint-badge (user principal) (action (string-ascii 256)))
    (let (
        (action-details (unwrap! (get-action-details action) ERR-INVALID-ACTION))
        (user-points (get total-points (get-user-stats user)))
        (badge-threshold (get badge-threshold action-details))
        (badge-uri (get badge-uri action-details))
        (badge-contract-opt (var-get badge-contract))
    )
    (if (and 
        (>= user-points badge-threshold)
        (is-none (map-get? earned-badges {user: user, badge-id: (var-get next-badge-id)}))
        (is-some badge-contract-opt)
    )
    (let (
        (badge-contract-principal (unwrap! badge-contract-opt ERR-NO-BADGE-CONTRACT))
    )
        (begin
            ;; Update user stats to increment badges earned
            (map-set user-stats user
                (merge (get-user-stats user)
                    {badges-earned: (+ (get badges-earned (get-user-stats user)) u1)}
                )
            )
            ;; Record the earned badge
            (map-set earned-badges {user: user, badge-id: (var-get next-badge-id)} true)
            ;; Increment the next badge ID
            (var-set next-badge-id (+ (var-get next-badge-id) u1))
            (ok true)))
    (ok false))))

;; Public Functions

(define-public (set-token-contract (new-token-contract <ft-trait>))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (ok (var-set token-contract (some (contract-of new-token-contract))))))

(define-public (set-badge-contract (new-badge-contract <badge-trait>))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (ok (var-set badge-contract (some (contract-of new-badge-contract))))))

(define-public (reward-participant (token <ft-trait>) (user principal) (action (string-ascii 256)))
    (let (
        (action-details (unwrap! (get-action-details action) ERR-INVALID-ACTION))
        (reward-amount (get reward-amount action-details))
        (points (get points action-details))
    )
    (begin
        (try! (contract-call? token transfer reward-amount tx-sender user none))
        (update-user-stats user points)
        (try! (check-and-mint-badge user action))
        (ok reward-amount))))

(define-public (donate-to-charity (token <ft-trait>) (amount uint))
    (let (
        (charity (var-get charity-address))
    )
    (begin
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (match (contract-call? token transfer amount tx-sender charity none)
            success 
            (begin
                (var-set total-donations (+ (var-get total-donations) amount))
                (update-user-stats tx-sender u20)  ;; Bonus points for donation
                (ok amount))
            error ERR-TRANSFER-FAILED))))

;; Read-only Functions

(define-read-only (get-user-achievements (user principal))
    (ok (get-user-stats user)))

(define-read-only (get-action-rewards (action (string-ascii 256)))
    (ok (get-action-details action)))

(define-read-only (has-earned-badge (user principal) (badge-id uint))
    (ok (default-to false (map-get? earned-badges {user: user, badge-id: badge-id}))))

(define-read-only (get-total-badges-earned (user principal))
    (ok (get badges-earned (get-user-stats user))))