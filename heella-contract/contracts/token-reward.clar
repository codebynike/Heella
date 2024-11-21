;; title: token-reward
;; version: 1.0.0
;; summary: Token reward system for platform users
;; description: Implements a native platform token with distribution and transfer capabilities.
;;              Includes operator approval system for delegated transfers and minting controls.


;; token definitions
(define-fungible-token reward-token)

;; constants
(define-constant contract-owner tx-sender)
(define-constant token-name "Platform Reward Token")
(define-constant token-symbol "PRT")
(define-constant token-decimals u6)

;; error constants
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-recipient (err u102))
(define-constant err-invalid-amount (err u103))

;; data vars
(define-data-var total-supply uint u0)

;; data maps
(define-map balances principal uint)
(define-map operators {owner: principal, operator: principal} bool)

;; public functions

;; @desc Mint new tokens and assign them to a recipient
;; @param amount: quantity of tokens to mint
;; @param recipient: principal to receive the tokens
;; @return (response bool uint)
(define-public (mint (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> amount u0) err-invalid-amount)
        (map-set balances 
                 recipient 
                 (+ (get-balance recipient) amount))
        (var-set total-supply (+ (var-get total-supply) amount))
        (ok true)
    )
)

;; @desc Transfer tokens to a recipient
;; @param amount: quantity of tokens to transfer
;; @param recipient: principal to receive the tokens
;; @return (response bool uint)
(define-public (transfer (amount uint) (recipient principal))
    (begin
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (>= (get-balance tx-sender) amount) err-insufficient-balance)
        
        (map-set balances 
                 tx-sender 
                 (- (get-balance tx-sender) amount))
        (map-set balances 
                 recipient 
                 (+ (get-balance recipient) amount))
        (ok true)
    )
)

;; @desc Approve an operator to transfer tokens on behalf of the sender
;; @param operator: principal to approve
;; @return (response bool uint)
(define-public (approve-operator (operator principal))
    (begin
        (map-set operators 
                 {owner: tx-sender, operator: operator}
                 true)
        (ok true)
    )
)

;; @desc Revoke an operator's approval
;; @param operator: principal to revoke
;; @return (response bool uint)
(define-public (revoke-operator (operator principal))
    (begin
        (map-set operators 
                 {owner: tx-sender, operator: operator}
                 false)
        (ok true)
    )
)

;; @desc Transfer tokens from one principal to another (requires operator approval)
;; @param amount: quantity of tokens to transfer
;; @param sender: principal sending the tokens
;; @param recipient: principal receiving the tokens
;; @return (response bool uint)
(define-public (transfer-from (amount uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-approved-operator tx-sender sender) err-owner-only)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (>= (get-balance sender) amount) err-insufficient-balance)
        
        (map-set balances 
                 sender 
                 (- (get-balance sender) amount))
        (map-set balances 
                 recipient 
                 (+ (get-balance recipient) amount))
        (ok true)
    )
)

;; read only functions

;; @desc Get the balance of an account
;; @param account: principal to check
;; @return uint
(define-read-only (get-balance (account principal))
    (default-to u0 (map-get? balances account))
)

;; @desc Get the total supply of tokens
;; @return uint
(define-read-only (get-total-supply)
    (var-get total-supply)
)

;; @desc Check if an operator is approved for an owner
;; @param operator: principal to check
;; @param owner: principal who may have approved the operator
;; @return bool
(define-read-only (is-approved-operator (operator principal) (owner principal))
    (default-to false (map-get? operators {owner: owner, operator: operator}))
)

;; @desc Get token name
;; @return string-ascii
(define-read-only (get-name)
    token-name
)

;; @desc Get token symbol
;; @return string-ascii
(define-read-only (get-symbol)
    token-symbol
)

;; @desc Get token decimals
;; @return uint
(define-read-only (get-decimals)
    token-decimals
)

;; private functions

;; @desc Internal function to validate a transfer
;; @param sender: principal sending tokens
;; @param amount: quantity of tokens
;; @return bool
(define-private (can-transfer (sender principal) (amount uint))
    (and
        (> amount u0)
        (>= (get-balance sender) amount)
    )
)