;; Reputation System Smart Contract
;; Implements user reputation scores, verification credentials, trust metrics, and community standing

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_USER_NOT_FOUND (err u101))
(define-constant ERR_INVALID_RATING (err u102))
(define-constant ERR_SELF_RATING (err u103))
(define-constant ERR_CREDENTIAL_EXISTS (err u104))
(define-constant ERR_CREDENTIAL_NOT_FOUND (err u105))
(define-constant ERR_INVALID_THRESHOLD (err u106))
(define-constant ERR_USER_EXISTS (err u107))

;; Define data maps

;; User profile with reputation metrics
(define-map user-profiles
  { user-id: principal }
  {
    reputation-score: uint,       ;; Overall reputation score (0-1000)
    trust-score: uint,            ;; Trust metric (0-100)
    community-standing: uint,     ;; Community standing (0-100)
    total-ratings-received: uint, ;; Number of ratings received
    activity-score: uint,         ;; Activity level (0-100)
    join-height: uint,            ;; Block height when user joined
    last-update: uint             ;; Block height of last update
  }
)

;; Verification credentials for users
(define-map user-credentials
  { user-id: principal, credential-type: (string-ascii 64) }
  {
    verified: bool,               ;; Whether credential is verified
    verifier: principal,          ;; Who verified this credential
    verification-height: uint,    ;; Block height of verification
    expiration-height: uint,      ;; Block height when credential expires (0 = no expiration)
    credential-score: uint        ;; Impact on reputation (0-100)
  }
)

;; Store individual ratings between users
(define-map user-ratings
  { rater: principal, rated-user: principal }
  {
    rating-value: uint,           ;; Rating value (0-10)
    rating-type: (string-ascii 20), ;; Type of rating ("trust", "skill", "reliability", etc)
    comment-hash: (buff 32),      ;; Hash of comment (stored off-chain)
    rating-height: uint           ;; Block height when rating was given
  }
)

;; Community thresholds for status levels
(define-map community-thresholds
  { threshold-id: (string-ascii 20) }
  {
    reputation-required: uint,    ;; Minimum reputation score needed
    trust-required: uint,         ;; Minimum trust score needed
    credentials-required: (list 10 (string-ascii 64)), ;; Required credential types
    activity-required: uint       ;; Minimum activity score needed
  }
)

;; System configuration
(define-map system-config
  { config-id: (string-ascii 20) }
  {
    decay-rate: uint,             ;; Rate at which scores decay over time (blocks)
    max-rating-weight: uint,      ;; Maximum weight of a single rating
    admin-list: (list 10 principal) ;; List of system administrators
  }
)

;; Functions for user registration and management

;; Register a new user
(define-public (register-user)
  (let ((user tx-sender))
    (if (is-some (map-get? user-profiles { user-id: user }))
        ERR_USER_EXISTS
        (begin
          (map-set user-profiles
            { user-id: user }
            {
              reputation-score: u500,       ;; Start at neutral score
              trust-score: u50,             ;; Start with neutral trust
              community-standing: u50,      ;; Start with neutral standing
              total-ratings-received: u0,   ;; No ratings yet
              activity-score: u10,          ;; Start with low activity
              join-height: stacks-block-height,    ;; Current block
              last-update: stacks-block-height     ;; Current block
            }
          )
          (ok true)
        )
    )
  )
)

;; Function for admins to register a user (for integrations)
(define-public (admin-register-user (user principal))
  (begin
    (asserts! (is-admin tx-sender) ERR_NOT_AUTHORIZED)
    (if (is-some (map-get? user-profiles { user-id: user }))
        ERR_USER_EXISTS
        (begin
          (map-set user-profiles
            { user-id: user }
            {
              reputation-score: u500,       ;; Start at neutral score
              trust-score: u50,             ;; Start with neutral trust
              community-standing: u50,      ;; Start with neutral standing
              total-ratings-received: u0,   ;; No ratings yet
              activity-score: u10,          ;; Start with low activity
              join-height: stacks-block-height,    ;; Current block
              last-update: stacks-block-height     ;; Current block
            }
          )
          (ok true)
        )
    )
  )
)

;; Check if principal is an admin
(define-private (is-admin (user principal))
  (if (is-eq user CONTRACT_OWNER)
      true
      (match (map-get? system-config { config-id: "main" })
        config (is-in-admin-list user (get admin-list config))
        false
      )
  )
)

(define-private (is-in-admin-list (user principal) (admins (list 10 principal)))
  (match (index-of admins user)
    index true
    false
  )
)

;; Submit a rating for another user
(define-public (submit-rating 
                (rated-user principal) 
                (rating-value uint) 
                (rating-type (string-ascii 20))
                (comment-hash (buff 32)))
  (let ((rater tx-sender))
    ;; Check that user is not rating themselves
    (asserts! (not (is-eq rater rated-user)) ERR_SELF_RATING)
    
    ;; Check rating is in valid range (0-10)
    (asserts! (<= rating-value u10) ERR_INVALID_RATING)
    
    ;; Check both users exist in the system
    (asserts! (is-some (map-get? user-profiles { user-id: rater })) ERR_USER_NOT_FOUND)
    (asserts! (is-some (map-get? user-profiles { user-id: rated-user })) ERR_USER_NOT_FOUND)
    
    ;; Record the rating
    (map-set user-ratings
      { rater: rater, rated-user: rated-user }
      {
        rating-value: rating-value,
        rating-type: rating-type,
        comment-hash: comment-hash,
        rating-height: stacks-block-height
      }
    )
    
    ;; Update user's reputation scores
    (update-reputation rated-user)
  )
)

;; Update a user's reputation based on all their ratings
(define-private (update-reputation (user principal))
  (match (map-get? user-profiles { user-id: user })
    user-profile
      (let (
        ;; Calculate new scores based on ratings, credentials, etc.
        (new-reputation (calculate-reputation-score user user-profile))
        (new-trust (calculate-trust-score user user-profile))
        (new-standing (calculate-community-standing user user-profile))
        (new-activity (calculate-activity-score user user-profile))
        (total-ratings (calculate-total-ratings user))
      )
        ;; Update the user profile with new scores
        (map-set user-profiles
          { user-id: user }
          {
            reputation-score: new-reputation,
            trust-score: new-trust,
            community-standing: new-standing,
            total-ratings-received: total-ratings,
            activity-score: new-activity,
            join-height: (get join-height user-profile),
            last-update: stacks-block-height
          }
        )
        (ok true)
      )
    ERR_USER_NOT_FOUND
  )
)

;; Calculate total number of ratings received by a user
(define-private (calculate-total-ratings (user principal))
  ;; In a production contract, this would query and count all ratings
  ;; For this example, we'll increment the current count
  (match (map-get? user-profiles { user-id: user })
    user-profile (+ (get total-ratings-received user-profile) u1)
    u0
  )
)

;; Calculate reputation score based on ratings and credentials
(define-private (calculate-reputation-score (user principal) (profile { 
                                                                 reputation-score: uint,
                                                                 trust-score: uint,
                                                                 community-standing: uint,
                                                                 total-ratings-received: uint,
                                                                 activity-score: uint,
                                                                 join-height: uint,
                                                                 last-update: uint }))
  ;; In a production contract, this would implement a comprehensive algorithm
  ;; For this example, we'll just adjust the existing score upward by 10 points (capped at 1000)
  (let ((current-score (get reputation-score profile)))
    (if (>= current-score u990)
        u1000
        (+ current-score u10))
  )
)

;; Calculate trust score based on user interactions
(define-private (calculate-trust-score (user principal) (profile { 
                                                           reputation-score: uint,
                                                           trust-score: uint,
                                                           community-standing: uint,
                                                           total-ratings-received: uint,
                                                           activity-score: uint,
                                                           join-height: uint,
                                                           last-update: uint }))
  ;; In a production contract, this would be a complex formula
  ;; For this example, we'll do a simple adjustment based on current trust (capped at 100)
  (let ((current-trust (get trust-score profile)))
    (if (>= current-trust u95)
        u100
        (+ current-trust u5))
  )
)

;; Calculate community standing based on activity and credentials
(define-private (calculate-community-standing (user principal) (profile { 
                                                                   reputation-score: uint,
                                                                   trust-score: uint,
                                                                   community-standing: uint,
                                                                   total-ratings-received: uint,
                                                                   activity-score: uint,
                                                                   join-height: uint,
                                                                   last-update: uint }))
  ;; In a production contract, this would consider community contributions
  ;; For this example, we'll use a simple formula (capped at 100)
  (let ((current-standing (get community-standing profile)))
    (if (>= current-standing u98)
        u100
        (+ current-standing u2))
  )
)

;; Calculate activity score based on user participation
(define-private (calculate-activity-score (user principal) (profile { 
                                                              reputation-score: uint,
                                                              trust-score: uint,
                                                              community-standing: uint,
                                                              total-ratings-received: uint,
                                                              activity-score: uint,
                                                              join-height: uint,
                                                              last-update: uint }))
  ;; In a production contract, this would track recent user actions
  ;; For this example, we'll do a simple increment (capped at 100)
  (let ((current-activity (get activity-score profile)))
    (if (>= current-activity u95)
        u100
        (+ current-activity u5))
  )
)

;; Verification credential functions

;; Issue a verification credential to a user
(define-public (issue-credential (user principal) 
                                (credential-type (string-ascii 64))
                                (credential-score uint)
                                (expiration-blocks uint))
  (begin
    ;; Only admins can issue credentials
    (asserts! (is-admin tx-sender) ERR_NOT_AUTHORIZED)
    
    ;; Check user exists
    (asserts! (is-some (map-get? user-profiles { user-id: user })) ERR_USER_NOT_FOUND)
    
    ;; Check credential score is valid (0-100)
    (asserts! (<= credential-score u100) ERR_INVALID_RATING)
    
    ;; Calculate expiration (0 means no expiration)
    (let ((expiration (if (is-eq expiration-blocks u0)
                         u0
                         (+ stacks-block-height expiration-blocks))))
      
      ;; Set the credential
      (map-set user-credentials
        { user-id: user, credential-type: credential-type }
        {
          verified: true,
          verifier: tx-sender,
          verification-height: stacks-block-height,
          expiration-height: expiration,
          credential-score: credential-score
        }
      )
      
      ;; Update reputation after new credential
      (update-reputation user)
    )
  )
)

;; Revoke a verification credential
(define-public (revoke-credential (user principal) (credential-type (string-ascii 64)))
  (begin
    ;; Only admins can revoke credentials
    (asserts! (is-admin tx-sender) ERR_NOT_AUTHORIZED)
    
    ;; Check credential exists
    (match (map-get? user-credentials { user-id: user, credential-type: credential-type })
      credential 
        (begin
          ;; Delete the credential
          (map-delete user-credentials { user-id: user, credential-type: credential-type })
          
          ;; Update reputation after credential removal
          (update-reputation user)
        )
      ERR_CREDENTIAL_NOT_FOUND
    )
  )
)

;; Community threshold management

;; Set a community threshold level
(define-public (set-threshold (threshold-id (string-ascii 20))
                             (reputation-required uint)
                             (trust-required uint)
                             (credentials-required (list 10 (string-ascii 64)))
                             (activity-required uint))
  (begin
    ;; Only admins can set thresholds
    (asserts! (is-admin tx-sender) ERR_NOT_AUTHORIZED)
    
    ;; Validate inputs
    (asserts! (<= reputation-required u1000) ERR_INVALID_THRESHOLD)
    (asserts! (<= trust-required u100) ERR_INVALID_THRESHOLD)
    (asserts! (<= activity-required u100) ERR_INVALID_THRESHOLD)
    
    ;; Set the threshold
    (map-set community-thresholds
      { threshold-id: threshold-id }
      {
        reputation-required: reputation-required,
        trust-required: trust-required,
        credentials-required: credentials-required,
        activity-required: activity-required
      }
    )
    (ok true)
  )
)

;; System configuration

;; Set system configuration
(define-public (set-system-config (decay-rate uint)
                                 (max-rating-weight uint)
                                 (admin-list (list 10 principal)))
  (begin
    ;; Only contract owner can set system config
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    ;; Set the config
    (map-set system-config
      { config-id: "main" }
      {
        decay-rate: decay-rate,
        max-rating-weight: max-rating-weight,
        admin-list: admin-list
      }
    )
    (ok true)
  )
)

;; Read-only functions

;; Get a user's profile with reputation metrics
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user-id: user })
)

;; Get a specific credential for a user
(define-read-only (get-user-credential (user principal) (credential-type (string-ascii 64)))
  (map-get? user-credentials { user-id: user, credential-type: credential-type })
)

;; Check if a rating exists between two users
(define-read-only (get-user-rating (rater principal) (rated-user principal))
  (map-get? user-ratings { rater: rater, rated-user: rated-user })
)

;; Get a community threshold
(define-read-only (get-threshold (threshold-id (string-ascii 20)))
  (map-get? community-thresholds { threshold-id: threshold-id })
)

;; Check if a user meets a specific threshold
(define-read-only (check-user-threshold (user principal) (threshold-id (string-ascii 20)))
  (let ((user-prof (map-get? user-profiles { user-id: user }))
        (threshold (map-get? community-thresholds { threshold-id: threshold-id })))
    (if (and (is-some user-prof) (is-some threshold))
        (let ((prof (unwrap! user-prof false))
              (thresh (unwrap! threshold false)))
          (and 
            (>= (get reputation-score prof) (get reputation-required thresh))
            (>= (get trust-score prof) (get trust-required thresh))
            (>= (get activity-score prof) (get activity-required thresh))
            (check-required-credentials user (get credentials-required thresh))
          )
        )
        false
    )
  )
)

;; Helper to check if user has all required credentials
(define-private (check-required-credentials (user principal) (required-credentials (list 10 (string-ascii 64))))
  (fold check-credential required-credentials true)
)

;; Helper to check if user has a specific credential
(define-private (check-credential (credential-type (string-ascii 64)) (previous-result bool))
  (if previous-result
      (match (map-get? user-credentials { user-id: tx-sender, credential-type: credential-type })
        cred (and 
               (get verified cred)
               (or 
                 (is-eq (get expiration-height cred) u0)
                 (> (get expiration-height cred) stacks-block-height)
               )
             )
        false
      )
      false
  )
)

;; Get system configuration
(define-read-only (get-system-config)
  (map-get? system-config { config-id: "main" })
)