;; FairTrace - Fair Trade Verification Platform
;; A blockchain-based platform for transparent supply chain tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-input (err u104))
(define-constant err-insufficient-funds (err u105))

;; Data Variables
(define-data-var product-counter uint u0)
(define-data-var verification-counter uint u0)
(define-data-var validator-reward uint u100) ;; STX tokens per verification

;; Data Maps

;; Product registry with supply chain data
(define-map products
  { product-id: uint }
  {
    farmer: principal,
    product-name: (string-ascii 100),
    origin-location: (string-ascii 200),
    timestamp: uint,
    quality-score: uint,
    carbon-footprint: uint,
    is-verified: bool,
    premium-amount: uint
  }
)

;; Product journey tracking
(define-map product-journey
  { product-id: uint, step: uint }
  {
    location: (string-ascii 200),
    handler: principal,
    timestamp: uint,
    gps-coordinates: (string-ascii 100),
    temperature: uint,
    humidity: uint
  }
)

;; Community validators
(define-map validators
  { validator: principal }
  {
    total-verifications: uint,
    reputation-score: uint,
    is-active: bool,
    joined-date: uint
  }
)

;; Verification records
(define-map verifications
  { verification-id: uint }
  {
    product-id: uint,
    validator: principal,
    timestamp: uint,
    verification-type: (string-ascii 50),
    status: bool,
    notes: (string-ascii 500)
  }
)

;; Cooperative governance
(define-map cooperatives
  { cooperative-id: principal }
  {
    name: (string-ascii 100),
    member-count: uint,
    total-funds: uint,
    is-active: bool
  }
)

;; Project proposals for community voting
(define-map proposals
  { proposal-id: uint }
  {
    cooperative: principal,
    title: (string-ascii 200),
    description: (string-ascii 1000),
    funding-required: uint,
    votes-for: uint,
    votes-against: uint,
    is-active: bool,
    created-at: uint
  }
)

;; Voter records to prevent double voting
(define-map votes
  { proposal-id: uint, voter: principal }
  { has-voted: bool, vote-for: bool }
)

;; Read-only functions

(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

(define-read-only (get-product-journey (product-id uint) (step uint))
  (map-get? product-journey { product-id: product-id, step: step })
)

(define-read-only (get-validator (validator principal))
  (map-get? validators { validator: validator })
)

(define-read-only (get-verification (verification-id uint))
  (map-get? verifications { verification-id: verification-id })
)

(define-read-only (get-cooperative (cooperative-id principal))
  (map-get? cooperatives { cooperative-id: cooperative-id })
)

(define-read-only (get-product-count)
  (ok (var-get product-counter))
)

(define-read-only (get-validator-reward)
  (ok (var-get validator-reward))
)

;; Public functions

;; Register a new product in the supply chain
(define-public (register-product 
  (product-name (string-ascii 100))
  (origin-location (string-ascii 200))
  (quality-score uint)
  (carbon-footprint uint))
  (let
    (
      (product-id (+ (var-get product-counter) u1))
    )
    (asserts! (> (len product-name) u0) err-invalid-input)
    (asserts! (<= quality-score u100) err-invalid-input)
    
    (map-set products
      { product-id: product-id }
      {
        farmer: tx-sender,
        product-name: product-name,
        origin-location: origin-location,
        timestamp: block-height,
        quality-score: quality-score,
        carbon-footprint: carbon-footprint,
        is-verified: false,
        premium-amount: u0
      }
    )
    (var-set product-counter product-id)
    (ok product-id)
  )
)

;; Add a journey step to product tracking
(define-public (add-journey-step
  (product-id uint)
  (step uint)
  (location (string-ascii 200))
  (gps-coordinates (string-ascii 100))
  (temperature uint)
  (humidity uint))
  (let
    (
      (product (unwrap! (get-product product-id) err-not-found))
    )
    (map-set product-journey
      { product-id: product-id, step: step }
      {
        location: location,
        handler: tx-sender,
        timestamp: block-height,
        gps-coordinates: gps-coordinates,
        temperature: temperature,
        humidity: humidity
      }
    )
    (ok true)
  )
)

;; Register as a community validator
(define-public (register-validator)
  (let
    (
      (existing-validator (get-validator tx-sender))
    )
    (asserts! (is-none existing-validator) err-already-exists)
    
    (map-set validators
      { validator: tx-sender }
      {
        total-verifications: u0,
        reputation-score: u100,
        is-active: true,
        joined-date: block-height
      }
    )
    (ok true)
  )
)

;; Submit a verification for a product
(define-public (submit-verification
  (product-id uint)
  (verification-type (string-ascii 50))
  (status bool)
  (notes (string-ascii 500)))
  (let
    (
      (verification-id (+ (var-get verification-counter) u1))
      (product (unwrap! (get-product product-id) err-not-found))
      (validator-data (unwrap! (get-validator tx-sender) err-unauthorized))
    )
    (asserts! (get is-active validator-data) err-unauthorized)
    
    ;; Record verification
    (map-set verifications
      { verification-id: verification-id }
      {
        product-id: product-id,
        validator: tx-sender,
        timestamp: block-height,
        verification-type: verification-type,
        status: status,
        notes: notes
      }
    )
    
    ;; Update product verification status
    (map-set products
      { product-id: product-id }
      (merge product { is-verified: status })
    )
    
    ;; Update validator stats
    (map-set validators
      { validator: tx-sender }
      (merge validator-data {
        total-verifications: (+ (get total-verifications validator-data) u1)
      })
    )
    
    (var-set verification-counter verification-id)
    (ok verification-id)
  )
)

;; Set fair trade premium for a product
(define-public (set-premium (product-id uint) (premium-amount uint))
  (let
    (
      (product (unwrap! (get-product product-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get farmer product)) err-unauthorized)
    
    (map-set products
      { product-id: product-id }
      (merge product { premium-amount: premium-amount })
    )
    (ok true)
  )
)

;; Register a cooperative
(define-public (register-cooperative
  (name (string-ascii 100))
  (member-count uint))
  (let
    (
      (existing-coop (get-cooperative tx-sender))
    )
    (asserts! (is-none existing-coop) err-already-exists)
    (asserts! (> member-count u0) err-invalid-input)
    
    (map-set cooperatives
      { cooperative-id: tx-sender }
      {
        name: name,
        member-count: member-count,
        total-funds: u0,
        is-active: true
      }
    )
    (ok true)
  )
)

;; Create a proposal for cooperative funding
(define-public (create-proposal
  (proposal-id uint)
  (title (string-ascii 200))
  (description (string-ascii 1000))
  (funding-required uint))
  (let
    (
      (coop (unwrap! (get-cooperative tx-sender) err-unauthorized))
    )
    (asserts! (get is-active coop) err-unauthorized)
    (asserts! (> funding-required u0) err-invalid-input)
    
    (map-set proposals
      { proposal-id: proposal-id }
      {
        cooperative: tx-sender,
        title: title,
        description: description,
        funding-required: funding-required,
        votes-for: u0,
        votes-against: u0,
        is-active: true,
        created-at: block-height
      }
    )
    (ok true)
  )
)

;; Vote on a proposal
(define-public (vote-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
      (existing-vote (map-get? votes { proposal-id: proposal-id, voter: tx-sender }))
    )
    (asserts! (is-none existing-vote) err-already-exists)
    (asserts! (get is-active proposal) err-invalid-input)
    
    ;; Record vote
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      { has-voted: true, vote-for: vote-for }
    )
    
    ;; Update proposal vote counts
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal {
        votes-for: (if vote-for (+ (get votes-for proposal) u1) (get votes-for proposal)),
        votes-against: (if vote-for (get votes-against proposal) (+ (get votes-against proposal) u1))
      })
    )
    (ok true)
  )
)

;; Update validator reward amount (owner only)
(define-public (update-validator-reward (new-reward uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set validator-reward new-reward)
    (ok true)
  )
)