;; DashRail - RailBeast Breeding & Racing Contract
;; A blockchain gaming ecosystem for virtual locomotive creatures

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-breed (err u104))
(define-constant err-insufficient-funds (err u105))

;; Breeding and race fees
(define-constant breeding-fee u1000000) ;; 1 STX in microSTX
(define-constant race-entry-fee u500000) ;; 0.5 STX

;; Data Variables
(define-data-var last-token-id uint u0)
(define-data-var total-races uint u0)

;; RailBeast NFT with genetic data
(define-non-fungible-token railbeast uint)

;; RailBeast data structure
(define-map railbeasts
  uint
  {
    owner: principal,
    dna: (buff 32),
    generation: uint,
    parent1: (optional uint),
    parent2: (optional uint),
    speed: uint,
    endurance: uint,
    cargo-capacity: uint,
    track-adaptation: uint,
    rarity: (string-ascii 20),
    wins: uint,
    races: uint,
    created-at: uint
  }
)

;; Race results tracking
(define-map race-history
  uint ;; race-id
  {
    winner: uint,
    participants: (list 10 uint),
    prize-pool: uint,
    timestamp: uint
  }
)

;; User statistics
(define-map user-stats
  principal
  {
    beasts-owned: uint,
    total-wins: uint,
    total-races: uint,
    rail-tokens-earned: uint
  }
)

;; Private Functions

;; Get maximum of two uints
(define-private (max-uint (a uint) (b uint))
  (if (>= a b) a b)
)

;; Generate pseudo-random DNA from parents
(define-private (generate-dna (parent1-dna (buff 32)) (parent2-dna (buff 32)) (block-hash (buff 32)))
  (let
    (
      (combined (concat parent1-dna parent2-dna))
      (hash-result (sha256 (concat combined block-hash)))
    )
    hash-result
  )
)

;; Calculate trait from DNA segment
(define-private (calculate-trait (dna (buff 32)) (offset uint))
  (let
    (
      (trait-byte (buff-to-uint-be (default-to 0x00 (element-at dna offset))))
    )
    (mod (+ trait-byte u1) u100)
  )
)

;; Determine rarity based on traits
(define-private (determine-rarity (speed uint) (endurance uint) (cargo uint) (adaptation uint))
  (let
    (
      (total-stats (+ (+ speed endurance) (+ cargo adaptation)))
    )
    (if (>= total-stats u350)
      "legendary"
      (if (>= total-stats u300)
        "epic"
        (if (>= total-stats u250)
          "rare"
          "common"
        )
      )
    )
  )
)

;; Public Functions

;; Mint genesis RailBeast
(define-public (mint-genesis (dna (buff 32)))
  (let
    (
      (token-id (+ (var-get last-token-id) u1))
      (speed (calculate-trait dna u0))
      (endurance (calculate-trait dna u8))
      (cargo (calculate-trait dna u16))
      (adaptation (calculate-trait dna u24))
      (rarity (determine-rarity speed endurance cargo adaptation))
    )
    (try! (nft-mint? railbeast token-id tx-sender))
    (map-set railbeasts token-id {
      owner: tx-sender,
      dna: dna,
      generation: u0,
      parent1: none,
      parent2: none,
      speed: speed,
      endurance: endurance,
      cargo-capacity: cargo,
      track-adaptation: adaptation,
      rarity: rarity,
      wins: u0,
      races: u0,
      created-at: block-height
    })
    (var-set last-token-id token-id)
    (ok token-id)
  )
)

;; Breed two RailBeasts
(define-public (breed-railbeasts (parent1-id uint) (parent2-id uint))
  (let
    (
      (parent1 (unwrap! (map-get? railbeasts parent1-id) err-not-found))
      (parent2 (unwrap! (map-get? railbeasts parent2-id) err-not-found))
      (token-id (+ (var-get last-token-id) u1))
      (new-dna (generate-dna (get dna parent1) (get dna parent2) (unwrap-panic (get-block-info? id-header-hash (- block-height u1)))))
      (speed (calculate-trait new-dna u0))
      (endurance (calculate-trait new-dna u8))
      (cargo (calculate-trait new-dna u16))
      (adaptation (calculate-trait new-dna u24))
      (rarity (determine-rarity speed endurance cargo adaptation))
      (new-gen (+ (max-uint (get generation parent1) (get generation parent2)) u1))
    )
    ;; Check ownership
    (asserts! (is-eq (get owner parent1) tx-sender) err-unauthorized)
    (asserts! (is-eq (get owner parent2) tx-sender) err-unauthorized)
    
    ;; Pay breeding fee
    (try! (stx-transfer? breeding-fee tx-sender contract-owner))
    
    ;; Mint new RailBeast
    (try! (nft-mint? railbeast token-id tx-sender))
    (map-set railbeasts token-id {
      owner: tx-sender,
      dna: new-dna,
      generation: new-gen,
      parent1: (some parent1-id),
      parent2: (some parent2-id),
      speed: speed,
      endurance: endurance,
      cargo-capacity: cargo,
      track-adaptation: adaptation,
      rarity: rarity,
      wins: u0,
      races: u0,
      created-at: block-height
    })
    (var-set last-token-id token-id)
    (ok token-id)
  )
)

;; Enter race
(define-public (enter-race (beast-id uint))
  (let
    (
      (beast (unwrap! (map-get? railbeasts beast-id) err-not-found))
    )
    ;; Check ownership
    (asserts! (is-eq (get owner beast) tx-sender) err-unauthorized)
    
    ;; Pay race entry fee
    (try! (stx-transfer? race-entry-fee tx-sender contract-owner))
    
    ;; Update race count
    (map-set railbeasts beast-id (merge beast { races: (+ (get races beast) u1) }))
    (ok true)
  )
)

;; Record race win (only contract owner can call)
(define-public (record-win (beast-id uint) (prize uint))
  (let
    (
      (beast (unwrap! (map-get? railbeasts beast-id) err-not-found))
      (owner (get owner beast))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Update beast stats
    (map-set railbeasts beast-id (merge beast { wins: (+ (get wins beast) u1) }))
    
    ;; Update user stats
    (match (map-get? user-stats owner)
      stats (map-set user-stats owner (merge stats {
        total-wins: (+ (get total-wins stats) u1),
        rail-tokens-earned: (+ (get rail-tokens-earned stats) prize)
      }))
      (map-set user-stats owner {
        beasts-owned: u1,
        total-wins: u1,
        total-races: u1,
        rail-tokens-earned: prize
      })
    )
    
    (ok true)
  )
)

;; Transfer RailBeast
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (let
    (
      (beast (unwrap! (map-get? railbeasts token-id) err-not-found))
    )
    (asserts! (is-eq tx-sender sender) err-unauthorized)
    (try! (nft-transfer? railbeast token-id sender recipient))
    (map-set railbeasts token-id (merge beast { owner: recipient }))
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-railbeast (token-id uint))
  (map-get? railbeasts token-id)
)

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? railbeast token-id))
)

(define-read-only (get-last-token-id)
  (ok (var-get last-token-id))
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats user)
)

(define-read-only (get-breeding-fee)
  (ok breeding-fee)
)

(define-read-only (get-race-fee)
  (ok race-entry-fee)
)