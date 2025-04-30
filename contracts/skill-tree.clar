;; skill-tree
;; A skill tree progression system for clarity-questhive
;; Users can invest experience points to unlock special abilities, increased rewards, and features
;; The skill tree offers different specialization paths matching various lifestyle goals (fitness, productivity, learning, etc.)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-SKILL (err u101))
(define-constant ERR-ALREADY-UNLOCKED (err u102))
(define-constant ERR-INSUFFICIENT-XP (err u103))
(define-constant ERR-PREREQUISITES-NOT-MET (err u104))
(define-constant ERR-INVALID-PATH (err u105))

;; Constants for skill paths
(define-constant PATH-FITNESS "fitness")
(define-constant PATH-PRODUCTIVITY "productivity")
(define-constant PATH-LEARNING "learning")
(define-constant PATH-CREATIVITY "creativity")
(define-constant PATH-WELLNESS "wellness")

;; Data maps and variables

;; Stores user progress in the skill tree
;; Maps user principal to a map of skill IDs they've unlocked
(define-map user-skills principal (list 50 uint))

;; Stores user's experience points
(define-map user-xp principal uint)

;; Skill definitions
;; Maps skill ID to its details
(define-map skills uint {
  name: (string-ascii 50),
  description: (string-ascii 255),
  xp-cost: uint,
  path: (string-ascii 20),
  level: uint,
  prerequisites: (list 5 uint)
})

;; Next available skill ID for adding new skills
(define-data-var next-skill-id uint u1)

;; Tracks available specialization paths
(define-data-var specialization-paths (list 10 (string-ascii 20)) (list PATH-FITNESS PATH-PRODUCTIVITY PATH-LEARNING PATH-CREATIVITY PATH-WELLNESS))

;; Contract owner for admin functions
(define-data-var contract-owner principal tx-sender)

;; Private functions

;; Check if the sender is the contract owner
(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; Check if a skill exists
(define-private (skill-exists? (skill-id uint))
  (is-some (map-get? skills skill-id))
)

;; Check if a user has a specific skill unlocked
(define-private (has-skill? (user principal) (skill-id uint))
  (if (is-none (map-get? user-skills user))
    false
    (is-some (index-of (unwrap-panic (map-get? user-skills user)) skill-id))
  )
)

;; Check if a user meets all prerequisites for a skill
(define-private (prerequisites-met? (user principal) (skill-id uint))
  (let (
    (skill-info (unwrap! (map-get? skills skill-id) false))
    (prerequisites (get prerequisites skill-info))
  )
    (is-eq (len prerequisites) 
      (fold check-prerequisite prerequisites u0))
  )
)

;; Helper for prerequisites-met? to count satisfied prerequisites
(define-private (check-prerequisite (prereq-id uint) (count uint))
  (if (has-skill? tx-sender prereq-id)
    (+ count u1)
    count
  )
)

;; Read-only functions

;; Get all available specialization paths
(define-read-only (get-specialization-paths)
  (var-get specialization-paths)
)

;; Get user's current XP
(define-read-only (get-user-xp (user principal))
  (default-to u0 (map-get? user-xp user))
)

;; Get all skills unlocked by a user
(define-read-only (get-user-skills (user principal))
  (default-to (list) (map-get? user-skills user))
)

;; Get details about a specific skill
(define-read-only (get-skill-details (skill-id uint))
  (map-get? skills skill-id)
)

;; Get all skills for a specific specialization path
(define-read-only (get-path-skills (path (string-ascii 20)))
  (let (
    (skill-count (var-get next-skill-id))
  )
    (filter-skills-by-path path skill-count)
  )
)

;; Helper function to filter skills by path
(define-private (filter-skills-by-path (path (string-ascii 20)) (skill-count uint))
  (filter is-path-match (generate-skill-id-list skill-count))
  
  ;; Check if a skill ID belongs to the specified path
  (define-private (is-path-match (skill-id uint))
    (let (
      (skill-info (map-get? skills skill-id))
    )
      (and
        (is-some skill-info)
        (is-eq path (get path (unwrap! skill-info false)))
      )
    )
  )
  
  ;; Generate a list of skill IDs from 1 to skill-count
  (define-private (generate-skill-id-list (count uint))
    (map generate-id (list-range-index u1 count u1))
  )
  
  ;; Helper to convert list index to ID
  (define-private (generate-id (id uint))
    id
  )
)

;; Check if user has enough XP to unlock a skill
(define-read-only (can-unlock-skill? (user principal) (skill-id uint))
  (let (
    (user-current-xp (get-user-xp user))
    (skill-info (map-get? skills skill-id))
  )
    (if (is-none skill-info)
      false
      (and
        (>= user-current-xp (get xp-cost (unwrap-panic skill-info)))
        (prerequisites-met? user skill-id)
        (not (has-skill? user skill-id))
      )
    )
  )
)

;; Public functions

;; Add experience points to a user
;; Note: In a real app, this would likely be controlled by another contract that awards XP
(define-public (add-experience (user principal) (amount uint))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (let (
      (current-xp (get-user-xp user))
    )
      (map-set user-xp user (+ current-xp amount))
      (ok true)
    )
  )
)

;; Allow a user to unlock a skill using their XP
(define-public (unlock-skill (skill-id uint))
  (let (
    (user tx-sender)
    (user-current-xp (get-user-xp user))
    (skill-info (unwrap! (map-get? skills skill-id) ERR-INVALID-SKILL))
    (skill-cost (get xp-cost skill-info))
  )
    ;; Validation checks
    (asserts! (not (has-skill? user skill-id)) ERR-ALREADY-UNLOCKED)
    (asserts! (>= user-current-xp skill-cost) ERR-INSUFFICIENT-XP)
    (asserts! (prerequisites-met? user skill-id) ERR-PREREQUISITES-NOT-MET)
    
    ;; Update user XP and skills
    (map-set user-xp user (- user-current-xp skill-cost))
    
    ;; Add the skill to user's unlocked skills
    (let (
      (current-skills (default-to (list) (map-get? user-skills user)))
      (updated-skills (append current-skills skill-id))
    )
      (map-set user-skills user updated-skills)
      (ok true)
    )
  )
)

;; Admin function to add a new skill to the tree
(define-public (add-skill 
  (name (string-ascii 50)) 
  (description (string-ascii 255)) 
  (xp-cost uint) 
  (path (string-ascii 20)) 
  (level uint) 
  (prerequisites (list 5 uint))
)
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    
    ;; Validate path exists
    (asserts! (is-some (index-of (var-get specialization-paths) path)) ERR-INVALID-PATH)
    
    ;; Validate all prerequisites exist
    (map check-prerequisite-exists prerequisites)
    
    ;; Add the new skill
    (let (
      (skill-id (var-get next-skill-id))
    )
      (map-set skills skill-id {
        name: name,
        description: description,
        xp-cost: xp-cost,
        path: path,
        level: level,
        prerequisites: prerequisites
      })
      
      ;; Increment skill ID counter
      (var-set next-skill-id (+ skill-id u1))
      (ok skill-id)
    )
  )
  
  ;; Helper to verify each prerequisite exists
  (define-private (check-prerequisite-exists (prereq-id uint))
    (asserts! (skill-exists? prereq-id) ERR-INVALID-SKILL)
    true
  )
)

;; Admin function to add a new specialization path
(define-public (add-specialization-path (path (string-ascii 20)))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (let (
      (current-paths (var-get specialization-paths))
    )
      (asserts! (is-none (index-of current-paths path)) (err u106)) ;; Path already exists
      (var-set specialization-paths (append current-paths path))
      (ok true)
    )
  )
)

;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)