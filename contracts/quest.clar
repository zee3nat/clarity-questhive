```clarity
;; Quest Contract
(define-non-fungible-token quest uint)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u404))
(define-constant err-unauthorized (err u401))
(define-constant err-invalid-category (err u402))
(define-constant err-invalid-verification-type (err u403))

;; Quest data structure
(define-map quests
  uint 
  {
    owner: principal,
    title: (string-ascii 64),
    description: (string-ascii 256),
    difficulty: uint,
    deadline: uint,
    completed: bool,
    category: (string-ascii 32),
    tags: (list 5 (string-ascii 20)),
    verification-required: bool,
    verification-type: (string-ascii 20),
    verification-data: (optional (string-utf8 256)),
    verified: bool
  }
)

;; Valid categories and verification types
(define-map valid-categories (string-ascii 32) bool)
(define-map valid-verification-types (string-ascii 20) bool)

;; Initialize valid categories
(map-set valid-categories "health" true)
(map-set valid-categories "learning" true)
(map-set valid-categories "finance" true)
(map-set valid-categories "fitness" true)
(map-set valid-categories "productivity" true)
(map-set valid-categories "other" true)

;; Initialize valid verification types
(map-set valid-verification-types "image" true)
(map-set valid-verification-types "witness" true)
(map-set valid-verification-types "none" true)

(define-data-var quest-nonce uint u0)

;; Create new quest
(define-public (create-quest (title (string-ascii 64)) 
                           (description (string-ascii 256))
                           (difficulty uint)
                           (deadline uint)
                           (category (string-ascii 32))
                           (tags (list 5 (string-ascii 20)))
                           (verification-required bool)
                           (verification-type (string-ascii 20)))
  (let ((quest-id (var-get quest-nonce)))
    ;; Verify if category is valid
    (asserts! (is-some (map-get? valid-categories category)) err-invalid-category)
    ;; Verify if verification type is valid
    (asserts! (is-some (map-get? valid-verification-types verification-type)) err-invalid-verification-type)
    (try! (nft-mint? quest quest-id tx-sender))
    (map-set quests quest-id {
      owner: tx-sender,
      title: title,
      description: description, 
      difficulty: difficulty,
      deadline: deadline,
      completed: false,
      category: category,
      tags: tags,
      verification-required: verification-required,
      verification-type: verification-type,
      verification-data: none,
      verified: false
    })
    (var-set quest-nonce (+ quest-id u1))
    (ok quest-id)))

;; Complete quest
(define-public (complete-quest (quest-id uint))
  (let ((quest (unwrap! (map-get? quests quest-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner quest)) err-unauthorized)
    (map-set quests quest-id (merge quest {completed: true}))
    (ok true)))

;; Submit verification data for a quest
(define-public (submit-verification (quest-id uint) (verification-data (string-utf8 256)))
  (let ((quest (unwrap! (map-get? quests quest-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner quest)) err-unauthorized)
    (asserts! (get verification-required quest) err-unauthorized)
    (map-set quests quest-id (merge quest {verification-data: (some verification-data)}))
    (ok true)))

;; Verify a quest (can only be done by contract owner or designated verifiers)
(define-public (verify-quest (quest-id uint))
  (let ((quest (unwrap! (map-get? quests quest-id) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (get verification-required quest) err-unauthorized)
    (asserts! (is-some (get verification-data quest)) err-unauthorized)
    (map-set quests quest-id (merge quest {verified: true}))
    (ok true)))

;; Read quest details
(define-read-only (get-quest (quest-id uint))
  (map-get? quests quest-id))

;; Get quests by category
(define-read-only (get-quests-by-category (category (string-ascii 32)) (limit uint))
  (let ((quest-ids (list)))
    (fold check-quest-category (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9) {ids: quest-ids, category: category, limit: limit, count: u0})))

(define-private (check-quest-category (id uint) (result {ids: (list 10 uint), category: (string-ascii 32), limit: uint, count: uint}))
  (let ((quest (map-get? quests id))
        (current-count (get count result))
        (current-limit (get limit result))
        (current-ids (get ids result))
        (target-category (get category result)))
    (if (and (< current-count current-limit) 
            (is-some quest) 
            (is-eq (get category (unwrap-panic quest)) target-category))
      {ids: (append current-ids id), category: target-category, limit: current-limit, count: (+ current-count u1)}
      result)))

;; Add a new valid category (only contract owner can add)
(define-public (add-valid-category (category (string-ascii 32)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (map-set valid-categories category true)
    (ok true)))

;; Add a new valid verification type (only contract owner can add)
(define-public (add-valid-verification-type (verification-type (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (map-set valid-verification-types verification-type true)
    (ok true)))
```