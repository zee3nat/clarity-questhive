;; Quest Contract
(define-non-fungible-token quest uint)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u404))
(define-constant err-unauthorized (err u401))

;; Quest data structure
(define-map quests
  uint 
  {
    owner: principal,
    title: (string-ascii 64),
    description: (string-ascii 256),
    difficulty: uint,
    deadline: uint,
    completed: bool
  }
)

(define-data-var quest-nonce uint u0)

;; Create new quest
(define-public (create-quest (title (string-ascii 64)) 
                           (description (string-ascii 256))
                           (difficulty uint)
                           (deadline uint))
  (let ((quest-id (var-get quest-nonce)))
    (try! (nft-mint? quest quest-id tx-sender))
    (map-set quests quest-id {
      owner: tx-sender,
      title: title,
      description: description, 
      difficulty: difficulty,
      deadline: deadline,
      completed: false
    })
    (var-set quest-nonce (+ quest-id u1))
    (ok quest-id)))

;; Complete quest
(define-public (complete-quest (quest-id uint))
  (let ((quest (unwrap! (map-get? quests quest-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner quest)) err-unauthorized)
    (map-set quests quest-id (merge quest {completed: true}))
    (ok true)))

;; Read quest details
(define-read-only (get-quest (quest-id uint))
  (map-get? quests quest-id))
