;; Profile Contract

;; Profile data structure
(define-map profiles
  principal
  {
    quests-completed: uint,
    total-rewards: uint,
    level: uint
  })

;; Initialize profile
(define-public (initialize-profile)
  (map-set profiles tx-sender {
    quests-completed: u0,
    total-rewards: u0,
    level: u1
  })
  (ok true))

;; Update stats after quest completion
(define-public (update-stats (reward-amount uint))
  (let ((profile (unwrap! (map-get? profiles tx-sender) (err u404))))
    (map-set profiles tx-sender
      (merge profile {
        quests-completed: (+ (get quests-completed profile) u1),
        total-rewards: (+ (get total-rewards profile) reward-amount)
      }))
    (ok true)))

;; Get profile
(define-read-only (get-profile (user principal))
  (map-get? profiles user))
