```clarity
;; Rewards Contract
(define-fungible-token hive-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u401))
(define-constant err-invalid-params (err u402))

;; Token rewards for different difficulties
(define-map difficulty-rewards
  uint
  uint)

;; Streak multiplier tiers (key: streak count, value: multiplier percentage)
(define-map streak-multipliers
  uint
  uint)

;; Theme-based reward boosts (key: theme-id, value: boost percentage)
(define-map theme-boosts
  uint
  uint)

;; User streak tracking
(define-map user-streaks
  principal
  uint)

;; Initialize rewards
(map-set difficulty-rewards u1 u10)  ;; Easy   = 10 tokens
(map-set difficulty-rewards u2 u25)  ;; Medium = 25 tokens
(map-set difficulty-rewards u3 u50)  ;; Hard   = 50 tokens

;; Initialize streak multipliers
(map-set streak-multipliers u3 u110)   ;; 3-day streak = 110% (10% bonus)
(map-set streak-multipliers u7 u125)   ;; 7-day streak = 125% (25% bonus)
(map-set streak-multipliers u14 u150)  ;; 14-day streak = 150% (50% bonus)
(map-set streak-multipliers u30 u200)  ;; 30-day streak = 200% (100% bonus)

;; Initialize theme boosts
(map-set theme-boosts u1 u115)  ;; Health theme = 15% boost
(map-set theme-boosts u2 u110)  ;; Learning theme = 10% boost
(map-set theme-boosts u3 u120)  ;; Productivity theme = 20% boost

;; Mint rewards for completing quests
(define-public (reward-completion (user principal) (difficulty uint))
  (let 
    (
      (reward-amount (unwrap! (map-get? difficulty-rewards difficulty) err-unauthorized))
      (current-streak (default-to u0 (map-get? user-streaks user)))
      (new-streak (+ current-streak u1))
      (streak-multiplier (get-streak-multiplier new-streak))
      (final-reward (/ (* reward-amount streak-multiplier) u100))
    )
    (map-set user-streaks user new-streak)
    (ft-mint? hive-token final-reward user)
  )
)

;; Mint rewards for completing quests with theme boost
(define-public (reward-themed-completion (user principal) (difficulty uint) (theme-id uint))
  (let 
    (
      (reward-amount (unwrap! (map-get? difficulty-rewards difficulty) err-unauthorized))
      (current-streak (default-to u0 (map-get? user-streaks user)))
      (new-streak (+ current-streak u1))
      (streak-multiplier (get-streak-multiplier new-streak))
      (theme-boost (default-to u100 (map-get? theme-boosts theme-id)))
      (boosted-reward (/ (* reward-amount theme-boost) u100))
      (final-reward (/ (* boosted-reward streak-multiplier) u100))
    )
    (map-set user-streaks user new-streak)
    (ft-mint? hive-token final-reward user)
  )
)

;; Reset a user's streak (e.g., when they miss a day)
(define-public (reset-streak (user principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (ok (map-set user-streaks user u0))
  )
)

;; Get the multiplier for a given streak length
(define-read-only (get-streak-multiplier (streak-length uint))
  (let ((multiplier-30 (map-get? streak-multipliers u30))
        (multiplier-14 (map-get? streak-multipliers u14))
        (multiplier-7 (map-get? streak-multipliers u7))
        (multiplier-3 (map-get? streak-multipliers u3)))
    (cond
      ((>= streak-length u30) (default-to u200 multiplier-30))
      ((>= streak-length u14) (default-to u150 multiplier-14))
      ((>= streak-length u7)  (default-to u125 multiplier-7))
      ((>= streak-length u3)  (default-to u110 multiplier-3))
      (true u100)
    )
  )
)

;; Update difficulty rewards (admin only)
(define-public (set-difficulty-reward (difficulty uint) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (> amount u0) err-invalid-params)
    (ok (map-set difficulty-rewards difficulty amount))
  )
)

;; Update streak multipliers (admin only)
(define-public (set-streak-multiplier (streak-days uint) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (and (> streak-days u0) (>= multiplier u100)) err-invalid-params)
    (ok (map-set streak-multipliers streak-days multiplier))
  )
)

;; Update theme boosts (admin only)
(define-public (set-theme-boost (theme-id uint) (boost-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (>= boost-percentage u100) err-invalid-params)
    (ok (map-set theme-boosts theme-id boost-percentage))
  )
)

;; Get user's current streak
(define-read-only (get-user-streak (user principal))
  (default-to u0 (map-get? user-streaks user))
)

;; Get user balance
(define-read-only (get-balance (account principal))
  (ok (ft-get-balance hive-token account)))
```