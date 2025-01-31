;; Rewards Contract
(define-fungible-token hive-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u401))

;; Token rewards for different difficulties
(define-map difficulty-rewards
  uint
  uint)

;; Initialize rewards
(map-set difficulty-rewards u1 u10)  ;; Easy   = 10 tokens
(map-set difficulty-rewards u2 u25)  ;; Medium = 25 tokens
(map-set difficulty-rewards u3 u50)  ;; Hard   = 50 tokens

;; Mint rewards for completing quests
(define-public (reward-completion (user principal) (difficulty uint))
  (let ((reward-amount (unwrap! (map-get? difficulty-rewards difficulty) err-unauthorized)))
    (ft-mint? hive-token reward-amount user)))

;; Get user balance
(define-read-only (get-balance (account principal))
  (ok (ft-get-balance hive-token account)))
