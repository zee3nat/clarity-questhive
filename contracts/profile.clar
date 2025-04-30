```clarity
;; Profile Contract

;; Constants for level progression
(define-constant LEVEL_UP_BASE_XP u100)
(define-constant LEVEL_UP_MULTIPLIER u15)

;; Profile data structure
(define-map profiles
  principal
  {
    quests-completed: uint,
    total-rewards: uint,
    level: uint,
    experience: uint,
    reputation: uint,
    badges: (list 10 uint)
  })

;; Badge definitions
(define-map badges
  uint
  {
    name: (string-ascii 30),
    description: (string-ascii 100),
    unlock-condition: (string-ascii 50)
  })

;; User customizations
(define-map profile-customizations
  principal
  {
    title: (optional (string-ascii 30)),
    theme: (optional uint),
    avatar: (optional uint)
  })

;; Calculate XP needed for a level
(define-read-only (xp-for-level (level uint))
  (* LEVEL_UP_BASE_XP (+ u1 (* (- level u1) LEVEL_UP_MULTIPLIER))))

;; Initialize profile
(define-public (initialize-profile)
  (begin
    (map-set profiles tx-sender {
      quests-completed: u0,
      total-rewards: u0,
      level: u1,
      experience: u0,
      reputation: u0,
      badges: (list)
    })
    (map-set profile-customizations tx-sender {
      title: none,
      theme: none,
      avatar: none
    })
    (ok true)))

;; Update stats after quest completion
(define-public (update-stats (reward-amount uint) (xp-earned uint))
  (let (
    (profile (unwrap! (map-get? profiles tx-sender) (err u404)))
    (current-xp (get experience profile))
    (new-xp (+ current-xp xp-earned))
    (current-level (get level profile))
    (new-level (determine-level new-xp current-level))
  )
    (map-set profiles tx-sender
      (merge profile {
        quests-completed: (+ (get quests-completed profile) u1),
        total-rewards: (+ (get total-rewards profile) reward-amount),
        experience: new-xp,
        level: new-level
      }))
    (ok true)))

;; Determine level based on total XP
(define-read-only (determine-level (total-xp uint) (current-level uint))
  (let (
    (next-level-xp (xp-for-level (+ current-level u1)))
  )
    (if (>= total-xp next-level-xp)
      (determine-level total-xp (+ current-level u1))
      current-level)))

;; Update reputation (from verification)
(define-public (update-reputation (user principal) (points-change int))
  (let (
    (profile (unwrap! (map-get? profiles user) (err u404)))
    (current-rep (get reputation profile))
    (new-rep (if (< points-change i0)
               (if (> (abs points-change) current-rep) 
                 u0
                 (- current-rep (abs points-change)))
               (+ current-rep (abs points-change))))
  )
    (map-set profiles user
      (merge profile {
        reputation: new-rep
      }))
    (ok true)))

;; Add a badge to a user
(define-public (award-badge (user principal) (badge-id uint))
  (let (
    (profile (unwrap! (map-get? profiles user) (err u404)))
    (badges-list (get badges profile))
  )
    (asserts! (is-some (map-get? badges badge-id)) (err u404))
    (map-set profiles user
      (merge profile {
        badges: (unwrap! (as-max-len? (append badges-list badge-id) u10) (err u500))
      }))
    (ok true)))

;; Define a new badge
(define-public (define-badge (badge-id uint) (name (string-ascii 30)) (description (string-ascii 100)) (condition (string-ascii 50)))
  (begin
    (map-set badges badge-id {
      name: name,
      description: description,
      unlock-condition: condition
    })
    (ok true)))

;; Update profile customizations
(define-public (update-customization (title (optional (string-ascii 30))) (theme (optional uint)) (avatar (optional uint)))
  (let (
    (customization (default-to {title: none, theme: none, avatar: none} (map-get? profile-customizations tx-sender)))
  )
    (map-set profile-customizations tx-sender {
      title: (if (is-some title) title (get title customization)),
      theme: (if (is-some theme) theme (get theme customization)),
      avatar: (if (is-some avatar) avatar (get avatar customization))
    })
    (ok true)))

;; Get profile
(define-read-only (get-profile (user principal))
  (map-get? profiles user))

;; Get profile customizations
(define-read-only (get-customizations (user principal))
  (map-get? profile-customizations user))

;; Get badge details
(define-read-only (get-badge-details (badge-id uint))
  (map-get? badges badge-id))

;; Get XP needed for next level
(define-read-only (get-next-level-xp (user principal))
  (let (
    (profile (unwrap! (map-get? profiles user) (err u404)))
    (current-level (get level profile))
  )
    (xp-for-level (+ current-level u1))))
```