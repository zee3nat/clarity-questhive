;; achievements.clar
;; This contract implements a comprehensive achievement system that rewards users for
;; completing milestones, special challenges, and community contributions. It manages
;; different achievement types and distributes badges, bonus tokens, and experience points
;; to incentivize platform engagement and recognize user dedication.

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ACHIEVEMENT-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-CLAIMED (err u102))
(define-constant ERR-REQUIREMENTS-NOT-MET (err u103))
(define-constant ERR-INVALID-ACHIEVEMENT-TYPE (err u104))
(define-constant ERR-ACHIEVEMENT-EXISTS (err u105))

;; Achievement types
(define-constant ACHIEVEMENT-TYPE-MILESTONE u1)  ;; Progress-based achievements
(define-constant ACHIEVEMENT-TYPE-CHALLENGE u2)  ;; Special limited-time challenges
(define-constant ACHIEVEMENT-TYPE-COMMUNITY u3)  ;; Community contribution rewards

;; Data space definitions
;; Maps and storage for achievement data

;; Achievement definitions
;; Stores the core information about each achievement
(define-map achievements
  { achievement-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    achievement-type: uint,
    xp-reward: uint,
    token-reward: uint,
    required-value: uint,  ;; Required progress/count to complete
    active: bool           ;; Whether this achievement can be earned
  }
)

;; Tracks which users have claimed which achievements
(define-map user-achievements
  { user: principal, achievement-id: uint }
  {
    claimed: bool,
    claim-time: uint,
    progress: uint  ;; For tracking partial progress on milestone achievements
  }
)

;; Tracks total user stats
(define-map user-stats
  { user: principal }
  {
    total-achievements: uint,
    total-xp: uint,
    total-tokens: uint
  }
)

;; Admin list
(define-map admins
  { admin: principal }
  { active: bool }
)

;; Private functions

;; Check if sender is an admin
(define-private (is-admin)
  (default-to
    false
    (get active (map-get? admins { admin: tx-sender }))
  )
)

;; Initialize user stats if they don't exist
(define-private (initialize-user-stats (user principal))
  (if (map-get? user-stats { user: user })
    true
    (map-set user-stats
      { user: user }
      {
        total-achievements: u0,
        total-xp: u0,
        total-tokens: u0
      }
    )
  )
)

;; Initialize user achievement progress if it doesn't exist
(define-private (initialize-user-achievement (user principal) (achievement-id uint))
  (if (map-get? user-achievements { user: user, achievement-id: achievement-id })
    true
    (map-set user-achievements
      { user: user, achievement-id: achievement-id }
      {
        claimed: false,
        claim-time: u0,
        progress: u0
      }
    )
  )
)

;; Update user stats after claiming an achievement
(define-private (update-user-stats (user principal) (xp-reward uint) (token-reward uint))
  (let (
    (current-stats (default-to 
                    { total-achievements: u0, total-xp: u0, total-tokens: u0 } 
                    (map-get? user-stats { user: user })))
  )
    (map-set user-stats
      { user: user }
      {
        total-achievements: (+ (get total-achievements current-stats) u1),
        total-xp: (+ (get total-xp current-stats) xp-reward),
        total-tokens: (+ (get total-tokens current-stats) token-reward)
      }
    )
  )
)

;; Read-only functions

;; Get achievement details
(define-read-only (get-achievement (achievement-id uint))
  (map-get? achievements { achievement-id: achievement-id })
)

;; Get user achievement status
(define-read-only (get-user-achievement (user principal) (achievement-id uint))
  (map-get? user-achievements { user: user, achievement-id: achievement-id })
)

;; Get user stats
(define-read-only (get-user-stats (user principal))
  (default-to
    { total-achievements: u0, total-xp: u0, total-tokens: u0 }
    (map-get? user-stats { user: user })
  )
)

;; Check if a user has met requirements for an achievement
(define-read-only (has-met-requirements (user principal) (achievement-id uint))
  (let (
    (achievement (map-get? achievements { achievement-id: achievement-id }))
    (user-achievement (map-get? user-achievements { user: user, achievement-id: achievement-id }))
  )
    (if (and achievement user-achievement)
      (>= (get progress user-achievement) (get required-value achievement))
      false
    )
  )
)

;; Public functions

;; Add a new achievement definition (admin only)
(define-public (add-achievement 
  (achievement-id uint)
  (name (string-ascii 50))
  (description (string-ascii 200))
  (achievement-type uint)
  (xp-reward uint)
  (token-reward uint)
  (required-value uint))
  
  (begin
    ;; Check authorization
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    
    ;; Verify valid achievement type
    (asserts! (or 
               (is-eq achievement-type ACHIEVEMENT-TYPE-MILESTONE)
               (is-eq achievement-type ACHIEVEMENT-TYPE-CHALLENGE)
               (is-eq achievement-type ACHIEVEMENT-TYPE-COMMUNITY))
             ERR-INVALID-ACHIEVEMENT-TYPE)
    
    ;; Check achievement doesn't already exist
    (asserts! (is-none (map-get? achievements { achievement-id: achievement-id })) 
              ERR-ACHIEVEMENT-EXISTS)
    
    ;; Add the achievement
    (map-set achievements
      { achievement-id: achievement-id }
      {
        name: name,
        description: description,
        achievement-type: achievement-type,
        xp-reward: xp-reward,
        token-reward: token-reward,
        required-value: required-value,
        active: true
      }
    )
    
    (ok true)
  )
)

;; Update achievement progress for a user
(define-public (update-achievement-progress 
  (user principal) 
  (achievement-id uint) 
  (progress-value uint))
  
  (begin
    ;; Check authorization
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    
    ;; Verify achievement exists
    (asserts! (is-some (map-get? achievements { achievement-id: achievement-id })) 
              ERR-ACHIEVEMENT-NOT-FOUND)
    
    ;; Initialize user stats and achievement if needed
    (initialize-user-stats user)
    (initialize-user-achievement user achievement-id)
    
    ;; Get current user achievement data
    (let (
      (user-achievement (unwrap-panic (map-get? user-achievements 
                                       { user: user, achievement-id: achievement-id })))
    )
      ;; Only update if not already claimed
      (if (get claimed user-achievement)
        (ok false)  ;; Already claimed, no update needed
        (begin
          ;; Update progress
          (map-set user-achievements
            { user: user, achievement-id: achievement-id }
            {
              claimed: false,
              claim-time: u0,
              progress: progress-value
            }
          )
          (ok true)
        )
      )
    )
  )
)

;; Claim an achievement reward
(define-public (claim-achievement (achievement-id uint))
  (let (
    (achievement (unwrap! (map-get? achievements { achievement-id: achievement-id }) 
                 ERR-ACHIEVEMENT-NOT-FOUND))
    (user tx-sender)
  )
    ;; Initialize if needed
    (initialize-user-stats user)
    (initialize-user-achievement user achievement-id)
    
    ;; Get current achievement status
    (let (
      (user-achievement (unwrap-panic (map-get? user-achievements 
                                      { user: user, achievement-id: achievement-id })))
    )
      ;; Verify achievement is active
      (asserts! (get active achievement) ERR-ACHIEVEMENT-NOT-FOUND)
      
      ;; Check it hasn't been claimed already
      (asserts! (not (get claimed user-achievement)) ERR-ALREADY-CLAIMED)
      
      ;; Check requirements are met
      (asserts! (>= (get progress user-achievement) (get required-value achievement)) 
                ERR-REQUIREMENTS-NOT-MET)
      
      ;; Update user achievement to claimed
      (map-set user-achievements
        { user: user, achievement-id: achievement-id }
        {
          claimed: true,
          claim-time: block-height,
          progress: (get progress user-achievement)
        }
      )
      
      ;; Update user stats with rewards
      (update-user-stats 
        user 
        (get xp-reward achievement) 
        (get token-reward achievement)
      )
      
      (ok {
        xp-awarded: (get xp-reward achievement),
        tokens-awarded: (get token-reward achievement)
      })
    )
  )
)

;; Update achievement details (admin only)
(define-public (update-achievement 
  (achievement-id uint)
  (name (string-ascii 50))
  (description (string-ascii 200))
  (xp-reward uint)
  (token-reward uint)
  (required-value uint)
  (active bool))
  
  (begin
    ;; Check authorization
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    
    ;; Verify achievement exists
    (let ((achievement (unwrap! (map-get? achievements { achievement-id: achievement-id }) 
                       ERR-ACHIEVEMENT-NOT-FOUND)))
      
      ;; Update achievement
      (map-set achievements
        { achievement-id: achievement-id }
        {
          name: name,
          description: description,
          achievement-type: (get achievement-type achievement),  ;; Keep original type
          xp-reward: xp-reward,
          token-reward: token-reward,
          required-value: required-value,
          active: active
        }
      )
      
      (ok true)
    )
  )
)

;; Add or remove admin
(define-public (set-admin (admin principal) (is-active bool))
  (begin
    ;; Only existing admins can manage other admins
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    
    (map-set admins
      { admin: admin }
      { active: is-active }
    )
    
    (ok true)
  )
)

;; Initialize contract admin (only callable once at contract deployment)
(define-public (initialize-contract-admin)
  (begin
    ;; Can only set admin if no admins exist
    (asserts! (is-none (map-get? admins { admin: tx-sender })) ERR-NOT-AUTHORIZED)
    
    (map-set admins
      { admin: tx-sender }
      { active: true }
    )
    
    (ok true)
  )
)