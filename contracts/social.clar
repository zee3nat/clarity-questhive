;; social.clar
;; 
;; This contract implements social features for clarity-questhive that enables:
;; 1. Friend connections management (sending/accepting friend requests)
;; 2. Challenges between users
;; 3. Group quests
;; 4. Leaderboards across different categories
;; 5. Sharing of accomplishments
;;
;; The social graph and interactions increase motivation through friendly
;; competition and community support.

;; ===================
;; Constants & Errors
;; ===================

;; Error codes
(define-constant err-unauthorized (err u1000))
(define-constant err-already-friends (err u1001))
(define-constant err-friend-request-not-found (err u1002))
(define-constant err-challenge-not-found (err u1003))
(define-constant err-group-not-found (err u1004))
(define-constant err-already-member (err u1005))
(define-constant err-not-a-member (err u1006))
(define-constant err-not-group-admin (err u1007))
(define-constant err-self-operation (err u1008))
(define-constant err-invalid-category (err u1009))
(define-constant err-challenge-expired (err u1010))
(define-constant err-challenge-already-completed (err u1011))
(define-constant err-quest-not-found (err u1012))
(define-constant err-quest-already-completed (err u1013))

;; Challenge status
(define-constant status-pending u1)
(define-constant status-accepted u2)
(define-constant status-completed u3)
(define-constant status-rejected u4)

;; Group quest status
(define-constant quest-status-active u1)
(define-constant quest-status-completed u2)
(define-constant quest-status-expired u3)

;; Valid categories for leaderboards and challenges
(define-constant categories (list "fitness" "learning" "productivity" "wellness" "custom"))

;; ===================
;; Data Maps
;; ===================

;; Friendship management
(define-map friend-connections 
  { user: principal, friend: principal } 
  { created-at: uint }
)

(define-map friend-requests 
  { from: principal, to: principal } 
  { message: (string-utf8 280), created-at: uint }
)

;; Challenge data
(define-map challenges 
  { challenge-id: uint } 
  {
    creator: principal,
    recipient: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    category: (string-ascii 20),
    points: uint,
    expires-at: uint,
    status: uint,
    created-at: uint,
    completed-at: (optional uint)
  }
)

;; Group management
(define-map groups 
  { group-id: uint } 
  {
    name: (string-utf8 100),
    description: (string-utf8 500),
    admin: principal,
    created-at: uint
  }
)

(define-map group-members 
  { group-id: uint, member: principal } 
  { joined-at: uint }
)

;; Group quests
(define-map group-quests 
  { quest-id: uint } 
  {
    group-id: uint,
    title: (string-utf8 100),
    description: (string-utf8 500),
    category: (string-ascii 20),
    points: uint,
    creator: principal,
    expires-at: uint,
    status: uint,
    created-at: uint
  }
)

(define-map quest-completions 
  { quest-id: uint, user: principal } 
  { completed-at: uint }
)

;; Accomplishments
(define-map user-accomplishments
  { user: principal, accomplishment-id: uint }
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    category: (string-ascii 20),
    points: uint,
    is-shared: bool,
    created-at: uint
  }
)

;; User points per category (for leaderboards)
(define-map user-points
  { user: principal, category: (string-ascii 20) }
  { points: uint }
)

;; ===================
;; Data Variables
;; ===================

(define-data-var challenge-id-nonce uint u0)
(define-data-var group-id-nonce uint u0)
(define-data-var quest-id-nonce uint u0)
(define-data-var accomplishment-id-nonce uint u0)

;; ===================
;; Private Functions
;; ===================

(define-private (is-valid-category (category (string-ascii 20)))
  (default-to false (map-get? categories category))
)

(define-private (is-friend (user-a principal) (user-b principal))
  (or
    (map-get? friend-connections { user: user-a, friend: user-b })
    (map-get? friend-connections { user: user-b, friend: user-a })
  )
)

(define-private (is-group-member (group-id uint) (user principal))
  (not (is-none (map-get? group-members { group-id: group-id, member: user })))
)

(define-private (is-group-admin (group-id uint) (user principal))
  (let ((group (map-get? groups { group-id: group-id })))
    (and
      (is-some group)
      (is-eq (get admin (unwrap! group err-group-not-found)) user)
    )
  )
)

(define-private (get-challenge-by-id (challenge-id uint))
  (map-get? challenges { challenge-id: challenge-id })
)

(define-private (get-next-challenge-id)
  (let ((next-id (+ (var-get challenge-id-nonce) u1)))
    (var-set challenge-id-nonce next-id)
    next-id
  )
)

(define-private (get-next-group-id)
  (let ((next-id (+ (var-get group-id-nonce) u1)))
    (var-set group-id-nonce next-id)
    next-id
  )
)

(define-private (get-next-quest-id)
  (let ((next-id (+ (var-get quest-id-nonce) u1)))
    (var-set quest-id-nonce next-id)
    next-id
  )
)

(define-private (get-next-accomplishment-id)
  (let ((next-id (+ (var-get accomplishment-id-nonce) u1)))
    (var-set accomplishment-id-nonce next-id)
    next-id
  )
)

(define-private (add-points (user principal) (category (string-ascii 20)) (points-to-add uint))
  (let ((current-points (default-to { points: u0 } (map-get? user-points { user: user, category: category }))))
    (map-set user-points 
      { user: user, category: category } 
      { points: (+ (get points current-points) points-to-add) }
    )
  )
)

;; ===================
;; Read-only Functions
;; ===================

;; Friend-related functions
(define-read-only (get-friends (user principal))
  (let
    (
      (sent-keys (map-keys friend-connections { user: user }))
      (received-keys (map-keys friend-connections { friend: user }))
    )
    {
      sent: (map 
        (lambda (connection) (get friend connection))
        sent-keys
      ),
      received: (map 
        (lambda (connection) (get user connection))
        received-keys
      )
    }
  )
)

(define-read-only (get-pending-friend-requests (user principal))
  (map
    (lambda (request) 
      {
        from: (get from request),
        message: (get message (unwrap! (map-get? friend-requests request) { message: "", created-at: u0 })),
        created-at: (get created-at (unwrap! (map-get? friend-requests request) { message: "", created-at: u0 }))
      }
    )
    (map-keys friend-requests { to: user })
  )
)

(define-read-only (are-friends (user-a principal) (user-b principal))
  (is-friend user-a user-b)
)

;; Challenge-related functions
(define-read-only (get-challenge (challenge-id uint))
  (map-get? challenges { challenge-id: challenge-id })
)

(define-read-only (get-user-challenges (user principal))
  (let 
    (
      (created-keys (map filter 
        (lambda (key) 
          (is-eq (get creator (unwrap! (map-get? challenges key) false)) user)
        )
        (map-keys challenges)
      ))
      (received-keys (map filter 
        (lambda (key) 
          (is-eq (get recipient (unwrap! (map-get? challenges key) false)) user)
        )
        (map-keys challenges)
      ))
    )
    {
      created: (map 
        (lambda (key) (unwrap! (map-get? challenges key) false)) 
        created-keys
      ),
      received: (map 
        (lambda (key) (unwrap! (map-get? challenges key) false)) 
        received-keys
      )
    }
  )
)

;; Group-related functions
(define-read-only (get-group (group-id uint))
  (map-get? groups { group-id: group-id })
)

(define-read-only (get-group-members-list (group-id uint))
  (map 
    (lambda (entry) (get member entry))
    (map-keys group-members { group-id: group-id })
  )
)

(define-read-only (get-user-groups (user principal))
  (map
    (lambda (entry) (get group-id entry))
    (map-keys group-members { member: user })
  )
)

;; Quest-related functions
(define-read-only (get-group-quest (quest-id uint))
  (map-get? group-quests { quest-id: quest-id })
)

(define-read-only (get-group-quests-list (group-id uint))
  (map filter
    (lambda (entry) 
      (is-eq (get group-id (unwrap! (map-get? group-quests { quest-id: (get quest-id entry) }) false)) group-id)
    )
    (map-keys group-quests)
  )
)

(define-read-only (get-quest-completions-list (quest-id uint))
  (map 
    (lambda (entry) (get user entry))
    (map-keys quest-completions { quest-id: quest-id })
  )
)

;; Leaderboard functions
(define-read-only (get-user-points (user principal) (category (string-ascii 20)))
  (default-to { points: u0 } (map-get? user-points { user: user, category: category }))
)

(define-read-only (get-user-accomplishments (user principal))
  (map
    (lambda (entry) 
      (unwrap! (map-get? user-accomplishments { user: user, accomplishment-id: (get accomplishment-id entry) }) false)
    )
    (map-keys user-accomplishments { user: user })
  )
)

(define-read-only (get-shared-accomplishments (user principal))
  (filter
    (lambda (accomplishment) (get is-shared accomplishment))
    (get-user-accomplishments user)
  )
)

;; ===================
;; Public Functions
;; ===================

;; Friend Management Functions

(define-public (send-friend-request (to principal) (message (string-utf8 280)))
  (let ((sender tx-sender))
    (asserts! (not (is-eq sender to)) err-self-operation)
    (asserts! (not (is-friend sender to)) err-already-friends)
    
    (map-set friend-requests
      { from: sender, to: to }
      { message: message, created-at: block-height }
    )
    (ok true)
  )
)

(define-public (accept-friend-request (from principal))
  (let ((user tx-sender))
    (asserts! (not (is-eq from user)) err-self-operation)
    (asserts! (not (is-none (map-get? friend-requests { from: from, to: user }))) err-friend-request-not-found)
    
    ;; Create friendship connection
    (map-set friend-connections
      { user: user, friend: from }
      { created-at: block-height }
    )
    
    ;; Delete the friend request
    (map-delete friend-requests { from: from, to: user })
    
    (ok true)
  )
)

(define-public (reject-friend-request (from principal))
  (let ((user tx-sender))
    (asserts! (not (is-none (map-get? friend-requests { from: from, to: user }))) err-friend-request-not-found)
    
    ;; Delete the friend request
    (map-delete friend-requests { from: from, to: user })
    
    (ok true)
  )
)

(define-public (remove-friend (friend principal))
  (let ((user tx-sender))
    (asserts! (not (is-eq friend user)) err-self-operation)
    (asserts! (is-friend user friend) err-friend-request-not-found)
    
    ;; Try both directions
    (map-delete friend-connections { user: user, friend: friend })
    (map-delete friend-connections { user: friend, friend: user })
    
    (ok true)
  )
)

;; Challenge Functions

(define-public (create-challenge (recipient principal) (title (string-utf8 100)) (description (string-utf8 500)) (category (string-ascii 20)) (points uint) (expires-in uint))
  (let 
    (
      (creator tx-sender)
      (challenge-id (get-next-challenge-id))
      (expires-at (+ block-height expires-in))
    )
    (asserts! (not (is-eq creator recipient)) err-self-operation)
    (asserts! (is-friend creator recipient) err-friend-request-not-found)
    (asserts! (is-valid-category category) err-invalid-category)
    
    (map-set challenges
      { challenge-id: challenge-id }
      {
        creator: creator,
        recipient: recipient,
        title: title,
        description: description,
        category: category,
        points: points,
        expires-at: expires-at,
        status: status-pending,
        created-at: block-height,
        completed-at: none
      }
    )
    
    (ok challenge-id)
  )
)

(define-public (accept-challenge (challenge-id uint))
  (let
    (
      (user tx-sender)
      (challenge (unwrap! (get-challenge-by-id challenge-id) err-challenge-not-found))
    )
    (asserts! (is-eq user (get recipient challenge)) err-unauthorized)
    (asserts! (is-eq (get status challenge) status-pending) err-challenge-already-completed)
    (asserts! (< block-height (get expires-at challenge)) err-challenge-expired)
    
    (map-set challenges
      { challenge-id: challenge-id }
      (merge challenge { status: status-accepted })
    )
    
    (ok true)
  )
)

(define-public (reject-challenge (challenge-id uint))
  (let
    (
      (user tx-sender)
      (challenge (unwrap! (get-challenge-by-id challenge-id) err-challenge-not-found))
    )
    (asserts! (is-eq user (get recipient challenge)) err-unauthorized)
    (asserts! (is-eq (get status challenge) status-pending) err-challenge-already-completed)
    
    (map-set challenges
      { challenge-id: challenge-id }
      (merge challenge { status: status-rejected })
    )
    
    (ok true)
  )
)

(define-public (complete-challenge (challenge-id uint))
  (let
    (
      (user tx-sender)
      (challenge (unwrap! (get-challenge-by-id challenge-id) err-challenge-not-found))
    )
    (asserts! (is-eq user (get recipient challenge)) err-unauthorized)
    (asserts! (or (is-eq (get status challenge) status-pending) (is-eq (get status challenge) status-accepted)) err-challenge-already-completed)
    (asserts! (< block-height (get expires-at challenge)) err-challenge-expired)
    
    ;; Update challenge status
    (map-set challenges
      { challenge-id: challenge-id }
      (merge challenge { 
        status: status-completed,
        completed-at: (some block-height)
      })
    )
    
    ;; Award points to the user
    (add-points user (get category challenge) (get points challenge))
    
    ;; Record as an accomplishment
    (let ((accomplishment-id (get-next-accomplishment-id)))
      (map-set user-accomplishments
        { user: user, accomplishment-id: accomplishment-id }
        {
          title: (get title challenge),
          description: (get description challenge),
          category: (get category challenge),
          points: (get points challenge),
          is-shared: true,
          created-at: block-height
        }
      )
    )
    
    (ok true)
  )
)

;; Group Functions

(define-public (create-group (name (string-utf8 100)) (description (string-utf8 500)))
  (let
    (
      (creator tx-sender)
      (group-id (get-next-group-id))
    )
    (map-set groups
      { group-id: group-id }
      {
        name: name,
        description: description,
        admin: creator,
        created-at: block-height
      }
    )
    
    ;; Add creator as first member
    (map-set group-members
      { group-id: group-id, member: creator }
      { joined-at: block-height }
    )
    
    (ok group-id)
  )
)

(define-public (join-group (group-id uint))
  (let ((user tx-sender))
    (asserts! (not (is-none (map-get? groups { group-id: group-id }))) err-group-not-found)
    (asserts! (not (is-group-member group-id user)) err-already-member)
    
    (map-set group-members
      { group-id: group-id, member: user }
      { joined-at: block-height }
    )
    
    (ok true)
  )
)

(define-public (leave-group (group-id uint))
  (let ((user tx-sender))
    (asserts! (is-group-member group-id user) err-not-a-member)
    
    ;; Cannot leave if you're the admin (must transfer admin first)
    (asserts! (not (is-group-admin group-id user)) err-not-group-admin)
    
    (map-delete group-members { group-id: group-id, member: user })
    
    (ok true)
  )
)

(define-public (transfer-group-admin (group-id uint) (new-admin principal))
  (let ((user tx-sender))
    (asserts! (is-group-admin group-id user) err-not-group-admin)
    (asserts! (is-group-member group-id new-admin) err-not-a-member)
    
    (let ((group (unwrap! (map-get? groups { group-id: group-id }) err-group-not-found)))
      (map-set groups
        { group-id: group-id }
        (merge group { admin: new-admin })
      )
    )
    
    (ok true)
  )
)

;; Group Quest Functions

(define-public (create-group-quest (group-id uint) (title (string-utf8 100)) (description (string-utf8 500)) (category (string-ascii 20)) (points uint) (expires-in uint))
  (let
    (
      (creator tx-sender)
      (quest-id (get-next-quest-id))
      (expires-at (+ block-height expires-in))
    )
    (asserts! (is-group-member group-id creator) err-not-a-member)
    (asserts! (is-valid-category category) err-invalid-category)
    
    (map-set group-quests
      { quest-id: quest-id }
      {
        group-id: group-id,
        title: title,
        description: description,
        category: category,
        points: points,
        creator: creator,
        expires-at: expires-at,
        status: quest-status-active,
        created-at: block-height
      }
    )
    
    (ok quest-id)
  )
)

(define-public (complete-quest (quest-id uint))
  (let
    (
      (user tx-sender)
      (quest (unwrap! (map-get? group-quests { quest-id: quest-id }) err-quest-not-found))
    )
    (asserts! (is-group-member (get group-id quest) user) err-not-a-member)
    (asserts! (is-eq (get status quest) quest-status-active) err-quest-already-completed)
    (asserts! (< block-height (get expires-at quest)) err-challenge-expired)
    (asserts! (is-none (map-get? quest-completions { quest-id: quest-id, user: user })) err-quest-already-completed)
    
    ;; Mark as completed for this user
    (map-set quest-completions
      { quest-id: quest-id, user: user }
      { completed-at: block-height }
    )
    
    ;; Award points to the user
    (add-points user (get category quest) (get points quest))
    
    ;; Record as an accomplishment
    (let ((accomplishment-id (get-next-accomplishment-id)))
      (map-set user-accomplishments
        { user: user, accomplishment-id: accomplishment-id }
        {
          title: (get title quest),
          description: (get description quest),
          category: (get category quest),
          points: (get points quest),
          is-shared: true,
          created-at: block-height
        }
      )
    )
    
    (ok true)
  )
)

;; Accomplishment Functions

(define-public (create-accomplishment (title (string-utf8 100)) (description (string-utf8 500)) (category (string-ascii 20)) (points uint) (is-shared bool))
  (let
    (
      (user tx-sender)
      (accomplishment-id (get-next-accomplishment-id))
    )
    (asserts! (is-valid-category category) err-invalid-category)
    
    (map-set user-accomplishments
      { user: user, accomplishment-id: accomplishment-id }
      {
        title: title,
        description: description,
        category: category,
        points: points,
        is-shared: is-shared,
        created-at: block-height
      }
    )
    
    ;; Add points to user's category total
    (add-points user category points)
    
    (ok accomplishment-id)
  )
)

(define-public (toggle-accomplishment-sharing (accomplishment-id uint))
  (let
    (
      (user tx-sender)
      (accomplishment-key { user: user, accomplishment-id: accomplishment-id })
      (accomplishment (unwrap! (map-get? user-accomplishments accomplishment-key) err-unauthorized))
    )
    
    (map-set user-accomplishments
      accomplishment-key
      (merge accomplishment { is-shared: (not (get is-shared accomplishment)) })
    )
    
    (ok true)
  )
)