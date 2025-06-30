;; FileComments Smart Contract
;; A comprehensive commenting system for files with moderation and threading support

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-comment (err u103))
(define-constant err-comment-too-long (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-comment-deleted (err u106))
(define-constant err-comment-moderated (err u107))

;; Data variables
(define-data-var next-comment-id uint u1)
(define-data-var max-comment-length uint u1000)

;; Comment status constants
(define-constant comment-status-active u1)
(define-constant comment-status-deleted u2)
(define-constant comment-status-moderated u3)

;; Data maps
(define-map comments
  { comment-id: uint }
  {
    file-id: (string-ascii 64),
    author: principal,
    content: (string-utf8 1000),
    parent-comment-id: (optional uint),
    timestamp: uint,
    status: uint,
    edit-count: uint,
    last-edited: (optional uint)
  }
)

(define-map file-comment-count
  { file-id: (string-ascii 64) }
  { count: uint }
)

(define-map user-comment-count
  { user: principal }
  { count: uint }
)

(define-map comment-replies
  { parent-comment-id: uint }
  { reply-count: uint }
)

(define-map moderators
  { moderator: principal }
  { is-active: bool }
)

(define-map comment-votes
  { comment-id: uint, voter: principal }
  { vote: int }
)

(define-map comment-vote-totals
  { comment-id: uint }
  { upvotes: uint, downvotes: uint }
)

;; Initialize contract with owner as first moderator
(map-set moderators { moderator: contract-owner } { is-active: true })

;; Private functions
(define-private (is-moderator (user principal))
  (default-to false (get is-active (map-get? moderators { moderator: user })))
)

(define-private (is-comment-owner (comment-id uint) (user principal))
  (match (map-get? comments { comment-id: comment-id })
    comment-data (is-eq (get author comment-data) user)
    false
  )
)

(define-private (comment-exists (comment-id uint))
  (is-some (map-get? comments { comment-id: comment-id }))
)

(define-private (is-comment-active (comment-id uint))
  (match (map-get? comments { comment-id: comment-id })
    comment-data (is-eq (get status comment-data) comment-status-active)
    false
  )
)

(define-private (increment-file-comment-count (file-id (string-ascii 64)))
  (let ((current-count (default-to u0 (get count (map-get? file-comment-count { file-id: file-id })))))
    (map-set file-comment-count { file-id: file-id } { count: (+ current-count u1) })
  )
)

(define-private (increment-user-comment-count (user principal))
  (let ((current-count (default-to u0 (get count (map-get? user-comment-count { user: user })))))
    (map-set user-comment-count { user: user } { count: (+ current-count u1) })
  )
)

(define-private (increment-reply-count (parent-comment-id uint))
  (let ((current-count (default-to u0 (get reply-count (map-get? comment-replies { parent-comment-id: parent-comment-id })))))
    (map-set comment-replies { parent-comment-id: parent-comment-id } { reply-count: (+ current-count u1) })
  )
)

;; Public functions

;; Add a new comment
(define-public (add-comment (file-id (string-ascii 64)) (content (string-utf8 1000)) (parent-comment-id (optional uint)))
  (let (
    (comment-id (var-get next-comment-id))
    (content-length (len content))
    (max-length (var-get max-comment-length))
  )
    (asserts! (> content-length u0) err-invalid-comment)
    (asserts! (<= content-length max-length) err-comment-too-long)
    
    ;; If replying to a comment, verify parent exists and is active
    (match parent-comment-id
      parent-id (asserts! (is-comment-active parent-id) err-not-found)
      true
    )
    
    ;; Create the comment
    (map-set comments
      { comment-id: comment-id }
      {
        file-id: file-id,
        author: tx-sender,
        content: content,
        parent-comment-id: parent-comment-id,
        timestamp: block-height,
        status: comment-status-active,
        edit-count: u0,
        last-edited: none
      }
    )
    
    ;; Update counters
    (increment-file-comment-count file-id)
    (increment-user-comment-count tx-sender)
    
    ;; If this is a reply, increment parent's reply count
    (match parent-comment-id
      parent-id (increment-reply-count parent-id)
      true
    )
    
    ;; Increment next comment ID
    (var-set next-comment-id (+ comment-id u1))
    
    (ok comment-id)
  )
)

;; Edit an existing comment
(define-public (edit-comment (comment-id uint) (new-content (string-utf8 1000)))
  (let (
    (comment-data (unwrap! (map-get? comments { comment-id: comment-id }) err-not-found))
    (content-length (len new-content))
    (max-length (var-get max-comment-length))
  )
    (asserts! (is-eq (get author comment-data) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status comment-data) comment-status-active) err-comment-deleted)
    (asserts! (> content-length u0) err-invalid-comment)
    (asserts! (<= content-length max-length) err-comment-too-long)
    
    (map-set comments
      { comment-id: comment-id }
      (merge comment-data {
        content: new-content,
        edit-count: (+ (get edit-count comment-data) u1),
        last-edited: (some block-height)
      })
    )
    
    (ok true)
  )
)

;; Delete a comment (soft delete)
(define-public (delete-comment (comment-id uint))
  (let (
    (comment-data (unwrap! (map-get? comments { comment-id: comment-id }) err-not-found))
  )
    (asserts! (is-eq (get author comment-data) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status comment-data) comment-status-active) err-comment-deleted)
    
    (map-set comments
      { comment-id: comment-id }
      (merge comment-data {
        status: comment-status-deleted,
        content: u"[Comment deleted by user]"
      })
    )
    
    (ok true)
  )
)

;; Moderate a comment (moderator only)
(define-public (moderate-comment (comment-id uint) (reason (string-utf8 200)))
  (let (
    (comment-data (unwrap! (map-get? comments { comment-id: comment-id }) err-not-found))
  )
    (asserts! (is-moderator tx-sender) err-unauthorized)
    (asserts! (is-eq (get status comment-data) comment-status-active) err-comment-deleted)
    
    (map-set comments
      { comment-id: comment-id }
      (merge comment-data {
        status: comment-status-moderated,
        content: (concat u"[Comment moderated: " (concat reason u"]"))
      })
    )
    
    (ok true)
  )
)

;; Add or update moderator (owner only)
(define-public (add-moderator (moderator principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set moderators { moderator: moderator } { is-active: true })
    (ok true)
  )
)

;; Remove moderator (owner only)
(define-public (remove-moderator (moderator principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (is-eq moderator contract-owner)) err-unauthorized)
    (map-set moderators { moderator: moderator } { is-active: false })
    (ok true)
  )
)

;; Vote on a comment
(define-public (vote-comment (comment-id uint) (vote int))
  (let (
    (comment-data (unwrap! (map-get? comments { comment-id: comment-id }) err-not-found))
    (existing-vote (map-get? comment-votes { comment-id: comment-id, voter: tx-sender }))
    (vote-totals (default-to { upvotes: u0, downvotes: u0 } 
                              (map-get? comment-vote-totals { comment-id: comment-id })))
  )
    (asserts! (is-eq (get status comment-data) comment-status-active) err-comment-deleted)
    (asserts! (or (is-eq vote 1) (is-eq vote -1)) err-invalid-comment)
    
    ;; Handle vote update
    (let (
      (new-upvotes (if (is-eq vote 1) 
                      (+ (get upvotes vote-totals) u1)
                      (get upvotes vote-totals)))
      (new-downvotes (if (is-eq vote -1) 
                        (+ (get downvotes vote-totals) u1)
                        (get downvotes vote-totals)))
    )
      ;; If user already voted, adjust totals
      (match existing-vote
        prev-vote (let (
          (adjusted-upvotes (if (is-eq (get vote prev-vote) 1) 
                               (if (> new-upvotes u0) (- new-upvotes u1) u0)
                               new-upvotes))
          (adjusted-downvotes (if (is-eq (get vote prev-vote) -1) 
                                 (if (> new-downvotes u0) (- new-downvotes u1) u0)
                                 new-downvotes))
        )
          (map-set comment-vote-totals 
            { comment-id: comment-id } 
            { upvotes: adjusted-upvotes, downvotes: adjusted-downvotes }))
        (map-set comment-vote-totals 
          { comment-id: comment-id } 
          { upvotes: new-upvotes, downvotes: new-downvotes })
      )
      
      ;; Set user's vote
      (map-set comment-votes { comment-id: comment-id, voter: tx-sender } { vote: vote })
      
      (ok true)
    )
  )
)

;; Set maximum comment length (owner only)
(define-public (set-max-comment-length (new-length uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-length u0) err-invalid-comment)
    (var-set max-comment-length new-length)
    (ok true)
  )
)

;; Read-only functions

;; Get comment details
(define-read-only (get-comment (comment-id uint))
  (map-get? comments { comment-id: comment-id })
)

;; Get comment replies count
(define-read-only (get-reply-count (comment-id uint))
  (default-to u0 (get reply-count (map-get? comment-replies { parent-comment-id: comment-id })))
)

;; Get file comment count
(define-read-only (get-file-comment-count (file-id (string-ascii 64)))
  (default-to u0 (get count (map-get? file-comment-count { file-id: file-id })))
)

;; Get user comment count
(define-read-only (get-user-comment-count (user principal))
  (default-to u0 (get count (map-get? user-comment-count { user: user })))
)

;; Check if user is moderator
(define-read-only (check-moderator (user principal))
  (is-moderator user)
)

;; Get comment vote totals
(define-read-only (get-comment-votes (comment-id uint))
  (default-to { upvotes: u0, downvotes: u0 } 
              (map-get? comment-vote-totals { comment-id: comment-id }))
)

;; Get user's vote on a comment
(define-read-only (get-user-vote (comment-id uint) (user principal))
  (map-get? comment-votes { comment-id: comment-id, voter: user })
)

;; Get next comment ID
(define-read-only (get-next-comment-id)
  (var-get next-comment-id)
)

;; Get maximum comment length
(define-read-only (get-max-comment-length)
  (var-get max-comment-length)
)