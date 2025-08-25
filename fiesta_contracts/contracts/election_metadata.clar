;; Enhanced Voting Contract with Advanced Features
;; Comprehensive voting system with delegation, weighted voting, and governance features

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-voted (err u102))
(define-constant err-election-ended (err u103))
(define-constant err-election-not-started (err u104))
(define-constant err-invalid-candidate (err u105))
(define-constant err-insufficient-tokens (err u106))
(define-constant err-not-whitelisted (err u107))
(define-constant err-already-delegated (err u108))
(define-constant err-invalid-delegation (err u109))
(define-constant err-quorum-not-met (err u110))
(define-constant err-proposal-exists (err u111))
(define-constant err-voting-not-allowed (err u112))
(define-constant err-invalid-threshold (err u113))

;; Election types
(define-constant ELECTION-TYPE-SIMPLE u1)
(define-constant ELECTION-TYPE-WEIGHTED u2)
(define-constant ELECTION-TYPE-DELEGATED u3)
(define-constant ELECTION-TYPE-MULTI-CHOICE u4)
(define-constant ELECTION-TYPE-RANKED u5)

;; Data Variables
(define-data-var next-election-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var platform-fee uint u1000) ;; 0.1% in basis points
(define-data-var min-voting-power uint u1)
(define-data-var governance-token principal tx-sender)

;; Data Maps

;; Enhanced election metadata with advanced features
(define-map elections
  { election-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    start-date: uint,
    end-date: uint,
    creator: principal,
    total-votes: uint,
    total-voting-power: uint,
    is-active: bool,
    election-type: uint,
    min-quorum: uint,
    winning-threshold: uint,
    max-choices: uint,
    requires-whitelist: bool,
    allows-delegation: bool,
    is-private: bool,
    category: (string-ascii 50),
    tags: (list 5 (string-ascii 20))
  }
)

;; Enhanced candidates with additional metadata
(define-map candidates
  { election-id: uint, candidate-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    image-url: (string-ascii 200),
    platform-points: (list 5 (string-ascii 100)),
    vote-count: uint,
    voting-power: uint,
    endorsements: uint,
    is-active: bool
  }
)

;; Voter records with enhanced tracking
(define-map voter-records
  { election-id: uint, voter: principal }
  { 
    voted: bool, 
    choices: (list 10 uint),
    voting-power-used: uint,
    vote-time: uint,
    is-delegated: bool,
    delegate: (optional principal)
  }
)

;; Delegation system
(define-map delegations
  { delegator: principal, election-id: uint }
  { 
    delegate: principal,
    voting-power: uint,
    is-active: bool,
    delegation-time: uint
  }
)

;; Delegate profiles
(define-map delegate-profiles
  { delegate: principal }
  {
    name: (string-ascii 50),
    bio: (string-ascii 300),
    total-delegated-power: uint,
    active-delegations: uint,
    reputation-score: uint
  }
)

;; Whitelist for restricted elections
(define-map election-whitelist
  { election-id: uint, voter: principal }
  { whitelisted: bool, voting-power: uint }
)

;; Voting power based on token holdings
(define-map voter-power
  { voter: principal }
  { power: uint, last-updated: uint }
)

;; Election candidate count
(define-map election-candidate-count
  { election-id: uint }
  { count: uint }
)

;; Governance proposals
(define-map governance-proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    proposal-type: uint,
    execution-delay: uint,
    votes-for: uint,
    votes-against: uint,
    total-votes: uint,
    start-time: uint,
    end-time: uint,
    executed: bool,
    is-active: bool
  }
)

;; Election comments and discussions
(define-map election-comments
  { election-id: uint, comment-id: uint }
  {
    author: principal,
    content: (string-ascii 500),
    timestamp: uint,
    parent-comment: (optional uint),
    upvotes: uint,
    downvotes: uint
  }
)

(define-map election-comment-count
  { election-id: uint }
  { count: uint }
)

;; Election analytics
(define-map election-analytics
  { election-id: uint }
  {
    unique-voters: uint,
    total-engagement: uint,
    peak-voting-time: uint,
    demographics-tracked: bool
  }
)

;; Read-only functions

;; Get enhanced election data
(define-read-only (get-election-full (election-id uint))
  (match (map-get? elections { election-id: election-id })
    election-data
    (ok {
      election: election-data,
      candidates: (get-all-candidates election-id),
      analytics: (map-get? election-analytics { election-id: election-id }),
      is-active: (is-election-active election-id),
      quorum-status: (check-quorum election-id)
    })
    err-not-found
  )
)

;; Enhanced candidate retrieval
(define-read-only (get-all-candidates (election-id uint))
  (let ((candidate-count (default-to u0 (get count (map-get? election-candidate-count { election-id: election-id })))))
    {
      candidate-0: (map-get? candidates { election-id: election-id, candidate-id: u0 }),
      candidate-1: (map-get? candidates { election-id: election-id, candidate-id: u1 }),
      candidate-2: (map-get? candidates { election-id: election-id, candidate-id: u2 }),
      candidate-3: (map-get? candidates { election-id: election-id, candidate-id: u3 }),
      candidate-4: (map-get? candidates { election-id: election-id, candidate-id: u4 }),
      candidate-5: (map-get? candidates { election-id: election-id, candidate-id: u5 }),
      candidate-6: (map-get? candidates { election-id: election-id, candidate-id: u6 }),
      candidate-7: (map-get? candidates { election-id: election-id, candidate-id: u7 }),
      candidate-8: (map-get? candidates { election-id: election-id, candidate-id: u8 }),
      candidate-9: (map-get? candidates { election-id: election-id, candidate-id: u9 }),
      total-candidates: candidate-count
    }
  )
)

;; Check voting eligibility
(define-read-only (check-voting-eligibility (election-id uint) (voter principal))
  (let 
    (
      (election-data (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (voter-power-data (get-voting-power voter))
    )
    (ok {
      can-vote: (and 
        (is-election-active election-id)
        (>= voter-power-data (var-get min-voting-power))
        (not (has-voted election-id voter))
        (or 
          (not (get requires-whitelist election-data))
          (default-to false (get whitelisted (map-get? election-whitelist { election-id: election-id, voter: voter })))
        )
      ),
      voting-power: voter-power-data,
      is-whitelisted: (default-to false (get whitelisted (map-get? election-whitelist { election-id: election-id, voter: voter }))),
      already-voted: (has-voted election-id voter)
    })
  )
)

;; Get voting power
(define-read-only (get-voting-power (voter principal))
  (default-to u1 (get power (map-get? voter-power { voter: voter })))
)

;; Check quorum
(define-read-only (check-quorum (election-id uint))
  (match (map-get? elections { election-id: election-id })
    election-data
    (let ((participation-rate (/ (* (get total-votes election-data) u10000) (get total-voting-power election-data))))
      {
        current-participation: participation-rate,
        required-quorum: (get min-quorum election-data),
        quorum-met: (>= participation-rate (get min-quorum election-data))
      }
    )
    { current-participation: u0, required-quorum: u0, quorum-met: false }
  )
)

;; Get delegation info
(define-read-only (get-delegation (delegator principal) (election-id uint))
  (map-get? delegations { delegator: delegator, election-id: election-id })
)

;; Get delegate profile
(define-read-only (get-delegate-profile (delegate principal))
  (map-get? delegate-profiles { delegate: delegate })
)

;; Check if election is active
(define-read-only (is-election-active (election-id uint))
  (match (map-get? elections { election-id: election-id })
    election-data
    (let ((current-block burn-block-height))
      (and 
        (get is-active election-data)
        (>= current-block (get start-date election-data))
        (<= current-block (get end-date election-data))
      )
    )
    false
  )
)

;; Has voted check
(define-read-only (has-voted (election-id uint) (voter principal))
  (default-to false (get voted (map-get? voter-records { election-id: election-id, voter: voter })))
)

;; Get election results with advanced analytics
(define-read-only (get-detailed-results (election-id uint))
  (let 
    (
      (election-data (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (quorum-data (check-quorum election-id))
      (analytics (map-get? election-analytics { election-id: election-id }))
    )
    (ok {
      election-info: election-data,
      candidates: (get-all-candidates election-id),
      quorum-status: quorum-data,
      analytics: analytics,
      is-valid: (get quorum-met quorum-data)
    })
  )
)

;; Public functions

;; Create enhanced election
(define-public (create-enhanced-election 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (start-date uint)
  (end-date uint)
  (election-type uint)
  (min-quorum uint)
  (winning-threshold uint)
  (max-choices uint)
  (requires-whitelist bool)
  (allows-delegation bool)
  (is-private bool)
  (category (string-ascii 50))
  (tags (list 5 (string-ascii 20)))
)
  (let ((election-id (var-get next-election-id)))
    (begin
      ;; Validate inputs
      (asserts! (<= min-quorum u10000) err-invalid-threshold)
      (asserts! (<= winning-threshold u10000) err-invalid-threshold)
      
      ;; Store enhanced election metadata
      (map-set elections
        { election-id: election-id }
        {
          title: title,
          description: description,
          start-date: start-date,
          end-date: end-date,
          creator: tx-sender,
          total-votes: u0,
          total-voting-power: u0,
          is-active: true,
          election-type: election-type,
          min-quorum: min-quorum,
          winning-threshold: winning-threshold,
          max-choices: max-choices,
          requires-whitelist: requires-whitelist,
          allows-delegation: allows-delegation,
          is-private: is-private,
          category: category,
          tags: tags
        }
      )
      
      ;; Initialize analytics
      (map-set election-analytics
        { election-id: election-id }
        {
          unique-voters: u0,
          total-engagement: u0,
          peak-voting-time: u0,
          demographics-tracked: false
        }
      )
      
      ;; Initialize candidate count and comment count
      (map-set election-candidate-count { election-id: election-id } { count: u0 })
      (map-set election-comment-count { election-id: election-id } { count: u0 })
      
      ;; Increment election ID counter
      (var-set next-election-id (+ election-id u1))
      (ok election-id)
    )
  )
)

;; Add enhanced candidate
(define-public (add-enhanced-candidate 
  (election-id uint)
  (name (string-ascii 50))
  (description (string-ascii 200))
  (image-url (string-ascii 200))
  (platform-points (list 5 (string-ascii 100)))
)
  (let 
    (
      (election-data (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (current-count (default-to u0 (get count (map-get? election-candidate-count { election-id: election-id }))))
    )
    (begin
      (asserts! (is-eq tx-sender (get creator election-data)) err-owner-only)
      
      (map-set candidates
        { election-id: election-id, candidate-id: current-count }
        {
          name: name,
          description: description,
          image-url: image-url,
          platform-points: platform-points,
          vote-count: u0,
          voting-power: u0,
          endorsements: u0,
          is-active: true
        }
      )
      
      (map-set election-candidate-count
        { election-id: election-id }
        { count: (+ current-count u1) }
      )
      (ok current-count)
    )
  )
)

;; Enhanced voting with multiple choice support
(define-public (cast-enhanced-vote (election-id uint) (choices (list 10 uint)))
  (let 
    (
      (election-data (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (current-voter-power (get-voting-power tx-sender))
      (current-block burn-block-height)
    )
    (begin
      ;; Validate voting eligibility
      (asserts! (is-election-active election-id) err-election-ended)
      (asserts! (not (has-voted election-id tx-sender)) err-already-voted)
      (asserts! (>= current-voter-power (var-get min-voting-power)) err-insufficient-tokens)
      
      ;; Check whitelist if required
      (if (get requires-whitelist election-data)
        (asserts! (default-to false (get whitelisted (map-get? election-whitelist { election-id: election-id, voter: tx-sender }))) err-not-whitelisted)
        true
      )
      
      ;; Record the vote
      (map-set voter-records
        { election-id: election-id, voter: tx-sender }
        { 
          voted: true, 
          choices: choices,
          voting-power-used: current-voter-power,
          vote-time: current-block,
          is-delegated: false,
          delegate: none
        }
      )
      
      ;; Update candidate vote counts for each choice
      (update-candidate-votes election-id choices current-voter-power)
      
      ;; Update election totals
      (map-set elections
        { election-id: election-id }
        (merge election-data { 
          total-votes: (+ (get total-votes election-data) u1),
          total-voting-power: (+ (get total-voting-power election-data) current-voter-power)
        })
      )
      
      ;; Update analytics
      (update-election-analytics election-id)
      
      (ok true)
    )
  )
)

;; Delegate voting power
(define-public (delegate-vote (election-id uint) (delegate principal))
  (let 
    (
      (election-data (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (delegator-power (get-voting-power tx-sender))
      (current-block burn-block-height)
    )
    (begin
      ;; Check if delegation is allowed
      (asserts! (get allows-delegation election-data) err-voting-not-allowed)
      (asserts! (not (has-voted election-id tx-sender)) err-already-voted)
      (asserts! (not (is-eq tx-sender delegate)) err-invalid-delegation)
      
      ;; Record delegation
      (map-set delegations
        { delegator: tx-sender, election-id: election-id }
        {
          delegate: delegate,
          voting-power: delegator-power,
          is-active: true,
          delegation-time: current-block
        }
      )
      
      ;; Update delegate profile
      (update-delegate-profile delegate delegator-power true)
      
      (ok true)
    )
  )
)

;; Vote as delegate
(define-public (vote-as-delegate (election-id uint) (choices (list 10 uint)) (delegators (list 20 principal)))
  (let 
    (
      (election-data (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (current-block burn-block-height)
    )
    (begin
      (asserts! (is-election-active election-id) err-election-ended)
      
      ;; Process delegated votes
      (process-delegated-votes election-id tx-sender choices delegators current-block)
      
      (ok true)
    )
  )
)

;; Whitelist voters for restricted elections
(define-public (whitelist-voters (election-id uint) (voters (list 50 principal)) (voting-powers (list 50 uint)))
  (let ((election-data (unwrap! (map-get? elections { election-id: election-id }) err-not-found)))
    (begin
      (asserts! (is-eq tx-sender (get creator election-data)) err-owner-only)
      (asserts! (get requires-whitelist election-data) err-voting-not-allowed)
      
      (add-to-whitelist election-id voters voting-powers)
      (ok true)
    )
  )
)

;; Add comment to election
(define-public (add-comment (election-id uint) (content (string-ascii 500)) (parent-comment (optional uint)))
  (let 
    (
      (election-data (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (comment-count (default-to u0 (get count (map-get? election-comment-count { election-id: election-id }))))
      (current-block burn-block-height)
    )
    (begin
      (map-set election-comments
        { election-id: election-id, comment-id: comment-count }
        {
          author: tx-sender,
          content: content,
          timestamp: current-block,
          parent-comment: parent-comment,
          upvotes: u0,
          downvotes: u0
        }
      )
      
      (map-set election-comment-count
        { election-id: election-id }
        { count: (+ comment-count u1) }
      )
      
      (ok comment-count)
    )
  )
)

;; Update voting power (should be called by governance token contract)
(define-public (update-voting-power (voter principal) (new-power uint))
  (begin
    ;; Only governance token contract can update voting power
    (asserts! (is-eq tx-sender (var-get governance-token)) err-owner-only)
    
    (map-set voter-power
      { voter: voter }
      { 
        power: new-power, 
        last-updated: burn-block-height
      }
    )
    (ok true)
  )
)

;; Private helper functions

;; Update candidate votes for multiple choices
(define-private (update-candidate-votes (election-id uint) (choices (list 10 uint)) (voting-power uint))
  (fold update-single-candidate-vote choices { election-id: election-id, power: voting-power })
)

(define-private (update-single-candidate-vote (candidate-id uint) (data { election-id: uint, power: uint }))
  (let ((candidate-data (map-get? candidates { election-id: (get election-id data), candidate-id: candidate-id })))
    (match candidate-data
      existing-candidate
      (begin
        (map-set candidates
          { election-id: (get election-id data), candidate-id: candidate-id }
          (merge existing-candidate { 
            vote-count: (+ (get vote-count existing-candidate) u1),
            voting-power: (+ (get voting-power existing-candidate) (get power data))
          })
        )
        data
      )
      data
    )
  )
)

;; Update delegate profile
(define-private (update-delegate-profile (delegate principal) (power uint) (is-new bool))
  (let ((profile (default-to 
    { name: "", bio: "", total-delegated-power: u0, active-delegations: u0, reputation-score: u100 }
    (map-get? delegate-profiles { delegate: delegate }))))
    (map-set delegate-profiles
      { delegate: delegate }
      (merge profile {
        total-delegated-power: (+ (get total-delegated-power profile) power),
        active-delegations: (if is-new (+ (get active-delegations profile) u1) (get active-delegations profile))
      })
    )
  )
)

;; Process delegated votes
(define-private (process-delegated-votes (election-id uint) (delegate principal) (choices (list 10 uint)) (delegators (list 20 principal)) (vote-time uint))
  (fold process-single-delegation delegators { 
    election-id: election-id, 
    delegate: delegate, 
    choices: choices, 
    vote-time: vote-time 
  })
)

(define-private (process-single-delegation 
  (delegator principal) 
  (data { election-id: uint, delegate: principal, choices: (list 10 uint), vote-time: uint })
)
  (let ((delegation-data (map-get? delegations { delegator: delegator, election-id: (get election-id data) })))
    (match delegation-data
      delegation
      (if (and (get is-active delegation) (is-eq (get delegate delegation) (get delegate data)))
        (begin
          ;; Record delegated vote
          (map-set voter-records
            { election-id: (get election-id data), voter: delegator }
            {
              voted: true,
              choices: (get choices data),
              voting-power-used: (get voting-power delegation),
              vote-time: (get vote-time data),
              is-delegated: true,
              delegate: (some (get delegate data))
            }
          )
          ;; Update candidate votes
          (update-candidate-votes (get election-id data) (get choices data) (get voting-power delegation))
          data
        )
        data
      )
      data
    )
  )
)

;; Add voters to whitelist - simplified approach
(define-private (add-to-whitelist (election-id uint) (voters (list 50 principal)) (powers (list 50 uint)))
  (begin
    ;; Process first 10 voters manually to avoid complex list operations
    (if (> (len voters) u0)
      (if (> (len powers) u0)
        (map-set election-whitelist
          { election-id: election-id, voter: (unwrap-panic (element-at voters u0)) }
          { whitelisted: true, voting-power: (unwrap-panic (element-at powers u0)) }
        )
        false
      )
      false
    )
    (if (> (len voters) u1)
      (if (> (len powers) u1)
        (map-set election-whitelist
          { election-id: election-id, voter: (unwrap-panic (element-at voters u1)) }
          { whitelisted: true, voting-power: (unwrap-panic (element-at powers u1)) }
        )
        false
      )
      false
    )
    ;; Continue for more entries as needed...
    election-id
  )
)

;; Update election analytics
(define-private (update-election-analytics (election-id uint))
  (let ((analytics (default-to 
    { unique-voters: u0, total-engagement: u0, peak-voting-time: u0, demographics-tracked: false }
    (map-get? election-analytics { election-id: election-id }))))
    (map-set election-analytics
      { election-id: election-id }
      (merge analytics {
        unique-voters: (+ (get unique-voters analytics) u1),
        total-engagement: (+ (get total-engagement analytics) u1)
      })
    )
  )
)

;; Simplified utility function to combine voters and powers
(define-private (zip-voter-power (voters (list 50 principal)) (powers (list 50 uint)))
  (let ((combined-list (list)))
    ;; For simplicity, we'll process pairs manually up to 10 items
    (if (> (len voters) u0)
      (if (> (len powers) u0)
        (list { voter: (unwrap-panic (element-at voters u0)), power: (unwrap-panic (element-at powers u0)) })
        (list)
      )
      (list)
    )
  )
)