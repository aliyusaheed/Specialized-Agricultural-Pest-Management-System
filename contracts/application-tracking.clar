;; Application Tracking Contract
;; Monitors usage of pest control methods

;; Data Variables
(define-data-var application-counter uint u0)

;; Data Maps
(define-map applications
  { application-id: uint }
  {
    applicator: principal,
    protocol-id: uint,
    location: (string-utf8 100),
    coordinates: (list 2 int),
    field-size: uint,
    crop: (string-utf8 50),
    application-date: uint,
    weather-conditions: (string-utf8 100),
    quantity-used: uint,
    effectiveness-notes: (string-utf8 500),
    follow-up-needed: bool,
    follow-up-date: (optional uint)
  }
)

(define-map protocol-applications
  { protocol-id: uint }
  { application-ids: (list 100 uint) }
)

(define-map location-applications
  { location: (string-utf8 100) }
  { application-ids: (list 100 uint) }
)

(define-map crop-applications
  { crop: (string-utf8 50) }
  { application-ids: (list 100 uint) }
)

(define-map applicator-applications
  { applicator: principal }
  { application-ids: (list 100 uint) }
)

;; Record a new treatment application
(define-public (record-application
    (protocol-id uint)
    (location (string-utf8 100))
    (coordinates (list 2 int))
    (field-size uint)
    (crop (string-utf8 50))
    (weather-conditions (string-utf8 100))
    (quantity-used uint)
    (effectiveness-notes (string-utf8 500))
    (follow-up-needed bool)
    (follow-up-date (optional uint)))
  (let ((application-id (var-get application-counter)))
    (begin
      ;; Store the application information
      (map-set applications
        { application-id: application-id }
        {
          applicator: tx-sender,
          protocol-id: protocol-id,
          location: location,
          coordinates: coordinates,
          field-size: field-size,
          crop: crop,
          application-date: block-height,
          weather-conditions: weather-conditions,
          quantity-used: quantity-used,
          effectiveness-notes: effectiveness-notes,
          follow-up-needed: follow-up-needed,
          follow-up-date: follow-up-date
        }
      )

      ;; Update protocol-to-application mapping
      (let ((protocol-list (default-to { application-ids: (list) } (map-get? protocol-applications { protocol-id: protocol-id }))))
        (map-set protocol-applications
          { protocol-id: protocol-id }
          { application-ids: (unwrap-panic (as-max-len? (append (get application-ids protocol-list) application-id) u100)) }
        )
      )

      ;; Update location-to-application mapping
      (let ((location-list (default-to { application-ids: (list) } (map-get? location-applications { location: location }))))
        (map-set location-applications
          { location: location }
          { application-ids: (unwrap-panic (as-max-len? (append (get application-ids location-list) application-id) u100)) }
        )
      )

      ;; Update crop-to-application mapping
      (let ((crop-list (default-to { application-ids: (list) } (map-get? crop-applications { crop: crop }))))
        (map-set crop-applications
          { crop: crop }
          { application-ids: (unwrap-panic (as-max-len? (append (get application-ids crop-list) application-id) u100)) }
        )
      )

      ;; Update applicator-to-application mapping
      (let ((applicator-list (default-to { application-ids: (list) } (map-get? applicator-applications { applicator: tx-sender }))))
        (map-set applicator-applications
          { applicator: tx-sender }
          { application-ids: (unwrap-panic (as-max-len? (append (get application-ids applicator-list) application-id) u100)) }
        )
      )

      ;; Increment counter and return application ID
      (var-set application-counter (+ application-id u1))
      (ok application-id)
    )
  )
)

;; Update follow-up information
(define-public (update-follow-up
    (application-id uint)
    (follow-up-needed bool)
    (follow-up-date (optional uint)))
  (let ((application-data (map-get? applications { application-id: application-id })))
    (if (and (is-some application-data) (is-eq tx-sender (get applicator (unwrap-panic application-data))))
      (begin
        (map-set applications
          { application-id: application-id }
          (merge (unwrap-panic application-data) {
            follow-up-needed: follow-up-needed,
            follow-up-date: follow-up-date
          })
        )
        (ok true)
      )
      (err u1) ;; Not the applicator or application doesn't exist
    )
  )
)

;; Update effectiveness notes
(define-public (update-effectiveness-notes
    (application-id uint)
    (effectiveness-notes (string-utf8 500)))
  (let ((application-data (map-get? applications { application-id: application-id })))
    (if (and (is-some application-data) (is-eq tx-sender (get applicator (unwrap-panic application-data))))
      (begin
        (map-set applications
          { application-id: application-id }
          (merge (unwrap-panic application-data) {
            effectiveness-notes: effectiveness-notes
          })
        )
        (ok true)
      )
      (err u1) ;; Not the applicator or application doesn't exist
    )
  )
)

;; Get application details by ID
(define-read-only (get-application (application-id uint))
  (map-get? applications { application-id: application-id })
)

;; Get all applications for a specific protocol
(define-read-only (get-applications-by-protocol (protocol-id uint))
  (default-to { application-ids: (list) } (map-get? protocol-applications { protocol-id: protocol-id }))
)

;; Get all applications at a specific location
(define-read-only (get-applications-by-location (location (string-utf8 100)))
  (default-to { application-ids: (list) } (map-get? location-applications { location: location }))
)

;; Get all applications for a specific crop
(define-read-only (get-applications-by-crop (crop (string-utf8 50)))
  (default-to { application-ids: (list) } (map-get? crop-applications { crop: crop }))
)

;; Get all applications by a specific applicator
(define-read-only (get-applications-by-applicator (applicator principal))
  (default-to { application-ids: (list) } (map-get? applicator-applications { applicator: applicator }))
)

;; Get applications needing follow-up
(define-read-only (get-applications-needing-follow-up)
  (ok {
    message: u"To find applications needing follow-up, please query by specific protocol, location, crop, or applicator and check the follow-up-needed status"
  })
)

;; Get the total number of recorded applications
(define-read-only (get-application-count)
  (var-get application-counter)
)

