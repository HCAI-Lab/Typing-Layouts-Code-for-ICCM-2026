; Linear/Abacus manual RT model: corrected version.
; Keeps the intended structure:
; letter -> number -> retrieve non-dominant hand -> retrieve dominant hand
; -> prepare one abstract two-hand manual gesture -> next character.
; No imaginal buffer productions are used.

(clear-all)

(defun get-tens (number)
  (* (floor (/ number 10)) 10))

(defun get-units (number)
  "Extracts the units digit from a number in the range 0-99."
  (mod number 10))

(defparameter *global-sentence* "")

(defun set-global-sentence (sentence)
  "Sets the given sentence as the global variable *global-sentence*."
  (when (stringp sentence)
    (setf *global-sentence* sentence)
    (length *global-sentence*)))

(defun char-at-index (index)
  "Returns the character at the given index in *global-sentence* as a lowercase string. If out of bounds, returns 0."
  (let ((sentence *global-sentence*))
    (if (or (< index 0) (>= index (length sentence)))
        "0"
        (string-downcase (string (char sentence index))))))

(defun random-sentences ()
  "Select 5 random sentences from a hardcoded list and join them with spaces."
  (let ((sentences '("peanut butter is very tasty"
                     "my watch fell in the water"
                     "never too rich and never too thin"
                     "we are having spaghetti"
                     "time to go shopping"
                     "my favorite place to visit"
                     "circumstances are unacceptable"
                     "please provide your date of birth"
                     "we run the risk of failure"
                     "he is just like everyone else"
                     "love means many things"
                     "you must be getting old"
                     "the world is a stage"
                     "one heck of a question"
                     "question that must be answered"
                     "I agree with you"
                     "do not say anything"
                     "the force is with you"
                     "an offer you cannot refuse"
                     "the dreamers of dreams"
                     "space is a high priority"
                     "you are a wonderful example"
                     "take a coffee break"
                     "the music is better than it sounds"
                     "come and see our new car"
                     "i am going to a music lesson"
                     "the quick brown fox jumped"
                     "there will be some fog tonight"
                     "my preferred treat is chocolate"
                     "we went grocery shopping"
                     "the assignment is due today"
                     "for your information only"
                     "a quarter of a century"
                     "this watch is too expensive"
                     "the back yard of our house"
                     "thank you for your help"
                     "vanilla flavored ice cream"
                     "the early bird gets the worm"
                     "the library is closed today"
                     "my favorite web browser"
                     "just in time for the party"
                     "life is but a dream"
                     "fall is my favorite season"
                     "the capital of our nation"
                     "the store will close at ten"
                     "healthy food is good for you"
                     "this is a very good idea"
                     "have a good weekend"
                     "the second largest country"
                     "this is too much to handle"
                     "the kids are very excited"
                     "my bike has a flat tire"
                     "the sun rises in the east"
                     "it is very windy today"
                     "do not worry about this"
                     "want to join us for lunch"
                     "I can play much better now"
                     "batman wears a cape"
                     "toss the ball around"
                     "win first prize in the contest"
                     "the sum of the parts"
                     "nobody cares anymore"
                     "the children are playing"
                     "can we play cards tonight"
                     "i listen to the tape everyday"
                     "be home before midnight"
                     "join us on the patio"
                     "what a lovely red jacket"
                     "my mother makes good cookies"
                     "where did i leave my glasses"
                     "do not drink the water"
                     "rain rain go away"
                     "the price of gas is high"
                     "effort is what it will take"
                     "i like to play tennis"
                     "please follow the guidelines"
                     "do you like to go camping"
                     "you should visit to a doctor"
                     "he called seven times"
                     "just like it says on the can"
                     "it should be sunny tomorrow"
                     "the fax machine is broken"
                     "do you prefer a window seat"
                     "they watched the entire movie"
                     "the winner of the race"
                     "the bus was very crowded"
                     "electric cars need big fuel cells"
                     "sad to hear that news"
                     "an airport is a very busy place"
                     "do not feel too bad about it"
                     "what goes up must come down"
                     "the minimum amount of time"
                     "a coupon for a free sample"
                     "are you sure you want this"
                     "that is a very odd question"
                     "i just cannot figure this out"
                     "the trains are always late"
                     "two or three cups of coffee"
                     "the most beautiful sunset"
                     "a subject one can really enjoy"
                     "time to go shopping")))
    (reduce (lambda (a b) (concatenate 'string a " " b))
            (loop repeat 5 collect (nth (random (length sentences)) sentences)))))

(defun random-letter ()
  (if (= (random 5) 4)
      " "
      (string (char "abcdefghijklmnopqrstuvwxyz" (random 26)))))

(define-model LinearAbacus

  (sgp          
		;--------------------------------
		;PRODUCTION-COMPILATION module
		;--------------------------------
		:TT                         2                ;default: 2.0        : Threshold time
		;:PCT                         T                ;default: NIL        : Production Compilation Trace
		;:EPL                         T                ;default: NIL        : Enable Production Learning
		;--------------------------------
		;DECLARATIVE module
		;--------------------------------
		:SPREADING-HOOK              NIL              ;default: NIL        : Spreading component hook
		;:MAS                         -2              ;default: NIL        : Maximum Associative Strength
		:ACTIVATION-OFFSETS          NIL              ;default: NIL        : Add additional activation equation components
		:RT                          -10            ;default: 0.0        : Retrieval Threshold
		;:CHUNK-MERGE-HOOK            T              ;default: NIL        : Hook called when a chunk is merged into dm
;		:MD                          -1.0             ;default: -1.0       : Maximum Difference
		:BLL                         0.5              ;default: NIL        : Base Level Learning
		:LF                          0.6
             ;default: 1.0        : Latency Factor
		:PAS                         NIL             ;default: NIL        : Permanent Activation noise S
		;:PM                          NIL              ;default: NIL        : Depricated - use :mp as both the flag and value instead (like :bll and :mas)
		:RETRIEVAL-REQUEST-HOOK      NIL              ;default: NIL        : Retrieval notification hook
		:LE                          1             ;default: 1.0        : Latency Exponent
		;:ANS                         0.5             ;default: NIL        : Activation Noise S
		:BL-HOOK                     NIL              ;default: NIL        : Baselevel component hook
		;:MS                          0.1              ;default: 0.0        : Maximum Similarity
		:PARTIAL-MATCHING-HOOK       NIL              ;default: NIL        : Partial matching component hook
		:SIM-HOOK                    NIL              ;default: NIL        : Similarity hook
		:RETRIEVAL-SET-HOOK          NIL              ;default: NIL        : Prospective retrievals hook
		:MP                          NIL              ;default: NIL        : Mismatch Penalty
		:RETRIEVED-CHUNK-HOOK        NIL              ;default: NIL        : Retrieval completion hook
		:DECLARATIVE-NUM-FINSTS      4                ;default: 4          : Number of declarative finst markers
		:DECLARATIVE-FINST-SPAN      3.0              ;default: 3.0        : Duration of declarative finst markers in seconds
		;:ACT                         T              ;default: NIL        : Activation Trace
		:CHUNK-ADD-HOOK              NIL              ;default: NIL        : Hook called when a chunk is added to dm
		:SJI-HOOK                    nil              ;default: NIL        : Sji hook
		;:BLC                         0.1              ;default: 0.0        : Base Level Constant
		;:FAST-MERGE                  T                ;default: T          : Whether or not to use the fast merge mechanism
		:NOISE-HOOK                  NIL              ;default: NIL        : Noise component hook
		:W-HOOK                      NIL              ;default: NIL        : Wkj hook
		;--------------------------------
		;BUFFER-HISTORY module
		;--------------------------------
		;:SAVE-BUFFER-HISTORY         NIL              ;default: NIL        : Whether or not to record the history of buffer changes.
		;--------------------------------
		;PROCEDURAL module
		;--------------------------------
		;:CST                         NIL              ;default: NIL        : Conflict Set Trace
		;:USE-TREE                    NIL              ;default: NIL        : Use a decision tree in production matching
		;:CRT                         NIL              ;default: NIL        : Conflict Resolution Trace
		;:PPM-HOOK                    NIL              ;default: NIL        : Procedural partial matching utility adjustment hook
		;:DO-NOT-HARVEST              NIL       		  ;default: NIL        : Buffers that are not strict harvested
		;:CONFLICT-SET-HOOK           NIL              ;default: NIL        : Conflict set hook
		;:RHST                        T                ;default: T          : Right Hand Side Trace
		;:VPFT                        NIL              ;default: NIL        : Variable Production Firing Time
		;:PPM                         NIL              ;default: NIL        : Procedural partial matching
		:DAT                         0.025             ;default: 0.05       : Default Action Time
		;:LHST                        T                ;default: T          : Left Hand Side Trace
		;:CYCLE-HOOK                  NIL              ;default: NIL        : Cycle hook
		;--------------------------------
		;RETRIEVAL-HISTORY module
		;--------------------------------
		;:SAVE-DM-HISTORY             NIL              ;default: NIL        : Whether or not to record the history of all retrieval events.
		;--------------------------------
		;CENTRAL-PARAMETERS module
		;--------------------------------
		;:OL                          NIL                ;default: T          : Optimized Learning
		:ESC                         T                ;default: NIL        : Enable Subsymbolic Computations
		:ER                          T              ;default: NIL        : Enable Randomness
		;--------------------------------
		;BUFFER-TRACE module
		;--------------------------------
		;:BUFFER-TRACE                NIL              ;Farnaz;default: NIL        : Display the trace as a buffer summary instead of as an event list.
		;:SAVE-BUFFER-TRACE           T              ;default: NIL        : Whether to save the buffer summary for a run or not.
		:TRACED-BUFFERS              NIL                ;default: T          : The list of buffers to be traced (all buffers if set to t).
		;:BUFFER-TRACE-COLORS         NIL              ;default: NIL        : The colors used to draw the buffer data using the graphic tracing tool.
		;:BUFFER-TRACE-HOOK           NIL              ;default: NIL        : A function to call with each buffer summary.
		;:BUFFER-TRACE-STEP           NIL              ;default: NIL        : The maximum amount of time allowed to elapse before creating a buffer summary.
		;:GRAPHIC-COLUMN-WIDTHS       NIL              ;default: NIL        : The pixel width of the columns drawn for the buffers using the graphic tracing tool.
		;--------------------------------
		;BOLD module
		;--------------------------------
		;:POINT-PREDICT               (GOAL)           ;default: (GOAL)     : List of buffers for which the point based computation should be used to compute the BOLD response.
		;:BOLD-SCALE                  0.75             ;default: 0.75       : Scale parameter for computing the BOLD response.
		;:NEG-BOLD-SCALE              1                ;default: 1          : Scale parameter for computing a negative component of the BOLD response.
		;:NEG-BOLD-EXP                15               ;default: 15         : Exponenet parameter for computing a negative component of the BOLD response.
		;:BOLD-POSITIVE               1                ;default: 1          : Factor for the positive component of the hemodynamic response curve.
		;:BOLD-INC                    1.5              ;default: 1.5        : Time increment in seconds for computing the BOLD response.
		;:BOLD-SETTLE                 40               ;default: 40         : Time window in seconds for computing the BOLD response.
		;:BOLD-NEGATIVE               0                ;default: 0          : Factor for the negative component of the hemodynamic response curve.
		;:BOLD-EXP                    6                ;default: 6          : Exponenet parameter for computing the BOLD response.
		;--------------------------------
		;RANDOM-MODULE module
		;--------------------------------
		;:SEED                        (947718053 7352) ;default: NO-DEFAULT : Current seed of the random number generator
		;:RANDOMIZE-TIME              NIL              ;default: NIL        : Allows the timing of certain actions to vary
		;--------------------------------
		;NAMING-MODULE module
		;--------------------------------
		;:DCNN                        T                ;default: T          : Dynamic chunk name normalizing
		;:DCSC-HOOK                   NIL		      ;default: NIL        : Hook called when a chunk is changed due to normalizing
		;:NCNAR                       NIL              ;default: T          : Normalize chunk names after run
		;:SHORT-COPY-NAMES            NIL              ;default: NIL        : Flag to signal how copied chunks are named
		;--------------------------------
		;TEMPORAL module
		;--------------------------------
		;:TIME-NOISE                  0.015            ;default: 0.015      : Temporal noise
		;:TIME-MULT                   1.1              ;default: 1.1        : Temporal multiplier
		;:RECORD-TICKS                T                ;default: T          : Record each time increment as a buffer event
		;:TIME-MASTER-START-INCREMENT 0.011            ;default: 0.011      : Temporal start interval
		;--------------------------------
		;UTILITY module
		;--------------------------------
		;:EGS                         NIL              ;default: 0.0        : Expected Gain S
		;:UT                          NIL              ;default: NIL        : Utility Threshold
		;:UTILITY-OFFSETS             NIL              ;default: NIL        : Add additional utility equation components
;		:NU                          20               ;default: 0          : default U(0) value for a newly learned production
		;:UL                          T                ;default: NIL        : Utility learning switch
		;:ALPHA                       0.2              ;default: 0.2        : Production learning rate
;		:IU                          10               ;default: 0          : default U(0) value for an initial (user defined) production
		;:UTILITY-HOOK                NIL              ;default: NIL        : Utility computation hook
		;--------------------------------
		;VISION module
		;--------------------------------
		;:VISUAL-ONSET-SPAN           0.5              ;default: 0.5        : Lifespan of new visual objects being marked as NEW
		;:SCENE-CHANGE-THRESHOLD      0.25             ;default: 0.25       : Proportion of visicon which must change to signal a scene change
		;:VISUAL-MOVEMENT-TOLERANCE   0.5              ;default: 0.5        : How far something can move while still being seen as the same object.
		;:VISUAL-NUM-FINSTS           4                ;default: 4          : Number of visual finsts.
		;:TEST-FEATS                  T                ;default: T          : Whether proc-display should use the features to compare items instead of just the chunk names
		;:DELETE-VISICON-CHUNKS       T                ;default: T          : Whether proc-display should delete and unintern the name of old chunks that were in the visicon
		;:VISUAL-FINST-SPAN           3.0              ;default: 3.0        : Lifespan of a visual finst
		:VISUAL-ATTENTION-LATENCY    0.01            ;default: 0.085      : Time for a shift of visual attention
		;:AUTO-ATTEND                 NIL              ;default: NIL        : Whether visual-location requests automatically generate an attention shift
		;:OPTIMIZE-VISUAL             T                ;default: T          : 
		;--------------------------------
		;BUFFER-PARAMS module
		;--------------------------------
		;:AURAL-LOCATION-ACTIVATION   0                ;default: 0          : source spread for the AURAL-LOCATION buffer
		;:RETRIEVAL-ACTIVATION        0                ;default: 0          : source spread for the RETRIEVAL buffer
		;:IMAGINAL-ACTION-ACTIVATION  0                ;default: 0          : source spread for the IMAGINAL-ACTION buffer
		;:MANUAL-ACTIVATION           0                ;default: 0          : source spread for the MANUAL buffer
		;:AURAL-ACTIVATION            0                ;default: 0          : source spread for the AURAL buffer
		;:IMAGINAL-ACTIVATION         0                ;default: 0          : source spread for the IMAGINAL buffer
		;:PRODUCTION-ACTIVATION       0                ;default: 0          : source spread for the PRODUCTION buffer
		;:VISUAL-LOCATION-ACTIVATION  0                ;default: 0          : source spread for the VISUAL-LOCATION buffer
		;:VOCAL-ACTIVATION            0                ;default: 0          : source spread for the VOCAL buffer
		;:GA                          1.0              ;default: 1.0        : source spread for the GOAL buffer
		;:VISUAL-ACTIVATION           0                ;default: 0          : source spread for the VISUAL buffer
		;:TEMPORAL-ACTIVATION         0                ;default: 0          : source spread for the TEMPORAL buffer
		;--------------------------------
		;DEVICE module
		;--------------------------------
		;:VWT                         NIL              ;default: NIL        : Virtual Window trace controls the << ... >> outputs from virtual windows
		;:PROCESS-CURSOR              NIL              ;default: NIL        : Should there be a visicon feature for the cursor?
		;:NEEDS-MOUSE                 T                ;default: T          : Does ACT-R control the mouse?
		;:MOUSE-FITTS-COEFF           0.1              ;default: 0.1        : b coefficient in Fitts's equation for aimed movements.
		;:PIXELS-PER-INCH             72.0             ;default: 72.0       : Pixels per inch of display
		;:STABLE-LOC-NAMES            T                ;default: T          : Whether or not to sort the virtual window's subviews to guarantee the names always line up
		;:VIEWING-DISTANCE            15.0             ;default: 15.0       : Distance of the eyes from the display, in inches.
		;:SHOW-FOCUS                  NIL              ;default: NIL        : Show the focus ring on the GUI?
		;--------------------------------
		;PRODUCTION-HISTORY module
		;--------------------------------
		;:P-HISTORY-COLORS            NIL              ;default: NIL        : The colors to use for the selected, other matched, and mismatched cells respectively.
		;:SAVE-P-HISTORY              NIL              ;default: NIL        : Whether or not to record the utility and whynot history of all conflict-resolution events.
		;:DRAW-BLANK-COLUMNS          NIL                ;default: T          : Whether or not to draw the columns which have no matched productions.
		;--------------------------------
		;AUDIO module
		;--------------------------------
		;:TONE-RECODE-DELAY           0.285            ;default: 0.285      : Recoding delay for tone sound content.
		;:SOUND-DECAY-TIME            3.0              ;default: 3.0        : The amount of time after a sound has finished it takes for the sound to be deleted from the audicon
		;:HEAR-NEWEST-ONLY            NIL              ;default: NIL        : Whether to stuff only the newest unattended audio-event from the audicon into the aural-location buffer.
		;:TONE-DETECT-DELAY           0.05             ;default: 0.05       : Lag between sound onset and detectability for tones
		;:DIGIT-RECODE-DELAY          0.5              ;default: 0.5        : Recoding delay for digit sound content.
		;:DIGIT-DURATION              0.6              ;default: 0.6        : Default duration for digit sounds.
		;:DIGIT-DETECT-DELAY          0.3              ;default: 0.3        : Lag between onset and detectability for digits
		;--------------------------------
		;SPEECH module
		;--------------------------------
		;:SYLLABLE-RATE               0.15             ;default: 0.15       : Seconds per syllable.
		;:SUBVOCALIZE-DETECT-DELAY    0.3              ;default: 0.3        : Sound detect time for a subvocalized word.
		;:CHAR-PER-SYLLABLE           3                ;default: 3          : Characters per syllable.
		;--------------------------------
		;PRINTING-MODULE module
		;--------------------------------
		;:CMDT                        T                ;default: T          : Commands trace controls output of commands
		;:SHOW-ALL-SLOTS              NIL              ;default: NIL        : Whether or not to show unfilled extended slots when printing chunks
		;:TRACE-FILTER                NIL              ;default: NIL        : Function to limit output shown in the trace
		:TRACE-DETAIL                low             ;default: MEDIUM     : Determines which events show in the trace
		:MODEL-WARNINGS               NIL              ;default: T          : Whether to output model warnings
		:V                           T                ;default: T          : Verbose controls model output
		;:CBCT                        NIL              ;default: NIL        : Whether or not to show an event in the trace when a buffer copies a chunk
		;--------------------------------
		;IMAGINAL module
		;--------------------------------
		;:IMAGINAL-DELAY              0.00001              ;default: 0.2        : Time in seconds to respond to an imaginal request
		;:VIDT                        NIL              ;default: NIL        : Variable Imaginal Delay Time
		;--------------------------------
		;MOTOR module
		;--------------------------------
		:MIN-FITTS-TIME              0.05              ;default: 0.1        : Minimum movement time for an aimed [Fitts's] movement.
		;:CURSOR-NOISE                NIL              ;default: NIL        : Is there noise in the final cursor location.
		:MOTOR-FEATURE-PREP-TIME     0.03             ;default: 0.05       : Time to prepare a movement feature.
		;:INCREMENTAL-MOUSE-MOVES     NIL              ;default: NIL        : Output mouse moves in stages?
		;:MOTOR-BURST-TIME            0.05             ;default: 0.05       : Minimum time for any movement.
		:MOTOR-INITIATION-TIME       0.03             ;default: 0.05       : Time to initiate a motor movement.
		:PECK-FITTS-COEFF            0.03            ;default: 0.075      : b coefficient in Fitts's equation for PECK movements.
		;:DEFAULT-TARGET-WIDTH        1.0              ;default: 1.0        : Effective width, in degrees visual angle, of targets with undefined widths.
		)

  (chunk-type non-dominant-number
      number2
      non-dominant-index
      non-dominant-middle
      non-dominant-ring
      non-dominant-little
      non-dominant-thumb)

  (chunk-type dominant-number
      number1
      dominant-index
      dominant-middle
      dominant-ring
      dominant-little
      dominant-thumb)

  (chunk-type letter
      num
      letter)

  (chunk-type air-typing
      start
      end
      wordcount
      charcount
      temp
      eval
      begin
      sentence
      char
      number
      nondominant-number
      dominant-number
      nd-thumb-state
      nd-index-state
      nd-middle-state
      nd-ring-state
      nd-little-state
      d-thumb-state
      d-index-state
      d-middle-state
      d-ring-state
      d-little-state)

  (buffer-chunk)

  (add-dm

   ;; Dominant hand: units 0-9.
   (n0 ISA dominant-number number1 0 dominant-index 0 dominant-middle 0 dominant-ring 0 dominant-little 0 dominant-thumb 0)
   (n1 ISA dominant-number number1 1 dominant-index 1 dominant-middle 0 dominant-ring 0 dominant-little 0 dominant-thumb 0)
   (n2 ISA dominant-number number1 2 dominant-index 1 dominant-middle 1 dominant-ring 0 dominant-little 0 dominant-thumb 0)
   (n3 ISA dominant-number number1 3 dominant-index 1 dominant-middle 1 dominant-ring 1 dominant-little 0 dominant-thumb 0)
   (n4 ISA dominant-number number1 4 dominant-index 1 dominant-middle 1 dominant-ring 1 dominant-little 1 dominant-thumb 0)
   (n5 ISA dominant-number number1 5 dominant-index 0 dominant-middle 0 dominant-ring 0 dominant-little 0 dominant-thumb 1)
   (n6 ISA dominant-number number1 6 dominant-index 1 dominant-middle 0 dominant-ring 0 dominant-little 0 dominant-thumb 1)
   (n7 ISA dominant-number number1 7 dominant-index 1 dominant-middle 1 dominant-ring 0 dominant-little 0 dominant-thumb 1)
   (n8 ISA dominant-number number1 8 dominant-index 1 dominant-middle 1 dominant-ring 1 dominant-little 0 dominant-thumb 1)
   (n9 ISA dominant-number number1 9 dominant-index 1 dominant-middle 1 dominant-ring 1 dominant-little 1 dominant-thumb 1)

   ;; Non-dominant hand: tens used by a-z plus space.
   (n10 ISA non-dominant-number number2 10 non-dominant-index 1 non-dominant-middle 0 non-dominant-ring 0 non-dominant-little 0 non-dominant-thumb 0)
   (n20 ISA non-dominant-number number2 20 non-dominant-index 1 non-dominant-middle 1 non-dominant-ring 0 non-dominant-little 0 non-dominant-thumb 0)
   (n30 ISA non-dominant-number number2 0 non-dominant-index 0 non-dominant-middle 0 non-dominant-ring 0 non-dominant-little 0 non-dominant-thumb 0)

   ;; Letter to number mapping.
   (l0 ISA letter letter " " num 0)
   (l1 ISA letter letter "a" num 1)
   (l2 ISA letter letter "b" num 2)
   (l3 ISA letter letter "c" num 3)
   (l4 ISA letter letter "d" num 4)
   (l5 ISA letter letter "e" num 5)
   (l6 ISA letter letter "f" num 6)
   (l7 ISA letter letter "g" num 7)
   (l8 ISA letter letter "h" num 8)
   (l9 ISA letter letter "i" num 9)
   (l10 ISA letter letter "j" num 10)
   (l11 ISA letter letter "k" num 11)
   (l12 ISA letter letter "l" num 12)
   (l13 ISA letter letter "m" num 13)
   (l14 ISA letter letter "n" num 14)
   (l15 ISA letter letter "o" num 15)
   (l16 ISA letter letter "p" num 16)
   (l17 ISA letter letter "q" num 17)
   (l18 ISA letter letter "r" num 18)
   (l19 ISA letter letter "s" num 19)
   (l20 ISA letter letter "t" num 20)
   (l21 ISA letter letter "u" num 21)
   (l22 ISA letter letter "v" num 22)
   (l23 ISA letter letter "w" num 23)
   (l24 ISA letter letter "x" num 24)
   (l25 ISA letter letter "y" num 25)
   (l26 ISA letter letter "z" num 26)

   (first-goal ISA air-typing start 0 end 10000 begin 0))

  (goal-focus first-goal)


  (p start
     =goal>
     ISA         air-typing
     start       =num
     begin       =n
     wordcount   nil
     charcount   nil
     eval        nil
     ==>
     !bind!   =f (random-sentences)
     =goal>
     ISA         air-typing
     wordcount   =num
     charcount   =n
     sentence    =f
     eval        0)

  (p globalvariable
     =goal>
     ISA         air-typing
     eval        0
     sentence    =f
     ==>
     !bind!   =y (set-global-sentence =f)
     =goal>
     ISA         air-typing
     eval        1)

  (p character
     =goal>
     ISA         air-typing
     wordcount   =num
     charcount   =n
     eval        1
     - end       =num
     ==>
     !bind!   =l (char-at-index =n)
     !bind!   =z (+ =n 1)
     =goal>
     ISA         air-typing
     eval        2
     charcount   =z
     char        =l
     +retrieval>
     ISA         letter
     letter      =l)

  (p nonspace
     =goal>
     ISA         air-typing
     wordcount   =num
     charcount   =n
     - char      " "
     eval        2
     ==>
     =goal>
     ISA         air-typing
     eval        3)

  (p space
     =goal>
     ISA         air-typing
     wordcount   =num
     charcount   =n
     char        " "
     eval        2
     ==>
     !bind!   =z (+ =num 1)
     !output! (************************************************** =z words **********************************)
     =goal>
     ISA         air-typing
     eval        3
     wordcount   =z
     charcount   =n)

  ;; ============================================================
  ;; Linear/Abacus representation and manual-action loop
  ;; ============================================================

  (p Letter-To-Number
     =goal>
     ISA         air-typing
     wordcount   =num
     charcount   =n
     eval        3
     - end       =num
     =retrieval>
     ISA         letter
     letter      =num0
     num         =f
     ==>
     !output! ("----- Linear/Abacus letter-to-number representation -----")
     !output! (=num0 letter)
     !output! (=f number)

     !bind!   =y (get-tens =f)
     !bind!   =x (get-units =f)

     !output! (=y nondominant-number)
     !output! (=x dominant-number)

     =goal>
     ISA                  air-typing
     eval                 4
     number               =f
     nondominant-number   =y
     dominant-number      =x

     +retrieval>
     ISA         non-dominant-number
     number2     =y)

  (p Retrieve-NonDominant-Hand
     =goal>
     ISA                  air-typing
     wordcount            =num
     charcount            =n
     eval                 4
     nondominant-number   =num0
     dominant-number      =x
     - end                =num

     =retrieval>
     ISA                  non-dominant-number
     number2              =num0
     non-dominant-thumb   =num6
     non-dominant-index   =num7
     non-dominant-middle  =num8
     non-dominant-ring    =num9
     non-dominant-little  =num10

     ?manual>
     state free
     ==>
     !output! ("----- Linear/Abacus non-dominant hand code -----")
     !output! (=num0 nondominant-number)
     !output! (=num6 non-dominant-thumb)
     !output! (=num7 non-dominant-index)
     !output! (=num8 non-dominant-middle)
     !output! (=num9 non-dominant-ring)
     !output! (=num10 non-dominant-little)

     =goal>
     ISA                  air-typing
     eval                 9
     nondominant-number   =num0
     dominant-number      =x

     ;; Retrieve dominant-hand code while the non-dominant hand action is executed.
     +retrieval>
     ISA         dominant-number
     number1     =x

     ;; Abstract non-dominant-hand abacus gesture.
     ;; If dominant hand is right, non-dominant hand is modeled as left.
     +manual>
     cmd     peck
     hand    left
     finger  index
     r       1
     theta   0)


  (p Retrieve-Dominant-Hand
     =goal>
     ISA                 air-typing
     wordcount           =num
     charcount           =n
     eval                9
     dominant-number     =num0
     - end               =num

     =retrieval>
     ISA                 dominant-number
     number1             =num0
     dominant-thumb      =num5
     dominant-index      =num4
     dominant-middle     =num3
     dominant-ring       =num2
     dominant-little     =num1

     ;; Wait until the non-dominant-hand manual action is complete.
     ?manual>
     state free
     ==>
     !output! ("----- Linear/Abacus dominant hand code -----")
     !output! (=num0 dominant-number)
     !output! (=num5 dominant-thumb)
     !output! (=num4 dominant-index)
     !output! (=num3 dominant-middle)
     !output! (=num2 dominant-ring)
     !output! (=num1 dominant-little)

     =goal>
     ISA                  air-typing
     eval                 10
     dominant-number      =num0

     ;; Abstract dominant-hand abacus gesture.
     +manual>
     cmd     peck
     hand    right
     finger  index
     r       1
     theta   0)


  (p Advance-To-Next-Character
     =goal>
     ISA         air-typing
     wordcount   =num
     charcount   =n
     eval        10
     - end       =num

     ;; Wait until the dominant-hand manual action is complete.
     ?manual>
     state free
     ==>
     !bind! =l (char-at-index =n)
     !bind! =z (+ =n 1)

     =goal>
     ISA         air-typing
     eval        16
     char        =l
     charcount   =z)

  (p Retrieval
     =goal>
     ISA         air-typing
     eval        16
     wordcount   =num
     charcount   =n
     - char      "0"
     char        =l
     ==>
     =goal>
     ISA         air-typing
     eval        2
     +retrieval>
     ISA         letter
     letter      =l)

  (p Generate-sentence-again
     =goal>
     ISA         air-typing
     eval        16
     wordcount   =num
     charcount   =n
     begin       =n1
     char        "0"
     ==>
     !bind!   =z (+ =num 1)
     !bind!   =f (random-sentences)
     =goal>
     ISA         air-typing
     eval        0
     wordcount   =z
     charcount   =n1
     sentence    =f)

  )

(run 500000)
