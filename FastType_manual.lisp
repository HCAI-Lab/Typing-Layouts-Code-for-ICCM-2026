

(clear-all)

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

(defun fasttype-transition-cost
    (old-thumb old-index old-middle old-ring old-little
     new-thumb new-index new-middle new-ring new-little)
  "Returns an abstract motor cost for transitioning between two FastType postures.
   Cost is based on the number of finger states that change.
   Minimum cost is 1, maximum cost is 6."
  (+ 1
     (if (= old-thumb new-thumb) 0 1)
     (if (= old-index new-index) 0 1)
     (if (= old-middle new-middle) 0 1)
     (if (= old-ring new-ring) 0 1)
     (if (= old-little new-little) 0 1)))
(define-model FastType


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

  ;; ----------------------------------------------------------------------
  ;; FastType representation
  ;; Each letter directly maps to a five-finger posture:
  ;; thumb index middle ring little.
  ;;
  ;; The model keeps track of current hand posture.
  ;; For each new character, it retrieves the target posture and executes
  ;; ONE abstract posture-transition action if any finger state changes.
  ;;
  ;; This avoids incorrectly serializing all five finger movements.
  ;; The selected finger represents the dominant/slowest changed finger.
  ;; peck is used as an abstract manual timing primitive for mid-air posture
  ;; execution, not as a literal keyboard keypress.
  ;; ----------------------------------------------------------------------

  (chunk-type letter letter thumb index middle ring little)

  (chunk-type air-typing
              start end wordcount charcount temp eval begin sentence char
              thumb-state index-state middle-state ring-state little-state
              target-thumb target-index target-middle target-ring target-little)

  (add-dm
   ;; -------------------------------------------------------------------
   ;; FASTTYPE chord/posture mapping
   ;; thumb index middle ring little
   ;; -------------------------------------------------------------------
   (n0  ISA letter letter " " thumb 0 index 1 middle 0 ring 0 little 0)
   (n1  ISA letter letter "a" thumb 1 index 0 middle 0 ring 0 little 0)
   (n2  ISA letter letter "b" thumb 0 index 0 middle 1 ring 0 little 1)
   (n3  ISA letter letter "c" thumb 1 index 1 middle 0 ring 1 little 0)
   (n4  ISA letter letter "d" thumb 0 index 1 middle 1 ring 1 little 0)
   (n5  ISA letter letter "e" thumb 0 index 0 middle 0 ring 1 little 0)
   (n6  ISA letter letter "f" thumb 1 index 1 middle 1 ring 1 little 0)
   (n7  ISA letter letter "g" thumb 0 index 1 middle 0 ring 0 little 1)
   (n8  ISA letter letter "h" thumb 0 index 0 middle 1 ring 1 little 0)
   (n9  ISA letter letter "i" thumb 1 index 0 middle 1 ring 0 little 0)
   (n10 ISA letter letter "j" thumb 0 index 1 middle 1 ring 0 little 1)
   (n11 ISA letter letter "k" thumb 1 index 1 middle 0 ring 0 little 1)
   (n12 ISA letter letter "l" thumb 1 index 1 middle 1 ring 0 little 0)
   (n13 ISA letter letter "m" thumb 1 index 0 middle 1 ring 1 little 0)
   (n14 ISA letter letter "n" thumb 1 index 1 middle 0 ring 0 little 0)
   (n15 ISA letter letter "o" thumb 1 index 0 middle 0 ring 1 little 0)
   (n16 ISA letter letter "p" thumb 0 index 0 middle 0 ring 1 little 1)
   (n17 ISA letter letter "q" thumb 0 index 1 middle 1 ring 1 little 1)
   (n18 ISA letter letter "r" thumb 0 index 1 middle 0 ring 1 little 0)
   (n19 ISA letter letter "s" thumb 0 index 1 middle 1 ring 0 little 0)
   (n20 ISA letter letter "t" thumb 0 index 0 middle 1 ring 0 little 0)
   (n21 ISA letter letter "u" thumb 0 index 0 middle 0 ring 0 little 1)
   (n22 ISA letter letter "v" thumb 1 index 0 middle 0 ring 1 little 1)
   (n23 ISA letter letter "w" thumb 1 index 0 middle 0 ring 0 little 1)
   (n24 ISA letter letter "x" thumb 0 index 0 middle 1 ring 1 little 1)
   (n25 ISA letter letter "y" thumb 0 index 1 middle 0 ring 1 little 1)
   (n26 ISA letter letter "z" thumb 1 index 0 middle 1 ring 0 little 1)

   ;; Start from neutral hand posture.
   ;; For formal runs, use end 1000 or higher.
   (first-goal ISA air-typing
               start 0
               end 10000
               begin 0
               thumb-state 0
               index-state 0
               middle-state 0
               ring-state 0
               little-state 0))

  (goal-focus first-goal)

  (p start
     =goal>
     ISA air-typing
     start =num
     begin =n
     wordcount nil
     charcount nil
     eval nil
     ==>
     !output! (Start Now!)
     !bind! =f (random-sentences)
     !output! (=f)
     =goal>
     ISA air-typing
     wordcount =num
     charcount =n
     sentence =f
     eval 0)

  (p globalvariable
     =goal>
     ISA air-typing
     eval 0
     sentence =f
     ==>
     !bind! =y (set-global-sentence =f)
     !output! (=y)
     =goal>
     ISA air-typing
     eval 1)

  (p character
     =goal>
     ISA air-typing
     wordcount =num
     charcount =n
     eval 1
     - end =num
     ==>
     !bind! =l (char-at-index =n)
     !bind! =z (+ =n 1)
     =goal>
     ISA air-typing
     eval 2
     charcount =z
     char =l
     +retrieval>
     ISA letter
     letter =l)

  (p nonspace
     =goal>
     ISA air-typing
     wordcount =num
     charcount =n
     - char " "
     eval 2
     ==>
     =goal>
     ISA air-typing
     eval 3)

  (p space
     =goal>
     ISA air-typing
     wordcount =num
     charcount =n
     char " "
     eval 2
     ==>
     !bind! =z (+ =num 1)
     !output! (************************************************** =z words **********************************)
     =goal>
     ISA air-typing
     eval 3
     wordcount =z
     charcount =n)

  ;; ----------------------------------------------------------------------
  ;; Retrieve target FastType posture.
  ;; ----------------------------------------------------------------------

  (p Representation2
     =goal>
     ISA air-typing
     wordcount =num
     charcount =n
     eval 3
     - end =num

     =retrieval>
     ISA letter
     letter =num0
     thumb =num5
     index =num7
     middle =num8
     ring =num9
     little =num10
     ==>
     !output! ("----- FastType target finger-code representation -----")
     !output! (=num0 letter)
     !output! (=num5 thumb)
     !output! (=num7 index)
     !output! (=num8 middle)
     !output! (=num9 ring)
     !output! (=num10 little)

     =goal>
     ISA air-typing
     eval 5
     char =num0
     target-thumb =num5
     target-index =num7
     target-middle =num8
     target-ring =num9
     target-little =num10)

  ;; ----------------------------------------------------------------------
  ;; If no finger changes, no manual action is needed.
  ;; ----------------------------------------------------------------------

  (p FT-No-Finger-Change
     =goal>
     ISA air-typing
     eval 5
     thumb-state =a
     target-thumb =a
     index-state =b
     target-index =b
     middle-state =c
     target-middle =c
     ring-state =d
     target-ring =d
     little-state =e
     target-little =e
     ==>
     !output! ("----- FastType no posture change -----")
     =goal>
     ISA air-typing
     eval 65)

  ;; ----------------------------------------------------------------------
  ;; If multiple fingers change, execute ONE posture-transition action.
  ;; The selected finger follows a slowest/dominant-finger priority.
  ;; Priority here: little > thumb > ring > middle > index.
  ;; This approximates parallel multi-finger articulation by the dominant
  ;; changed finger instead of serially summing all finger movements.
  ;; ----------------------------------------------------------------------

  (p FT-Move-Little
     =goal>
     ISA air-typing
     eval 5
     target-little =new-little
     - little-state =new-little

     ?manual>
     state free
     ==>
     !output! ("----- FastType posture transition: dominant little -----")
     =goal>
     ISA air-typing
     eval 6

     +manual>
     cmd peck
     hand right
     finger pinkie
     r 1
     theta 0)

  (p FT-Move-Thumb
     =goal>
     ISA air-typing
     eval 5

     little-state =same-little
     target-little =same-little

     target-thumb =new-thumb
     - thumb-state =new-thumb

     ?manual>
     state free
     ==>
     !output! ("----- FastType posture transition: dominant thumb -----")
     =goal>
     ISA air-typing
     eval 6

     +manual>
     cmd peck
     hand right
     finger thumb
     r 1
     theta 0)

  (p FT-Move-Ring
     =goal>
     ISA air-typing
     eval 5

     little-state =same-little
     target-little =same-little
     thumb-state =same-thumb
     target-thumb =same-thumb

     target-ring =new-ring
     - ring-state =new-ring

     ?manual>
     state free
     ==>
     !output! ("----- FastType posture transition: dominant ring -----")
     =goal>
     ISA air-typing
     eval 6

     +manual>
     cmd peck
     hand right
     finger ring
     r 1
     theta 0)

  (p FT-Move-Middle
     =goal>
     ISA air-typing
     eval 5

     little-state =same-little
     target-little =same-little
     thumb-state =same-thumb
     target-thumb =same-thumb
     ring-state =same-ring
     target-ring =same-ring

     target-middle =new-middle
     - middle-state =new-middle

     ?manual>
     state free
     ==>
     !output! ("----- FastType posture transition: dominant middle -----")
     =goal>
     ISA air-typing
     eval 6

     +manual>
     cmd peck
     hand right
     finger middle
     r 1
     theta 0)

  (p FT-Move-Index
     =goal>
     ISA air-typing
     eval 5

     little-state =same-little
     target-little =same-little
     thumb-state =same-thumb
     target-thumb =same-thumb
     ring-state =same-ring
     target-ring =same-ring
     middle-state =same-middle
     target-middle =same-middle

     target-index =new-index
     - index-state =new-index

     ?manual>
     state free
     ==>
     !output! ("----- FastType posture transition: dominant index -----")
     =goal>
     ISA air-typing
     eval 6

     +manual>
     cmd peck
     hand right
     finger index
     r 1
     theta 0)

  ;; ----------------------------------------------------------------------
  ;; Meaningful recognition/commit step.
  ;; The FASTTYPE prototype used dwell/peak detection to recognize posture.
  ;; This step represents detecting/committing the mid-air posture after
  ;; the posture-transition action. If this is still too slow, remove this
  ;; production and advance directly from eval 6.
  ;; ----------------------------------------------------------------------

  (p FT-Posture-Detected
     =goal>
     ISA air-typing
     eval 6
     ?manual>
     state free
     ==>
     !output! ("----- FastType posture detected -----")
     =goal>
     ISA air-typing
     eval 65)

  ;; ----------------------------------------------------------------------
  ;; Advance to next character and commit current posture = target posture.
  ;; ----------------------------------------------------------------------

  (p Advance-After-Manual
     =goal>
     ISA air-typing
     wordcount =num
     charcount =n
     eval 65
     - end =num
     target-thumb =num5
     target-index =num7
     target-middle =num8
     target-ring =num9
     target-little =num10

     ?manual>
     state free
     ==>
     !bind! =l (char-at-index =n)
     !bind! =z (+ =n 1)

     =goal>
     ISA air-typing
     eval 7
     char =l
     charcount =z
     thumb-state =num5
     index-state =num7
     middle-state =num8
     ring-state =num9
     little-state =num10)

  (p Retrieval
     =goal>
     ISA air-typing
     eval 7
     wordcount =num
     charcount =n
     - char "0"
     char =l
     ==>
     =goal>
     ISA air-typing
     eval 2

     +retrieval>
     ISA letter
     letter =l)

  (p Generate-sentence-again
     =goal>
     ISA air-typing
     eval 7
     wordcount =num
     charcount =n
     begin =n1
     char "0"
     ==>
     !bind! =z (+ =num 1)
     !output! (************************************************** =z words **********************************)
     !bind! =f (random-sentences)
     !output! (=f)

     =goal>
     ISA air-typing
     eval 0
     wordcount =z
     charcount =n1
     sentence =f)

)

(run 500000)
