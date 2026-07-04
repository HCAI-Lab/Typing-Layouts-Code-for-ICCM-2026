

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
  "Return one fixed sentence for Vision + FastType."
  "buy fresh milk")


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
		:DAT                         0.015             ;default: 0.05       : Default Action Time
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

(defun random-sentences ()
  "Return one fixed sentence for Vision + FastType."
  "buy fresh milk")


;; ----------------------------------------------------------------------
;; FastType + Vision
;; Fixed sentence:
;;   buy fresh milk
;;
;; Internal character stream:
;;   b u y [space] f r e s h [space] m i l k
;;
;; char_index 3  -> internal char " ", visual target "space1"
;; char_index 9  -> internal char " ", visual target "space2"
;; ----------------------------------------------------------------------

(chunk-type ft-letter letter thumb index middle ring little)

(chunk-type air-typing
            start end wordcount charcount temp eval begin sentence char
            char-index vis-target
            thumb-state index-state middle-state ring-state little-state
            target-thumb target-index target-middle target-ring target-little)


;; ----------------------------------------------------------------------
;; Visual targets for VisiTor
;; ----------------------------------------------------------------------

(add-visicon-features '(screen-x 24  screen-y 42 value "b"      char_index 0))
(add-visicon-features '(screen-x 54  screen-y 42 value "u"      char_index 1))
(add-visicon-features '(screen-x 84  screen-y 42 value "y"      char_index 2))
(add-visicon-features '(screen-x 114 screen-y 42 value "space1" char_index 3))
(add-visicon-features '(screen-x 144 screen-y 42 value "f"      char_index 4))
(add-visicon-features '(screen-x 174 screen-y 42 value "r"      char_index 5))
(add-visicon-features '(screen-x 204 screen-y 42 value "e"      char_index 6))
(add-visicon-features '(screen-x 234 screen-y 42 value "s"      char_index 7))
(add-visicon-features '(screen-x 264 screen-y 42 value "h"      char_index 8))
(add-visicon-features '(screen-x 294 screen-y 42 value "space2" char_index 9))
(add-visicon-features '(screen-x 324 screen-y 42 value "m"      char_index 10))
(add-visicon-features '(screen-x 354 screen-y 42 value "i"      char_index 11))
(add-visicon-features '(screen-x 384 screen-y 42 value "l"      char_index 12))
(add-visicon-features '(screen-x 414 screen-y 42 value "k"      char_index 13))


(add-dm
 ;; -------------------------------------------------------------------
 ;; FASTTYPE chord/posture mapping
 ;; thumb index middle ring little
 ;; -------------------------------------------------------------------
 (n0  ISA ft-letter letter " " thumb 0 index 1 middle 0 ring 0 little 0)
 (n1  ISA ft-letter letter "a" thumb 1 index 0 middle 0 ring 0 little 0)
 (n2  ISA ft-letter letter "b" thumb 0 index 0 middle 1 ring 0 little 1)
 (n3  ISA ft-letter letter "c" thumb 1 index 1 middle 0 ring 1 little 0)
 (n4  ISA ft-letter letter "d" thumb 0 index 1 middle 1 ring 1 little 0)
 (n5  ISA ft-letter letter "e" thumb 0 index 0 middle 0 ring 1 little 0)
 (n6  ISA ft-letter letter "f" thumb 1 index 1 middle 1 ring 1 little 0)
 (n7  ISA ft-letter letter "g" thumb 0 index 1 middle 0 ring 0 little 1)
 (n8  ISA ft-letter letter "h" thumb 0 index 0 middle 1 ring 1 little 0)
 (n9  ISA ft-letter letter "i" thumb 1 index 0 middle 1 ring 0 little 0)
 (n10 ISA ft-letter letter "j" thumb 0 index 1 middle 1 ring 0 little 1)
 (n11 ISA ft-letter letter "k" thumb 1 index 1 middle 0 ring 0 little 1)
 (n12 ISA ft-letter letter "l" thumb 1 index 1 middle 1 ring 0 little 0)
 (n13 ISA ft-letter letter "m" thumb 1 index 0 middle 1 ring 1 little 0)
 (n14 ISA ft-letter letter "n" thumb 1 index 1 middle 0 ring 0 little 0)
 (n15 ISA ft-letter letter "o" thumb 1 index 0 middle 0 ring 1 little 0)
 (n16 ISA ft-letter letter "p" thumb 0 index 0 middle 0 ring 1 little 1)
 (n17 ISA ft-letter letter "q" thumb 0 index 1 middle 1 ring 1 little 1)
 (n18 ISA ft-letter letter "r" thumb 0 index 1 middle 0 ring 1 little 0)
 (n19 ISA ft-letter letter "s" thumb 0 index 1 middle 1 ring 0 little 0)
 (n20 ISA ft-letter letter "t" thumb 0 index 0 middle 1 ring 0 little 0)
 (n21 ISA ft-letter letter "u" thumb 0 index 0 middle 0 ring 0 little 1)
 (n22 ISA ft-letter letter "v" thumb 1 index 0 middle 0 ring 1 little 1)
 (n23 ISA ft-letter letter "w" thumb 1 index 0 middle 0 ring 0 little 1)
 (n24 ISA ft-letter letter "x" thumb 0 index 0 middle 1 ring 1 little 1)
 (n25 ISA ft-letter letter "y" thumb 0 index 1 middle 0 ring 1 little 1)
 (n26 ISA ft-letter letter "z" thumb 1 index 0 middle 1 ring 0 little 1)

 ;; For demo use end 100 or 500.
 ;; For formal comparison, change end to 10000.
 (first-goal ISA air-typing
             start 0
             end 300
             begin 0
             thumb-state 0
             index-state 0
             middle-state 0
             ring-state 0
             little-state 0))

(goal-focus first-goal)


;; ============================================================
;; Sentence setup
;; ============================================================

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


;; ============================================================
;; Character reading + visual attention
;; ============================================================

(p character
   =goal>
   ISA air-typing
   wordcount =num
   charcount =n
   eval 1
   - end =num
   ==>
   !bind! =l (char-at-index =n)
   !output! (=l Letter)
   !output! (=n index)

   =goal>
   ISA air-typing
   eval 1.2
   charcount =n
   char-index =n
   char =l)


(p character-finished
   =goal>
   ISA air-typing
   eval 1.2
   char "0"
   ==>
   =goal>
   ISA air-typing
   eval 7
   char "0")


(p request-visual-location-current-target
   =goal>
   ISA air-typing
   eval 1.2
   char =l
   - char "0"
   char-index =n

   ?visual-location>
   state free
   ==>
   =goal>
   ISA air-typing
   eval 1.5
   charcount =n
   char-index =n
   char =l

   +visual-location>
   ISA visual-location
   char_index =n)


(p move-attention-to-current-target
   =goal>
   ISA air-typing
   eval 1.5
   char =l
   char-index =idx
   charcount =n

   =visual-location>
   value =x
   char_index =idx

   ?visual>
   state free
   ==>
   ;; VisiTor where-is response.
   ;; For spaces, =x is "space1" or "space2".
   ;; For letters, =x is the letter itself.
   !eval! (respond-to-where-is '(=x))

   !bind! =z (+ =n 1)

   =goal>
   ISA air-typing
   eval 2
   char =l
   charcount =z
   char-index =idx
   vis-target =x

   +visual>
   ISA move-attention
   screen-pos =visual-location

   +retrieval>
   ISA ft-letter
   letter =l)


;; ============================================================
;; Word count
;; ============================================================

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
   ISA ft-letter
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
;; If fingers change, execute ONE posture-transition action.
;; Priority: little > thumb > ring > middle > index.
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
;; Posture detection / commit step.
;; This represents recognition of the mid-air posture after manual action.
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
;; IMPORTANT: go back to eval 1 so the next character again uses vision.
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
   =goal>
   ISA air-typing
   eval 1
   thumb-state =num5
   index-state =num7
   middle-state =num8
   ring-state =num9
   little-state =num10)


;; ----------------------------------------------------------------------
;; End of fixed sentence: count final word and restart same sentence.
;; ----------------------------------------------------------------------

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
   !output! (Start Again)
   !output! (***********************************************************)

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
