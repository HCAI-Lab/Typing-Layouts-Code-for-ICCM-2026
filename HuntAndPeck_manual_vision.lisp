

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
  "Return one fixed sentence for Vision + Hunt and peck."
  "buy fresh milk")


(define-model HuntAndPeck



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

  (chunk-type hp-letter
              letter
              direction)

  (chunk-type hp-keyloc
              key
              region
              row
              column
              distance)

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
              char-index
              vis-target
              prechar
              dist
              region
              row
              column)


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
   ;; Letter-level representation: letter -> search region
   ;; -------------------------------------------------------------------

   (n0  ISA hp-letter letter " " direction "down")
   (n1  ISA hp-letter letter "q" direction "left-upper")
   (n2  ISA hp-letter letter "w" direction "left-upper")
   (n3  ISA hp-letter letter "e" direction "left-upper")
   (n4  ISA hp-letter letter "r" direction "left-upper")
   (n5  ISA hp-letter letter "t" direction "left-upper")
   (n6  ISA hp-letter letter "y" direction "right-upper")
   (n7  ISA hp-letter letter "u" direction "right-upper")
   (n8  ISA hp-letter letter "i" direction "right-upper")
   (n9  ISA hp-letter letter "o" direction "right-upper")
   (n10 ISA hp-letter letter "p" direction "right-upper")

   (n11 ISA hp-letter letter "a" direction "left")
   (n12 ISA hp-letter letter "s" direction "left")
   (n13 ISA hp-letter letter "d" direction "left")
   (n14 ISA hp-letter letter "f" direction "left")
   (n15 ISA hp-letter letter "g" direction "left")
   (n16 ISA hp-letter letter "h" direction "right")
   (n17 ISA hp-letter letter "j" direction "right")
   (n18 ISA hp-letter letter "k" direction "right")
   (n19 ISA hp-letter letter "l" direction "right")

   (n20 ISA hp-letter letter "z" direction "left-bottom")
   (n21 ISA hp-letter letter "x" direction "left-bottom")
   (n22 ISA hp-letter letter "c" direction "left-bottom")
   (n23 ISA hp-letter letter "v" direction "left-bottom")
   (n24 ISA hp-letter letter "b" direction "left-bottom")
   (n25 ISA hp-letter letter "n" direction "right-bottom")
   (n26 ISA hp-letter letter "m" direction "right-bottom")


   ;; -------------------------------------------------------------------
   ;; Exact key-location representation
   ;; distance is used as one-finger movement amplitude for peck-recoil.
   ;; -------------------------------------------------------------------

   (kl0  ISA hp-keyloc key " " region "down"         row 2  column 0 distance 3)

   (kl1  ISA hp-keyloc key "q" region "left-upper"   row 1  column 5 distance 6)
   (kl2  ISA hp-keyloc key "w" region "left-upper"   row 1  column 4 distance 5)
   (kl3  ISA hp-keyloc key "e" region "left-upper"   row 1  column 3 distance 4)
   (kl4  ISA hp-keyloc key "r" region "left-upper"   row 1  column 2 distance 3)
   (kl5  ISA hp-keyloc key "t" region "left-upper"   row 1  column 1 distance 2)

   (kl6  ISA hp-keyloc key "y" region "right-upper"  row 1  column 1 distance 2)
   (kl7  ISA hp-keyloc key "u" region "right-upper"  row 1  column 2 distance 3)
   (kl8  ISA hp-keyloc key "i" region "right-upper"  row 1  column 3 distance 4)
   (kl9  ISA hp-keyloc key "o" region "right-upper"  row 1  column 4 distance 5)
   (kl10 ISA hp-keyloc key "p" region "right-upper"  row 1  column 5 distance 6)

   (kl11 ISA hp-keyloc key "a" region "left"         row 0  column 5 distance 6)
   (kl12 ISA hp-keyloc key "s" region "left"         row 0  column 4 distance 5)
   (kl13 ISA hp-keyloc key "d" region "left"         row 0  column 3 distance 4)
   (kl14 ISA hp-keyloc key "f" region "left"         row 0  column 2 distance 3)
   (kl15 ISA hp-keyloc key "g" region "left"         row 0  column 1 distance 2)

   (kl16 ISA hp-keyloc key "h" region "right"        row 0  column 1 distance 2)
   (kl17 ISA hp-keyloc key "j" region "right"        row 0  column 2 distance 3)
   (kl18 ISA hp-keyloc key "k" region "right"        row 0  column 3 distance 4)
   (kl19 ISA hp-keyloc key "l" region "right"        row 0  column 4 distance 5)

   (kl20 ISA hp-keyloc key "z" region "left-bottom"  row -1 column 5 distance 6)
   (kl21 ISA hp-keyloc key "x" region "left-bottom"  row -1 column 4 distance 5)
   (kl22 ISA hp-keyloc key "c" region "left-bottom"  row -1 column 3 distance 4)
   (kl23 ISA hp-keyloc key "v" region "left-bottom"  row -1 column 2 distance 3)
   (kl24 ISA hp-keyloc key "b" region "left-bottom"  row -1 column 1 distance 2)

   (kl25 ISA hp-keyloc key "n" region "right-bottom" row -1 column 1 distance 2)
   (kl26 ISA hp-keyloc key "m" region "right-bottom" row -1 column 2 distance 3)

   (first-goal ISA air-typing
               start 0
               end 300
               begin 0))


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
     ISA hp-letter
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


  ;; ============================================================
  ;; HuntAndPeck representation + one-finger manual action
  ;; ============================================================

  (p Representation2
     =goal>
     ISA air-typing
     wordcount =num
     charcount =n
     eval 3
     - end =num

     =retrieval>
     ISA hp-letter
     letter =num0
     direction =d
     ==>
     !output! ("----- HuntAndPeck letter representation -----")
     !output! (=num0 letter)
     !output! (=d direction)

     =goal>
     ISA air-typing
     eval 4
     prechar =num0
     region =d

     +retrieval>
     ISA hp-keyloc
     key =num0
     region =d)


  (p Representation3
     =goal>
     ISA air-typing
     wordcount =num
     charcount =n
     eval 4
     prechar =num0
     - end =num

     =retrieval>
     ISA hp-keyloc
     key =num0
     region =region
     row =num2
     column =num3
     distance =num1

     ?manual>
     state free
     ==>
     !output! ("----- HuntAndPeck exact key-location retrieval -----")
     !output! (=num0 key)
     !output! (=region region)
     !output! (=num1 distance)
     !output! (=num2 row)
     !output! (=num3 column)

     =goal>
     ISA air-typing
     eval 5
     dist =num1
     region =region
     row =num2
     column =num3

     +manual>
     cmd peck-recoil
     hand right
     finger index
     r =num1
     theta 0)


  (p Advance-After-Manual
     =goal>
     ISA air-typing
     eval 5
     wordcount =num
     charcount =n
     - end =num

     ?manual>
     state free
     ==>
     ;; Return to character-reading stage.
     ;; The next character will again go through vision.
     =goal>
     ISA air-typing
     eval 1)


  ;; ============================================================
  ;; End of fixed sentence: count final word and restart same sentence.
  ;; ============================================================

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
