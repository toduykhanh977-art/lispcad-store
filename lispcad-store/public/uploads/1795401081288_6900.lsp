;; ==========================================================================
;;  BO CONG CU THIET KE KIEN TRUC - KET CAU TOAN DIEN (ALL-IN-ONE)
;;  1. VE LUOI TRUC:          VLT   hoac  LUOITRUC
;;  2. CHEN COT / TRU:        VCOT  hoac  TRU
;;  3. KEO TRUC KIEU REVIT:   KT    hoac  KTRUC   hoac  KEO
;;  4. SUA NET DUT 15-20 DOT: FXT   hoac  FIXTRUC
;;  5. VE TUONG TU DONG CAT:  VT    hoac  TUONG
;;  6. CAT THONG DIEM GIAO:   CT    hoac  CATTUONG
;; ==========================================================================

(vl-load-com)

;; ==========================================================================
;;  CAC BIEN TOAN CUC LUU CAU HINH
;; ==========================================================================
;; Luoi truc
(if (not *vlt_x_dist*)    (setq *vlt_x_dist*    "4000, 2*3500, 5000"))
(if (not *vlt_y_dist*)    (setq *vlt_y_dist*    "3600, 4200, 3600"))
(if (not *vlt_x_tag*)     (setq *vlt_x_tag*     "1"))
(if (not *vlt_y_tag*)     (setq *vlt_y_tag*     "A"))
(if (not *vlt_x_len*)     (setq *vlt_x_len*     "0"))
(if (not *vlt_y_len*)     (setq *vlt_y_len*     "0"))
(if (not *vlt_overhang*)  (setq *vlt_overhang*  "1000"))
(if (not *vlt_radius*)    (setq *vlt_radius*    "400"))
(if (not *vlt_txth*)      (setq *vlt_txth*      "250"))
(if (not *vlt_bubble_pos*)(setq *vlt_bubble_pos* "1"))
(if (not *vlt_num_dots*)  (setq *vlt_num_dots*  "18"))
(if (not *vlt_gen_col*)   (setq *vlt_gen_col*   "0"))
(if (not *vlt_col_b*)     (setq *vlt_col_b*     "220"))
(if (not *vlt_col_h*)     (setq *vlt_col_h*     "220"))
(if (not *vlt_col_hatch*) (setq *vlt_col_hatch* "1"))

;; Cot / Tru
(if (not *vcot_type*)     (setq *vcot_type*     "0"))
(if (not *vcot_b*)        (setq *vcot_b*        "220"))
(if (not *vcot_h*)        (setq *vcot_h*        "300"))
(if (not *vcot_ang*)      (setq *vcot_ang*      "0"))
(if (not *vcot_hatch*)    (setq *vcot_hatch*    "1"))
(if (not *vcot_mode*)     (setq *vcot_mode*     "0"))

;; Tuong
(if (not *vlt_wall_w*)    (setq *vlt_wall_w*    "220")) ; Be day tuong (110, 200, 220, 300)
(if (not *vlt_wall_just*) (setq *vlt_wall_just* "0"))   ; 0: Can giua tim, 1: Mep trai, 2: Mep phai

;; ==========================================================================
;;  CAC HAM TIEN ICH CHUNG
;; ==========================================================================

(defun vlt:split (str del / pos lst)
  (setq lst '())
  (while (setq pos (vl-string-search del str))
    (setq lst (cons (substr str 1 pos) lst))
    (setq str (substr str (+ pos 1 (strlen del))))
  )
  (setq lst (cons str lst))
  (reverse lst)
)

(defun vlt:parse-segment (seg / s pos n d out)
  (setq s (vl-string-trim " \t" (strcase seg)))
  (cond
    ((or (setq pos (vl-string-search "*" s))
         (setq pos (vl-string-search "X" s)))
     (setq n (atoi (substr s 1 pos)))
     (setq d (distof (substr s (+ pos 2))))
     (if (and (> n 0) d (> d 0))
       (repeat n (setq out (cons d out)))
     )
    )
    ((setq d (distof s))
     (if (> d 0)
       (setq out (list d))
     )
    )
  )
  out
)

(defun vlt:parse-list (str / items res)
  (setq res '())
  (setq items (vlt:split str ","))
  (foreach item items
    (setq res (append res (vlt:parse-segment item)))
  )
  res
)

(defun vlt:get-tag (base idx / num)
  (cond
    ((numberp (distof base))
     (itoa (+ (atoi base) idx))
    )
    ((and (= (strlen base) 1)
          (>= (ascii (strcase base)) 65)
          (<= (ascii (strcase base)) 90))
     (setq num (+ (ascii (strcase base)) idx))
     (if (<= num 90)
       (chr num)
       (strcat "A" (chr (+ 65 (- num 91))))
     )
    )
    (t
     (if (= idx 0) base (strcat base (itoa (+ 1 idx))))
    )
  )
)

(defun vlt:load-ltype (ltName / doc lts isMetric linFile)
  (if (and ltName (/= (strcase ltName) "CONTINUOUS"))
    (if (not (tblsearch "LTYPE" ltName))
      (progn
        (setq isMetric (= (getvar "MEASUREMENT") 1))
        (setq linFile (if isMetric "acadiso.lin" "acad.lin"))
        (setq doc (vla-get-activedocument (vlax-get-acad-object)))
        (setq lts (vla-get-linetypes doc))
        (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-load (list lts ltName linFile)))
          (vl-catch-all-apply 'vla-load (list lts ltName (if isMetric "acad.lin" "acadiso.lin")))
        )
      )
    )
  )
)

(defun vlt:calc-ltscale (len numDots / basePatternLen sysLtSc msLtSc annoVal annoFactor mAutoCAD targetCycle effScale)
  (if (or (null numDots) (<= numDots 0.0)) (setq numDots 18.0))
  (setq targetCycle (/ len (float numDots)))
  (if (< targetCycle 50.0) (setq targetCycle 50.0))

  (vlt:load-ltype "CENTER")
  (if (tblsearch "LTYPE" "CENTER")
    (setq basePatternLen (cdr (assoc 40 (tblsearch "LTYPE" "CENTER"))))
  )
  (if (or (null basePatternLen) (<= basePatternLen 0.0))
    (setq basePatternLen (if (= (getvar "MEASUREMENT") 1) 50.8 2.0))
  )

  (setq sysLtSc (getvar "LTSCALE"))
  (if (or (null sysLtSc) (<= sysLtSc 0.0)) (setq sysLtSc 1.0))

  (setq msLtSc (getvar "MSLTSCALE"))
  (setq annoFactor 1.0)
  (if (and msLtSc (= msLtSc 1))
    (progn
      (setq annoVal (getvar "CANNOSCALEVALUE"))
      (if (and annoVal (> annoVal 0.0))
        (setq annoFactor (/ 1.0 annoVal))
      )
    )
  )

  (setq mAutoCAD (* sysLtSc annoFactor basePatternLen))
  (if (<= mAutoCAD 0.0) (setq mAutoCAD 50.8))

  (setq effScale (/ targetCycle mAutoCAD))
  (max 0.0001 effScale)
)

(defun vlt:ensure-truc-layer (/ layEnt el)
  (vlt:load-ltype "CENTER")
  (if (not (tblsearch "LAYER" "TRUC"))
    (entmake
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        '(2 . "TRUC")
        '(70 . 0)
        '(62 . 1)
        (cons 6 (if (tblsearch "LTYPE" "CENTER") "CENTER" "Continuous"))
      )
    )
    (if (and (tblsearch "LTYPE" "CENTER") (setq layEnt (tblobjname "LAYER" "TRUC")))
      (vl-catch-all-apply
        '(lambda ()
           (setq el (entget layEnt))
           (setq el (subst '(6 . "CENTER") (assoc 6 el) el))
           (entmod el)
         )
      )
    )
  )
)

(defun vlt:make-layer (layName layColor layLtype)
  (if (not (tblsearch "LAYER" layName))
    (progn
      (vlt:load-ltype layLtype)
      (entmake
        (list
          '(0 . "LAYER")
          '(100 . "AcDbSymbolTableRecord")
          '(100 . "AcDbLayerTableRecord")
          (cons 2 layName)
          '(70 . 0)
          (cons 62 layColor)
          (cons 6 (if (tblsearch "LTYPE" layLtype) layLtype "Continuous"))
        )
      )
    )
  )
)

(defun vlt:draw-axis-line (p1 p2 lay ltScale)
  (vlt:load-ltype "CENTER")
  (entmake
    (list
      '(0 . "LINE")
      (cons 8 lay)
      (cons 6 "CENTER")
      (cons 10 p1)
      (cons 11 p2)
      (cons 48 ltScale)
    )
  )
  (entlast)
)

(defun vlt:draw-circle (cen rad lay)
  (entmake
    (list
      '(0 . "CIRCLE")
      (cons 8 lay)
      (cons 10 cen)
      (cons 40 rad)
    )
  )
  (entlast)
)

(defun vlt:draw-text-mc (cen txt h lay)
  (entmake
    (list
      '(0 . "TEXT")
      (cons 8 lay)
      (cons 10 cen)
      (cons 11 cen)
      (cons 40 h)
      (cons 1 txt)
      (cons 72 1)
      (cons 73 2)
    )
  )
  (entlast)
)

(defun vlt:rotate-pt (pt cen radAng / d a)
  (if (= radAng 0.0)
    pt
    (progn
      (setq d (distance (list (car cen) (cadr cen)) (list (car pt) (cadr pt))))
      (setq a (+ (angle (list (car cen) (cadr cen)) (list (car pt) (cadr pt))) radAng))
      (list (+ (car cen) (* d (cos a)))
            (+ (cadr cen) (* d (sin a)))
            (caddr pt))
    )
  )
)

(defun vlt:draw-rect-col (cen b h ang lay / dx dy radAng p1 p2 p3 p4)
  (setq dx (/ b 2.0)
        dy (/ h 2.0)
        radAng (* ang (/ pi 180.0)))
  (setq p1 (vlt:rotate-pt (list (- (car cen) dx) (- (cadr cen) dy) 0.0) cen radAng))
  (setq p2 (vlt:rotate-pt (list (+ (car cen) dx) (- (cadr cen) dy) 0.0) cen radAng))
  (setq p3 (vlt:rotate-pt (list (+ (car cen) dx) (+ (cadr cen) dy) 0.0) cen radAng))
  (setq p4 (vlt:rotate-pt (list (- (car cen) dx) (+ (cadr cen) dy) 0.0) cen radAng))
  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      (cons 8 lay)
      '(100 . "AcDbPolyline")
      '(90 . 4)
      '(70 . 1)
      (cons 10 (list (car p1) (cadr p1)))
      (cons 10 (list (car p2) (cadr p2)))
      (cons 10 (list (car p3) (cadr p3)))
      (cons 10 (list (car p4) (cadr p4)))
    )
  )
  (entlast)
)

(defun vlt:add-solid-hatch (ent lay / doc modelSpace hatchObj safearray)
  (vl-catch-all-apply
    '(lambda ()
       (setq doc (vla-get-activedocument (vlax-get-acad-object)))
       (setq modelSpace (vla-get-modelspace doc))
       (setq hatchObj (vla-AddHatch modelSpace 0 "SOLID" :vlax-true))
       (vla-put-layer hatchObj lay)
       (setq safearray (vlax-make-safearray vlax-vbObject '(0 . 0)))
       (vlax-safearray-fill safearray (list (vlax-ename->vla-object ent)))
       (vla-AppendOuterLoop hatchObj safearray)
       (vla-Evaluate hatchObj)
     )
  )
)

(defun vlt:draw-column (cen colType b h ang doHatch lay / entCol)
  (if (or (null h) (<= h 0)) (setq h b))
  (cond
    ((= colType 0)
     (setq entCol (vlt:draw-rect-col cen b h ang lay))
    )
    ((= colType 1)
     (setq entCol (vlt:draw-circle cen (/ b 2.0) lay))
    )
  )
  (if (and doHatch entCol)
    (vlt:add-solid-hatch entCol lay)
  )
  entCol
)

(defun vlt:remove-duplicates (pts tol / out exists)
  (setq out '())
  (foreach p pts
    (setq exists nil)
    (foreach q out
      (if (< (distance (list (car p) (cadr p)) (list (car q) (cadr q))) tol)
        (setq exists t)
      )
    )
    (if (not exists)
      (setq out (cons p out))
    )
  )
  (reverse out)
)

(defun vlt:get-intersections (ss / objList i ent o1 o2 var allPts pt)
  (setq objList '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq objList (cons (vlax-ename->vla-object ent) objList))
    (setq i (+ i 1))
  )
  (setq allPts '())
  (while (setq o1 (car objList))
    (setq objList (cdr objList))
    (foreach o2 objList
      (setq var (vl-catch-all-apply 'vlax-invoke (list o1 'IntersectWith o2 0)))
      (if (and (not (vl-catch-all-error-p var)) var)
        (while (>= (length var) 3)
          (setq pt (list (car var) (cadr var) (caddr var)))
          (setq allPts (cons pt allPts))
          (setq var (cdddr var))
        )
      )
    )
  )
  (vlt:remove-duplicates allPts 50.0)
)

(defun vlt:set-line-pt (lineEnt isStart newPt newScale / el code)
  (setq el (entget lineEnt))
  (setq code (if isStart 10 11))
  (setq el (subst (cons code newPt) (assoc code el) el))
  (if (assoc 6 el)
    (setq el (subst '(6 . "CENTER") (assoc 6 el) el))
    (setq el (append el '((6 . "CENTER"))))
  )
  (if newScale
    (if (assoc 48 el)
      (setq el (subst (cons 48 newScale) (assoc 48 el) el))
      (setq el (append el (list (cons 48 newScale))))
    )
  )
  (entmod el)
  (entupd lineEnt)
)

(defun vlt:set-circle-cen (circEnt newCen / el)
  (setq el (entget circEnt))
  (setq el (subst (cons 10 newCen) (assoc 10 el) el))
  (entmod el)
  (entupd circEnt)
)

(defun vlt:set-text-pt (txtEnt newPt / el)
  (setq el (entget txtEnt))
  (setq el (subst (cons 10 newPt) (assoc 10 el) el))
  (if (assoc 11 el)
    (setq el (subst (cons 11 newPt) (assoc 11 el) el))
  )
  (entmod el)
  (entupd txtEnt)
)

(defun vlt:find-bubble-near (pt maxDist / ss i ent d cen circ txt minD_c minD_t)
  (setq ss (ssget "_C"
                  (list (- (car pt) maxDist) (- (cadr pt) maxDist))
                  (list (+ (car pt) maxDist) (+ (cadr pt) maxDist))
                  '((0 . "CIRCLE,TEXT"))
           )
  )
  (setq minD_c 1e9 minD_t 1e9 circ nil txt nil)
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (cond
          ((= (cdr (assoc 0 (entget ent))) "CIRCLE")
           (setq cen (cdr (assoc 10 (entget ent))))
           (setq d (distance (list (car pt) (cadr pt)) (list (car cen) (cadr cen))))
           (if (< d minD_c)
             (setq minD_c d circ ent)
           )
          )
          ((= (cdr (assoc 0 (entget ent))) "TEXT")
           (setq cen (cdr (assoc 10 (entget ent))))
           (setq d (distance (list (car pt) (cadr pt)) (list (car cen) (cadr cen))))
           (if (< d minD_t)
             (setq minD_t d txt ent)
           )
          )
        )
        (setq i (+ i 1))
      )
    )
  )
  (list circ txt)
)

;; ==========================================================================
;;  LENH 1: VE LUOI TRUC (VLT / LUOITRUC)
;; ==========================================================================

(defun vlt:create-grid-dcl (/ path f)
  (setq path (vl-filename-mktemp "vlt_grid.dcl"))
  (setq f (open path "w"))
  (if f
    (progn
      (write-line "vlt_dialog : dialog {" f)
      (write-line "  label = \"VE LUOI TRUC TU DONG - AUTO GRID\";" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"Truc doc (Phuong X: 1, 2, 3...)\";" f)
      (write-line "    : edit_box { key = \"eb_x_dist\"; label = \"Khoang cach X (vd: 4000,2*3500,5000):\"; edit_width = 32; }" f)
      (write-line "    : row {" f)
      (write-line "      : edit_box { key = \"eb_x_tag\"; label = \"Ky hieu dau:\"; edit_width = 6; }" f)
      (write-line "      : edit_box { key = \"eb_y_len\"; label = \"Chieu dai truc (0 = tu tinh):\"; edit_width = 12; }" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"Truc ngang (Phuong Y: A, B, C...)\";" f)
      (write-line "    : edit_box { key = \"eb_y_dist\"; label = \"Khoang cach Y (vd: 3600,4200,3600):\"; edit_width = 32; }" f)
      (write-line "    : row {" f)
      (write-line "      : edit_box { key = \"eb_y_tag\"; label = \"Ky hieu dau:\"; edit_width = 6; }" f)
      (write-line "      : edit_box { key = \"eb_x_len\"; label = \"Chieu dai truc (0 = tu tinh):\"; edit_width = 12; }" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"Kich thuoc hien thi & Mat do net dut\";" f)
      (write-line "    : row {" f)
      (write-line "      : edit_box { key = \"eb_overhang\"; label = \"Doan nho:\"; edit_width = 8; }" f)
      (write-line "      : edit_box { key = \"eb_radius\"; label = \"Ban kinh bong:\"; edit_width = 8; }" f)
      (write-line "      : edit_box { key = \"eb_txth\"; label = \"Co chu:\"; edit_width = 8; }" f)
      (write-line "    }" f)
      (write-line "    : row {" f)
      (write-line "      : edit_box { key = \"eb_num_dots\"; label = \"So dot cham gach tren truc (15-20):\"; edit_width = 8; }" f)
      (write-line "      : popup_list { key = \"pl_bubble\"; label = \"Bong bong:\"; edit_width = 18; }" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"Them tru/cot tai cac diem giao (Tuy chon)\";" f)
      (write-line "    : row {" f)
      (write-line "      : toggle { key = \"chk_gen_col\"; label = \"Tu dong ve cot tai cac giao diem\"; }" f)
      (write-line "      : toggle { key = \"chk_col_hatch\"; label = \"To Hatch Solid\"; }" f)
      (write-line "    }" f)
      (write-line "    : row {" f)
      (write-line "      : edit_box { key = \"eb_col_b\"; label = \"Rong B (mm):\"; edit_width = 10; }" f)
      (write-line "      : edit_box { key = \"eb_col_h\"; label = \"Dai H (mm):\"; edit_width = 10; }" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  : spacer { height = 1; }" f)
      (write-line "  : row {" f)
      (write-line "    : spacer { width = 1; }" f)
      (write-line "    : button { key = \"accept\"; label = \"Ve luoi truc\"; is_default = true; width = 14; }" f)
      (write-line "    : button { key = \"cancel\"; label = \"Huy bo\"; is_cancel = true; width = 12; }" f)
      (write-line "    : spacer { width = 1; }" f)
      (write-line "  }" f)
      (write-line "}" f)
      (close f)
    )
  )
  path
)

(defun c:VLT ( / dcl_file dcl_id status x_list y_list pt0
                 total_w total_h len_x len_y overhang rad txth bpos
                 num_dots sc_vert sc_horiz len_vert len_horiz
                 i cur_x y_start y_end cen_b cen_t
                 j cur_y x_start x_end cen_l cen_r
                 col_b col_h do_gen_col do_col_hatch
                 x_coords y_coords xi yj
                 doc lay_truc lay_txt lay_cot )
  
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq dcl_file (vlt:create-grid-dcl))
  (if (or (null dcl_file) (not (findfile dcl_file)))
    (progn (alert "Khong the tao file giao dien DCL tam thoi!") (exit))
  )

  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "vlt_dialog" dcl_id))
    (progn
      (unload_dialog dcl_id)
      (vl-file-delete dcl_file)
      (alert "Loi khi nap hop thoai DCL!")
      (exit)
    )
  )

  (set_tile "eb_x_dist"    *vlt_x_dist*)
  (set_tile "eb_y_dist"    *vlt_y_dist*)
  (set_tile "eb_x_tag"     *vlt_x_tag*)
  (set_tile "eb_y_tag"     *vlt_y_tag*)
  (set_tile "eb_x_len"     *vlt_x_len*)
  (set_tile "eb_y_len"     *vlt_y_len*)
  (set_tile "eb_overhang"  *vlt_overhang*)
  (set_tile "eb_radius"    *vlt_radius*)
  (set_tile "eb_txth"      *vlt_txth*)
  (set_tile "eb_num_dots"  *vlt_num_dots*)
  (set_tile "chk_gen_col"  *vlt_gen_col*)
  (set_tile "chk_col_hatch"*vlt_col_hatch*)
  (set_tile "eb_col_b"     *vlt_col_b*)
  (set_tile "eb_col_h"     *vlt_col_h*)

  (start_list "pl_bubble")
  (mapcar 'add_list '("Ca 2 dau (Tren/Duoi & Trai/Phai)"
                      "Duoi & Trai (Tieu chuan)"
                      "Tren & Phai"
                      "Khong ve bong bong (Chi ve truc)"))
  (end_list)
  (set_tile "pl_bubble" *vlt_bubble_pos*)

  (action_tile "accept"
    (strcat
      "(setq *vlt_x_dist*    (get_tile \"eb_x_dist\"))"
      "(setq *vlt_y_dist*    (get_tile \"eb_y_dist\"))"
      "(setq *vlt_x_tag*     (get_tile \"eb_x_tag\"))"
      "(setq *vlt_y_tag*     (get_tile \"eb_y_tag\"))"
      "(setq *vlt_x_len*     (get_tile \"eb_x_len\"))"
      "(setq *vlt_y_len*     (get_tile \"eb_y_len\"))"
      "(setq *vlt_overhang*  (get_tile \"eb_overhang\"))"
      "(setq *vlt_radius*    (get_tile \"eb_radius\"))"
      "(setq *vlt_txth*      (get_tile \"eb_txth\"))"
      "(setq *vlt_num_dots*  (get_tile \"eb_num_dots\"))"
      "(setq *vlt_bubble_pos*(get_tile \"pl_bubble\"))"
      "(setq *vlt_gen_col*   (get_tile \"chk_gen_col\"))"
      "(setq *vlt_col_hatch* (get_tile \"chk_col_hatch\"))"
      "(setq *vlt_col_b*     (get_tile \"eb_col_b\"))"
      "(setq *vlt_col_h*     (get_tile \"eb_col_h\"))"
      "(done_dialog 1)"
    )
  )
  (action_tile "cancel" "(done_dialog 0)")

  (setq status (start_dialog))
  (unload_dialog dcl_id)
  (vl-file-delete dcl_file)

  (if (/= status 1)
    (progn (princ "\nDa huy lenh.") (princ))
    (progn
      (setq x_list (vlt:parse-list *vlt_x_dist*))
      (setq y_list (vlt:parse-list *vlt_y_dist*))

      (if (and (null x_list) (null y_list))
        (progn
          (alert "Ban chua nhap khoang cach cho truc nao!")
          (exit)
        )
      )

      (setq overhang (distof *vlt_overhang*))
      (if (or (null overhang) (< overhang 0)) (setq overhang 1000.0))

      (setq rad (distof *vlt_radius*))
      (if (or (null rad) (<= rad 0)) (setq rad 400.0))

      (setq txth (distof *vlt_txth*))
      (if (or (null txth) (<= txth 0)) (setq txth 250.0))

      (setq num_dots (distof *vlt_num_dots*))
      (if (or (null num_dots) (<= num_dots 0.0)) (setq num_dots 18.0))

      (setq bpos (atoi *vlt_bubble_pos*))
      (setq do_gen_col (= *vlt_gen_col* "1"))
      (setq do_col_hatch (= *vlt_col_hatch* "1"))

      (setq col_b (distof *vlt_col_b*))
      (if (or (null col_b) (<= col_b 0)) (setq col_b 220.0))

      (setq col_h (distof *vlt_col_h*))
      (if (or (null col_h) (<= col_h 0)) (setq col_h col_b))

      (setq total_w (if x_list (apply '+ x_list) 0.0))
      (setq total_h (if y_list (apply '+ y_list) 0.0))

      (setq len_x (distof *vlt_x_len*))
      (if (or (null len_x) (<= len_x 0)) (setq len_x total_w))

      (setq len_y (distof *vlt_y_len*))
      (if (or (null len_y) (<= len_y 0)) (setq len_y total_h))

      (if (and (null y_list) (<= len_y 0)) (setq len_y 10000.0))
      (if (and (null x_list) (<= len_x 0)) (setq len_x 10000.0))

      (setq pt0 (getpoint "\nChon diem dat goc luoi truc (Giao diem truc dau tien): "))
      (if pt0
        (progn
          (vla-startundomark doc)
          
          (setq lay_truc "TRUC")
          (setq lay_txt  "TRUC_TEXT")
          (setq lay_cot  "COT")
          (vlt:ensure-truc-layer)
          (vlt:make-layer lay_txt  2 "Continuous")
          (vlt:make-layer lay_cot  4 "Continuous")

          (setq x_coords (list (car pt0)))
          (setq y_coords (list (cadr pt0)))

          ;; --- VE TRUC DOC ---
          (if (or x_list (> len_y 0))
            (progn
              (setq y_start (- (cadr pt0) overhang))
              (setq y_end   (+ (cadr pt0) len_y overhang))
              (setq len_vert (abs (- y_end y_start)))
              (setq sc_vert (vlt:calc-ltscale len_vert num_dots))
              
              (setq cur_x (car pt0))
              (setq i 0)

              (defun draw-vert-grid (posX idx)
                (vlt:draw-axis-line (list posX y_start 0.0) (list posX y_end 0.0) lay_truc sc_vert)
                (if (or (= bpos 0) (= bpos 1))
                  (progn
                    (setq cen_b (list posX (- y_start rad) 0.0))
                    (vlt:draw-circle cen_b rad lay_txt)
                    (vlt:draw-text-mc cen_b (vlt:get-tag *vlt_x_tag* idx) txth lay_txt)
                  )
                )
                (if (or (= bpos 0) (= bpos 2))
                  (progn
                    (setq cen_t (list posX (+ y_end rad) 0.0))
                    (vlt:draw-circle cen_t rad lay_txt)
                    (vlt:draw-text-mc cen_t (vlt:get-tag *vlt_x_tag* idx) txth lay_txt)
                  )
                )
              )

              (draw-vert-grid cur_x i)

              (if x_list
                (foreach dx x_list
                  (setq i (+ i 1))
                  (setq cur_x (+ cur_x dx))
                  (setq x_coords (cons cur_x x_coords))
                  (draw-vert-grid cur_x i)
                )
              )
            )
          )

          ;; --- VE TRUC NGANG ---
          (if (or y_list (> len_x 0))
            (progn
              (setq x_start (- (car pt0) overhang))
              (setq x_end   (+ (car pt0) len_x overhang))
              (setq len_horiz (abs (- x_end x_start)))
              (setq sc_horiz (vlt:calc-ltscale len_horiz num_dots))
              
              (setq cur_y (cadr pt0))
              (setq j 0)

              (defun draw-horiz-grid (posY idx)
                (vlt:draw-axis-line (list x_start posY 0.0) (list x_end posY 0.0) lay_truc sc_horiz)
                (if (or (= bpos 0) (= bpos 1))
                  (progn
                    (setq cen_l (list (- x_start rad) posY 0.0))
                    (vlt:draw-circle cen_l rad lay_txt)
                    (vlt:draw-text-mc cen_l (vlt:get-tag *vlt_y_tag* idx) txth lay_txt)
                  )
                )
                (if (or (= bpos 0) (= bpos 2))
                  (progn
                    (setq cen_r (list (+ x_end rad) posY 0.0))
                    (vlt:draw-circle cen_r rad lay_txt)
                    (vlt:draw-text-mc cen_r (vlt:get-tag *vlt_y_tag* idx) txth lay_txt)
                  )
                )
              )

              (draw-horiz-grid cur_y j)

              (if y_list
                (foreach dy y_list
                  (setq j (+ j 1))
                  (setq cur_y (+ cur_y dy))
                  (setq y_coords (cons cur_y y_coords))
                  (draw-horiz-grid cur_y j)
                )
              )
            )
          )

          ;; --- VE COT TAI GIAO DIEM ---
          (if (and do_gen_col x_list y_list)
            (progn
              (foreach xi x_coords
                (foreach yj y_coords
                  (vlt:draw-column (list xi yj 0.0) 0 col_b col_h 0.0 do_col_hatch lay_cot)
                )
              )
            )
          )

          (vla-regen doc 0)
          (vla-endundomark doc)
          (princ (strcat "\n>>> Da ve luoi truc voi " (rtos num_dots 2 0) " dot cham gach ro dep! <<<"))
        )
      )
    )
  )
  (princ)
)

(defun c:LUOITRUC () (c:VLT))

;; ==========================================================================
;;  LENH 2: CHEN COT / TRU (VCOT / TRU)
;; ==========================================================================

(defun vcot:create-dcl-file (/ path f)
  (setq path (vl-filename-mktemp "vcot_dialog.dcl"))
  (setq f (open path "w"))
  (if f
    (progn
      (write-line "vcot_dialog : dialog {" f)
      (write-line "  label = \"THIET LAP THONG SO COT / TRU\";" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"Hinh dang & Kich thuoc cot (mm)\";" f)
      (write-line "    : popup_list { key = \"pop_type\"; label = \"Hinh dang cot:\"; edit_width = 24; }" f)
      (write-line "    : row {" f)
      (write-line "      : edit_box { key = \"eb_b\"; label = \"Rong B (D cot tron):\"; edit_width = 10; }" f)
      (write-line "      : edit_box { key = \"eb_h\"; label = \"Dai H (chu nhat):\"; edit_width = 10; }" f)
      (write-line "    }" f)
      (write-line "    : row {" f)
      (write-line "      : edit_box { key = \"eb_ang\"; label = \"Goc xoay (do):\"; edit_width = 10; }" f)
      (write-line "      : toggle { key = \"chk_hatch\"; label = \"To Hatch Solid\"; }" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"Phuong thuc chen cot\";" f)
      (write-line "    : popup_list { key = \"pop_mode\"; label = \"Cach thuc hien:\"; edit_width = 24; }" f)
      (write-line "  }" f)
      (write-line "  : spacer { height = 1; }" f)
      (write-line "  : row {" f)
      (write-line "    : spacer { width = 1; }" f)
      (write-line "    : button { key = \"accept\"; label = \"Thuc hien\"; is_default = true; width = 14; }" f)
      (write-line "    : button { key = \"cancel\"; label = \"Huy bo\"; is_cancel = true; width = 12; }" f)
      (write-line "    : spacer { width = 1; }" f)
      (write-line "  }" f)
      (write-line "}" f)
      (close f)
    )
  )
  path
)

(defun c:VCOT ( / dcl_file dcl_id status doc lay_cot
                  col_type b h ang do_hatch mode
                  ss pts pt count )
  
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq dcl_file (vcot:create-dcl-file))
  (if (or (null dcl_file) (not (findfile dcl_file)))
    (progn (alert "Khong the tao file giao dien DCL tam thoi!") (exit))
  )

  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "vcot_dialog" dcl_id))
    (progn
      (unload_dialog dcl_id)
      (vl-file-delete dcl_file)
      (alert "Loi khi nap hop thoai DCL!")
      (exit)
    )
  )

  (start_list "pop_type")
  (mapcar 'add_list '("Cot chu nhat / Vuong" "Cot tron"))
  (end_list)
  (set_tile "pop_type" *vcot_type*)

  (start_list "pop_mode")
  (mapcar 'add_list '("Quet chon cac truc giao nhau (Nhanh)"
                      "Click chon tung giao diem"))
  (end_list)
  (set_tile "pop_mode" *vcot_mode*)

  (set_tile "eb_b"      *vcot_b*)
  (set_tile "eb_h"      *vcot_h*)
  (set_tile "eb_ang"    *vcot_ang*)
  (set_tile "chk_hatch" *vcot_hatch*)

  (action_tile "accept"
    (strcat
      "(setq *vcot_type*  (get_tile \"pop_type\"))"
      "(setq *vcot_b*     (get_tile \"eb_b\"))"
      "(setq *vcot_h*     (get_tile \"eb_h\"))"
      "(setq *vcot_ang*   (get_tile \"eb_ang\"))"
      "(setq *vcot_hatch* (get_tile \"chk_hatch\"))"
      "(setq *vcot_mode*  (get_tile \"pop_mode\"))"
      "(done_dialog 1)"
    )
  )
  (action_tile "cancel" "(done_dialog 0)")

  (setq status (start_dialog))
  (unload_dialog dcl_id)
  (vl-file-delete dcl_file)

  (if (/= status 1)
    (progn (princ "\nDa huy lenh.") (princ))
    (progn
      (setq col_type (atoi *vcot_type*))
      (setq b (distof *vcot_b*))
      (if (or (null b) (<= b 0)) (setq b 220.0))

      (setq h (distof *vcot_h*))
      (if (or (null h) (<= h 0)) (setq h b))

      (setq ang (distof *vcot_ang*))
      (if (null ang) (setq ang 0.0))

      (setq do_hatch (= *vcot_hatch* "1"))
      (setq mode (atoi *vcot_mode*))

      (setq lay_cot "COT")
      (vlt:make-layer lay_cot 4 "Continuous")

      (cond
        ((= mode 0)
         (princ "\nQuet chon cac duong truc tren ban ve de dat cot tai cac giao diem: ")
         ;; Loai bo cac layer khong phai truc: TUONG, WALL, COT, DIM, TEXT, HATCH de khong bi ve cot tai mep tuong
         (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,XLINE")
                           (8 . "~*TUONG*,~*WALL*,~*COT*,~*DIM*,~*TEXT*,~*HATCH*"))))
         (if ss
           (progn
             ;; Neu trong tap chon co cac duong thuoc layer TRUC hoac mau do (mau 1), chi lay duong truc
             (setq ssTruc (ssadd))
             (setq i 0)
             (while (< i (sslength ss))
               (setq ent (ssname ss i)
                     el (entget ent)
                     lay (strcase (cdr (assoc 8 el))))
               (if (or (wcmatch lay "*TRUC*") (= (cdr (assoc 62 el)) 1))
                 (ssadd ent ssTruc)
               )
               (setq i (+ i 1))
             )
             (if (> (sslength ssTruc) 1)
               (setq ss ssTruc)
             )

             (if (> (sslength ss) 1)
               (progn
                 (vla-startundomark doc)
                 (setq pts (vlt:get-intersections ss))
                 (if pts
                   (progn
                     (setq count 0)
                     (foreach pt pts
                       (vlt:draw-column pt col_type b h ang do_hatch lay_cot)
                       (setq count (+ count 1))
                     )
                     (princ (strcat "\n>>> Da tao thanh cong " (itoa count) " cot tai cac giao diem truc! <<<"))
                   )
                   (princ "\nKhong tim thay giao diem nao giua cac duong truc da chon!")
                 )
                 (vla-endundomark doc)
               )
               (princ "\nCan quet chon it nhat 2 duong truc cat nhau!")
             )
           )
           (princ "\nChua chon duong truc nao!")
         )
        )

        ((= mode 1)
         (vla-startundomark doc)
         (setq count 0)
         (while (setq pt (getpoint "\nClick chon giao diem dat cot (nhan Enter hoac ESC de dung): "))
           (vlt:draw-column pt col_type b h ang do_hatch lay_cot)
           (setq count (+ count 1))
         )
         (if (> count 0)
           (princ (strcat "\n>>> Da chen thanh cong " (itoa count) " cot! <<<"))
         )
         (vla-endundomark doc)
        )
      )
    )
  )
  (princ)
)

(defun c:TRU () (c:VCOT))
(defun c:COTRU () (c:VCOT))

;; ==========================================================================
;;  LENH 3: KEO TRUC KIEU REVIT (KT / KTRUC / KEO)
;; ==========================================================================

(defun c:KT ( / sel ent pickPt el objType
                lineEnt p1 p2 isVert isHoriz
                pTop pBot pLeft pRight isP1Top isP1Right
                side basePt rad circ txt ssAll i testEnt
                t_el t_p1 t_p2 t_isVert t_isHoriz
                t_top t_bot t_left t_right t_isP1Top t_isP1Right
                items newPt targetCoord count newTop newBot newLeft newRight
                bInfo curCirc curTxt cRad newCen doc lay newLineLen newScale )
  
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setvar "CMDECHO" 0)
  (setq sel (entsel "\n[REVIT GRID] Click chon dau truc hoac bong bong muon keo: "))
  (if (null sel)
    (progn (princ "\nChua chon doi tuong.") (princ))
    (progn
      (setq ent (car sel)
            pickPt (cadr sel)
            el (entget ent)
            objType (cdr (assoc 0 el)))

      (cond
        ((= objType "LINE") (setq lineEnt ent))
        ((= objType "CIRCLE")
         (setq cen (cdr (assoc 10 el)) rad (cdr (assoc 40 el)))
         (setq ssAll (ssget "_C" (list (- (car cen) (* rad 3.0)) (- (cadr cen) (* rad 3.0)))
                                 (list (+ (car cen) (* rad 3.0)) (+ (cadr cen) (* rad 3.0)))
                                 '((0 . "LINE"))))
         (if ssAll (setq lineEnt (ssname ssAll 0)))
        )
        ((= objType "TEXT")
         (setq cen (cdr (assoc 10 el)))
         (setq ssAll (ssget "_C" (list (- (car cen) 1500.0) (- (cadr cen) 1500.0))
                                 (list (+ (car cen) 1500.0) (+ (cadr cen) 1500.0))
                                 '((0 . "LINE"))))
         (if ssAll (setq lineEnt (ssname ssAll 0)))
        )
      )

      (if (null lineEnt)
        (princ "\nKhong xac dinh duoc duong truc lien quan!")
        (progn
          (setq el (entget lineEnt)
                lay (cdr (assoc 8 el))
                p1 (cdr (assoc 10 el))
                p2 (cdr (assoc 11 el)))

          (setq isVert (< (abs (- (car p1) (car p2))) 50.0))
          (setq isHoriz (< (abs (- (cadr p1) (cadr p2))) 50.0))

          (cond
            (isVert
             (if (> (cadr p1) (cadr p2))
               (setq pTop p1 pBot p2 isP1Top t)
               (setq pTop p2 pBot p1 isP1Top nil)
             )
             (if (< (distance pickPt pTop) (distance pickPt pBot))
               (setq side "TOP" basePt pTop)
               (setq side "BOTTOM" basePt pBot)
             )
            )
            (isHoriz
             (if (> (car p1) (car p2))
               (setq pRight p1 pLeft p2 isP1Right t)
               (setq pRight p2 pLeft p1 isP1Right nil)
             )
             (if (< (distance pickPt pRight) (distance pickPt pLeft))
               (setq side "RIGHT" basePt pRight)
               (setq side "LEFT" basePt pLeft)
             )
            )
            (t (setq side nil))
          )

          (if (null side)
            (princ "\nDuong duoc chon khong phai truc thang dung hoac nam ngang!")
            (progn
              (setq ssAll (ssget "_X" (list '(0 . "LINE") (cons 8 lay))))
              (setq items '())

              (if ssAll
                (progn
                  (setq i 0)
                  (while (< i (sslength ssAll))
                    (setq testEnt (ssname ssAll i)
                          t_el (entget testEnt)
                          t_p1 (cdr (assoc 10 t_el))
                          t_p2 (cdr (assoc 11 t_el)))

                    (cond
                      (isVert
                       (if (< (abs (- (car t_p1) (car t_p2))) 50.0)
                         (progn
                           (if (> (cadr t_p1) (cadr t_p2))
                             (setq t_top t_p1 t_bot t_p2 t_isP1Top t)
                             (setq t_top t_p2 t_bot t_p1 t_isP1Top nil)
                           )
                           (cond
                             ((and (= side "TOP") (< (abs (- (cadr t_top) (cadr basePt))) 3000.0))
                              (setq bInfo (vlt:find-bubble-near t_top 2500.0))
                              (setq items (cons (list testEnt t_isP1Top t_top t_bot (car bInfo) (cadr bInfo)) items))
                             )
                             ((and (= side "BOTTOM") (< (abs (- (cadr t_bot) (cadr basePt))) 3000.0))
                              (setq bInfo (vlt:find-bubble-near t_bot 2500.0))
                              (setq items (cons (list testEnt t_isP1Top t_top t_bot (car bInfo) (cadr bInfo)) items))
                             )
                           )
                         )
                       )
                      )

                      (isHoriz
                       (if (< (abs (- (cadr t_p1) (cadr t_p2))) 50.0)
                         (progn
                           (if (> (car t_p1) (car t_p2))
                             (setq t_right t_p1 t_left t_p2 t_isP1Right t)
                             (setq t_right t_p2 t_left t_p1 t_isP1Right nil)
                           )
                           (cond
                             ((and (= side "RIGHT") (< (abs (- (car t_right) (car basePt))) 3000.0))
                              (setq bInfo (vlt:find-bubble-near t_right 2500.0))
                              (setq items (cons (list testEnt t_isP1Right t_right t_left (car bInfo) (cadr bInfo)) items))
                             )
                             ((and (= side "LEFT") (< (abs (- (car t_left) (car basePt))) 3000.0))
                              (setq bInfo (vlt:find-bubble-near t_left 2500.0))
                              (setq items (cons (list testEnt t_isP1Right t_right t_left (car bInfo) (cadr bInfo)) items))
                             )
                           )
                         )
                       )
                      )
                    )
                    (setq i (+ i 1))
                  )
                )
              )

              (if (null items)
                (princ "\nKhong tim thay hang truc lien ket!")
                (progn
                  (princ (strcat "\n>>> Da khoa lien ket " (itoa (length items)) " truc cung hang (Kieu Revit) <<<"))
                  (setq newPt (getpoint basePt (strcat "\nRe chuot de keo gian / thu ngan tat ca cac truc hang [" side "]: ")))
                  
                  (if newPt
                    (progn
                      (vla-startundomark doc)
                      (setq count 0)

                      (foreach itm items
                        (setq testEnt (nth 0 itm)
                              curCirc (nth 4 itm)
                              curTxt  (nth 5 itm)
                              cRad    (if curCirc (cdr (assoc 40 (entget curCirc))) (distof *vlt_radius*)))
                        (if (or (null cRad) (<= cRad 0)) (setq cRad 400.0))

                        (cond
                          ((= side "TOP")
                           (setq targetCoord (cadr newPt))
                           (setq newTop (list (car (nth 2 itm)) targetCoord 0.0))
                           (setq newLineLen (distance newTop (nth 3 itm)))
                           (setq newScale (vlt:calc-ltscale newLineLen (distof *vlt_num_dots*)))
                           (vlt:set-line-pt testEnt (nth 1 itm) newTop newScale)
                           (if curCirc (vlt:set-circle-cen curCirc (list (car newTop) (+ targetCoord cRad) 0.0)))
                           (if curTxt  (vlt:set-text-pt curTxt (list (car newTop) (+ targetCoord cRad) 0.0)))
                          )

                          ((= side "BOTTOM")
                           (setq targetCoord (cadr newPt))
                           (setq newBot (list (car (nth 3 itm)) targetCoord 0.0))
                           (setq newLineLen (distance (nth 2 itm) newBot))
                           (setq newScale (vlt:calc-ltscale newLineLen (distof *vlt_num_dots*)))
                           (vlt:set-line-pt testEnt (not (nth 1 itm)) newBot newScale)
                           (if curCirc (vlt:set-circle-cen curCirc (list (car newBot) (- targetCoord cRad) 0.0)))
                           (if curTxt  (vlt:set-text-pt curTxt (list (car newBot) (- targetCoord cRad) 0.0)))
                          )

                          ((= side "RIGHT")
                           (setq targetCoord (car newPt))
                           (setq newRight (list targetCoord (cadr (nth 2 itm)) 0.0))
                           (setq newLineLen (distance (nth 3 itm) newRight))
                           (setq newScale (vlt:calc-ltscale newLineLen (distof *vlt_num_dots*)))
                           (vlt:set-line-pt testEnt (nth 1 itm) newRight newScale)
                           (if curCirc (vlt:set-circle-cen curCirc (list (+ targetCoord cRad) (cadr newRight) 0.0)))
                           (if curTxt  (vlt:set-text-pt curTxt (list (+ targetCoord cRad) (cadr newRight) 0.0)))
                          )

                          ((= side "LEFT")
                           (setq targetCoord (car newPt))
                           (setq newLeft (list targetCoord (cadr (nth 3 itm)) 0.0))
                           (setq newLineLen (distance newLeft (nth 2 itm)))
                           (setq newScale (vlt:calc-ltscale newLineLen (distof *vlt_num_dots*)))
                           (vlt:set-line-pt testEnt (not (nth 1 itm)) newLeft newScale)
                           (if curCirc (vlt:set-circle-cen curCirc (list (- targetCoord cRad) (cadr newLeft) 0.0)))
                           (if curTxt  (vlt:set-text-pt curTxt (list (- targetCoord cRad) (cadr newLeft) 0.0)))
                          )
                        )
                        (setq count (+ count 1))
                      )

                      (vla-regen doc 0)
                      (vla-endundomark doc)
                      (princ (strcat "\n>>> Da keo dong loat " (itoa count) " truc va tu dong can chinh 15-20 dot net dut! <<<"))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

(defun c:KTRUC () (c:KT))
(defun c:KEO () (c:KT))

;; ==========================================================================
;;  LENH 4: SUA NET DUT 15-20 DOT CHO TRUC DANG CO (FXT / FIXTRUC)
;; ==========================================================================
(defun c:FXT ( / doc ss i ent el p1 p2 len sc count numDots )
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vlt:ensure-truc-layer)
  
  (setq numDots (distof *vlt_num_dots*))
  (if (or (null numDots) (<= numDots 0.0)) (setq numDots 18.0))

  (setq ss (ssget "_X" '((0 . "LINE") (8 . "TRUC"))))
  (if (null ss)
    (princ "\nKhong tim thay duong line nao tren layer TRUC!")
    (progn
      (vla-startundomark doc)
      (setq count 0 i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              el (entget ent)
              p1 (cdr (assoc 10 el))
              p2 (cdr (assoc 11 el))
              len (distance p1 p2)
              sc (vlt:calc-ltscale len numDots))
        
        (if (assoc 6 el)
          (setq el (subst '(6 . "CENTER") (assoc 6 el) el))
          (setq el (append el '((6 . "CENTER"))))
        )
        (if (assoc 48 el)
          (setq el (subst (cons 48 sc) (assoc 48 el) el))
          (setq el (append el (list (cons 48 sc))))
        )
        (entmod el)
        (entupd ent)
        (setq count (+ count 1) i (+ i 1))
      )
      (vla-regen doc 0)
      (vla-endundomark doc)
      (princ (strcat "\n>>> Da cap nhat thanh cong " (itoa count) " duong truc ve chuan 15-20 dot cham gach! <<<"))
    )
  )
  (princ)
)

(defun c:FIXTRUC () (c:FXT))

;; ==========================================================================
;;  LENH 5: VE TUONG THONG MINH (VT / TUONG)
;;  Gom 2 lua chon:
;;    1. Quet truc giao diem (Nhanh nhat): Tu dong dung toan bo he tuong,
;;       tu dong bo goc L o cac goc ngoai va duc thong tat ca chu T, nga tu.
;;    2. Ve tay (Click diem): Click tung diem ve tuong tu do,
;;       tu dong bo goc L khi re huong va cat chu T khi cham tuong khac.

;; ==========================================================================
;;  LENH 5: VE TUONG THONG MINH (VT / TUONG)
;;  Gom 2 lua chon:
;;    1. Quet truc giao diem (Nhanh nhat): Tu dong dung toan bo he tuong,
;;       tu dong bo goc L o cac goc ngoai va duc thong tat ca chu T, nga tu.
;;    2. Ve tay (Click diem): Click tung diem ve tuong tu do,
;;       tu dong bo goc L khi re huong va tu dong cat duc thong chu T khi giao nhau.

;; ==========================================================================
;;  LENH 5: VE TUONG THONG MINH (VT / TUONG)
;;  Gom 2 lua chon:
;;    1. Quet truc giao diem (Nhanh nhat): Tu dong dung toan bo he tuong,
;;       tu dong bo goc L o cac goc ngoai va duc thong tat ca chu T, nga tu.
;;    2. Ve tay (2 diem): Click diem 1 -> Diem 2 ve tuong tu do,
;;       chuot doc lap khong dinh day, tu dong duc thong chu T khi cham tuong vuong goc.

;; ==========================================================================
;;  LENH 5: VE TUONG THONG MINH (VT / TUONG)
;;  Gom 2 lua chon:
;;    1. Quet truc giao diem (Nhanh nhat): Tu dong dung toan bo he tuong,
;;       tu dong bo goc L o cac goc ngoai va duc thong tat ca chu T, nga tu.
;;    2. Ve tay (2 diem): Click diem 1 -> Diem 2 ve tuong tu do,
;;       bat diem Nearest/Perp khong nhay toa do, tu dong duc thong chu T khi cham tuong vuong goc.
;; ==========================================================================

;; 1. Ve 1 doan line tuong (Layer TUONG, mau 7 - trang, Continuous)
(defun vlt:draw-wall-line (p1 p2 lay)
  (entmake
    (list
      '(0 . "LINE")
      (cons 8 lay)
      (cons 62 7)
      (cons 6 "Continuous")
      (cons 10 p1)
      (cons 11 p2)
    )
  )
  (entlast)
)

;; 2. Cap nhat toa do dau/cuoi cua 1 doan Line
(defun vlt:update-line-endpoint (lineEnt isStart newPt / el code)
  (if (and lineEnt (setq el (entget lineEnt)))
    (progn
      (setq code (if isStart 10 11))
      (setq el (subst (cons code newPt) (assoc code el) el))
      (entmod el)
      (entupd lineEnt)
    )
  )
)

;; 3. Kiem tra 1 diem co nam tren doan thang q1-q2 khong (sai so tol)
(defun vlt:pt-on-seg (pt q1 q2 tol / d1 d2 d12)
  (if pt
    (progn
      (setq d1 (distance q1 pt)
            d2 (distance q2 pt)
            d12 (distance q1 q2))
      (<= (abs (- (+ d1 d2) d12)) tol)
    )
    nil
  )
)

;; 4. Duc thong 1 doan line tuong cu giua 2 diem i1 va i2 (tao khe ho w)
(defun vlt:break-line-between (lineEnt i1 i2 / el q1 q2 d1 d2 iA iB lay)
  (if (and lineEnt (entget lineEnt))
    (progn
      (setq el (entget lineEnt))
      (setq q1 (cdr (assoc 10 el))
            q2 (cdr (assoc 11 el))
            lay (cdr (assoc 8 el)))
      (setq d1 (distance q1 i1)
            d2 (distance q1 i2))
      (if (> d1 d2)
        (setq iA i2 iB i1)
        (setq iA i1 iB i2)
      )
      (cond
        ((and (> (distance q1 iA) 5.0) (> (distance q2 iB) 5.0))
         (setq el (subst (cons 11 iA) (assoc 11 el) el))
         (entmod el)
         (entupd lineEnt)
         (vlt:draw-wall-line iB q2 lay)
        )
        ((and (<= (distance q1 iA) 5.0) (> (distance q2 iB) 5.0))
         (setq el (subst (cons 10 iB) (assoc 10 el) el))
         (entmod el)
         (entupd lineEnt)
        )
        ((and (> (distance q1 iA) 5.0) (<= (distance q2 iB) 5.0))
         (setq el (subst (cons 11 iA) (assoc 11 el) el))
         (entmod el)
         (entupd lineEnt)
        )
        (t
         (entdel lineEnt)
        )
      )
    )
  )
)

;; 5. Tim va duc thong net tuong cu khi gap giao diem chu T (o dau hoac cuoi doan tuong)
(defun vlt:trim-t-junction (ptFrom ptTo rL1 rL2 wVal angWall ignoreList /
                               ss i candEnt el q1 q2 lineAng
                               iL iR dL dR bestEnt bestIL bestIR minD candLay searchRad half_w ptTo_ucs)
  (setq half_w (/ wVal 2.0))
  (setq searchRad (+ half_w 250.0)) ; Pham vi tim kiem bao quat (khoang 360mm)
  (setq ptTo_ucs (trans ptTo 0 1)) ; Chuyen WCS sang UCS de ssget _C hoat dong chuan xac tren viewport
  (setq ss (ssget "_C"
                  (list (- (car ptTo_ucs) searchRad) (- (cadr ptTo_ucs) searchRad))
                  (list (+ (car ptTo_ucs) searchRad) (+ (cadr ptTo_ucs) searchRad))
                  '((0 . "LINE"))))
  (if ss
    (progn
      (setq minD 1e9 bestEnt nil bestIL nil bestIR nil)
      (setq i 0)
      (while (< i (sslength ss))
        (setq candEnt (ssname ss i))
        (if (not (member candEnt ignoreList))
          (progn
            (setq el (entget candEnt))
            (setq candLay (strcase (cdr (assoc 8 el))))
            ;; Chi xet net tuong, bo qua net truc (do), cot, dim
            (if (and (or (wcmatch candLay "*TUONG*") (wcmatch candLay "*WALL*"))
                     (not (wcmatch candLay "*TRUC*"))
                     (not (wcmatch candLay "*COT*"))
                     (not (wcmatch candLay "*DIM*"))
                     (not (wcmatch candLay "*TEXT*"))
                     (/= (cdr (assoc 62 el)) 1))
              (progn
                (setq q1 (cdr (assoc 10 el))
                      q2 (cdr (assoc 11 el)))
                (setq lineAng (angle q1 q2))
                ;; 1. Kiem tra goc: Tuong bi dam phai lech it nhat 24 do (tranh tuong song song)
                (if (> (abs (sin (- angWall lineAng))) 0.40)
                  (progn
                    (setq iL (inters (car rL1) (cadr rL1) q1 q2 nil)
                          iR (inters (car rL2) (cadr rL2) q1 q2 nil))
                    (if (and iL iR
                             (< (distance ptTo iL) searchRad)
                             (< (distance ptTo iR) searchRad)
                             (or (vlt:pt-on-seg iL q1 q2 35.0)
                                 (vlt:pt-on-seg iR q1 q2 35.0)))
                      (progn
                        (setq dL (distance ptFrom iL)
                              dR (distance ptFrom iR))
                        ;; Chon duong mep ngoai bi dam vao dau tien (gan ptFrom nhat)
                        (if (< (+ dL dR) minD)
                          (setq minD (+ dL dR)
                                bestEnt candEnt
                                bestIL iL
                                bestIR iR)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (setq i (+ i 1))
      )
      (if (and bestEnt bestIL bestIR)
        (progn
          (vlt:break-line-between bestEnt bestIL bestIR)
          (list bestIL bestIR)
        )
        nil
      )
    )
    nil
  )
)

;; 5.1 Cap nhat toa do diem dau hoac diem cuoi cua 1 duong Line
(defun vlt:set-line-endpoint (ent pt isEnd / el key)
  (if (and ent (entget ent))
    (progn
      (setq el (entget ent))
      (setq key (if isEnd 11 10))
      (setq el (subst (cons key pt) (assoc key el) el))
      (entmod el)
      (entupd ent)
    )
  )
)

;; 5.2 Tinh toan giao diem miter cat vat goc L giua 2 doan tuong lien tiep
(defun vlt:miter-corner (pA_L pA_R pB_L1 pB_R1 pB_L2 pB_R2 pC_L pC_R ang1 ang2 / BL BR)
  (setq BL (inters pA_L pB_L1 pB_L2 pC_L nil))
  (setq BR (inters pA_R pB_R1 pB_R2 pC_R nil))

  ;; Fallback neu 2 doan song song hoac inters tra ve nil
  (if (or (null BL) (< (abs (sin (- ang2 ang1))) 0.05))
    (setq BL pB_L1)
  )
  (if (or (null BR) (< (abs (sin (- ang2 ang1))) 0.05))
    (setq BR pB_R1)
  )
  (list BL BR)
)

;; 6. Loc toa do duy nhat va sap xep tang dan
(defun vlt:unique-sorted-coords (vals tol / sorted res prev)
  (if (null vals) nil
    (progn
      (setq sorted (vl-sort vals '<))
      (setq prev (car sorted) res (list prev))
      (foreach v (cdr sorted)
        (if (> (abs (- v prev)) tol)
          (setq res (cons v res) prev v)
        )
      )
      (reverse res)
    )
  )
)

;; 7. Ve mot duong duoc cat thong qua cac giao diem ben trong
(defun vlt:draw-segmented-line (cStart cEnd fixedCoord isHoriz crossCoords h lay /
                                curr innerCrossings xVal xLeft xRight)
  (setq curr cStart)
  (setq innerCrossings '())
  (foreach xVal crossCoords
    (if (and (> xVal (+ cStart h)) (< xVal (- cEnd h)))
      (setq innerCrossings (cons xVal innerCrossings))
    )
  )
  (setq innerCrossings (vl-sort innerCrossings '<))

  (foreach xVal innerCrossings
    (setq xLeft (- xVal h)
          xRight (+ xVal h))
    (if (> (- xLeft curr) 5.0)
      (if isHoriz
        (vlt:draw-wall-line (list curr fixedCoord 0.0) (list xLeft fixedCoord 0.0) lay)
        (vlt:draw-wall-line (list fixedCoord curr 0.0) (list fixedCoord xLeft 0.0) lay)
      )
    )
    (if (> xRight curr)
      (setq curr xRight)
    )
  )

  (if (> (- cEnd curr) 5.0)
    (if isHoriz
      (vlt:draw-wall-line (list curr fixedCoord 0.0) (list cEnd fixedCoord 0.0) lay)
      (vlt:draw-wall-line (list fixedCoord curr 0.0) (list fixedCoord cEnd 0.0) lay)
    )
  )
)

;; 8. CHE DO 1: QUET CHON GIAO DIEM TRUC (NHANH NHAT - CHUAN XAC 100%)
;;    Tu dong bo goc L va duc thong tat ca giao diem chu T, nga tu
(defun vlt:wall-from-grid-perfect (w lay doc / ss i ent el p1 p2
                                              xVals yVals xList yList
                                              xMin xMax yMin yMax h count curY curX )
  (setq h (/ w 2.0))
  (princ "\nQuet chon cac duong truc co giao diem can dung tuong: ")
  
  (setq ss (ssget '((0 . "LINE"))))
  (if ss
    (progn
      (setq xVals '() yVals '() i 0)

      ;; 1. Thu thap toa do truc X (dung) va truc Y (ngang)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              el (entget ent))
        (if (/= (strcase (cdr (assoc 8 el))) (strcase lay))
          (progn
            (setq p1 (cdr (assoc 10 el))
                  p2 (cdr (assoc 11 el)))
            (if (< (abs (- (car p1) (car p2))) (abs (- (cadr p1) (cadr p2))))
              (setq xVals (cons (/ (+ (car p1) (car p2)) 2.0) xVals))
              (setq yVals (cons (/ (+ (cadr p1) (cadr p2)) 2.0) yVals))
            )
          )
        )
        (setq i (+ i 1))
      )

      ;; 2. Sap xep va loc trung
      (setq xList (vlt:unique-sorted-coords xVals 100.0))
      (setq yList (vlt:unique-sorted-coords yVals 100.0))

      (if (or (< (length xList) 2) (< (length yList) 2))
        (princ "\nCan quet chon it nhat 2 truc doc va 2 truc ngang giao nhau!")
        (progn
          (vla-startundomark doc)
          (setq xMin (car xList)
                xMax (last xList)
                yMin (car yList)
                yMax (last yList))

          (setq count 0)

          ;; 3. DUNG CAC NET TUONG NAM NGANG
          (foreach curY yList
            (cond
              ;; Tuong bien duoi cung
              ((< (abs (- curY yMin)) 50.0)
               (vlt:draw-wall-line (list (- xMin h) (- curY h) 0.0)
                                   (list (+ xMax h) (- curY h) 0.0) lay)
               (setq count (+ count 1))
               (vlt:draw-segmented-line (+ xMin h) (- xMax h) (+ curY h) t
                                        xList h lay)
               (setq count (+ count 1))
              )

              ;; Tuong bien tren cung
              ((< (abs (- curY yMax)) 50.0)
               (vlt:draw-wall-line (list (- xMin h) (+ curY h) 0.0)
                                   (list (+ xMax h) (+ curY h) 0.0) lay)
               (setq count (+ count 1))
               (vlt:draw-segmented-line (+ xMin h) (- xMax h) (- curY h) t
                                        xList h lay)
               (setq count (+ count 1))
              )

              ;; Tuong ngan o giua
              (t
               (vlt:draw-segmented-line (+ xMin h) (- xMax h) (+ curY h) t
                                        xList h lay)
               (vlt:draw-segmented-line (+ xMin h) (- xMax h) (- curY h) t
                                        xList h lay)
               (setq count (+ count 2))
              )
            )
          )

          ;; 4. DUNG CAC NET TUONG THANG DUNG
          (foreach curX xList
            (cond
              ;; Tuong bien trai cung
              ((< (abs (- curX xMin)) 50.0)
               (vlt:draw-wall-line (list (- curX h) (- yMin h) 0.0)
                                   (list (- curX h) (+ yMax h) 0.0) lay)
               (setq count (+ count 1))
               (vlt:draw-segmented-line (+ yMin h) (- yMax h) (+ curX h) nil
                                        yList h lay)
               (setq count (+ count 1))
              )

              ;; Tuong bien phai cung
              ((< (abs (- curX xMax)) 50.0)
               (vlt:draw-wall-line (list (+ curX h) (- yMin h) 0.0)
                                   (list (+ curX h) (+ yMax h) 0.0) lay)
               (setq count (+ count 1))
               (vlt:draw-segmented-line (+ yMin h) (- yMax h) (- curX h) nil
                                        yList h lay)
               (setq count (+ count 1))
              )

              ;; Tuong ngan o giua
              (t
               (vlt:draw-segmented-line (+ yMin h) (- yMax h) (- curX h) nil
                                        yList h lay)
               (vlt:draw-segmented-line (+ yMin h) (- yMax h) (+ curX h) nil
                                        yList h lay)
               (setq count (+ count 2))
              )
            )
          )

          (vla-regen doc 0)
          (vla-endundomark doc)
          (princ (strcat "\n>>> HOAN TAT! Da dung thanh cong he tuong day "
                         (rtos w 2 0) "mm! Toan bo goc L va giao diem da bo va duc thong! <<<"))
        )
      )
    )
    (princ "\nKhong chon duong truc nao!")
  )
)

;; 9. CHE DO 2: VE TAY LIEN TUC (NHU ML) - TU DONG NOI GOC L & KHOET LO CHU T
(defun vlt:wall-by-points (w lay doc / half_w old_osm old_err
                                      contTotal contRun isFirstSeg
                                      ptStart ptPrev ptCurr inp
                                      ang angPrev ang_p
                                      pA_L pA_R pB_L pB_R
                                      prevA_L prevA_R prevB_L prevB_R
                                      firstPtStart firstPtSecond firstA_L firstA_R firstB_L firstB_R
                                      firstLine1 firstLine2 angFirst
                                      line1 line2 lastL1 lastL2
                                      tTrim1 tTrim2 miterPts BL BR
                                      drawnLines segHistory count
                                      lastRec prevRec origPts
                                      angClose closeA_L closeA_R closeB_L closeB_R
                                      miterPrev miterStart BL_start BR_start )
  (setq half_w (/ w 2.0) count 0)
  (setq old_osm (getvar "OSMODE"))
  (setq old_err *error*)
  (defun *error* (msg)
    (if old_osm (setvar "OSMODE" old_osm))
    (vla-endundomark doc)
    (setq *error* old_err)
    (princ)
  )

  ;; Snap: Endpoint(1) + Midpoint(2) + Center(4) + Intersection(32) + Perpendicular(128) + Nearest(512) = 679
  (setvar "OSMODE" 679)

  (princ (strcat "\n>> CHE DO 2: VE TUONG TAY LIEN TUC (DAY " (rtos w 2 0) "mm, NHU LENH ML) <<"))
  (princ "\n* Huong dan: Click lien tuc cac diem. Tu dong noi goc L & khoet lo chu T.")
  (princ "\n* Phim tat: [U] Quay lai | [C] Dong kin phong | [Enter/ESC] Ket thuc.")

  (setq contTotal T)
  (while contTotal
    (initget "Thoat,E")
    (setq ptStart (getpoint "\nVE TUONG - Chon diem bat dau (Enter de thoat): "))
    (if (or (null ptStart) (= ptStart "Thoat") (= ptStart "E"))
      (setq contTotal nil)
      (progn
        (vla-startundomark doc)
        (setq ptStart (trans ptStart 1 0))
        (setq ptPrev ptStart)
        (setq isFirstSeg T)
        (setq lastL1 nil lastL2 nil)
        (setq firstLine1 nil firstLine2 nil)
        (setq firstA_L nil firstA_R nil firstB_L nil firstB_R nil)
        (setq firstPtStart ptStart firstPtSecond nil angFirst nil)
        (setq drawnLines '())
        (setq segHistory '())
        (setq contRun T)

        (while contRun
          (initget "Undo,U Close,C Thoat,E")
          (setq inp (getpoint (trans ptPrev 0 1)
                              (if isFirstSeg
                                "\nVE TUONG - Chon diem tiep theo (Enter de xong doan): "
                                "\nVE TUONG - Chon diem tiep theo [Undo(U)/Close(C)] (Enter de xong doan): ")))
          (cond
            ;; 1. Nguoi dung nhan Enter / Thoat: Hoan tat chuoi doan tuong nay
            ((or (null inp) (= inp "Thoat") (= inp "E"))
             (setq contRun nil)
             (vla-regen doc 0)
             (vla-endundomark doc)
            )

            ;; 2. Nguoi dung go U / Undo: Xoa doan tuong vua ve
            ((or (= inp "U") (= inp "Undo"))
             (if segHistory
               (progn
                 (setq lastRec (car segHistory)
                       segHistory (cdr segHistory))
                 ;; Xoa 2 duong vua ve
                 (if (car lastRec) (entdel (car lastRec)))
                 (if (cadr lastRec) (entdel (cadr lastRec)))
                 (setq drawnLines (vl-remove (car lastRec) (vl-remove (cadr lastRec) drawnLines)))
                 ;; Phuc hoi diem ptPrev
                 (setq ptPrev (nth 2 lastRec))
                 (if segHistory
                   (progn
                     (setq prevRec (car segHistory))
                     (setq lastL1 (car prevRec)
                           lastL2 (cadr prevRec))
                     ;; Phuc hoi dau miter cua doan truoc do ve toa do goc
                     (setq origPts (nth 3 prevRec))
                     (vlt:set-line-endpoint lastL1 (nth 2 origPts) T)
                     (vlt:set-line-endpoint lastL2 (nth 3 origPts) T)
                     (setq prevA_L (nth 0 origPts) prevA_R (nth 1 origPts)
                           prevB_L (nth 2 origPts) prevB_R (nth 3 origPts))
                     (setq angPrev (angle (nth 2 prevRec) ptPrev))
                     (setq isFirstSeg nil)
                   )
                   (progn
                     (setq lastL1 nil lastL2 nil isFirstSeg T)
                     (setq firstLine1 nil firstLine2 nil)
                   )
                 )
                 (vla-regen doc 0)
                 (princ "\n-> Da quay lai (Undo) 1 doan tuong!")
               )
               (princ "\n-> Khong co doan tuong nao de Undo!")
             )
            )

            ;; 3. Nguoi dung go C / Close HOAC click trung ptStart: Dong kin phong
            ((or (= inp "C") (= inp "Close")
                 (and (listp inp) (not isFirstSeg) (>= (length segHistory) 2)
                      (< (distance (trans inp 1 0) ptStart) 30.0)))
             (if (>= (length segHistory) 2)
               (progn
                 (setq ptCurr ptStart)
                 (setq angClose (angle ptPrev ptCurr))
                 (setq ang_p (+ angClose (/ pi 2.0)))
                 (setq closeA_L (polar ptPrev ang_p half_w)
                       closeA_R (polar ptPrev ang_p (- half_w))
                       closeB_L (polar ptCurr ang_p half_w)
                       closeB_R (polar ptCurr ang_p (- half_w)))

                 ;; Miter tai goc ptPrev (noi doan cuoi voi doan dong kin)
                 (setq miterPrev (vlt:miter-corner prevA_L prevA_R prevB_L prevB_R closeA_L closeA_R closeB_L closeB_R angPrev angClose))
                 (setq BL (car miterPrev) BR (cadr miterPrev))
                 (vlt:set-line-endpoint lastL1 BL T)
                 (vlt:set-line-endpoint lastL2 BR T)
                 (setq closeA_L BL closeA_R BR)

                 ;; Miter tai goc ptStart (noi doan dong kin voi doan dau tien)
                 (setq miterStart (vlt:miter-corner closeA_L closeA_R closeB_L closeB_R firstA_L firstA_R firstB_L firstB_R angClose angFirst))
                 (setq BL_start (car miterStart) BR_start (cadr miterStart))
                 (vlt:set-line-endpoint firstLine1 BL_start nil)
                 (vlt:set-line-endpoint firstLine2 BR_start nil)
                 (setq closeB_L BL_start closeB_R BR_start)

                 ;; Ve doan khep goc
                 (setq line1 (vlt:draw-wall-line closeA_L closeB_L lay))
                 (setq line2 (vlt:draw-wall-line closeA_R closeB_R lay))

                 (vla-regen doc 0)
                 (vla-endundomark doc)
                 (princ "\n>>> Da tu dong dong kin va khoet thong 4 goc phong! <<<")
                 (setq contRun nil)
               )
               (princ "\n-> Can it nhat 3 doan de khep kin phong!")
             )
            )

            ;; 4. Nguoi dung click 1 diem tiep theo tren ban ve
            ((listp inp)
             (setq ptCurr (trans inp 1 0))
             (if (> (distance ptPrev ptCurr) 10.0)
               (progn
                 (setq ang (angle ptPrev ptCurr))
                 (setq ang_p (+ ang (/ pi 2.0)))

                 (setq pA_L (polar ptPrev ang_p half_w)
                       pA_R (polar ptPrev ang_p (- half_w))
                       pB_L (polar ptCurr ang_p half_w)
                       pB_R (polar ptCurr ang_p (- half_w)))

                 (if isFirstSeg
                   ;; Doan dau tien: Kiem tra diem bat dau ptPrev co dam vao tuong cu tren ban ve khong
                   (progn
                     (setq tTrim1 (vlt:trim-t-junction ptCurr ptPrev (list pB_L pA_L) (list pB_R pA_R) w ang drawnLines))
                     (if tTrim1
                       (setq pA_L (car tTrim1)
                             pA_R (cadr tTrim1))
                     )
                     (setq firstPtStart ptPrev
                           firstPtSecond ptCurr
                           firstA_L pA_L
                           firstA_R pA_R
                           firstB_L pB_L
                           firstB_R pB_R
                           angFirst ang)
                   )
                   ;; Cac doan tiep theo: Giao goc L voi doan truoc do -> Miter khoet thong goc L
                   (progn
                     (setq miterPts (vlt:miter-corner prevA_L prevA_R prevB_L prevB_R pA_L pA_R pB_L pB_R angPrev ang))
                     (setq BL (car miterPts) BR (cadr miterPts))
                     ;; Cap nhat diem cuoi cua 2 duong doan truoc
                     (vlt:set-line-endpoint lastL1 BL T)
                     (vlt:set-line-endpoint lastL2 BR T)
                     ;; Diem bat dau cua doan hien tai bat dau tu giao diem miter BL, BR
                     (setq pA_L BL pA_R BR)
                   )
                 )

                 ;; Kiem tra diem cuoi ptCurr: Neu dam vao tuong cu tren ban ve thi duc thong chu T
                 (setq tTrim2 (vlt:trim-t-junction ptPrev ptCurr (list pA_L pB_L) (list pA_R pB_R) w ang drawnLines))
                 (if tTrim2
                   (setq pB_L (car tTrim2)
                         pB_R (cadr tTrim2))
                 )

                 ;; Ve 2 duong net tuong cua doan nay
                 (setq line1 (vlt:draw-wall-line pA_L pB_L lay))
                 (setq line2 (vlt:draw-wall-line pA_R pB_R lay))

                 (if isFirstSeg
                   (setq firstLine1 line1 firstLine2 line2)
                 )

                 (setq drawnLines (cons line1 (cons line2 drawnLines)))

                 ;; Luu lich su de ho tro Undo
                 (setq segHistory (cons (list line1 line2 ptPrev (list pA_L pA_R pB_L pB_R)) segHistory))

                 ;; Cap nhat trang thai cho doan tiep theo
                 (setq lastL1 line1
                       lastL2 line2
                       prevA_L pA_L prevA_R pA_R
                       prevB_L pB_L prevB_R pB_R
                       angPrev ang
                       ptPrev ptCurr
                       isFirstSeg nil)

                 (setq count (+ count 1))
                 (vla-regen doc 0)

                 ;; Neu diem cuoi da dam thong vao 1 tuong cu thi thong bao
                 (if tTrim2
                   (princ "\n-> Da tu dong khoet lo chu T vao tuong lien ket!")
                 )
               )
             )
            )
          )
        )
      )
    )
  )

  (if old_osm (setvar "OSMODE" old_osm))
  (setq *error* old_err)
  (princ "\n>>> Hoan tat ve tuong! <<<\n")
)

;; ==========================================================================
;;  LENH TONG HOP DUY NHAT: VT (HOAC TUONG)
;;  Chi hien thi lua chon 1 va 2 (khong hien thi chu VT gay loi)
;; ==========================================================================
(defun c:VT ( / doc lay_tuong w half_w opt inpW )
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq lay_tuong "TUONG")
  (vlt:make-layer lay_tuong 7 "Continuous")

  (setq inpW (getdist (strcat "\nNhap be day tuong (mm) <" *vlt_wall_w* ">: ")))
  (if (and inpW (> inpW 0))
    (setq *vlt_wall_w* (rtos inpW 2 0))
  )
  (setq w (distof *vlt_wall_w*))
  (if (or (null w) (<= w 0)) (setq w 220.0))
  (setq half_w (/ w 2.0))

  (if (not *vlt_wall_mode*) (setq *vlt_wall_mode* "2"))
  (initget "1-QuetGiaoDiem,1 2-VeTayLienTuc,2")
  (setq opt (getkword (strcat "\nVE TUONG - Chon che do [1-QuetGiaoDiem/2-VeTayLienTuc] <" *vlt_wall_mode* ">: ")))
  (cond
    ((or (null opt) (wcmatch opt "*2*")) (setq opt "2"))
    ((wcmatch opt "*1*")                 (setq opt "1"))
    (t (setq opt "2"))
  )
  (setq *vlt_wall_mode* opt)

  (cond
    ((= opt "1")
     (vlt:wall-from-grid-perfect w lay_tuong doc)
    )
    ((= opt "2")
     (vlt:wall-by-points w lay_tuong doc)
    )
  )
  (princ)
)

(defun c:TUONG () (c:VT))

;; ==========================================================================
;;  LENH PHU: VC (ALIAS CHO VE COT)
;; ==========================================================================
(defun c:VC () (c:VCOT))

;; ==========================================================================
;;  THONG BAO KHI LOAD FILE - BO 4 LENH TINH GON
;; ==========================================================================


;; ==========================================================================
;;  LENH 6: DIM KICH THUOC TRUC & TONG THE TU DONG (DT / DTRUC / DIMTRUC)
;;  Chi can quet truc hoac Enter de tu dong dim toan bo truc va tong the
;; ==========================================================================

;; ======================================================================

;; ======================================================================
;; MODULE: DIM TRUC THONG MINH (TU DONG NHAN DIEN TRUC, HUONG VAO TRONG / RA NGOAI)
;; ======================================================================

;; 1. Tu dong nhan dien ty le ban ve va kich thuoc chu toi uu
(defun vlt:auto-detect-scale (xList yList / totalW totalH maxSpan minSpan txth ss i ent el h textList)
  ;; 1.1 Tinh toan kich thuoc luoi truc
  (setq totalW (if (and xList (>= (length xList) 2)) (- (last xList) (car xList)) 0.0)
        totalH (if (and yList (>= (length yList) 2)) (- (last yList) (car yList)) 0.0)
        maxSpan (max totalW totalH))

  ;; Tinh buoc nhip nho nhat de chu khong bi de len nhau
  (setq minSpan 1e9)
  (if (and xList (>= (length xList) 2))
    (progn
      (setq i 0)
      (while (< i (1- (length xList)))
        (setq minSpan (min minSpan (abs (- (nth (1+ i) xList) (nth i xList)))))
        (setq i (1+ i))
      )
    )
  )
  (if (and yList (>= (length yList) 2))
    (progn
      (setq i 0)
      (while (< i (1- (length yList)))
        (setq minSpan (min minSpan (abs (- (nth (1+ i) yList) (nth i yList)))))
        (setq i (1+ i))
      )
    )
  )
  (if (> minSpan 1e8) (setq minSpan 3000.0))

  ;; 1.2 Quet tim text hien co tren ban ve (uu tien cac layer TRUC, TEXT, COT)
  (setq textList '())
  (setq ss (ssget "_X" '((0 . "TEXT,MTEXT") (8 . "*TRUC*,*TEXT*,*COT*,*DIM*,*TEN*"))))
  (if (null ss)
    (setq ss (ssget "_X" '((0 . "TEXT,MTEXT"))))
  )
  (if ss
    (progn
      (setq i 0)
      (while (and (< i (sslength ss)) (< (length textList) 25))
        (setq ent (ssname ss i)
              el (entget ent)
              h (cdr (assoc 40 el)))
        (if (and h (> h 0.05) (< h (* (max maxSpan 1000.0) 0.25)))
          (setq textList (cons h textList))
        )
        (setq i (1+ i))
      )
    )
  )

  ;; 1.3 Uu tien su dung chieu cao chu tim thay trong ban ve
  (if (and textList (> (length textList) 0))
    (progn
      (setq textList (vl-sort textList '<))
      (setq txth (nth (/ (length textList) 2) textList))
      (if (> txth 50.0)
        (setq txth (min txth 350.0))
      )
    )
    (progn
      (cond
        ((< maxSpan 200.0)
         (setq txth (max 0.15 (min 0.35 (* maxSpan 0.015))))
        )
        ((<= maxSpan 15000.0)
         (setq txth 200.0)
        )
        ((<= maxSpan 35000.0)
         (setq txth 220.0)
        )
        ((<= maxSpan 70000.0)
         (setq txth 280.0)
        )
        (t
         (setq txth 350.0)
        )
      )
    )
  )

  ;; Gioi han chieu cao chu khong vuot qua 1/6 buoc nhip nho nhat
  (if (> minSpan 0.0)
    (setq txth (min txth (/ minSpan 6.0)))
  )
  (if (> maxSpan 200.0)
    (setq txth (max txth 150.0))
  )

  txth
)

;; 2. Ham tat toan bo dau ngoac don vi phu (Alternate Units) tren ban ve
(defun vlt:remove-all-dim-brackets ( / ss i ent obj)
  (vl-catch-all-apply 'setvar (list "DIMALT" 0))
  (vl-catch-all-apply 'setvar (list "DIMAPOST" ""))
  (setq ss (ssget "_X" '((0 . "DIMENSION") (8 . "*DIM*,*KICHTHUOC*"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (vl-catch-all-apply 'vla-put-AltUnits (list obj :vlax-false))
        (vl-catch-all-apply 'vla-put-TextPrefix (list obj ""))
        (vl-catch-all-apply 'vla-put-TextSuffix (list obj ""))
        (setq i (1+ i))
      )
    )
  )
)

;; 3. Ham thiet lap vi tri chu dim (ActiveX + DXF) de hoan toan khong bi de len nhau
(defun vlt:set-dim-text-pos (dimObj xPos yPos / ent el dxf70)
  (if dimObj
    (progn
      ;; Cho phep di chuyen chu khong kem duong dan leader (TextMovement = 2: acMoveTextNoLeader)
      (vl-catch-all-apply 'vla-put-TextMovement (list dimObj 2))
      (vl-catch-all-apply 'vla-put-TextInside (list dimObj :vlax-true))
      ;; Dat toa do chu qua ActiveX
      (vl-catch-all-apply 'vla-put-TextPosition (list dimObj (vlax-3d-point (list xPos yPos 0.0))))
      ;; Dong thoi cap nhat qua DXF de dam bao tuong thich 100% tren moi ban CAD
      (setq ent (vl-catch-all-apply 'vlax-vla-object->ename (list dimObj)))
      (if (and ent (not (vl-catch-all-error-p ent)) (setq el (entget ent)))
        (progn
          (setq el (subst (cons 11 (list xPos yPos 0.0)) (assoc 11 el) el))
          (setq dxf70 (cdr (assoc 70 el)))
          (if (and dxf70 (= (logand dxf70 128) 0))
            (setq el (subst (cons 70 (+ dxf70 128)) (assoc 70 el) el))
          )
          (entmod el)
          (entupd ent)
        )
      )
    )
  )
)

;; 3.1 Ham tinh toan so le (ne chu) thong minh cho cac phan doan hep (< 500mm nhu cot, doan tuong nho)
;; Tra ve danh sach level: 0 = baseline (nam tren duong dim), 1 = so le (day cao len tranh de chu)
(defun vlt:compute-stagger-levels (coordList txth / n i lenList thresh levels prevLvl L)
  (setq n (1- (length coordList)))
  (setq thresh (max 500.0 (* 2.8 txth)))
  (setq levels '() i 0 prevLvl 0)
  (while (< i n)
    (setq L (abs (- (nth (1+ i) coordList) (nth i coordList))))
    (cond
      ;; Neu phan doan nho hon nguong, so le de nhin ro chu va tranh de chu ke ben
      ((< L thresh)
       (if (= prevLvl 1)
         (setq prevLvl 0)
         (setq prevLvl 1)
       )
      )
      ;; Phan doan du rong (>= thresh), giu nguyen tren duong dim co so
      (t
       (setq prevLvl 0)
      )
    )
    (setq levels (cons prevLvl levels))
    (setq i (1+ i))
  )
  (reverse levels)
)

;; 3.2 Ham tao doi tuong Dimension xoay (Rotated Dimension) va tu dong ap dung ti le
(defun vlt:draw-dim-rotated (p1 p2 pDim rotAng txth arrowSz lay / doc modelSpace dimObj cur_dimscale prop_txth)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq modelSpace (vla-get-modelspace doc))
  (setq dimObj (vla-adddimrotated modelSpace
                                  (vlax-3d-point p1)
                                  (vlax-3d-point p2)
                                  (vlax-3d-point pDim)
                                  rotAng))
  (if dimObj
    (progn
      (if lay (vl-catch-all-apply 'vla-put-Layer (list dimObj lay)))

      ;; TAT TRIET DE DON VI PHU (ALTERNATE UNITS) - BO HOAN TOAN DAU NGOAC [...]
      (vl-catch-all-apply 'vla-put-AltUnits (list dimObj :vlax-false))
      (vl-catch-all-apply 'vla-put-TextPrefix (list dimObj ""))
      (vl-catch-all-apply 'vla-put-TextSuffix (list dimObj ""))

      ;; Can chinh ty le chu va dau mui ten phu hop voi DIMSCALE
      (setq cur_dimscale (getvar "DIMSCALE"))
      (if (or (null cur_dimscale) (<= cur_dimscale 0.0)) (setq cur_dimscale 1.0))
      (setq prop_txth (/ txth cur_dimscale))

      (vl-catch-all-apply 'vla-put-TextHeight (list dimObj prop_txth))
      (vl-catch-all-apply 'vla-put-ArrowheadSize (list dimObj (/ arrowSz cur_dimscale)))
      (vl-catch-all-apply 'vla-put-ExtensionLineExtend (list dimObj (* prop_txth 0.6)))
      (vl-catch-all-apply 'vla-put-ExtensionLineOffset (list dimObj (* prop_txth 0.5)))
      (vl-catch-all-apply 'vla-put-LinearScaleFactor (list dimObj 1.0))

      ;; EP CHU DIM KHONG BI DAY DAU DANG SANG DOAN KE BEN, GIU O GIUA
      (vl-catch-all-apply 'vla-put-TextInside (list dimObj :vlax-true))
      (vl-catch-all-apply 'vla-put-Fit (list dimObj 3))
      (vl-catch-all-apply 'vla-put-TextMovement (list dimObj 2))
    )
  )
  dimObj
)

;; 4. Ham thu thap toa do chi tiet kien truc (Cot, Tuong, Cua) doc theo mat ngang Y = yRef
(defun vlt:collect-detail-coords-h (yRef minX maxX / wallW halfStrip ss i ent el typ
                                    p1 p2 vObj err pMinL pMaxL ptMin ptMax cen r xRaw res
                                    blkName rot isHoriz)
  (setq wallW (if (and *vlt_wall_w* (> (distof *vlt_wall_w*) 0)) (distof *vlt_wall_w*) 220.0))
  ;; Chi quet trong pham vi be day tuong (+ 40mm) de tranh bat nham cua va tuong ben trong phong
  (setq halfStrip (+ (/ wallW 2.0) 40.0) xRaw '())
  (setq ss (ssget "_C"
                  (list (- minX 250.0) (- yRef halfStrip) 0.0)
                  (list (+ maxX 250.0) (+ yRef halfStrip) 0.0)
                  '((0 . "LINE,LWPOLYLINE,INSERT,CIRCLE")
                    (8 . "*TUONG*,*WALL*,*COT*,*COL*,*CUA*,*DOOR*,*WINDOW*,0"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              el (entget ent)
              typ (cdr (assoc 0 el)))

        (cond
          ;; 1. Doan thang LINE (Tuong, chan tuong, canh cot)
          ((= typ "LINE")
           (setq p1 (cdr (assoc 10 el))
                 p2 (cdr (assoc 11 el)))
           (if (and p1 p2)
             (progn
               (cond
                 ;; Doan nam ngang: lay 2 diem dau mut X
                 ((< (abs (- (cadr p1) (cadr p2))) 30.0)
                  (setq xRaw (cons (car p1) xRaw))
                  (setq xRaw (cons (car p2) xRaw))
                 )
                 ;; Doan thang dung (canh cot, mep tuong giao): lay vi tri X
                 ((< (abs (- (car p1) (car p2))) 30.0)
                  (setq xRaw (cons (/ (+ (car p1) (car p2)) 2.0) xRaw))
                 )
               )
             )
           )
          )

          ;; 2. Khoi Block INSERT (Cua di, Cua so, Cot Block)
          ((= typ "INSERT")
           (setq vObj (vlax-ename->vla-object ent))
           (setq blkName (cdr (assoc 2 el)))
           (if (null blkName) (setq blkName ""))
           (setq rot (vla-get-Rotation vObj))
           ;; Neu la cua di hoac cua so, chi lay neu nam ngang (Rotation gan 0 hoac pi)
           (setq isHoriz T)
           (if (or (wcmatch (strcase blkName) "*CUA*,*DOOR*,*WINDOW*,*CS*,*CD*")
                   (wcmatch (strcase (cdr (assoc 8 el))) "*CUA*,*DOOR*,*WINDOW*"))
             (setq isHoriz (or (< rot 0.15)
                               (> rot (- (* 2.0 pi) 0.15))
                               (< (abs (- rot pi)) 0.15)))
           )
           (if isHoriz
             (progn
               (setq err (vl-catch-all-apply 'vla-getboundingbox (list vObj 'ptMin 'ptMax)))
               (if (not (vl-catch-all-error-p err))
                 (progn
                   (setq pMinL (vlax-safearray->list ptMin)
                         pMaxL (vlax-safearray->list ptMax))
                   (setq xRaw (cons (car pMinL) xRaw))
                   (setq xRaw (cons (car pMaxL) xRaw))
                 )
               )
             )
           )
          )

          ;; 3. Polyline LWPOLYLINE (Cot chu nhat)
          ((= typ "LWPOLYLINE")
           (setq vObj (vlax-ename->vla-object ent))
           (setq err (vl-catch-all-apply 'vla-getboundingbox (list vObj 'ptMin 'ptMax)))
           (if (not (vl-catch-all-error-p err))
             (progn
               (setq pMinL (vlax-safearray->list ptMin)
                     pMaxL (vlax-safearray->list ptMax))
               (setq xRaw (cons (car pMinL) xRaw))
               (setq xRaw (cons (car pMaxL) xRaw))
             )
           )
          )

          ;; 4. Hinh tron CIRCLE (Cot tron)
          ((= typ "CIRCLE")
           (setq cen (cdr (assoc 10 el))
                 r (cdr (assoc 40 el)))
           (if (and cen r)
             (progn
               (setq xRaw (cons (- (car cen) r) xRaw))
               (setq xRaw (cons (+ (car cen) r) xRaw))
             )
           )
          )
        )
        (setq i (1+ i))
      )
    )
  )

  ;; Loc chi giu cac diem trong pham vi [minX - 300, maxX + 300]
  (setq xRaw (vl-remove-if '(lambda (x) (or (< x (- minX 300.0)) (> x (+ maxX 300.0)))) xRaw))

  ;; Sap xep va loc trung voi sai so 50mm (tranh tao phan doan qua nho)
  (setq res (vlt:unique-sorted-coords xRaw 50.0))
  res
)

;; 5. Ham thu thap toa do chi tiet kien truc (Cot, Tuong, Cua) doc theo mat dung X = xRef
(defun vlt:collect-detail-coords-v (xRef minY maxY / wallW halfStrip ss i ent el typ
                                    p1 p2 vObj err pMinL pMaxL ptMin ptMax cen r yRaw res
                                    blkName rot isVert)
  (setq wallW (if (and *vlt_wall_w* (> (distof *vlt_wall_w*) 0)) (distof *vlt_wall_w*) 220.0))
  (setq halfStrip (+ (/ wallW 2.0) 40.0) yRaw '())
  (setq ss (ssget "_C"
                  (list (- xRef halfStrip) (- minY 250.0) 0.0)
                  (list (+ xRef halfStrip) (+ maxY 250.0) 0.0)
                  '((0 . "LINE,LWPOLYLINE,INSERT,CIRCLE")
                    (8 . "*TUONG*,*WALL*,*COT*,*COL*,*CUA*,*DOOR*,*WINDOW*,0"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              el (entget ent)
              typ (cdr (assoc 0 el)))

        (cond
          ;; 1. Doan thang LINE (Tuong doc, chan tuong, canh cot)
          ((= typ "LINE")
           (setq p1 (cdr (assoc 10 el))
                 p2 (cdr (assoc 11 el)))
           (if (and p1 p2)
             (progn
               (cond
                 ;; Doan thang dung: lay 2 diem dau mut Y
                 ((< (abs (- (car p1) (car p2))) 30.0)
                  (setq yRaw (cons (cadr p1) yRaw))
                  (setq yRaw (cons (cadr p2) yRaw))
                 )
                 ;; Doan nam ngang (canh cot, mep tuong giao): lay vi tri Y
                 ((< (abs (- (cadr p1) (cadr p2))) 30.0)
                  (setq yRaw (cons (/ (+ (cadr p1) (cadr p2)) 2.0) yRaw))
                 )
               )
             )
           )
          )

          ;; 2. Khoi Block INSERT (Cua di, Cua so, Cot Block)
          ((= typ "INSERT")
           (setq vObj (vlax-ename->vla-object ent))
           (setq blkName (cdr (assoc 2 el)))
           (if (null blkName) (setq blkName ""))
           (setq rot (vla-get-Rotation vObj))
           ;; Neu la cua di hoac cua so, chi lay neu nam dung (Rotation gan pi/2 hoac 3*pi/2)
           (setq isVert T)
           (if (or (wcmatch (strcase blkName) "*CUA*,*DOOR*,*WINDOW*,*CS*,*CD*")
                   (wcmatch (strcase (cdr (assoc 8 el))) "*CUA*,*DOOR*,*WINDOW*"))
             (setq isVert (or (< (abs (- rot (/ pi 2.0))) 0.15)
                              (< (abs (- rot (* 1.5 pi))) 0.15)))
           )
           (if isVert
             (progn
               (setq err (vl-catch-all-apply 'vla-getboundingbox (list vObj 'ptMin 'ptMax)))
               (if (not (vl-catch-all-error-p err))
                 (progn
                   (setq pMinL (vlax-safearray->list ptMin)
                         pMaxL (vlax-safearray->list ptMax))
                   (setq yRaw (cons (cadr pMinL) yRaw))
                   (setq yRaw (cons (cadr pMaxL) yRaw))
                 )
               )
             )
           )
          )

          ;; 3. Polyline LWPOLYLINE (Cot chu nhat)
          ((= typ "LWPOLYLINE")
           (setq vObj (vlax-ename->vla-object ent))
           (setq err (vl-catch-all-apply 'vla-getboundingbox (list vObj 'ptMin 'ptMax)))
           (if (not (vl-catch-all-error-p err))
             (progn
               (setq pMinL (vlax-safearray->list ptMin)
                     pMaxL (vlax-safearray->list ptMax))
               (setq yRaw (cons (cadr pMinL) yRaw))
               (setq yRaw (cons (cadr pMaxL) yRaw))
             )
           )
          )

          ;; 4. Hinh tron CIRCLE (Cot tron)
          ((= typ "CIRCLE")
           (setq cen (cdr (assoc 10 el))
                 r (cdr (assoc 40 el)))
           (if (and cen r)
             (progn
               (setq yRaw (cons (- (cadr cen) r) yRaw))
               (setq yRaw (cons (+ (cadr cen) r) yRaw))
             )
           )
          )
        )
        (setq i (1+ i))
      )
    )
  )

  ;; Loc chi giu cac diem trong pham vi [minY - 300, maxY + 300]
  (setq yRaw (vl-remove-if '(lambda (y) (or (< y (- minY 300.0)) (> y (+ maxY 300.0)))) yRaw))

  ;; Sap xep va loc trung voi sai so 50mm
  (setq res (vlt:unique-sorted-coords yRaw 50.0))
  res
)

;; 6. Ham dat 1, 2 hoac 3 lop Dim phuong ngang (cho cac truc doc X)
;; isTop = T: Huong len tren (Vao trong hoac Len tren)
;; isTop = nil: Huong xuong duoi (Ra ngoai hoac Xuong duoi)
;; numLayers: 1 = Chi tiet, 2 = Chi tiet + Truc, 3 = Chi tiet + Truc + Tong the
(defun vlt:place-dim-horizontal (xList yRef yD1 isTop txth arrowSz lay numLayers /
                                 n k xA xB xFirst xLast yD2 yD3 dli_dist count
                                 xDetailList m detailCount lvlList curLvl dimObj yStagger
                                 hasDetail)
  (setq count 0)
  (if (and xList (>= (length xList) 2))
    (progn
      (setq n (length xList)
            xFirst (car xList)
            xLast (last xList)
            dli_dist (max 500.0 (* txth 3.2)))

      ;; Thu thap chi tiet tuong/cot/cua doc theo mat ngang nay
      (setq xDetailList (vlt:collect-detail-coords-h yRef xFirst xLast))
      (setq hasDetail (and xDetailList (>= (length xDetailList) 2)))

      (cond
        ;; ==============================================================
        ;; TRUONG HOP CO CHI TIET (TUONG, COT, CUA)
        ;; ==============================================================
        (hasDetail
         (setq m (length xDetailList)
               lvlList (vlt:compute-stagger-levels xDetailList txth))

         ;; --- LOP 1: DIM CHI TIET (Co mat o ca 3 che do: 1, 2, 3 lop) ---
         (setq k 0 detailCount 0)
         (while (< k (1- m))
           (setq xA (nth k xDetailList) xB (nth (1+ k) xDetailList))
           (if (>= (abs (- xB xA)) 70.0)
             (progn
               (setq dimObj (vlt:draw-dim-rotated (list xA yRef 0.0) (list xB yRef 0.0)
                                                  (list (/ (+ xA xB) 2.0) yD1 0.0) 0.0 txth arrowSz lay))
               (if dimObj
                 (progn
                   (setq curLvl (if lvlList (nth k lvlList) 0))
                   (if (= curLvl 1)
                     (progn
                       ;; So le ne chu: doi chu len/xuong 1 khoang 1.4 * txth de hoan toan tach biet
                       (setq yStagger (if isTop (+ yD1 (* 1.4 txth)) (- yD1 (* 1.4 txth))))
                       (vlt:set-dim-text-pos dimObj (/ (+ xA xB) 2.0) yStagger)
                     )
                   )
                 )
               )
               (setq count (1+ count) detailCount (1+ detailCount))
             )
           )
           (setq k (1+ k))
         )

         ;; --- LOP 2: DIM CAC TRUC (Xuat hien o che do 2 lop va 3 lop) ---
         (if (>= numLayers 2)
           (progn
             (setq yD2 (if isTop (+ yD1 dli_dist) (- yD1 dli_dist)))
             (setq k 0)
             (while (< k (1- n))
               (setq xA (nth k xList) xB (nth (1+ k) xList))
               (vlt:draw-dim-rotated (list xA yRef 0.0) (list xB yRef 0.0)
                                     (list (/ (+ xA xB) 2.0) yD2 0.0) 0.0 txth arrowSz lay)
               (setq count (1+ count) k (1+ k))
             )
           )
         )

         ;; --- LOP 3: DIM TONG THE (Xuat hien o che do 3 lop) ---
         (if (>= numLayers 3)
           (progn
             (setq yD3 (if isTop (+ yD2 dli_dist) (- yD2 dli_dist)))
             (vlt:draw-dim-rotated (list xFirst yRef 0.0) (list xLast yRef 0.0)
                                   (list (/ (+ xFirst xLast) 2.0) yD3 0.0) 0.0 txth arrowSz lay)
             (setq count (1+ count))
           )
         )
        )

        ;; ==============================================================
        ;; TRUONG HOP KHONG TIM THAY CHI TIET (Chi co luoi truc)
        ;; Fallback: 1 lop = truc, 2 lop = truc + tong the, 3 lop = truc + tong the
        ;; ==============================================================
        (t
         (cond
           ((= numLayers 1)
            (setq k 0)
            (while (< k (1- n))
              (setq xA (nth k xList) xB (nth (1+ k) xList))
              (vlt:draw-dim-rotated (list xA yRef 0.0) (list xB yRef 0.0)
                                    (list (/ (+ xA xB) 2.0) yD1 0.0) 0.0 txth arrowSz lay)
              (setq count (1+ count) k (1+ k))
            )
           )
           (t
            (setq yD2 (if isTop (+ yD1 dli_dist) (- yD1 dli_dist)))
            (setq k 0)
            (while (< k (1- n))
              (setq xA (nth k xList) xB (nth (1+ k) xList))
              (vlt:draw-dim-rotated (list xA yRef 0.0) (list xB yRef 0.0)
                                    (list (/ (+ xA xB) 2.0) yD1 0.0) 0.0 txth arrowSz lay)
              (setq count (1+ count) k (1+ k))
            )
            (vlt:draw-dim-rotated (list xFirst yRef 0.0) (list xLast yRef 0.0)
                                  (list (/ (+ xFirst xLast) 2.0) yD2 0.0) 0.0 txth arrowSz lay)
            (setq count (1+ count))
           )
         )
        )
      )
    )
  )
  count
)

;; 7. Ham dat 1, 2 hoac 3 lop Dim phuong dung (cho cac truc ngang Y)
;; isRight = T: Huong sang phai (Vao trong hoac Sang phai)
;; isRight = nil: Huong sang trai (Ra ngoai hoac Sang trai)
(defun vlt:place-dim-vertical (yList xRef xD1 isRight txth arrowSz lay numLayers /
                              m k yA yB yFirst yLast xD2 xD3 dli_dist count
                              yDetailList p detailCount lvlList curLvl dimObj xStagger
                              hasDetail)
  (setq count 0)
  (if (and yList (>= (length yList) 2))
    (progn
      (setq m (length yList)
            yFirst (car yList)
            yLast (last yList)
            dli_dist (max 500.0 (* txth 3.2)))

      ;; Thu thap chi tiet tuong/cot/cua doc theo mat dung nay
      (setq yDetailList (vlt:collect-detail-coords-v xRef yFirst yLast))
      (setq hasDetail (and yDetailList (>= (length yDetailList) 2)))

      (cond
        ;; ==============================================================
        ;; TRUONG HOP CO CHI TIET (TUONG, COT, CUA)
        ;; ==============================================================
        (hasDetail
         (setq p (length yDetailList)
               lvlList (vlt:compute-stagger-levels yDetailList txth))

         ;; --- LOP 1: DIM CHI TIET (Co mat o ca 3 che do: 1, 2, 3 lop) ---
         (setq k 0 detailCount 0)
         (while (< k (1- p))
           (setq yA (nth k yDetailList) yB (nth (1+ k) yDetailList))
           (if (>= (abs (- yB yA)) 70.0)
             (progn
               (setq dimObj (vlt:draw-dim-rotated (list xRef yA 0.0) (list xRef yB 0.0)
                                                  (list xD1 (/ (+ yA yB) 2.0) 0.0) (/ pi 2.0) txth arrowSz lay))
               (if dimObj
                 (progn
                   (setq curLvl (if lvlList (nth k lvlList) 0))
                   (if (= curLvl 1)
                     (progn
                       ;; So le ne chu: doi chu sang trai/phai 1 khoang 1.4 * txth de hoan toan tach biet
                       (setq xStagger (if isRight (+ xD1 (* 1.4 txth)) (- xD1 (* 1.4 txth))))
                       (vlt:set-dim-text-pos dimObj xStagger (/ (+ yA yB) 2.0))
                     )
                   )
                 )
               )
               (setq count (1+ count) detailCount (1+ detailCount))
             )
           )
           (setq k (1+ k))
         )

         ;; --- LOP 2: DIM CAC TRUC (Xuat hien o che do 2 lop va 3 lop) ---
         (if (>= numLayers 2)
           (progn
             (setq xD2 (if isRight (+ xD1 dli_dist) (- xD1 dli_dist)))
             (setq k 0)
             (while (< k (1- m))
               (setq yA (nth k yList) yB (nth (1+ k) yList))
               (vlt:draw-dim-rotated (list xRef yA 0.0) (list xRef yB 0.0)
                                     (list xD2 (/ (+ yA yB) 2.0) 0.0) (/ pi 2.0) txth arrowSz lay)
               (setq count (1+ count) k (1+ k))
             )
           )
         )

         ;; --- LOP 3: DIM TONG THE (Xuat hien o che do 3 lop) ---
         (if (>= numLayers 3)
           (progn
             (setq xD3 (if isRight (+ xD2 dli_dist) (- xD2 dli_dist)))
             (vlt:draw-dim-rotated (list xRef yFirst 0.0) (list xRef yLast 0.0)
                                   (list xD3 (/ (+ yFirst yLast) 2.0) 0.0) (/ pi 2.0) txth arrowSz lay)
             (setq count (1+ count))
           )
         )
        )

        ;; ==============================================================
        ;; TRUONG HOP KHONG TIM THAY CHI TIET (Chi co luoi truc)
        ;; Fallback: 1 lop = truc, 2 lop = truc + tong the, 3 lop = truc + tong the
        ;; ==============================================================
        (t
         (cond
           ((= numLayers 1)
            (setq k 0)
            (while (< k (1- m))
              (setq yA (nth k yList) yB (nth (1+ k) yList))
              (vlt:draw-dim-rotated (list xRef yA 0.0) (list xRef yB 0.0)
                                    (list xD1 (/ (+ yA yB) 2.0) 0.0) (/ pi 2.0) txth arrowSz lay)
              (setq count (1+ count) k (1+ k))
            )
           )
           (t
            (setq xD2 (if isRight (+ xD1 dli_dist) (- xD1 dli_dist)))
            (setq k 0)
            (while (< k (1- m))
              (setq yA (nth k yList) yB (nth (1+ k) yList))
              (vlt:draw-dim-rotated (list xRef yA 0.0) (list xRef yB 0.0)
                                    (list xD1 (/ (+ yA yB) 2.0) 0.0) (/ pi 2.0) txth arrowSz lay)
              (setq count (1+ count) k (1+ k))
            )
            (vlt:draw-dim-rotated (list xRef yFirst 0.0) (list xRef yLast 0.0)
                                  (list xD2 (/ (+ yFirst yLast) 2.0) 0.0) (/ pi 2.0) txth arrowSz lay)
            (setq count (1+ count))
           )
         )
        )
      )
    )
  )
  count
)

;; 8. Ham tim gia tri toa do truc gan nhat voi diem click
(defun vlt:find-nearest-coord (val coordList / bestVal bestDist d)
  (if (null coordList)
    val
    (progn
      (setq bestVal (car coordList)
            bestDist (abs (- val bestVal)))
      (foreach c (cdr coordList)
        (setq d (abs (- val c)))
        (if (< d bestDist)
          (setq bestVal c bestDist d)
        )
      )
      bestVal
    )
  )
)

;; 9. Ham xu ly diem click chuot thong minh (tu dong hieu truc can dim va huong vao trong / ra ngoai)
(defun vlt:process-dim-click (pt xList yList allYVals allXVals txth arrowSz lay numLayers /
                              nearestY nearestX distY distX hasX hasY placedCount yD1 xD1)
  (setq hasX (and xList (>= (length xList) 2))
        hasY (and yList (>= (length yList) 2))
        placedCount 0)

  ;; Tim truc ngang va truc doc gan nhat de lay lam chan duong giong
  (setq nearestY (vlt:find-nearest-coord (cadr pt) (if allYVals allYVals yList)))
  (setq nearestX (vlt:find-nearest-coord (car pt) (if allXVals allXVals xList)))

  (setq distY (abs (- (cadr pt) nearestY))
        distX (abs (- (car pt) nearestX)))

  (cond
    ;; Neu chi co truc doc (phuong ngang) hoac khoang cach toi truc ngang gan hon
    ((or (and hasX (not hasY))
         (and hasX hasY (<= distY distX)))
     (setq yD1 (cadr pt))
     (if (>= yD1 nearestY)
       ;; Click o phia TREN truc -> Dim huong LEN TREN (VAO TRONG)
       (progn
         (setq placedCount (vlt:place-dim-horizontal xList nearestY yD1 T txth arrowSz lay numLayers))
         (princ (strcat "\n[DT] -> Da dat " (itoa numLayers) " lop Dim phuong ngang: HUONG LEN TREN (VAO TRONG)!"))
       )
       ;; Click o phia DUOI truc -> Dim huong XUONG DUOI (RA NGOAI)
       (progn
         (setq placedCount (vlt:place-dim-horizontal xList nearestY yD1 nil txth arrowSz lay numLayers))
         (princ (strcat "\n[DT] -> Da dat " (itoa numLayers) " lop Dim phuong ngang: HUONG XUONG DUOI (RA NGOAI)!"))
       )
     )
    )

    ;; Neu chi co truc ngang (phuong dung) hoac khoang cach toi truc doc gan hon
    ((or (and hasY (not hasX))
         (and hasX hasY (< distX distY)))
     (setq xD1 (car pt))
     (if (>= xD1 nearestX)
       ;; Click o ben PHAI truc -> Dim huong SANG PHAI (VAO TRONG)
       (progn
         (setq placedCount (vlt:place-dim-vertical yList nearestX xD1 T txth arrowSz lay numLayers))
         (princ (strcat "\n[DT] -> Da dat " (itoa numLayers) " lop Dim phuong dung: HUONG SANG PHAI (VAO TRONG)!"))
       )
       ;; Click o ben TRAI truc -> Dim huong SANG TRAI (RA NGOAI)
       (progn
         (setq placedCount (vlt:place-dim-vertical yList nearestX xD1 nil txth arrowSz lay numLayers))
         (princ (strcat "\n[DT] -> Da dat " (itoa numLayers) " lop Dim phuong dung: HUONG SANG TRAI (RA NGOAI)!"))
       )
     )
    )
  )
  placedCount
)

;; 10. LENH CHINH: DT / DTRUC / DIMTRUC
(defun c:DT ( / doc ss ssTruc i ent el lay p1 p2 dx dy xMid yMid
                xVals yVals minX maxX minY maxY xList yList
                grid_xMin grid_xMax grid_yMin grid_yMax
                lay_dim txth_auto txth arrowSz dli_dist off1
                pt ptNext cont totalCount optL numLayers inpTxt )
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  ;; 10.0 Tat tinh nang don vi phu (Alternate Units) va go bo dau ngoac [...] tren toan bo ban ve
  (vlt:remove-all-dim-brackets)

  ;; 10.1 Khoi tao Layer DIM (mau 3 - xanh la)
  (setq lay_dim "DIM")
  (if (tblsearch "layer" "KICHTHUOC") (setq lay_dim "KICHTHUOC"))
  (vlt:make-layer lay_dim 3 "Continuous")

  ;; 10.2 Quet chon duong truc
  (princ "\n[DIM TRUC] Quet chon cac duong truc can ghi kich thuoc (Enter de chon tat ca truc): ")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,XLINE")
                    (8 . "~*TUONG*,~*WALL*,~*COT*,~*DIM*,~*TEXT*,~*HATCH*"))))
  (if (null ss)
    (setq ss (ssget "_X" '((0 . "LINE") (8 . "*TRUC*"))))
  )
  (if (null ss)
    (setq ss (ssget "_X" '((0 . "LINE") (62 . 1))))
  )

  (if (null ss)
    (progn (princ "\nKhong tim thay duong truc nao de ghi kich thuoc!") (princ))
    (progn
      ;; Loc chi lay duong truc neu tap chon co ca layer TRUC hoac mau do
      (setq ssTruc (ssadd) i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              el (entget ent)
              lay (strcase (cdr (assoc 8 el))))
        (if (or (wcmatch lay "*TRUC*") (= (cdr (assoc 62 el)) 1))
          (ssadd ent ssTruc)
        )
        (setq i (+ i 1))
      )
      (if (> (sslength ssTruc) 1)
        (setq ss ssTruc)
      )

      ;; 10.3 Thu thap toa do truc X va Y
      (setq xVals '() yVals '()
            minX 1e9 maxX -1e9
            minY 1e9 maxY -1e9
            i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              el (entget ent)
              p1 (cdr (assoc 10 el))
              p2 (cdr (assoc 11 el)))
        (if (and p1 p2)
          (progn
            (setq dx (abs (- (car p1) (car p2)))
                  dy (abs (- (cadr p1) (cadr p2))))
            (if (< dx dy)
              ;; Truc doc -> Lay toa do X
              (progn
                (setq xMid (/ (+ (car p1) (car p2)) 2.0))
                (setq xVals (cons xMid xVals))
                (setq minY (min minY (min (cadr p1) (cadr p2))))
                (setq maxY (max maxY (max (cadr p1) (cadr p2))))
              )
              ;; Truc ngang -> Lay toa do Y
              (progn
                (setq yMid (/ (+ (cadr p1) (cadr p2)) 2.0))
                (setq yVals (cons yMid yVals))
                (setq minX (min minX (min (car p1) (car p2))))
                (setq maxX (max maxX (max (car p1) (car p2))))
              )
            )
          )
        )
        (setq i (+ i 1))
      )

      (setq xList (vlt:unique-sorted-coords xVals 100.0))
      (setq yList (vlt:unique-sorted-coords yVals 100.0))

      (if (and (or (null xList) (< (length xList) 2))
               (or (null yList) (< (length yList) 2)))
        (progn (princ "\nCan it nhat 2 truc cung phuong de ghi kich thuoc!") (princ))
        (progn
          ;; Toa do truc can dim (Uu tien dung toa do truc that, tranh lay phan duoi bong truc)
          (setq grid_xMin (if xList (car xList) minX))
          (setq grid_xMax (if xList (last xList) maxX))
          (setq grid_yMin (if (and yList (> (length yList) 0)) (car yList) (if yVals (car (vl-sort yVals '<)) minY)))
          (setq grid_yMax (if (and yList (> (length yList) 0)) (last yList) (if yVals (last (vl-sort yVals '<)) maxY)))

          ;; 10.4 Chon so lop Dim [1-LopChiTiet / 2-LopChiTietVaTruc / 3-LopDayDu]
          (if (not *vlt_dim_layers*) (setq *vlt_dim_layers* "3"))
          (initget "1-LopChiTiet,1 2-LopChiTietVaTruc,2 3-LopDayDu,3")
          (setq optL (getkword (strcat "\nDIM TRUC - Chon so lop Dim [1-LopChiTiet/2-LopChiTietVaTruc/3-LopDayDu] <" *vlt_dim_layers* ">: ")))
          (cond
            ((or (null optL) (wcmatch optL "*3*")) (setq optL "3"))
            ((wcmatch optL "*1*")                 (setq optL "1"))
            ((wcmatch optL "*2*")                 (setq optL "2"))
            (t (setq optL "3"))
          )
          (setq *vlt_dim_layers* optL)
          (setq numLayers (atoi optL))

          ;; 10.5 Chieu cao chu: Tu dong kiem soat theo ty le ban ve HOAC Nguoi dung nhap tuy y
          (setq txth_auto (vlt:auto-detect-scale xList yList))
          (if (= numLayers 3) (setq txth_auto (min txth_auto 200.0)))
          (if (or (null *vlt_dim_txth*) (<= *vlt_dim_txth* 0.0))
            (setq *vlt_dim_txth* txth_auto)
          )
          (setq inpTxt (getdist (strcat "\nDIM TRUC - Chieu cao chu Dim (mm) <" (rtos *vlt_dim_txth* 2 0) ">: ")))
          (if (and inpTxt (> inpTxt 0.0))
            (setq txth inpTxt)
            (setq txth *vlt_dim_txth*)
          )
          (setq *vlt_dim_txth* txth)
          (setq arrowSz (* txth 0.75))
          (setq dli_dist (max 500.0 (* txth 3.2)))
          (setq off1 (+ dli_dist 200.0))

          (princ (strcat "\nDIM TRUC - Thiet lap: " (itoa numLayers) " LOP DIM (" (cond ((= numLayers 1) "Chi tiet") ((= numLayers 2) "Chi tiet + Truc") (t "Chi tiet + Truc + Tong the")) ") | Chieu cao chu: "
                         (rtos txth 2 0) "mm | Khoang cach giua cac lop: " (rtos dli_dist 2 0) "mm"))

          (vla-startundomark doc)
          (setq totalCount 0)

          ;; 10.6 Nhap lua chon huong hoac Click chuot dat Dim
          (initget "1-HuongVaoTrong,1 2-HuongRaNgoai,2 C-Ca4Ben,C,4")
          (setq pt (getpoint "\nDIM TRUC - Chon huong dat Dim [1-HuongVaoTrong/2-HuongRaNgoai/C-Ca4Ben] <1>: "))

          (cond
            ;; Option 1 hoac Enter (Mac dinh): HUONG VAO TRONG
            ((or (null pt) (= pt "1") (and (= (type pt) 'STR) (wcmatch pt "*1*")))
             (if (and xList (>= (length xList) 2))
               (progn
                 ;; Truc ngang o duoi: Dim huong LEN TREN (Vao trong)
                 (setq totalCount (+ totalCount (vlt:place-dim-horizontal xList grid_yMin (+ grid_yMin off1) T txth arrowSz lay_dim numLayers)))
                 (if (and yList (> (length yList) 1))
                   ;; Truc ngang o tren: Dim huong XUONG DUOI (Vao trong)
                   (setq totalCount (+ totalCount (vlt:place-dim-horizontal xList grid_yMax (- grid_yMax off1) nil txth arrowSz lay_dim numLayers)))
                 )
               )
             )
             (if (and yList (>= (length yList) 2))
               (progn
                 ;; Truc doc o ben trai: Dim huong SANG PHAI (Vao trong)
                 (setq totalCount (+ totalCount (vlt:place-dim-vertical yList grid_xMin (+ grid_xMin off1) T txth arrowSz lay_dim numLayers)))
                 ;; Truc doc o ben phai: Dim huong SANG TRAI (Vao trong)
                 (setq totalCount (+ totalCount (vlt:place-dim-vertical yList grid_xMax (- grid_xMax off1) nil txth arrowSz lay_dim numLayers)))
               )
             )
             (princ (strcat "\nDIM TRUC -> Da tu dong dat " (itoa numLayers) " lop Dim: HUONG VAO TRONG CONG TRINH!"))
            )

            ;; Option 2: HUONG RA NGOAI (Ve phia bong truc)
            ((or (= pt "2") (and (= (type pt) 'STR) (wcmatch pt "*2*")))
             (if (and xList (>= (length xList) 2))
               (progn
                 (setq totalCount (+ totalCount (vlt:place-dim-horizontal xList grid_yMin (- grid_yMin off1) nil txth arrowSz lay_dim numLayers)))
                 (if (and yList (> (length yList) 1))
                   (setq totalCount (+ totalCount (vlt:place-dim-horizontal xList grid_yMax (+ grid_yMax off1) T txth arrowSz lay_dim numLayers)))
                 )
               )
             )
             (if (and yList (>= (length yList) 2))
               (progn
                 (setq totalCount (+ totalCount (vlt:place-dim-vertical yList grid_xMin (- grid_xMin off1) nil txth arrowSz lay_dim numLayers)))
                 (setq totalCount (+ totalCount (vlt:place-dim-vertical yList grid_xMax (+ grid_xMax off1) T txth arrowSz lay_dim numLayers)))
               )
             )
             (princ (strcat "\nDIM TRUC -> Da tu dong dat " (itoa numLayers) " lop Dim: HUONG RA NGOAI (Ve phia bong truc)!"))
            )

            ;; Option C hoac 4: Dat ca 4 ben
            ((or (= pt "C") (= pt "4") (and (= (type pt) 'STR) (or (wcmatch pt "*C*") (wcmatch pt "*4*"))))
             (if (and xList (>= (length xList) 2))
               (progn
                 (setq totalCount (+ totalCount (vlt:place-dim-horizontal xList grid_yMin (- grid_yMin off1) nil txth arrowSz lay_dim numLayers)))
                 (setq totalCount (+ totalCount (vlt:place-dim-horizontal xList grid_yMax (+ grid_yMax off1) T txth arrowSz lay_dim numLayers)))
               )
             )
             (if (and yList (>= (length yList) 2))
               (progn
                 (setq totalCount (+ totalCount (vlt:place-dim-vertical yList grid_xMin (- grid_xMin off1) nil txth arrowSz lay_dim numLayers)))
                 (setq totalCount (+ totalCount (vlt:place-dim-vertical yList grid_xMax (+ grid_xMax off1) T txth arrowSz lay_dim numLayers)))
               )
             )
             (princ (strcat "\nDIM TRUC -> Da tu dong dat " (itoa numLayers) " lop Dim cho ca 4 ben (Tren, Duoi, Trai, Phai)!"))
            )

            ;; Option: Nguoi dung Click chuot chon vi tri
            ((listp pt)
             (setq pt (trans pt 1 0))
             (setq totalCount (+ totalCount (vlt:process-dim-click pt xList yList yVals xVals txth arrowSz lay_dim numLayers)))

             ;; Vong lap cho phep click tiep cac vi tri khac tuy y
             (setq cont T)
             (while cont
               (setq ptNext (getpoint "\nClick vi tri tiep theo (Tren/Duoi/Trai/Phai - Enter de hoan tat): "))
               (if ptNext
                 (progn
                   (setq ptNext (trans ptNext 1 0))
                   (setq totalCount (+ totalCount (vlt:process-dim-click ptNext xList yList yVals xVals txth arrowSz lay_dim numLayers)))
                 )
                 (setq cont nil)
               )
             )
            )
          )

          (vla-regen doc 0)
          (vla-endundomark doc)
          (princ (strcat "\n>>> HOAN TAT! Da tao thanh cong " (itoa totalCount)
                         " duong dim (" (itoa numLayers) " Lop: Chi tiet / Truc / Tong the)! <<<"))
        )
      )
    )
  )
  (princ)
)

(defun c:DTRUC () (c:DT))
(defun c:DIMTRUC () (c:DT))
(defun c:DIMT () (c:DT))

;; ======================================================================

;; ======================================================================

;; ======================================================================
;; MODULE: VE CUA DI VA CUA SO KIEN TRUC (XUAT KHOI BLOCK CHUYEN NGHIEP)
;; Cac lenh: CD (Cua di 1-2 canh), CS (Cua so), CUA (Bang tuy chon)
;; ======================================================================

(vl-load-com)

;; Bien toan cuc luu thong so cua
(if (not *vcu_type*) (setq *vcu_type* "D1"))
(if (not *vcu_w*)    (setq *vcu_w*    900.0))
(if (not *vcu_t*)    (setq *vcu_t*    200.0))
(if (not *vcu_offset*) (setq *vcu_offset* 300.0))

;; 1. Tao file DCL tam thoi cho hop thoai CUA
(defun vlt:create-door-dcl ( / dcl_path f )
  (setq dcl_path (vl-filename-mktemp "vlt_door.dcl"))
  (setq f (open dcl_path "w"))
  (if f
    (progn
      (write-line "vlt_door_dialog : dialog {" f)
      (write-line "  label = \"VE CUA DI VA CUA SO KIEN TRUC (XUAT BLOCK)\";" f)
      (write-line "  : boxed_radio_column {" f)
      (write-line "    label = \"1. Chon loai cua\";" f)
      (write-line "    key = \"rb_type\";" f)
      (write-line "    : radio_button { label = \"Cua di 1 canh (Cua phong, ve sinh) - Mac dinh W=900\"; key = \"rb_d1\"; }" f)
      (write-line "    : radio_button { label = \"Cua di 2 canh (Cua chinh, ban cong) - Mac dinh W=1400\"; key = \"rb_d2\"; }" f)
      (write-line "    : radio_button { label = \"Cua di 4 canh (Cua chinh mat tien) - Mac dinh W=2400\"; key = \"rb_d4\"; }" f)
      (write-line "    : radio_button { label = \"Cua so 1 canh (Cua so nho) - Mac dinh W=800\"; key = \"rb_s1\"; }" f)
      (write-line "    : radio_button { label = \"Cua so 2 canh (Cua so tieu chuan) - Mac dinh W=1400\"; key = \"rb_s2\"; }" f)
      (write-line "    : radio_button { label = \"Cua so 4 canh (Cua so phong khach) - Mac dinh W=2400\"; key = \"rb_s4\"; }" f)
      (write-line "  }" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"2. Kich thuoc thiet ke\";" f)
      (write-line "    : edit_box { label = \"Chieu rong phu bi cua W (mm):\"; key = \"eb_w\"; edit_width = 12; }" f)
      (write-line "    : edit_box { label = \"Be day tuong lap dat T (mm):\"; key = \"eb_t\"; edit_width = 12; }" f)
      (write-line "  }" f)
      (write-line "  : row {" f)
      (write-line "    fixed_width = true;" f)
      (write-line "    alignment = centered;" f)
      (write-line "    ok_button;" f)
      (write-line "    cancel_button;" f)
      (write-line "  }" f)
      (write-line "}" f)
      (close f)
      dcl_path
    )
    nil
  )
)

;; 2. Cac ham them doi tuong hinh hoc vao dinh nghia Block
(defun vlt:add-box-to-block (blkObj x1 y1 x2 y2 lay / coords pline)
  (setq coords (vlax-make-safearray vlax-vbDouble '(0 . 7)))
  (vlax-safearray-fill coords (list x1 y1 x2 y1 x2 y2 x1 y2))
  (setq pline (vla-addlightweightpolyline blkObj coords))
  (vla-put-closed pline :vlax-true)
  (if lay (vl-catch-all-apply 'vla-put-layer (list pline lay)))
  pline
)

(defun vlt:add-line-to-block (blkObj x1 y1 x2 y2 lay / p1 p2 lineObj)
  (setq p1 (vlax-3d-point (list x1 y1 0.0))
        p2 (vlax-3d-point (list x2 y2 0.0)))
  (setq lineObj (vla-addline blkObj p1 p2))
  (if lay (vl-catch-all-apply 'vla-put-layer (list lineObj lay)))
  lineObj
)

(defun vlt:add-arc-to-block (blkObj cx cy r startAng endAng lay / cen arcObj)
  (setq cen (vlax-3d-point (list cx cy 0.0)))
  (setq arcObj (vla-addarc blkObj cen r startAng endAng))
  (if lay (vl-catch-all-apply 'vla-put-layer (list arcObj lay)))
  arcObj
)

;; 3. Tao dinh nghia Block Cua di 1 canh
(defun vlt:build-block-door-1leaf (blkObj W T_wall / fW tLeaf clearW hY)
  (setq fW 50.0 tLeaf 40.0)
  (setq clearW (- W (* 2.0 fW)))
  (setq hY (/ T_wall 2.0))

  ;; Khuon A (ben trai)
  (vlt:add-box-to-block blkObj 0.0 (- hY) fW hY "0")
  ;; Khuon B (ben phai)
  (vlt:add-box-to-block blkObj (- W fW) (- hY) W hY "0")
  ;; Canh cua mo 90 do vao phia +Y
  (vlt:add-box-to-block blkObj fW hY (+ fW tLeaf) (+ hY clearW) "0")
  ;; Cung tron mo canh (Quarter circle tu 0 den 90 do)
  (vlt:add-arc-to-block blkObj fW hY clearW 0.0 (/ pi 2.0) "0")
)

;; 4. Tao dinh nghia Block Cua di 2 canh
(defun vlt:build-block-door-2leaf (blkObj W T_wall / fW tLeaf clearW halfW hY)
  (setq fW 50.0 tLeaf 40.0)
  (setq clearW (- W (* 2.0 fW)))
  (setq halfW (/ clearW 2.0))
  (setq hY (/ T_wall 2.0))

  ;; Khuon A va Khuon B
  (vlt:add-box-to-block blkObj 0.0 (- hY) fW hY "0")
  (vlt:add-box-to-block blkObj (- W fW) (- hY) W hY "0")

  ;; Canh trai mo 90 do
  (vlt:add-box-to-block blkObj fW hY (+ fW tLeaf) (+ hY halfW) "0")
  ;; Cung mo trai
  (vlt:add-arc-to-block blkObj fW hY halfW 0.0 (/ pi 2.0) "0")

  ;; Canh phai mo 90 do
  (vlt:add-box-to-block blkObj (- W fW tLeaf) hY (- W fW) (+ hY halfW) "0")
  ;; Cung mo phai (gap nhau o giua)
  (vlt:add-arc-to-block blkObj (- W fW) hY halfW (/ pi 2.0) pi "0")
)

;; 4.1 Tao dinh nghia Block Cua di 4 canh (4 canh deu nhau W/4, mo quay dep mat)
(defun vlt:build-block-door-4leaf (blkObj W T_wall / fW tLeaf clearW leafW hY x0 x1 x2 x3 x4)
  (setq fW 50.0 tLeaf 40.0)
  (setq clearW (- W (* 2.0 fW)))
  (setq leafW (/ clearW 4.0))
  (setq hY (/ T_wall 2.0))

  (setq x0 fW)
  (setq x1 (+ fW leafW))
  (setq x2 (+ fW (* 2.0 leafW))) ; Chinh giua tim cua (= W / 2)
  (setq x3 (+ fW (* 3.0 leafW)))
  (setq x4 (- W fW))

  ;; 1. Khuon A (ben trai) va Khuon B (ben phai)
  (vlt:add-box-to-block blkObj 0.0 (- hY) fW hY "0")
  (vlt:add-box-to-block blkObj (- W fW) (- hY) W hY "0")

  ;; 2. Canh 1 (sat khuon trai): mo 90 do
  (vlt:add-box-to-block blkObj x0 hY (+ x0 tLeaf) (+ hY leafW) "0")
  (vlt:add-arc-to-block blkObj x0 hY leafW 0.0 (/ pi 2.0) "0")

  ;; 3. Canh 2 (giua ben trai): mo 90 do
  (vlt:add-box-to-block blkObj x1 hY (+ x1 tLeaf) (+ hY leafW) "0")
  (vlt:add-arc-to-block blkObj x1 hY leafW 0.0 (/ pi 2.0) "0")

  ;; 4. Canh 3 (giua ben phai): mo 90 do
  (vlt:add-box-to-block blkObj (- x3 tLeaf) hY x3 (+ hY leafW) "0")
  (vlt:add-arc-to-block blkObj x3 hY leafW (/ pi 2.0) pi "0")

  ;; 5. Canh 4 (sat khuon phai): mo 90 do
  (vlt:add-box-to-block blkObj (- x4 tLeaf) hY x4 (+ hY leafW) "0")
  (vlt:add-arc-to-block blkObj x4 hY leafW (/ pi 2.0) pi "0")
)

;; 5. Tao dinh nghia Block Cua so 2 canh (Dap ung chuan hinh 1)
(defun vlt:build-block-window-2pane (blkObj W T_wall / fW hY midX)
  (setq fW 50.0)
  (setq hY (/ T_wall 2.0))
  (setq midX (/ W 2.0))

  ;; 2 Khuon o 2 dau
  (vlt:add-box-to-block blkObj 0.0 (- hY) fW hY "0")
  (vlt:add-box-to-block blkObj (- W fW) (- hY) W hY "0")

  ;; 2 Duong nguong / ray cua so
  (vlt:add-line-to-block blkObj 0.0 (- hY) W (- hY) "0")
  (vlt:add-line-to-block blkObj 0.0 hY W hY "0")

  ;; 2 Canh kinh lua o giua so le nhau
  (vlt:add-line-to-block blkObj fW -15.0 (+ midX 25.0) -15.0 "0")
  (vlt:add-line-to-block blkObj (- midX 25.0) 15.0 (- W fW) 15.0 "0")
)

;; 6. Tao dinh nghia Block Cua so 1 canh
(defun vlt:build-block-window-1pane (blkObj W T_wall / fW hY)
  (setq fW 50.0)
  (setq hY (/ T_wall 2.0))

  (vlt:add-box-to-block blkObj 0.0 (- hY) fW hY "0")
  (vlt:add-box-to-block blkObj (- W fW) (- hY) W hY "0")
  (vlt:add-line-to-block blkObj 0.0 (- hY) W (- hY) "0")
  (vlt:add-line-to-block blkObj 0.0 hY W hY "0")
  (vlt:add-line-to-block blkObj fW 0.0 (- W fW) 0.0 "0")
)

;; 7. Tao dinh nghia Block Cua so 4 canh
(defun vlt:build-block-window-4pane (blkObj W T_wall / fW hY)
  (setq fW 50.0)
  (setq hY (/ T_wall 2.0))
  (vlt:build-block-window-2pane blkObj W T_wall)
  ;; Them 2 vach ngan chia 4 canh
  (vlt:add-line-to-block blkObj (* W 0.25) -20.0 (* W 0.25) 20.0 "0")
  (vlt:add-line-to-block blkObj (* W 0.75) -20.0 (* W 0.75) 20.0 "0")
)

;; 8. Ham lay hoac tao Block Definition trong ban ve
(defun vlt:get-or-create-door-block (doc blkName doorType W T_wall / blocks blkObj)
  (setq blocks (vla-get-blocks doc))
  (setq blkObj (vl-catch-all-apply 'vla-item (list blocks blkName)))
  (if (vl-catch-all-error-p blkObj)
    (progn
      (setq blkObj (vla-add blocks (vlax-3d-point '(0.0 0.0 0.0)) blkName))
      (cond
        ((= doorType "D1") (vlt:build-block-door-1leaf blkObj W T_wall))
        ((= doorType "D2") (vlt:build-block-door-2leaf blkObj W T_wall))
        ((= doorType "D4") (vlt:build-block-door-4leaf blkObj W T_wall))
        ((= doorType "S1") (vlt:build-block-window-1pane blkObj W T_wall))
        ((= doorType "S2") (vlt:build-block-window-2pane blkObj W T_wall))
        ((= doorType "S4") (vlt:build-block-window-4pane blkObj W T_wall))
      )
    )
  )
  blkName
)

;; 9. Ham tu dong cat duc thong tuong chuan xac 100% (Khong bao gio bi ho hoac veo net)
(defun vlt:cut-wall-opening (pA pB W T_wall u v lay_tuong / ss i ent el p1 p2
                             d1 d2 dmid s1 s2 sStart sEnd overlapStart overlapEnd
                             hasA hasB ptStart ptEnd pCut0 pCutW cA1 cA2 cB1 cB2
                             hasCutA hasCutB)
  (setq ss (ssget "_C" (list (- (min (car pA) (car pB)) (+ T_wall 100.0))
                             (- (min (cadr pA) (cadr pB)) (+ T_wall 100.0)) 0.0)
                       (list (+ (max (car pA) (car pB)) (+ T_wall 100.0))
                             (+ (max (cadr pA) (cadr pB)) (+ T_wall 100.0)) 0.0)
                       '((0 . "LINE") (8 . "*TUONG*,*WALL*,0"))))
  (setq hasCutA nil hasCutB nil)
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              el (entget ent)
              p1 (cdr (assoc 10 el))
              p2 (cdr (assoc 11 el)))
        (if (and p1 p2)
          (progn
            (setq d1 (+ (* (- (car p1) (car pA)) (car v)) (* (- (cadr p1) (cadr pA)) (cadr v))))
            (setq d2 (+ (* (- (car p2) (car pA)) (car v)) (* (- (cadr p2) (cadr pA)) (cadr v))))
            (if (and (< (abs (- d1 d2)) 25.0) (<= (abs (/ (+ d1 d2) 2.0)) (+ (/ T_wall 2.0) 40.0)))
              (progn
                (setq dmid (/ (+ d1 d2) 2.0))
                (setq s1 (+ (* (- (car p1) (car pA)) (car u)) (* (- (cadr p1) (cadr pA)) (cadr u))))
                (setq s2 (+ (* (- (car p2) (car pA)) (car u)) (* (- (cadr p2) (cadr pA)) (cadr u))))
                (setq sStart (min s1 s2) sEnd (max s1 s2))
                (setq overlapStart (max sStart 0.0) overlapEnd (min sEnd W))
                (if (> overlapEnd (+ overlapStart 1.0))
                  (progn
                    (setq hasA (< sStart -5.0) hasB (> sEnd (+ W 5.0)))
                    (if (<= s1 s2)
                      (setq ptStart p1 ptEnd p2)
                      (setq ptStart p2 ptEnd p1)
                    )
                    ;; Tinh toa do cat: X dung (car v), Y dung (cadr v) - dam bao 100% song song khong bao gio bi veo
                    (setq pCut0 (list (+ (car pA) (* 0.0 (car u)) (* dmid (car v)))
                                      (+ (cadr pA) (* 0.0 (cadr u)) (* dmid (cadr v))) 0.0)
                          pCutW (list (+ (car pA) (* W (car u)) (* dmid (car v)))
                                      (+ (cadr pA) (* W (cadr u)) (* dmid (cadr v))) 0.0))

                    (if hasA (setq hasCutA T))
                    (if hasB (setq hasCutB T))

                    (cond
                      ((and hasA hasB)
                       (entdel ent)
                       (if (> (distance ptStart pCut0) 5.0)
                         (entmake (list '(0 . "LINE")
                                        (cons 8 (cdr (assoc 8 el)))
                                        (if (assoc 62 el) (assoc 62 el) '(62 . 256))
                                        (cons 10 ptStart)
                                        (cons 11 pCut0)))
                       )
                       (if (> (distance pCutW ptEnd) 5.0)
                         (entmake (list '(0 . "LINE")
                                        (cons 8 (cdr (assoc 8 el)))
                                        (if (assoc 62 el) (assoc 62 el) '(62 . 256))
                                        (cons 10 pCutW)
                                        (cons 11 ptEnd)))
                       )
                      )
                      (hasA
                       (entdel ent)
                       (if (> (distance ptStart pCut0) 5.0)
                         (entmake (list '(0 . "LINE")
                                        (cons 8 (cdr (assoc 8 el)))
                                        (if (assoc 62 el) (assoc 62 el) '(62 . 256))
                                        (cons 10 ptStart)
                                        (cons 11 pCut0)))
                       )
                      )
                      (hasB
                       (entdel ent)
                       (if (> (distance pCutW ptEnd) 5.0)
                         (entmake (list '(0 . "LINE")
                                        (cons 8 (cdr (assoc 8 el)))
                                        (if (assoc 62 el) (assoc 62 el) '(62 . 256))
                                        (cons 10 pCutW)
                                        (cons 11 ptEnd)))
                       )
                      )
                      (t
                       (entdel ent)
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )

  ;; Ve net chan dau tuong tai cac mep cua bi cat
  (setq cA1 (list (- (car pA) (* (/ T_wall 2.0) (car v))) (- (cadr pA) (* (/ T_wall 2.0) (cadr v))) 0.0)
        cA2 (list (+ (car pA) (* (/ T_wall 2.0) (car v))) (+ (cadr pA) (* (/ T_wall 2.0) (cadr v))) 0.0)
        cB1 (list (- (car pB) (* (/ T_wall 2.0) (car v))) (- (cadr pB) (* (/ T_wall 2.0) (cadr v))) 0.0)
        cB2 (list (+ (car pB) (* (/ T_wall 2.0) (car v))) (+ (cadr pB) (* (/ T_wall 2.0) (cadr v))) 0.0))
  (if hasCutA
    (entmake (list '(0 . "LINE") (cons 8 lay_tuong) (cons 10 cA1) (cons 11 cA2)))
  )
  (if hasCutB
    (entmake (list '(0 . "LINE") (cons 8 lay_tuong) (cons 10 cB1) (cons 11 cB2)))
  )
)

;; 10. Ham tu dong tim tim tuong va be day tuong tai diem click
(defun vlt:snap-to-wall-center (pt ang / v_perp ss i ent el p1 p2 d distList dLeft dRight dCenter pCenter tFound)
  (setq v_perp (list (- (sin ang)) (cos ang) 0.0))
  (setq ss (ssget "_C" (list (- (car pt) 300.0) (- (cadr pt) 300.0) 0.0)
                       (list (+ (car pt) 300.0) (+ (cadr pt) 300.0) 0.0)
                       '((0 . "LINE") (8 . "*TUONG*,*WALL*,0"))))
  (if ss
    (progn
      (setq distList '() i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              el (entget ent)
              p1 (cdr (assoc 10 el))
              p2 (cdr (assoc 11 el)))
        (if (and p1 p2)
          (progn
            (setq d (+ (* (- (car p1) (car pt)) (car v_perp)) (* (- (cadr p1) (cadr pt)) (cadr v_perp))))
            (setq distList (cons d distList))
          )
        )
        (setq i (1+ i))
      )
      (if (>= (length distList) 2)
        (progn
          (setq distList (vl-sort distList '<))
          (setq i 0)
          (while (< i (1- (length distList)))
            (setq dLeft (nth i distList)
                  dRight (nth (1+ i) distList))
            (if (and (>= (abs (- dRight dLeft)) 90.0) (<= (abs (- dRight dLeft)) 350.0))
              (progn
                (setq tFound (abs (- dRight dLeft)))
                (setq dCenter (/ (+ dLeft dRight) 2.0))
                (setq pCenter (list (+ (car pt) (* dCenter (car v_perp)))
                                    (+ (cadr pt) (* dCenter (cadr v_perp))) 0.0))
              )
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  (if pCenter (list pCenter tFound) nil)
)

;; 11. Ham thuc thi dat cua xuat ra khoi BLOCK chuyen nghiep
(defun vlt:insert-door-or-window (doorType W T_wall / doc modelSpace lay_cua lay_tuong cont pt1 pt2 pt3
                                  ang u v_test d3 dot v pA pB isWindow snapRes blkName blkRef yScale
                                  inpDist offset)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq modelSpace (vla-get-modelspace doc))
  (setq lay_cua "CUA")
  (vlt:make-layer lay_cua 4 "Continuous")
  (setq lay_tuong "TUONG")
  (if (not (tblsearch "layer" lay_tuong)) (setq lay_tuong "0"))

  (setq isWindow (or (= doorType "S1") (= doorType "S2") (= doorType "S4")))
  (setq cont T)

  (if (or (null *vcu_offset*) (< *vcu_offset* 0.0))
    (setq *vcu_offset* 300.0)
  )

  (while cont
    ;; Bat cac diem bat buoc: End(1) + Mid(2) + Center(4) + Intersect(32) + Nearest(512) = 551
    (setvar "OSMODE" 551)
    (setq pt1 (getpoint (strcat "\nVECUA - Pick diem moc (Tam cot / Tim truc / Mep tuong) dat cua (W=" (rtos W 2 0) "mm) (Enter de ket thuc): ")))
    (if (null pt1)
      (setq cont nil)
      (progn
        (setq pt1 (trans pt1 1 0))
        (setvar "OSMODE" 512) ;; Nearest
        (setq pt2 (getpoint pt1 "\nPick huong chay doc theo tuong (huong dat cua): "))
        (if (null pt2)
          (setq cont nil)
          (progn
            (setq pt2 (trans pt2 1 0))
            (setq ang (angle pt1 pt2))

            ;; Can chinh goc chuan truc giao
            (cond
              ((or (< ang 0.08) (> ang (- (* 2.0 pi) 0.08))) (setq ang 0.0))
              ((< (abs (- ang (/ pi 2.0))) 0.08) (setq ang (/ pi 2.0)))
              ((< (abs (- ang pi)) 0.08) (setq ang pi))
              ((< (abs (- ang (* 1.5 pi))) 0.08) (setq ang (* 1.5 pi)))
            )

            (setq u (list (cos ang) (sin ang) 0.0))
            (setq v_test (list (- (sin ang)) (cos ang) 0.0))

            ;; Tu dong can tim tuong va bat be day tuong thuc te
            (setq snapRes (vlt:snap-to-wall-center pt1 ang))
            (if snapRes
              (progn
                (setq pt1 (car snapRes))
                (if (and (cadr snapRes) (> (cadr snapRes) 0))
                  (setq T_wall (cadr snapRes))
                )
              )
            )

            ;; Nhap khoang cach tu tam moc den mep bat dau cua (vi du 300mm nhu trong ban ve)
            (setq inpDist (getdist pt1 (strcat "\nVECUA - Khoang cach tu moc den mep bat dau cua (mm) <" (rtos *vcu_offset* 2 0) "> (0=ngay tai moc): ")))
            (if (and inpDist (>= inpDist 0.0))
              (setq *vcu_offset* inpDist)
            )
            (setq offset *vcu_offset*)

            ;; Tinh vi tri mep bat dau cua pA va mep ket thuc pB
            (setq pA (list (+ (car pt1) (* offset (car u)))
                           (+ (cadr pt1) (* offset (cadr u)))
                           0.0))
            (setq pB (list (+ (car pA) (* W (car u)))
                           (+ (cadr pA) (* W (cadr u)))
                           0.0))

            ;; Huong mo canh cua
            (setq yScale 1.0)
            (if (not isWindow)
              (progn
                (setvar "OSMODE" 0)
                (setq pt3 (getpoint pA "\nPick huong mo canh cua (vao trong hay ra ngoai): "))
                (if pt3
                  (progn
                    (setq pt3 (trans pt3 1 0))
                    (setq d3 (list (- (car pt3) (car pA)) (- (cadr pt3) (cadr pA)) 0.0))
                    (setq dot (+ (* (car d3) (car v_test)) (* (cadr d3) (cadr v_test))))
                    (if (>= dot 0.0)
                      (setq v v_test yScale 1.0)
                      (setq v (list (- (car v_test)) (- (cadr v_test)) 0.0) yScale -1.0)
                    )
                  )
                  (setq v v_test yScale 1.0)
                )
              )
              (setq v v_test yScale 1.0)
            )

            (vla-startundomark doc)

            ;; 1. Cat duc thong tuong sat mep 100%
            (vlt:cut-wall-opening pA pB W T_wall u v lay_tuong)

            ;; 2. Tao hoac lay dinh nghia BLOCK
            (setq blkName (strcat "CUA_" (if isWindow "SO_" "DI_") doorType "_" (rtos W 2 0) "x" (rtos T_wall 2 0)))
            (vlt:get-or-create-door-block doc blkName doorType W T_wall)

            ;; 3. Chen BLOCK vao ModelSpace
            (setq blkRef (vla-insertblock modelSpace
                                          (vlax-3d-point pA)
                                          blkName
                                          1.0
                                          yScale
                                          1.0
                                          ang))
            (if blkRef (vla-put-layer blkRef lay_cua))

            (vla-endundomark doc)
            (princ (strcat "\n-> Da lap dat thanh cong BLOCK: " blkName " (Cach tam moc: " (rtos offset 2 0) "mm, Rong W=" (rtos W 2 0) "mm)!"))
          )
        )
      )
    )
  )
  (setvar "OSMODE" 675)
  (princ)
)

;; 12. LENH CD / CUADI - VE CUA DI NHANH (XUAT BLOCK)
(defun c:CD ( / opt inpW w t_wall )
  (initget "1-Cua1Canh,1 2-Cua2Canh,2 4-Cua4Canh,4 3")
  (setq opt (getkword "\nCUA DI - Chon loai cua [1-Cua1Canh/2-Cua2Canh/4-Cua4Canh] <1>: "))
  (cond
    ((or (null opt) (wcmatch opt "*1*")) (setq opt "1"))
    ((wcmatch opt "*2*")                 (setq opt "2"))
    ((or (wcmatch opt "*4*") (wcmatch opt "*3*")) (setq opt "4"))
    (t (setq opt "1"))
  )

  (setq w (cond ((= opt "1") (if *vcu_d1_w* *vcu_d1_w* 900.0))
                ((= opt "2") (if *vcu_d2_w* *vcu_d2_w* 1400.0))
                (t           (if *vcu_d4_w* *vcu_d4_w* 2400.0))))

  (setq inpW (getdist (strcat "\nCUA DI - Nhap chieu rong cua W (mm) <" (rtos w 2 0) ">: ")))
  (if (and inpW (> inpW 0)) (setq w inpW))

  (cond
    ((= opt "1") (setq *vcu_d1_w* w))
    ((= opt "2") (setq *vcu_d2_w* w))
    (t           (setq *vcu_d4_w* w))
  )

  (setq t_wall (if (and *vlt_wall_w* (> (distof *vlt_wall_w*) 0)) (distof *vlt_wall_w*) 200.0))

  (cond
    ((= opt "1") (vlt:insert-door-or-window "D1" w t_wall))
    ((= opt "2") (vlt:insert-door-or-window "D2" w t_wall))
    (t           (vlt:insert-door-or-window "D4" w t_wall))
  )
  (princ)
)
(defun c:CUADI () (c:CD))

;; 13. LENH CS / CUASO - VE CUA SO NHANH (XUAT BLOCK)
(defun c:CS ( / opt inpW w t_wall )
  (initget "1-CuaSo1Canh,1 2-CuaSo2Canh,2 4-CuaSo4Canh,4 3")
  (setq opt (getkword "\nCUA SO - Chon loai cua [1-CuaSo1Canh/2-CuaSo2Canh/4-CuaSo4Canh] <2>: "))
  (cond
    ((or (null opt) (wcmatch opt "*2*")) (setq opt "2"))
    ((wcmatch opt "*1*")                 (setq opt "1"))
    ((or (wcmatch opt "*4*") (wcmatch opt "*3*")) (setq opt "4"))
    (t (setq opt "2"))
  )

  (setq w (cond ((= opt "1") (if *vcu_s1_w* *vcu_s1_w* 800.0))
                ((= opt "2") (if *vcu_s2_w* *vcu_s2_w* 1400.0))
                (t           (if *vcu_s4_w* *vcu_s4_w* 2400.0))))

  (setq inpW (getdist (strcat "\nCUA SO - Nhap chieu rong cua so W (mm) <" (rtos w 2 0) ">: ")))
  (if (and inpW (> inpW 0)) (setq w inpW))

  (cond
    ((= opt "1") (setq *vcu_s1_w* w))
    ((= opt "2") (setq *vcu_s2_w* w))
    (t           (setq *vcu_s4_w* w))
  )

  (setq t_wall (if (and *vlt_wall_w* (> (distof *vlt_wall_w*) 0)) (distof *vlt_wall_w*) 200.0))

  (cond
    ((= opt "1") (vlt:insert-door-or-window "S1" w t_wall))
    ((= opt "2") (vlt:insert-door-or-window "S2" w t_wall))
    (t           (vlt:insert-door-or-window "S4" w t_wall))
  )
  (princ)
)
(defun c:CUASO () (c:CS))

;; 14. LENH CUA / VCU - BANG CHON LOAI CUA & THONG SO TRUC QUAN (DCL DIALOG)
(defun c:CUA ( / dcl_file dcl_id status selType wVal tVal )
  (setq dcl_file (vlt:create-door-dcl))
  (if (or (null dcl_file) (not (findfile dcl_file)))
    (progn (c:CD) (exit))
  )
  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "vlt_door_dialog" dcl_id))
    (progn (unload_dialog dcl_id) (vl-file-delete dcl_file) (c:CD) (exit))
  )

  (cond
    ((= *vcu_type* "D1") (set_tile "rb_d1" "1"))
    ((= *vcu_type* "D2") (set_tile "rb_d2" "1"))
    ((= *vcu_type* "D4") (set_tile "rb_d4" "1"))
    ((= *vcu_type* "S1") (set_tile "rb_s1" "1"))
    ((= *vcu_type* "S2") (set_tile "rb_s2" "1"))
    ((= *vcu_type* "S4") (set_tile "rb_s4" "1"))
    (t                   (set_tile "rb_d1" "1"))
  )
  (set_tile "eb_w" (rtos *vcu_w* 2 0))
  (set_tile "eb_t" (rtos *vcu_t* 2 0))

  (action_tile "rb_d1" "(set_tile \"eb_w\" \"900\")  (setq *vcu_type* \"D1\")")
  (action_tile "rb_d2" "(set_tile \"eb_w\" \"1400\") (setq *vcu_type* \"D2\")")
  (action_tile "rb_d4" "(set_tile \"eb_w\" \"2400\") (setq *vcu_type* \"D4\")")
  (action_tile "rb_s1" "(set_tile \"eb_w\" \"800\")  (setq *vcu_type* \"S1\")")
  (action_tile "rb_s2" "(set_tile \"eb_w\" \"1400\") (setq *vcu_type* \"S2\")")
  (action_tile "rb_s4" "(set_tile \"eb_w\" \"2400\") (setq *vcu_type* \"S4\")")

  (action_tile "accept"
    (strcat
      "(setq *vcu_w* (distof (get_tile \"eb_w\")))"
      "(setq *vcu_t* (distof (get_tile \"eb_t\")))"
      "(done_dialog 1)"
    )
  )
  (action_tile "cancel" "(done_dialog 0)")

  (setq status (start_dialog))
  (unload_dialog dcl_id)
  (vl-file-delete dcl_file)

  (if (= status 1)
    (progn
      (if (or (null *vcu_w*) (<= *vcu_w* 0)) (setq *vcu_w* 900.0))
      (if (or (null *vcu_t*) (<= *vcu_t* 0)) (setq *vcu_t* 200.0))
      (vlt:insert-door-or-window *vcu_type* *vcu_w* *vcu_t*)
    )
  )
  (princ)
)
(defun c:VCU () (c:CUA))

(princ "\n======================================================================")
(princ "\n  BO CONG CU VE TRUC - COT - TUONG - CUA - DIM (BO DAY DU CHUYEN NGHIEP)")
(princ "\n  1. Go: VLT  hoac LUOITRUC -> Bang ve luoi truc (+ dat cot tu dong)")
(princ "\n  2. Go: VC   hoac VCOT     -> Chen cot tai cac diem giao truc")
(princ "\n  3. Go: KT   hoac KTRUC    -> KEO TRUC DONG LOAT GIONG REVIT")
(princ "\n  4. Go: VT   hoac TUONG    -> VE TUONG (1-Quet giao diem / 2-Ve tay 2 diem)")
(princ "\n  5. Go: CD   hoac CUADI    -> VE CUA DI (XUAT KHOI BLOCK - 1 hoac 2 canh)")
(princ "\n  6. Go: CS   hoac CUASO    -> VE CUA SO (XUAT KHOI BLOCK - 1, 2, 4 canh)")
(princ "\n  7. Go: CUA  hoac VCU      -> BANG CHON LOAI CUA & THONG SO TRUC QUAN (DCL)")
(princ "\n  8. Go: DT   hoac DTRUC    -> DIM THONG MINH 3 LOP (Chi tiet cot tuong cua / Nhip truc / Tong the)")
(princ "\n======================================================================\n")
(princ)
