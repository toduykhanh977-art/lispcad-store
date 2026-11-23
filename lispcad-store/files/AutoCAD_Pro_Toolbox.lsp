;;; ====================================================================
;;; BỘ LISPCAD PRO - TỰ ĐỘNG HÓA BẢN VẼ AUTOCAD
;;; Lưu ý: Bạn có thể thay thế toàn bộ nội dung file này bằng mã Lisp của bạn!
;;; ====================================================================

(vl-load-com)

;;; --------------------------------------------------------------------
;;; Lệnh 1: TL - Tính tổng chiều dài tất cả các đối tượng được chọn (Line, Pline, Arc, Spline)
;;; --------------------------------------------------------------------
(defun c:TL (/ ss i total len obj)
  (princ "\n[LispCAD Pro] Chọn các đối tượng cần tính tổng chiều dài:")
  (if (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE,ELLIPSE,CIRCLE"))))
    (progn
      (setq total 0.0)
      (setq i (sslength ss))
      (while (> i 0)
        (setq i (1- i))
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (setq len (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
        (setq total (+ total len))
      )
      (alert (strcat "TỔNG CHIỀU DÀI: " (rtos total 2 2) " mm"))
      (princ (strcat "\n[LispCAD Pro] Tổng chiều dài: " (rtos total 2 2)))
    )
    (princ "\nKhông có đối tượng nào được chọn!")
  )
  (princ)
)

;;; --------------------------------------------------------------------
;;; Lệnh 2: STT - Đánh số thứ tự tăng dần tự động vào bản vẽ
;;; --------------------------------------------------------------------
(defun c:STT (/ start step pt h)
  (setq start (getint "\n[LispCAD Pro] Nhập số bắt đầu <1>: "))
  (if (null start) (setq start 1))
  (setq step (getint "\n[LispCAD Pro] Bước nhảy <1>: "))
  (if (null step) (setq step 1))
  (setq h (getdist "\n[LispCAD Pro] Chiều cao chữ <2.5>: "))
  (if (null h) (setq h 2.5))
  
  (while (setq pt (getpoint (strcat "\nClick vị trí đặt số [" (itoa start) "] (Enter để dừng): ")))
    (entmake (list '(0 . "TEXT")
                   (cons 10 pt)
                   (cons 40 h)
                   (cons 1 (itoa start))
                   '(72 . 1)
                   '(73 . 2)
                   (cons 11 pt)))
    (setq start (+ start step))
  )
  (princ)
)

;;; --------------------------------------------------------------------
;;; Lệnh 3: DTL - Tính diện tích hình kín và chèn text kết quả
;;; --------------------------------------------------------------------
(defun c:DTL (/ pt areaObj areaVal)
  (setq pt (getpoint "\n[LispCAD Pro] Click vào miền kín cần tính diện tích: "))
  (if pt
    (progn
      (command "-boundary" pt "")
      (setq areaObj (vlax-ename->vla-object (entlast)))
      (setq areaVal (vlax-get-property areaObj 'Area))
      (entdel (entlast))
      (alert (strcat "DIỆN TÍCH MIỀN: " (rtos (/ areaVal 1000000.0) 2 3) " m² (" (rtos areaVal 2 2) " mm²)"))
    )
  )
  (princ)
)

(princ "\n=======================================================")
(princ "\n  ĐÃ TẢI THÀNH CÔNG BỘ LISPCAD PRO!")
 các lệnh hỗ trợ: TL (Tổng chiều dài), STT (Đánh số), DTL (Tính diện tích)")
(princ "\n=======================================================\n")
(princ)
