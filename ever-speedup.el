;; from: [http://www.bookshelf.jp/soft/meadow_31.html#SEC427]

;; 10 回ごとに加速
(defvar ever-scroll-speedup-count 6)
;; 10 回下カーソルを入力すると，次からは 1+1 で 2 行ずつの
;; 移動になる
(defvar ever-scroll-speedup-rate 1)
;; 800ms 経過したら通常のスクロールに戻す
(defvar ever-scroll-speedup-time 800)

;; 以下，内部変数
(defvar ever-scroll-step-default 1)
(defvar ever-scroll-step-count 1)
(defvar ever-scroll-speedup-zero (current-time))

(defun ever-scroll-speedup-setspeed ()
  (let* ((now (current-time))
         (min (- (car now)
                 (car ever-scroll-speedup-zero)))
         (sec (- (car (cdr now))
                 (car (cdr ever-scroll-speedup-zero))))
         (msec
          (/ (- (car (cdr (cdr now)))
                (car
                 (cdr (cdr ever-scroll-speedup-zero))))
                     1000))
         (lag
          (+ (* 60000 min)
             (* 1000 sec) msec)))
    (if (> lag ever-scroll-speedup-time)
        (progn
          (setq ever-scroll-step-default 1)
          (setq ever-scroll-step-count 1))
      (setq ever-scroll-step-count
            (+ 1 ever-scroll-step-count)))
    (setq ever-scroll-speedup-zero (current-time))))

(defun ever-scroll-speedup-next-line (arg)
  (if (= (% ever-scroll-step-count
            ever-scroll-speedup-count) 0)
      (setq ever-scroll-step-default
            (+ ever-scroll-speedup-rate
               ever-scroll-step-default)))
  (if (string= arg 'next)
      (line-move ever-scroll-step-default)
    (line-move (* -1 ever-scroll-step-default))))


(defadvice ever-next-line
  (around ever-next-line-speedup activate)
  (if (and (string= last-command 'ever-next-line)
           (interactive-p))
      (progn
        (ever-scroll-speedup-setspeed)
        (condition-case err
            (ever-scroll-speedup-next-line 'next)
          (error
           (if (and
                next-line-add-newlines
                (save-excursion
                  (end-of-line) (eobp)))
               (let ((abbrev-mode nil))
                 (end-of-line)
                 (insert "\n"))
             (line-move 1)))))
    (setq ever-scroll-step-default 1)
    (setq ever-scroll-step-count 1)
    ad-do-it))

(defadvice ever-previous-line
  (around ever-previous-line-speedup activate)
  (if (and
       (string= last-command 'ever-previous-line)
       (interactive-p))
      (progn
        (ever-scroll-speedup-setspeed)
        (ever-scroll-speedup-next-line 'previous))
    (setq ever-scroll-step-default 1)
    (setq ever-scroll-step-count 1)
    ad-do-it))

(provide 'ever-speedup)