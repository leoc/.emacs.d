;; (add-to-list 'org-export-latex-classes
;;              ;; beamer class, for presentations
;;              '("beamer"
;;                "\\documentclass[11pt]{beamer}\n
;;                 \\mode<{{{beamermode}}}>\n
;;                 \\usetheme{{{{beamertheme}}}}\n
;;                 \\usecolortheme{{{{beamercolortheme}}}}\n
;;                 \\beamertemplateballitem\n
;;                 \\setbeameroption{show notes}
;;                 \\usepackage[utf8]{inputenc}\n
;;                 \\usepackage[T1]{fontenc}\n
;;                 \\usepackage{hyperref}\n
;;                 \\usepackage{minted}\n
;;                 \\usepackage{color}
;;                 \\usepackage{listings}
;;                 \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
;;                       frame=single,
;;                       basicstyle=\\small,
;;                       showspaces=false,showstringspaces=false,
;;                       showtabs=false,
;;                       keywordstyle=\\color{blue}\\bfseries,
;;                       commentstyle=\\color{red},
;;                       }\n
;;                 \\usepackage{verbatim}\n
;;                 \\institute{{{{beamerinstitute}}}}\n
;;                 \\subject{{{{beamersubject}}}}\n"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\begin{frame}[fragile]\\frametitle{%s}"
;;                 "\\end{frame}"
;;                 "\\begin{frame}[fragile]\\frametitle{%s}"
;;                 "\\end{frame}")))


;; (add-to-list 'org-export-latex-classes
;;              '("letter"
;;                "\\documentclass[11pt]{letter}\n
;;                 \\usepackage[utf8]{inputenc}\n
;;                 \\usepackage[T1]{fontenc}\n
;;                 \\usepackage{minted}\n
;;                 \\usepackage{color}"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; (add-to-list 'org-export-latex-classes
;;              '("article"
;;                "\\documentclass{article}\n
;;                 \\usepackage[utf8]{inputenc}\n
;;                 \\usepackage[T1]{fontenc}\n
;;                 \\usepackage{minted}\n
;;                 \\usepackage{color}"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; (add-to-list 'org-export-latex-classes
;;              '("ieee"
;;                "\\documentclass{IEEEtran}\n
;;                 \\usepackage[utf8]{inputenc}\n
;;                 \\usepackage[T1]{fontenc}\n
;;                 \\usepackage{minted}\n
;;                 \\usepackage{color}"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))


;; (add-to-list 'org-export-latex-classes
;;              '("daidoc"
;; "\\documentclass{daidoc}
;; \\usepackage[utf8]{inputenc}
;; \\usepackage{rotating}
;; \\usepackage{datetime,color,listings,booktabs}
;; \\usepackage[ngerman]{babel}
;; \\usepackage{minted}
;; \\usepackage{listings}
;; \\definecolor{dkgreen}{rgb}{0,0.6,0}
;; \\definecolor{gray}{rgb}{0.3,0.3,0.3}
;; \\definecolor{mauve}{rgb}{0.58,0,0.82}
;; \\lstset{
;;   basicstyle=\\footnotesize\\ttfamily,
;;   keywordstyle=\\color{OliveGreen},
;;   commentstyle=\\color{gray},
;;   stepnumber=1,
;;   numbersep=5pt,
;;   backgroundcolor=\\color{white},
;;   frame=shadowbox,
;;   rulesepcolor=\\color{black},
;;   tabsize=2,
;;   captionpos=t,
;;   breaklines=true,                        % Automatic line breaking?
;;   breakatwhitespace=false,                % Automatic breaks only at whitespace?
;;   showspaces=false,                       % Dont make spaces visible
;;   showtabs=false,                         % Dont make tabls visible
;;   morekeywords={__global__, __device__},
;;   title=\\lstname,
;;   keywordstyle=\\color{black},
;;   commentstyle=\\color{gray},
;;   stringstyle=\\color{dkgreen},
;;   literate=%
;;   {Ö}{{\\\"O}}1
;;   {Ä}{{\\\"A}}1
;;   {Ü}{{\\\"U}}1
;;   {ß}{{\\\ss}}2
;;   {ü}{{\\\"u}}1
;;   {ä}{{\\\"a}}1
;;   {ö}{{\\\"o}}1
;; }
;; \\usepackage{hyperref}
;; \\makeindex
;; \\newcolumntype{C}[1]{>{\\centering\\arraybackslash}p{#1}}
;; \[NO-DEFAULT-PACKAGES\]
;; \[NO-PACKAGES\]"
;;                ("\\chapter{%s}" . "\\chapter*{%s}")
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")))
