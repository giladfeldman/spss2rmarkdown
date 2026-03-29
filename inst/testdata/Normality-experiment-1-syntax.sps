* Encoding: UTF-8.
***************************
* comparing types of normality - social norms versus past behavior
***************************.

compute condition = (~missing(actslost))*1+(~missing(actswon))*2+(~missing(inasint2))*3+(~missing(Q391))*4.
EXECUTE.

VALUE LABELS condition
1 'Action society - lost last game - action expectations'
2 'Action society - won last game - inaction expectations'
3 'Inaction society - lost last game - action expectations'
4 'Inaction society - won last game - inaction expectations'.

missing values condition (0).

FREQUENCIES condition.

recode condition (1,2=1) (3,4=2) into soccon.
recode condition (1,3=1) (2,4=2) into wincon.


VALUE LABELS soccon
1 'Action society'
2 'Inaction society'.

VALUE LABELS wincon
1 'Lost last game - lost last games - action expectations'
2 'Action society - won last game - inaction expectations'
3 'Inaction society - lost last game - action expectations'
4 'Inaction society - won last game - inaction expectations'.

VALUE LABELS soccon
1 'Action society'
2 'Inaction society'.

VALUE LABELS wincon
1 'Lost last game'
2 'Won last game'.

variable labels 
soccon 'Action-inaction society'
wincon 'Action-inaction past behavior expectations'.

FREQUENCIES soccon wincon.

compute quiz=(soccon=1)*actsquiz+(soccon=2)*inasquiz.
EXECUTE.

variable labels
quiz 'Society manipulation check'.

* check manipulation checks.
CROSSTABS
  /TABLES=soccon BY quiz
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT ROW 
  /COUNT ROUND CELL.

compute regret=(soccon=1)*actsregret+(soccon=2)*inasregret.
compute joy=(soccon=1)*actsjoy+(soccon=2)*inasjoy.
EXECUTE.

variable labels
regret 'higher regret for action'
joy 'higher joy for action'.

UNIANOVA regret BY soccon wincon
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PLOT=PROFILE(soccon*wincon)
  /EMMEANS=TABLES(soccon) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(wincon) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(soccon*wincon) 
  /PRINT=OPOWER ETASQ HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=soccon wincon soccon*wincon.

UNIANOVA joy BY soccon wincon
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PLOT=PROFILE(soccon*wincon)
  /EMMEANS=TABLES(soccon) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(wincon) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(soccon*wincon) 
  /PRINT=OPOWER ETASQ HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=soccon wincon soccon*wincon.
