
CALL OpenWindowCruci

IF menu000% = 0 THEN GOTO FIN102:		'*Si no se escoge ning£n crucigrama sale

MLOC.3% = CVI(RIGHT$(Cruci$(menu000%),2))
GET 3,MLOC.3%
CALL GetCruciDat
