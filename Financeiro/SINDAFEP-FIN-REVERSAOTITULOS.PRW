/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Manutenção 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Financeiro												 !
+------------------+---------------------------------------------------------+
!Nome 			   ! SINDAFEP-FIN-REVERSAOTITULOS.PRW						 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para a reversão de títulos e informações dos	 !
!				   ! cadastros envolvidos									 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 06/08/2015												 !
+------------------+---------------------------------------------------------+
! ATUALIZACOES 	   															 !
+-------------------------------------------+-----------+-----------+--------+
! Descricao detalhada da atualizacao 		!Nome do    ! Analista  !Data da !
! 											!Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
! 											! 			! 			! 		 !
! 											! 			! 			! 		 !
+-------------------------------------------+-----------+-----------+--------*/

#Include "Protheus.ch"

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! REVTIT	! Autor ! Gilson Lima 		 ! Data ! 06/08/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Tela da Rotina de Reversão dos Títulos						 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function RevTit()

	Private cPerg		:= "RevTit"
	Private aPergs		:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Nr. Título","","","mv_ch01","C",9,0,0,"G","","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Cliente","","","mv_ch02","C",6,0,0,"G","","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","SA1","","","",""})
		AADD(aPergs,{"Tipo de Título","","","mv_ch03","C",1,0,0,"C","","MV_PAR03","Cotistas","","","1","","Fiscais","","","","","Planos Saúde/Odonto","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Competência","","","mv_ch04","C",7,0,0,"G","","MV_PAR04","","","","","","","","","","","","","","","","","","","","","","","","","","","","","@E 99/9999"})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	If (!Pergunte(cPerg,.T.))
		Return
	EndIf

	//Chama função geração de títulos
	If MsgYesNo("Confirma a Reversão dos Títulos?","Atenção")
		Processa({||RevTit1()},"Processando Reversão") 
	Endif 
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! REVTIT1	! Autor ! Gilson Lima 		 ! Data ! 06/08/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Rotina para reversão dos títulos gerados conforme parâmetro da !
! 			! tela de chamada														 !
+----------------------------------------------------------------------------*/
Static Function RevTit1()

	Local	cQuery	 := ''
	
	Private cNrTit	 := MV_PAR01
	Private cCliente := MV_PAR02
	Private nTpTit	 := MV_PAR03
	Private cCompet	 := Substr(MV_PAR04,4,4) + Substr(MV_PAR04,1,2)
	
	cQuery := "SELECT *"
	cQuery += " FROM " + RetSqlName('SZL') + " SZL"
	cQuery += " WHERE SZL.D_E_L_E_T_ <> '*'"

	// Filtra Nr. Título
	If AllTrim(cNrTit) != ''
		cQuery += " AND SZL.ZL_TITNUM = '" + cNrTit + "'"
	EndIf
	
	// Filtra Cliente
	If AllTrim(cCliente) != ''
		cQuery += " AND SZL.ZL_TITCLI = '" + cCliente + "'"
	EndIf
	
	// Filtra Tipo Título
	Do Case
		Case nTpTit == 1 // 1 = Cotistas
			cQuery += " AND SZL.ZL_TITPREF = 'TXM'"	
		Case nTpTit == 2 // 2 = Fiscais
			cQuery += " AND (SZL.ZL_TITPREF = 'MSL'"
			cQuery += " OR SZL.ZL_TITPREF = 'MS1'"
			cQuery += " OR SZL.ZL_TITPREF = 'MS2'"
			cQuery += " OR SZL.ZL_TITPREF = 'MS3')"	
		Case nTpTit == 3 // 3 = Planos Saúde / Odontológico
			cQuery += " AND (SZL.ZL_TITPREF = 'PLN'"
			cQuery += " OR SZL.ZL_TITPREF = 'PLO'"
			cQuery += " OR SZL.ZL_TITPREF = 'PLS')"
	EndCase
		
	// Filtra Competência
	If AllTrim(cCompet) != ''
		cQuery += " AND SZL.ZL_COMPET = '" + cCompet + "'"
	EndIf
	
	Alert('teste')

Return