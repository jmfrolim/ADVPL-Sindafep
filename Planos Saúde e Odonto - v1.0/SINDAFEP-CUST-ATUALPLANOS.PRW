/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Manutenção 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Plano de Saúde											 !
+------------------+---------------------------------------------------------+
!Nome 			   ! ATUALPLANOS											 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Reajusta valores de planos								 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 05/01/2015												 !
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
! Programa 	! ATVLPLAN  ! Autor !Gilson Lima 		 ! Data ! 05/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Capta valores de Reajuste dos planos 									 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function AtVlPlan()

	Private cPerg		:= "ATVLPLAN"
	Private aPergs		:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
		AADD(aPergs,{"Grupo","","","mv_ch1","C",6,0,0,"G","NAOVAZIO()","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","SZDGRP","","","",""})
		AADD(aPergs,{"% Reaj. Plano","","","mv_ch2","N",8,4,0,"G","","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"% Reaj. SOS","","","mv_ch3","N",8,4,0,"G","","MV_PAR03","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"% Reaj. Aero","","","mv_ch4","N",8,4,0,"G","","MV_PAR04","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"% Reaj. Fundo","","","mv_ch5","N",8,4,0,"G","","MV_PAR05","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Novo Vl. Tx. Adm. Plano","","","mv_ch6","N",14,2,0,"G","","MV_PAR06","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Novo Vl. Tx. Adm. SOS","","","mv_ch7","N",14,2,0,"G","","MV_PAR07","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Novo Vl. Tx. Adm. Aero","","","mv_ch8","N",14,2,0,"G","","MV_PAR08","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)

	//Chama função de Reajuste dos Planos
	If MsgYesNo("Confirma o reajuste dos planos do grupo selecionado?","Atenção")
		Processa({||U_AtVlPl()}) 
	Endif 
	
Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! ATVLPL    ! Autor !Gilson Lima 		 ! Data ! 05/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Atualiza valores de planos 									 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function AtVlPl()

	Local nPlanos 		:= 0
	Local nNovoVl 		:= 0
	Local nNovoVlSOS	:= 0
	Local nNovoVlAero	:= 0
	Local nNovoVlFundo	:= 0
	Local nNovoVlTxAdm	:= 0
	Local nNovoVlTxAdmS := 0 // Tx Adm SOS
	Local nNovoVlTxAdmA := 0 // Tx Adm Aero

	dbSelectArea("SZF")
	SZF->(dbSetOrder(2))
	SZF->(dbSeek(xFilial('SZF')+MV_PAR01))
	
	While SZF->(!EOF()) .And. (SZF->(ZF_FILIAL+ZF_CODGRP) == xFilial('SZF')+MV_PAR01)
		
		nPlanos++

		nNovoVl 		:= 0
		nNovoVlSOS		:= 0
		nNovoVlAero		:= 0
		nNovoVlFundo	:= 0
		nNovoVlTxAdm	:= 0
		nNovoVlTxAdmS   := 0
		nNovoVlTxAdmA   := 0	
		
		nNovoVl 	 	:= IIf (MV_PAR02 > 0, SZF->ZF_VALOR  + (SZF->ZF_VALOR  * (MV_PAR02 / 100)), SZF->ZF_VALOR)
		nNovoVlSOS	 	:= IIf (MV_PAR03 > 0, SZF->ZF_VLSOS  + (SZF->ZF_VLSOS  * (MV_PAR03 / 100)), SZF->ZF_VLSOS)
		nNovoVlAero	 	:= IIf (MV_PAR04 > 0, SZF->ZF_VLAERO + (SZF->ZF_VLAERO * (MV_PAR04 / 100)), SZF->ZF_VLAERO)
		nNovoVlFundo 	:= IIf (MV_PAR05 > 0, SZF->ZF_FUNDO  + (SZF->ZF_FUNDO  * (MV_PAR05 / 100)), SZF->ZF_FUNDO)
		nNovoVlTxAdm 	:= IIf (MV_PAR06 > 0, MV_PAR06, SZF->ZF_TXADM)
		nNovoVlTxAdmS 	:= IIf (MV_PAR07 > 0, MV_PAR07, SZF->ZF_TXSOS)
		nNovoVlTxAdmA 	:= IIf (MV_PAR08 > 0, MV_PAR08, SZF->ZF_TXAERO)
		
		RecLock('SZF',.F.)

			SZF->ZF_VALOR  := nNovoVl
			SZF->ZF_VLSOS  := nNovoVlSOS
			SZF->ZF_VLAERO := nNovoVlAero
			SZF->ZF_FUNDO  := nNovoVlFundo
			SZF->ZF_TXADM  := nNovoVlTxAdm
			SZF->ZF_TXSOS  := nNovoVlTxAdmS
			SZF->ZF_TXAERO := nNovoVlTxAdmA

		SZF->(MsUnLock())
		
		SZF->(dbSkip())
	End
	
	SZF->(dbCloseArea())
	
	If (nPlanos > 0)
		MsgInfo("Foram reajustados " + cValToChar(nPlanos) + " ítens.","Atenção")
	Else
		Alert('Nenhum reajuste efetuado! Verifique os parâmetros')
	EndIf
	
Return
