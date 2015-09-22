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
		AADD(aPergs,{"% Reaj. Fundo","","","mv_ch3","N",8,4,0,"G","","MV_PAR03","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"% Reaj. Desc. Funcionário","","","mv_ch4","N",8,4,0,"G","","MV_PAR04","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Novo Vl. Tx. Adm. Plano","","","mv_ch5","N",14,2,0,"G","","MV_PAR05","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
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
	Local nNovoVlFundo	:= 0
	Local nNovoVlTxAdm	:= 0
	Local nNovoVlDFunc  := 0

	dbSelectArea("SZF")
	SZF->(dbSetOrder(2))
	SZF->(dbSeek(xFilial('SZF')+MV_PAR01))
	
	While SZF->(!EOF()) .And. (SZF->(ZF_FILIAL+ZF_CODGRP) == xFilial('SZF')+MV_PAR01)
		
		nPlanos++

		nNovoVl 		:= 0
		nNovoVlFundo	:= 0
		nNovoVlTxAdm	:= 0
		nNovoVlDFunc	:= 0
		
		nNovoVl 	 	:= IIf (MV_PAR02 > 0, SZF->ZF_VALOR  + (SZF->ZF_VALOR  * (MV_PAR02 / 100)), SZF->ZF_VALOR)
		nNovoVlFundo 	:= IIf (MV_PAR03 > 0, SZF->ZF_FUNDO  + (SZF->ZF_FUNDO  * (MV_PAR03 / 100)), SZF->ZF_FUNDO)
		nNovoVlDFunc 	:= IIf (MV_PAR04 > 0, SZF->ZF_DESFUNC  + (SZF->ZF_DESFUNC  * (MV_PAR04 / 100)), SZF->ZF_DESFUNC)
		nNovoVlTxAdm 	:= IIf (MV_PAR05 > 0, MV_PAR05, SZF->ZF_TXADM)
		
		RecLock('SZF',.F.)

			SZF->ZF_VALOR   := nNovoVl
			SZF->ZF_FUNDO   := nNovoVlFundo
			SZF->ZF_DESFUNC := nNovoVlDFunc
			SZF->ZF_TXADM   := nNovoVlTxAdm

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
