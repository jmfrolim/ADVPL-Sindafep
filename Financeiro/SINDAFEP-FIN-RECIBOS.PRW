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
!Nome 			   ! FIN-RECIBOS 											 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para a manutenção de Recibos do Financeiro		 !
!				   !  														 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 30/03/2015												 !
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
! Programa 	! FINREC	! Autor !Gilson Lima 		 ! Data ! 30/03/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Tela da Rotina Recibos										 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function FinRec()

	Local aCores	:= {{"SZR->ZR_STATUS=='1'",'BR_VERMELHO'},;	// Ativo
						{"SZR->ZR_STATUS=='2'",'BR_PRETO'}}	// Inativo
	
	Private cCadastro	:= 'Recibos'
	Private aRotina		:= {{"Pesquisar", 	"AxPesqui" 	,0,1},;
							{"Visualizar", 	"U_FinRecB"	,0,2},;
							{"Legenda",		"U_RecLeg"	,0,6}}

	dbSelectArea("SZR")
	SZR->(dbSetOrder(1))

	mBrowse( ,,,,"SZR",,,,,,aCores,,,,,,,,)

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! RECLEG	! Autor !Gilson Lima 		 ! Data ! 30/03/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Legenda										 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function RECLEG()

	BrwLegenda(cCadastro,"Status", { {"BR_VERMELHO","Ativo"},;
									 {"BR_PRETO","Cancelado"}})

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! FINRECB	! Autor !Gilson Lima 		 ! Data amdin! 30/03/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Tela da Processamento da rotina Recibos						 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function FinRecB(cAlias,nRecno,nOpc)
	
	Do Case
		Case nOpc == 2
			cMensagem := 'Visualizar'
			U_RRecib000(2,nRecno)
	EndCase

Return