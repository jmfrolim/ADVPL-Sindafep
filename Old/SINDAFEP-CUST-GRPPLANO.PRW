/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Manuten��o 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Plano de Sa�de											 !
+------------------+---------------------------------------------------------+
!Nome 			   ! GRPPLANO 												 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Cadastro de Grupo de Planos							 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 18/11/2014												 !
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
! Programa 	! AXCADSZD  ! Autor !Gilson Lima 		 ! Data ! 18/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Tela de Grupo de Planos - axCadastro							 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function AxCadSZD()

	Local cAlias	:= "SZD"
	Local cTitulo	:= "Cadastro de Grupo de Planos"
	
	dbSelectArea(cAlias)
	(cAlias)->(dbSetOrder(1))
	
	AxCadastro(cAlias,cTitulo,"U_VldExSZD()","U_VldAlSZD()")

Return Nil

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! VldExSZD  ! Autor !Gilson Lima 		 ! Data ! 18/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao Auxiliar												 !
! 			! Valida Exclusao de Registro na SZD							 !
+----------------------------------------------------------------------------*/
User Function VldExSZD()

	Local lRet	:= .T.
	
Return(lRet)

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! VldAlSZD  ! Autor !Gilson Lima 		 ! Data ! 18/11/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao Auxiliar												 !
! 			! Valida Alteracao de Registro na SZD							 !
+----------------------------------------------------------------------------*/
User Function VldAlSZD()

	Local lRet	:= .T.
	
Return(lRet)