/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Ponto de Entrada										 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Cadastro de Clientes									 !
+------------------+---------------------------------------------------------+
!Nome 			   ! SINDAFEP-PE-MA030BUT									 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para inclus�o de �tem no menu A��es Relacionadas !
!				   ! no cadastro de clientes								 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 23/12/2014												 !
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
! Programa 	! MA030BUT	! Autor !Gilson Lima 		 ! Data ! 23/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Adiciona �tem no menu A��es Relacionadas				 		 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function MA030BUT()

	Local aButtons := {} // Botoes a adicionar
	
	aAdd(aButtons,{'BUDGETY',{|| U_FAltFil()},'Fiscal - Filia��o e Desfilia��o','Fiscal - Filia��o'})
	aAdd(aButtons,{'BUDGETY',{|| U_ExbHFil()},'Fiscal - Hist�rico de Filia��o','Fiscal - Hist. Filia��o'})

	// Relat�rios de autoriza��o banc�ria
	// 341 � Ita�, 104 � CEF, 001 � Banco do Brasil.
	aAdd(aButtons,{'BUDGETY',{|| U_RFAT001()},'Autoriza��o de D�bito Banc�rio BB','Aut. Deb. Banc. BB'})
	aAdd(aButtons,{'BUDGETY',{|| U_RFAT002()},'Autoriza��o de D�bito Banc�rio CEF','Aut. Deb. Banc. CEF'})
	
Return (aButtons )